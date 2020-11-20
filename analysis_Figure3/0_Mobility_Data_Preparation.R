## -----------------------------------------------------------------------------
## Step 0: Function Prep
## -----------------------------------------------------------------------------

# Loading Required Libraries
library(gbm); library(dismo); library(conflicted); library(gtools); library(lubridate);
library(dplyr); library(tidyverse)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("area", "patchwork")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")

download_url <- function(url) {
  tryCatch({
    tf <- tempfile()
    code <- download.file(url, tf, mode = "wb")
    if (code != 0) {
      stop("Error downloading file")
    }
  },
  error = function(e) {
    stop(sprintf("Error downloading file '%s': %s, please check %s",
                 url, e$message))
  })
  return(tf)
}

match_clean <- function(a,b, quiet=TRUE){
  a <- gsub("[[:punct:][:space:]]","",tolower(stringi::stri_trans_general(a, "latin-ascii")))
  b <- gsub("[[:punct:][:space:]]","",tolower(stringi::stri_trans_general(b, "latin-ascii")))
  ret <- match(a,b)
  if(sum(is.na(ret)>0)){
    dists <- stringdist::seq_distmatrix(lapply(a,utf8ToInt),lapply(b,utf8ToInt))
    ret[is.na(ret)] <- apply(dists[which(is.na(ret)),,drop=FALSE],1,which.min)
    if(!quiet){
      return(unique(cbind(a,b[ret])))
    }
  }
  return(ret)
}

## -----------------------------------------------------------------------------
## Step 1: Data Loading
## -----------------------------------------------------------------------------
date <- Sys.Date()
date <- as.Date(date)

# Loading Google Mobility Data
goog_tf <- download_url("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
goog <- read.csv(goog_tf, stringsAsFactors = FALSE)
goog$iso3c <- countrycode::countrycode(goog$country_region, "country.name", "iso3c")

# N.B. The date format changed recently in Google and may change again. Look out for errors related to this

# the subregions change over time so catch for what should be included if this breaks.
# we just want to filter to country level measures
mob <-  goog %>%
  filter(sub_region_1 == "" & sub_region_2 == "" & metro_area == "") %>%
  mutate(overall = 1/4 * retail_and_recreation_percent_change_from_baseline +
           1/4 * grocery_and_pharmacy_percent_change_from_baseline +
           1/4 * transit_stations_percent_change_from_baseline +
           1/4 * workplaces_percent_change_from_baseline) %>%
  mutate(date = as.Date(date, format="%Y-%m-%d")) %>%
  select(country_region, iso3c, date, overall)

# Loading World Bank Metadata (Charlie can update to url as needed)
wb_metadata <- read.csv("cluster_running/Inputs/World_Bank_Country_Metadata.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = TRUE) %>%
  rename(ISO = country_code) %>%
  select(ISO, income_group, region) %>%
  filter(region != "")

# Loading in ACAPs Data
acap_site <- "http://www.acaps.org/covid19-government-measures-dataset"
xml <- xml2::read_html(acap_site)
url <- rvest::html_attr(rvest::html_nodes(xml, ".file a"), "href")
acap_tf <- download_url(url)

# HTPPS ISSUE - TEMPORARY FIX TO MANUALLY PLACE acaps download into directory
#acap_tf <- "acaps.xlsx"
sheets <- readxl::excel_sheets(acap_tf)
if("Database" %in% sheets) {
  acap <- readxl::read_excel(acap_tf, progress = FALSE, sheet = "Database")
} else if("Dataset" %in% sheets) {
  acap <- readxl::read_excel(acap_tf, progress = FALSE, sheet = "Dataset")
}

# rename starting _
names(acap) <- gsub("^_","",names(acap))

# country name fixes. We want to use the ISO3C eventually but there are typos...
acap$ISO <- countrycode::countrycode(acap$COUNTRY, "country.name", "iso3c",
                                     custom_match = c("Eswatini"="SWZ", "Micronesia"="FSM","CAR"="CAF"))

## -----------------------------------------------------------------------------
## Step 1: Data Loading
## -----------------------------------------------------------------------------
ACAPs_measure <- acap %>%
  rename(country = COUNTRY, measure = MEASURE, type = LOG_TYPE, date = DATE_IMPLEMENTED) %>%
  select(ISO, measure, type, date) %>%
  filter(measure != "", type != "") %>%
  mutate(measure = as.numeric(factor(tolower(measure))), type = as.numeric(factor(type))) %>%
  mutate(combined = as.factor(paste0("m_",type, "_", measure))) %>%
  mutate(date = as.Date(date)) %>%
  select(ISO, date, combined) %>%
  pivot_wider(names_from = combined, values_from = combined) %>%
  filter(!is.na(date))

## -----------------------------------------------------------------------------
## Step 2: Data Cleaning
## -----------------------------------------------------------------------------

# Renaming Columns of ACAPs Data and Inputting In Number of Each Measure Put In On Each Date
measures <- colnames(ACAPs_measure)[-(1:2)]
for (i in 1:length(measures)) {
  index <- measures[i]
  ACAPs_measure[, index] <- unlist(lapply(ACAPs_measure[[index]], length))
}

# Tracking Cumulative Number of Each Type of Measure Implemented and Creating New Rows for Dates
# Not Present in Each Country
new_ACAPs_measure <- ACAPs_measure %>%
  group_by(ISO) %>%
  arrange(date) %>%
  mutate_at(vars(-ISO, -date), funs(cumsum(.))) %>%
  complete(date = seq.Date(min(ACAPs_measure$date), max(ACAPs_measure$date), by = "days")) %>%
  mutate_at(vars( -ISO, -date), funs(replace(., row_number() == 1, 0)))

# Filling Those Newly Created Rows With the Value In the Row Above (As We're Tracking Cumulative)
column_names <- colnames(new_ACAPs_measure)[-c(1:2)]
new_ACAPs_cat <- fill(new_ACAPs_measure, column_names, .direction = c("down"))

# Joining This Data to the World Bank and Google Mobility Data
overall <- new_ACAPs_cat %>%
  left_join(mob, by = c("ISO" = "iso3c", "date" = "date")) %>%
  filter(!is.na(overall)) %>%
  left_join(wb_metadata, by = "ISO") %>%
  select(-country_region)

overall_test <- overall %>%
  ungroup(ISO) %>%
  select(overall, everything(), -ISO, -date)

# # for some reason what used to be factors are now characters...
# classes <- unlist(lapply(overall_test, class))
# for(i in seq_along(classes)) {
#   if(classes[i] == "character") {
#     overall_test[[i]] <- as.factor(overall_test[[i]])
#   }
# }
#

## -----------------------------------------------------------------------------
## Step 3: BRT
## -----------------------------------------------------------------------------

# Running the BRT
tree_complexity <- 8
bag_fraction <- 0.5
short_run <- FALSE
if (short_run) {
  max_trees <- 30
} else {
  max_trees <- 3000
}
learning_rate <- 0.05
x <- as.data.frame(overall_test)

brt <- gbm.step(data = x,
                gbm.x = 2:ncol(x),
                gbm.y = 1,
                family = "gaussian",
                tree.complexity = tree_complexity,
                learning.rate = learning_rate,
                bag.fraction = bag_fraction,
                max.trees = max_trees,
                n.folds = 5,
                plot.main = FALSE,
                plot.folds = FALSE)

predicted <- predict.gbm(brt, x[, c(2:ncol(x))], n.trees = brt$gbm.call$best.trees, type = "response")
plot(overall$overall, predicted, ylim = c(-100, 20), xlim = c(-100, 20), pch = 20, cex = 2, ylab = "")

# let's create the complete data set and predict where data is missing
output_data <- new_ACAPs_cat %>%
  left_join(mob, by = c("ISO" = "iso3c", "date" = "date")) %>%
  left_join(wb_metadata, by = "ISO") %>%
  ungroup(ISO) %>%
  select(overall, everything(), -country_region)
predicted <- predict.gbm(brt, output_data[which(is.na(output_data$overall)), c(4:(ncol(output_data)))], n.trees = brt$gbm.call$best.trees, type = "response")
output_data$observed <- !is.na(output_data$overall)
output_data$overall[which(is.na(output_data$overall))] <- predicted
output_data$all_overall <- predict.gbm(brt, output_data[, c(4:(ncol(output_data)))], n.trees = brt$gbm.call$best.trees, type = "response")

# adding back in countries that have google mobility data but no ACAPS data (and hence no predictions)
yes_mob_no_acaps_countries <- unique(mob$iso3c)[!(unique(mob$iso3c) %in% unique(new_ACAPs_cat$ISO))]
yes_mob_no_acaps_countries <- yes_mob_no_acaps_countries[!is.na(yes_mob_no_acaps_countries)]
yes_mob_no_acaps <- mob %>%
  filter(iso3c %in% yes_mob_no_acaps_countries) %>%
  group_by(iso3c, country_region) %>%
  complete(date = seq.Date(min(output_data$date), max(output_data$date), by = "days")) %>%
  mutate(check = overall) %>%
  fill(overall, .direction = "updown") %>%
  left_join(wb_metadata, by = c("iso3c" = "ISO")) %>%
  rename("ISO" = "iso3c") %>%
  ungroup()
yes_mob_no_acaps$observed <- !is.na(yes_mob_no_acaps$check)
yes_mob_no_acaps$all_overall <- yes_mob_no_acaps$overall
yes_mob_no_acaps <- select(yes_mob_no_acaps, ISO, date, overall, all_overall, observed, income_group)

## -----------------------------------------------------------------------------
## Step 4: Data Formatting
## -----------------------------------------------------------------------------

## Format the data into the correct form the orderly task
res <- select(output_data, ISO, date, overall, all_overall, observed, income_group) %>%
  rbind(yes_mob_no_acaps) %>%
  mutate(overall = (overall+100)/100,
         all_overall = (all_overall+100)/100) %>%
  rename(C = overall,
         C_predict = all_overall,
         iso3c = ISO)
res <- split.data.frame(res, res$iso3c)

# Add in missing countries
nms <- unique(squire::population$iso3c)
res_no <- lapply(nms[!nms %in% names(res)], function(x) {
  return(data.frame())
})
names(res_no) <- nms[!nms %in% names(res)]

# group them and make sure they are all data frames
res <- append(res, res_no)
res <- lapply(res, function(x) {

  if(nrow(x) > 0) {
    return(as.data.frame(x))
  } else {
    return(x)
  }
})

# make sure they all have a pre first switch date
res <- lapply(res,function(x){

  if(length(unique(x$C)) == 1) {
    x <- rbind(x[1,],x)
    x$date[1] <- x$date[2]-1
    x[1,"C"] <- 1
  }
  return(x)

})

# adjust the unobserved data in the future to be the mean of the last 2 weeks
for(r in seq_along(res)) {

  if(any(res[[r]]$observed)) {

    # first for prior to the first mobility date use the mean of the first week
    lw <- res[[r]]$C[which(res[[r]]$observed)][1:7]
    res[[r]]$C[1:(which(res[[r]]$observed)[1]-1)] <- mean(lw)

    # for the end use the mean of the final 2 weeks
    rw <- tail(res[[r]]$C[which(res[[r]]$observed)], 14)
    rw_end <- tail(which(res[[r]]$observed),1)
    rw_7 <- res[[r]]$C[(rw_end+1):(rw_end+7)]
    res[[r]]$C[(rw_end+1):nrow(res[[r]])] <- mean(rw)

  }

}

saveRDS(res, "cluster_running/Inputs/google_brt.rds")
saveRDS(brt, "cluster_running/Inputs/google_brt_model.rds")
saveRDS(output_data, "cluster_running/Inputs/output_data.rds")
