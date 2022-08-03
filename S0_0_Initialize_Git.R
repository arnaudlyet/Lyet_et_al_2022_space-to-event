maxFileSize = 200    # in MB: Max File Size authorized for Upload
options(shiny.maxRequestSize = maxFileSize * 1024^2) 

# Load other packages 
library(magrittr)
library(stringr)
library(dplyr)
library(lubridate)
library(MASS)
library(msm)
library(SpatialEpi)
library(unmarked)
library(modelr)
library(AICcmodavg)
library(knitrProgressBar)
library(ggplot2)



# Source general function in global environment 
source_url("https://raw.githubusercontent.com/arnaudlyet/Lyet_et_al_2022_space-to-event/main/S1b_fun_STE.R")
source_url("https://raw.githubusercontent.com/arnaudlyet/Lyet_et_al_2022_space-to-event/main/S2b_get_estimates_fun.R")
source_url("https://raw.githubusercontent.com/arnaudlyet/Lyet_et_al_2022_space-to-event/main/S2d_unmarked_fun_general.R")
source_url("https://raw.githubusercontent.com/arnaudlyet/Lyet_et_al_2022_space-to-event/main/S2e_unmarked_fun_models.R")

## Functions
# Declare a function to homogenize species names
clean_names <- function(any_words){
  any_words %>%
    iconv(., to="ASCII//TRANSLIT") %>% 
    tolower %>% 
    stringr::str_replace_all(., " ", "_")  %>% 
    stringr::str_replace_all(., "'", "_") %>%
    stringr::str_replace_all(., "-", "_") %>%
    stringr::str_replace_all(., ":", "") %>%
    stringr::str_replace_all(., "/", "") %>%
    stringr::str_replace_all(., "«", "") %>%
    stringr::str_replace_all(., "»", "") %>%
    stringr::str_replace_all(., "__", "_") 
}



# Function to check if the format of date is consistent with the format provided
clean_dates <- function(xDATE, xFORMAT){
  tryCatch({
    xDATE <- xDATE %>% lubridate::parse_date_time(xFORMAT)
  }, warning = function(w) {
    warn <- "ERROR: The date format does not seem correct. Please check and correct."
    return(warn)
  }, error = function(e) {
    warn <- "ERROR: The date format does not seem correct. Please check and correct."
    return(warn)
  }, finally = {
  })
} # end of function clean_dates



# Function to check data files selected
check_image <- function(IMAGE){
  tryCatch({
    dplyr::select(IMAGE[1:3,], common_name, timestamp, number_of_animals)
  }, warning = function(w) {
    return("error")
  }, error = function(e) {
    return("error")
  }, finally = {
  })
} # end function check_image

check_deploy <- function(DEPLOY){
  tryCatch({
    dplyr::select(DEPLOY[1:3,], placename, longitude, latitude, start_date, end_date)
  }, warning = function(w) {
    return("error")
  }, error = function(e) {
    return("error")
  }, finally = {
  })
} # end function check_deploy

## 
msg_error_data_selected <- 'ERROR : It looks like you did not select the right file. 
Or some fields are missing or misnamed in your file. 
Please check, correct and reload'


### get the space to event area 
get_Sja <- function(x) {if (max(x)==0) {NA} else {
  deptd <- names(x)[1:min(which(x > 0))] # get the names of the deployments till the detection
  sum(cta[deptd,])}} # select ct from the ct area vector and sum up area

# remove na
rm_na <- function(x) x[!is.na(x)]


len_no_na <- function(x) length( which(!is.na(x)) )

# Function to overcome NA's that should be zero: a photo exist so camera was obviously active !
no_na <- function(x) ifelse(is.na(x), 0, x)


# function for random time selection per day. t_laps = min time between consec pic
rnd2.sample <- function(avail=all_times, n_timelapse=n_timelapse, t_laps=t_laps) {
  
  jj <- 1
  tsamp <- rep(NA,n_timelapse)
  
  while (length(avail) > 0) {
    t <- sample(avail, size = 1)
    nt <- seq(t-t_laps, t+t_laps, by = "sec")
    nt <- nt[nt %in% avail]
    avail <- avail[-match(nt, avail)]
    tsamp[jj] <- t %>% strftime(format = "%H:%M:%S", tz = "UTC")
    jj <- jj+1
  }
  tsamp <- list(sort(tsamp[1:int_per_day]))
  return(tsamp)
} # end function

# function for regular time selection per day
rnd.sample <- function(avail=all_times, n_timelapse=n_timelapse) {
  
  t <- sample(length(avail), size = n_timelapse)
  tsamp <- avail[t] %>% strftime(format = "%H:%M:%S", tz = "UTC")
  tsamp <- list(tsamp)
  return(tsamp)

} # end function



# function for regular time selection per day
reg.sample <- function(avail=all_times, n_timelapse=n_timelapse) {
  
  window <- floor(length(avail)/n_timelapse -1)
  tsamp <- c(0:(int_per_day-1))*window
  t <- sample(0:window, size = 1)
  tsamp <- tsamp + t
  tsamp <- avail[tsamp] %>% strftime(format = "%H:%M:%S", tz = "UTC")
  tsamp <- list(tsamp)
  return(tsamp)
} # end function



# function to update progress bar. borrowed from example by @hrbrmstr
arduously_long_nchar <- function(input_var, .pb=NULL) {  
  update_progress(.pb)  
  Sys.sleep(0.5)  
  nchar(input_var)
} # end function



# Arbitrary area, for STE scaling 
Area = 100
