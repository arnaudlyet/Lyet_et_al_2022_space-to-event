##################################################################
## Project: Spatial To Event for Prey density estimate
##
## What does the code do:
## Run the STE model on motion triggered camera trap data
## Model with bootstraping, allowing for variable camera viewshed
##
## Dependencies: need to run first the DataPrep_Palencia.R file
##
## Authors: Arnaud Lyet
## File created: December 21 2021
## Last modified: December 22 2021
## R version 4.0.3 (2020-10-10)
##################################################################

rm(list = ls(all = T))


library(devtools)

# Load functions, source file and data
source_url("https://raw.githubusercontent.com/arnaudlyet/Lyet_et_al_2022_space-to-event/main/S0_0_Initialize_Git.R")

load("BC_dataset.RData")


########################################################################################
### Data prep, period adjustment and declaration of variables and parameters

# Create file to write all the results
resall <- NULL

# Subset data for that species only
(species <-
    unique(dat0$common_name)[1])# red deer = 1, red fox = 2, wild boar = 3
dat <- dat0
head(dat) # Check data

# check daily activity pattern and adjust the daily period for analysis
temp <-
  data.frame(date.time = as.POSIXct(strptime(dat$time, "%H:%M:%S")))
hist(temp$date.time, "hours", format = "%H")

# Identify all possible HH:MM:SS
tstart_txt <- "03:00:00"
tend_txt <- "23:59:59"
time_start <- parse_date_time(paste(tstart_txt, "UTC"), "HMS")
time_end <- parse_date_time(paste(tend_txt, "UTC"), "HMS")

tbkstart_txt <- "12:00:00" #
tbkend_txt <- "19:00:00"   #
time_break_start <-
  parse_date_time(paste(tbkstart_txt, "UTC"), "HMS")
time_break_end <- parse_date_time(paste(tbkend_txt, "UTC"), "HMS")

# Calculate total time per day in min
period1 <- period_to_seconds(hms(tend_txt)) + 1 - period_to_seconds(hms(tstart_txt))
period2 <- period_to_seconds(hms(tbkend_txt)) - period_to_seconds(hms(tbkstart_txt))

tot.seconds <- (period1 - period2)
tot.minutes <- tot.seconds / 60 # total numbers of minutes

### Declare parameters of the model
Nboot = 20 # number of bootstraps
det.period = c(1)#, 5, 10, 30, 60)# window of detection in second

interval <- 10 # average interval between occasion

# number of daily detection events
(n.timelaps <- floor(tot.minutes / interval))


#aCam = mean(cta) # overwrite aCam with the average viewshed measured in the field
Area = 1 # overwrite default Area = 100 km2


##################################################################################
###### Loop to Run the Whole STE Model

# Summarizes the observation by time period
period = paste(det.period, "sec") # in seconds
tmp <- as.POSIXct(dat$time, format = "%H:%M:%S", tz = "UTC")
dat$time <-
  strftime(floor_date(tmp, unit = period),
           format = "%H:%M:%S",
           tz = "UTC")
dat <- dat %>% group_by(cam, common_name, date, time) %>%
  summarize(n = sum(n))


### Detection Matrix
#----------------------------
# Declare object for detection matrix
detec <-
  matrix(
    NA,
    nrow = nrow(activ),
    ncol = ncol(activ) * n.timelaps,
    dimnames = list(rownames(activ), NULL)
  )
unique(as.vector(unlist(detec)))

# Expand detection matrix to include each photo interval within days
for (j in 1:ncol(activ)) {
  idx = (j - 1) * n.timelaps + (1:n.timelaps)
  detec[, idx] <- activ[, j]
} # j


### Analysis
#----------------------------
select <- function(x, index)
  x[index]

# Identify all possible HH:MM::SS
all_times <- seq(time_start, time_end, by = period)

# Remove times if there is a break during the day
if (time_break_start != time_break_end) {
  all_times <-
    all_times[-which(all_times %in% seq(time_break_start, time_break_end, by = period))]
}

# Select times for bootstrap: Chose between regular or random with
sel_times <-
  replicate(n = Nboot, rnd.sample(avail = all_times, int_per_day = n.timelaps))
#sel_times <- replicate(n = Nboot, reg.sample(avail = all_times, int_per_day = int_per_day))

# Declare object to store estimates' results
STE <-
  data.frame(
    Iter = 1:Nboot,
    NbDet = NA,
    EST = NA,
    SE = NA
  )


k = 1

for (k in 1:Nboot) {
  # Loop over all k bootstrap iterations
  # Increment the progress in the console and update the detail text.
  
  print(paste(
    "iteration:",
    k,
    "; time period:",
    det.period,
    "; nb intervals:",
    n.timelaps
  ))
  print(paste("####################"))
  flush.console()
  
  # Go over the k selected times (within 1 day) for "instant" (n seconds) snapshots
  photo_time <- sel_times[[k]]
  
  # Select only detection that occurred at photo times
  sel <- which(as.character(dat$time) %in% photo_time)
  
  # Reduce the dataset to only keep detection at photo time
  dat.sel <-
    unique(dat[sel, c("common_name", "date", "time", "cam", "n")])
  
  # Where and when did detections happen
  detec <- detec
  for (i in 1:nrow(dat.sel)) {
    rw <- which(station == as.character(dat.sel$cam)[i])
    c1 <- which(survey_day == as.integer(ymd(dat.sel$date[i])))
    c2 <- which(photo_time == as.character(dat.sel$time[i]))
    cl <- ((c1 - 1) * n.timelaps) + c2
    detec[rw, cl] <- no_na(detec[rw, cl]) + 1
    rm(rw)
    rm(cl)
  } # i
  
  n_det <- sum(detec, na.rm = TRUE)
  STE[k, "NbDet"] <- n_det
  
  # Estimation
  ### STE ###
  # Remove Occasions (days) with no active camera
  nb_cam <- apply(detec, 2, len_no_na)
  photo = detec[, nb_cam > 0] # select columns with active cam
  
  # Redefine nb of active cameras per occasion, removing case of 0 active camera
  nb_cam <-
    apply(photo, 2, len_no_na) # recreate the vector of active cameras per occasion
  
  # Find total area of active cameras at each occasion
  # Reset detection file
  detec[detec > 0] <- 0
  active.area.tmp <- (detec + 1) * cta[[1]]
  areaCam <- apply(active.area.tmp, 2, sum, na.rm = T)
  # should I remove the ones with no camera?
  
  ## Data
  S = NA
  # this line below resamples the camera active on this date
  for (p in 1:ncol(photo)) {
    S[p] <-
      get_Sja(rm_na(photo[, p])[sample(length(rm_na(photo[, p])))])
  }
  
  dat_ste <-
    list(toevent = matrix(S, ncol = ncol(photo)),
         censor = areaCam,
         # sum areas ative camera traps at each occasion
         # could also use censor = nb_cam * aCam if area same
         # for every station
         A = Area)
  
  
  # Estimate abundance with Space-to-Event
  run = STE_estN_fn(dat_ste, -10, 10)
  EST_N = run$estN
  SE_N = run$SE_N
  
  # Save output
  fill_col <- which(is.na(STE[k, ]))
  STE[k, fill_col] <- c(EST_N, SE_N)
  
  
} # k

#})


###-Summarize results for STE and IS
nZeros <- sum(STE$NbDet == 0) / Nboot

STE_EST_N <- mean(STE$EST) / Area
STE_SE_N <- mean(STE$SE) / Area
STE_CI_N <- quantile(STE$EST, probs = c(0.025, 0.975)) / Area

# Get bootstrap averaged results

resSTE <-
  data.frame(
    Species = species,
    Nboot = Nboot,
    period = det.period,
    Avg_time_int = interval,
    N_occ_per_day = n.timelaps,
    N_Zeros = nZeros,
    Density = STE_EST_N,
    SE = STE_SE_N,
    Lower_95p_CI = STE_CI_N[1],
    Upper_95p_CI = STE_CI_N[2]
  )

res <- resSTE#
row.names(res) <- NULL

(resall <- rbind(resall, res))



print(resall)

#resall <- resall %>% arrange(N_intervals)
resall$avg_grp_size <- mean(ct[["number_of_animals"]], na.rm = TRUE)
resall$effort <- sum(activ)
resall$nObs   <- nrow(dat0)
resall$startDate <- start_date
resall$endDate   <- end_date
resall$timeTotal <- tot.seconds
