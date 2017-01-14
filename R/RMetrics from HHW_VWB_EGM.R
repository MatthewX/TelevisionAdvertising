# Cleaning the workspace
rm(list=ls())

# Setting working direction
getwd()
setwd("~/Desktop/JP_Project_01")

# Including useful packages
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

# Read number of households and members
hh_mem_number = read.csv("num_sample_20150607-21.csv")
#head(hh_mem_number)
files_full = list.files("output_HHW_VWB_EGM_20150607-0614")

# Distinguish HHW, EGM and VWB
HHW = files_full[grep("HHW", files_full)]
VWB = files_full[grep("VWB", files_full)]
EGM = files_full[grep("EGM", files_full)]

# Calculate the Watching Rate
for(i in 1: length(HHW)) {
  print(i)
  hh_number = hh_mem_number[i,2]
  current_day_HHW = read.csv(paste("output_HHW_VWB_EGM_20150607-0614", HHW[i], sep = "/"), header = F)
  current_day_HHW = current_day_HHW[,1:length(current_day_HHW[1,]) - 1]
  names(current_day_HHW) = c("Time","NHK","ETV","NTV","EX","TBS","TX","CX","total")
  temp = current_day_HHW[,2:length(current_day_HHW[1,])]
  temp = temp / hh_number
  current_day_HHW[,2:length(current_day_HHW[1,])] = temp
  write.csv(current_day_HHW, paste("WROut/",substr(HHW[i],1,10),"_WR.csv", sep = ""),row.names = F)
}

# Calculate the Viewbility Rate
for(i in 1: length(VWB)) {
  print(i)
  person_number = hh_mem_number[i,3]
  current_day_VWB = read.csv(paste("output_HHW_VWB_EGM_20150607-0614", VWB[i], sep = "/"), header = F)
  current_day_VWB = current_day_VWB[,1:length(current_day_VWB[1,]) - 1]
  names(current_day_VWB) = c("Time","NHK","ETV","NTV","EX","TBS","TX","CX","total")
  temp = current_day_VWB[,2:length(current_day_VWB[1,])]
  temp = temp / person_number
  current_day_VWB[,2:length(current_day_VWB[1,])] = temp
  write.csv(current_day_VWB, paste("VROut/",substr(VWB[i],1,10),"_VR.csv", sep = ""),row.names = F)
}

# Calculate the Engagement Rate
for(i in 1: length(EGM)) {
  print(i)
  person_number = hh_mem_number[i,3]
  current_day_EGM = read.csv(paste("output_HHW_VWB_EGM_20150607-0614", EGM[i], sep = "/"), header = F)
  current_day_EGM = current_day_EGM[,1:length(current_day_EGM[1,]) - 1]
  names(current_day_EGM) = c("Time","NHK","ETV","NTV","EX","TBS","TX","CX","total")
  temp = current_day_EGM[,2:length(current_day_EGM[1,])]
  temp = temp / person_number
  current_day_EGM[,2:length(current_day_EGM[1,])] = temp
  write.csv(current_day_EGM, paste("EROut/",substr(EGM[i],1,10),"_ER.csv", sep = ""),row.names = F)
}