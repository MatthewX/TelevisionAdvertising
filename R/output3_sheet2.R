# Cleaning the workspace
rm(list=ls())

# Setting working direction
getwd()
setwd("~/Desktop")

# Including useful packages
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(xlsx))

# Read CM data
CM1 = read.csv("CM1.csv",header=T)
CM2 = read.csv("CM2.csv",header=T)
CMData = rbind(CM1, CM2)

################ Currently may not be used ########################

# # Loading all raw data files' name in the specified folder
# files_full  <- list.files("VIOut", full.names=TRUE)
# filesNumber <- length(files_full)
# # Loading all VI data in one file
# VIData      <- read.csv(files_full[1])
# for (i in 2:filesNumber){
#   VIData  <- rbind(VIData, read.csv(files_full[i]))
# }
# VIData$Time <- as.character(VIData$Time)
# 
# # Loading all raw data files' name in the specified folder
# files_full  <- list.files("AIOut", full.names=TRUE)
# filesNumber <- length(files_full)
# # Loading all AI data in one file
# AIData      <- read.csv(files_full[1])
# for (i in 2:filesNumber){
#   AIData  <- rbind(AIData, read.csv(files_full[i]))
# }
# AIData$Time <- as.character(AIData$Time)

############################################################

# Loading all raw data files' name in the specified folder
files_full  <- list.files("VROut", full.names=TRUE)
filesNumber <- length(files_full)
# Loading all VR data in one file
VRData      <- read.csv(files_full[1])
for (i in 2:filesNumber){
  VRData  <- rbind(VRData, read.csv(files_full[i]))
}
VRData$Time <- as.character(VRData$Time)

# Loading all raw data files' name in the specified folder
files_full  <- list.files("EROut", full.names=TRUE)
filesNumber <- length(files_full)
# Loading all ER data in one file
ERData      <- read.csv(files_full[1])
for (i in 2:filesNumber){
  ERData  <- rbind(ERData, read.csv(files_full[i]))
}
ERData$Time <- as.character(ERData$Time)

# Loading all raw data files' name in the specified folder
files_full  <- list.files("WROut", full.names=TRUE)
filesNumber <- length(files_full)
# Loading all WR data in one file
WRData      <- read.csv(files_full[1])
for (i in 2:filesNumber){
  WRData  <- rbind(WRData, read.csv(files_full[i]))
}
WRData$Time <- as.character(WRData$Time)


########### Load HHW, EGM and VWB Data ###########
path = "JP_Project_01/output_HHW_VWB_EGM_20150607-0614"
files_full = list.files(path)

# Distinguish HHW, EGM and VWB
HHW = files_full[grep("HHW", files_full)]
VWB = files_full[grep("VWB", files_full)]
EGM = files_full[grep("EGM", files_full)]

# Load HHW, EGM and VWB data
HHWData = read.csv(paste(path,HHW[1],sep = "/"), header = F)
for(i in 2: length(HHW)) {
  HHWData  <- rbind(HHWData, read.csv(paste(path,HHW[i],sep = "/"), header = F))
}
HHWData = HHWData[,1:9]
names(HHWData) = c("Time","NHK","ETV","NTV","EX","TBS","TX","CX","total")

VWBData = read.csv(paste(path,VWB[1],sep = "/"), header = F)
for(i in 2: length(VWB)) {
  VWBData  <- rbind(VWBData, read.csv(paste(path,VWB[i],sep = "/"), header = F))
}
VWBData = VWBData[,1:9]
names(VWBData) = c("Time","NHK","ETV","NTV","EX","TBS","TX","CX","total")

EGMData = read.csv(paste(path,EGM[1],sep = "/"), header = F)
for(i in 2: length(EGM)) {
  EGMData  <- rbind(EGMData, read.csv(paste(path,EGM[i],sep = "/"), header = F))
}
EGMData = EGMData[,1:9]
names(EGMData) = c("Time","NHK","ETV","NTV","EX","TBS","TX","CX","total")


# clean CM data
CMData$Date = as.character(CMData$Date)
Date_temp = paste("2015-", chartr("/","-",CMData$Date),sep = "")
Date_temp = as.Date(substr(Date_temp, 1, nchar(Date_temp) - 3))
Time = substr(as.POSIXlt(paste(Date_temp, CMData$StartTime)),1,19)
CMData = mutate(CMData, Time = Time)

# VI and AI, Get the StartTime ID and Channel ID 
CMData    <- mutate(CMData, 
                    StartTimeID = match(CMData$Time, VRData$Time),
                    ChannelID   = match(as.character(CMData$Channel), colnames(VRData)))

# Integrate VI and AI
#CMData = CMData[,1:7]
CMData    <- mutate(CMData, sumVR = 0, sumER = 0, sumWR = 0, sumHHW = 0, sumVWB = 0, sumEGM = 0)
#head(CMData)
for (i in 1:nrow(CMData)){
  CMData$sumVR[i] <- sum(VRData[CMData$StartTimeID[i]:(CMData$StartTimeID[i] + CMData$Duration[i] -1), 
                                CMData$ChannelID[i]],na.rm = T)
  CMData$sumER[i] <- sum(ERData[CMData$StartTimeID[i]:(CMData$StartTimeID[i] + CMData$Duration[i] -1), 
                                CMData$ChannelID[i]],na.rm = T)
  CMData$sumWR[i] <- sum(WRData[CMData$StartTimeID[i]:(CMData$StartTimeID[i] + CMData$Duration[i] -1), 
                                CMData$ChannelID[i]],na.rm = T)
  CMData$sumHHW[i] <- sum(HHWData[CMData$StartTimeID[i]:(CMData$StartTimeID[i] + CMData$Duration[i] -1), 
                                CMData$ChannelID[i]],na.rm = T)
  CMData$sumVWB[i] <- sum(VWBData[CMData$StartTimeID[i]:(CMData$StartTimeID[i] + CMData$Duration[i] -1), 
                                CMData$ChannelID[i]],na.rm = T)
  CMData$sumEGM[i] <- sum(EGMData[CMData$StartTimeID[i]:(CMData$StartTimeID[i] + CMData$Duration[i] -1), 
                                CMData$ChannelID[i]],na.rm = T)
}

#############################################
# need to recalculate when VR ER WR change

#           first week   second week      
VIOrigin = c(0.1868966, 0.1765199)
EIOrigin = c(0.03017915,0.02459273)
#############################################

# Output the result
result = CMData[,c(6,1,3,5,2)]
result = mutate(result, Avg_HHW = CMData$sumHHW/CMData$Duration, Avg_VWB = CMData$sumVWB/CMData$Duration,
                        Avg_EGM = CMData$sumEGM/CMData$Duration, Avg_VI = CMData$sumVR/CMData$sumWR,
                        Avg_EI = CMData$sumER/CMData$sumWR)
fir_week_len = nrow(CM1)
sec_week_len = nrow(CM2)

result$Avg_VI[1:fir_week_len] = result$Avg_VI[1:fir_week_len] / VIOrigin[1]
result$Avg_VI[(fir_week_len + 1): sec_week_len] = result$Avg_VI[(fir_week_len + 1): sec_week_len] / VIOrigin[2]
result$Avg_EI[1:fir_week_len] = result$Avg_EI[1:fir_week_len] / EIOrigin[1]
result$Avg_EI[(fir_week_len + 1): sec_week_len] = result$Avg_EI[(fir_week_len + 1): sec_week_len] / EIOrigin[2]
result = mutate(result, Avg_AI = result$Avg_EI / result$Avg_VI)
#head(result,n=100)

write.csv(result, "output3_sheet2.csv")
