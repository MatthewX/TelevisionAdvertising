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
CM = rbind(CM1, CM2)
# Get all unique CM_ID
CM1_ID = unique(CM1$CM_ID)
CM2_ID = unique(CM2$CM_ID)
CM_ID = unique(CM$CM_ID)

#############$ Currently may not be used #######################################

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
# files_full  <- list.files("EIOut", full.names=TRUE)
# filesNumber <- length(files_full)
# # Loading all EI data in one file
# EIData      <- read.csv(files_full[1])
# for (i in 2:filesNumber){
#   EIData  <- rbind(EIData, read.csv(files_full[i]))
# }
# EIData$Time <- as.character(EIData$Time)
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
############################################################################

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

#############################################
# need to recalculate when VR ER WR change

#           first week   second week      
VIOrigin = c(0.1868966, 0.1765199)
EIOrigin = c(0.03017915,0.02459273)
#############################################
week = c("CM1.csv","CM2.csv")

for(k in 1: 2) {
  CMData = read.csv(week[k],header=T)
  
  # clean CM data
  CMData$Date = as.character(CMData$Date)
  Date_temp = paste("2015-", chartr("/","-",CMData$Date),sep = "")
  Date_temp = as.Date(substr(Date_temp, 1, nchar(Date_temp) - 3))
  Time = substr(as.POSIXlt(paste(Date_temp, CMData$StartTime)),1,19)
  CMData = mutate(CMData, Time = Time)

  # Get the StartTime ID and Channel ID 
  CMData    <- mutate(CMData, 
                      StartTimeID = match(CMData$Time, VRData$Time),
                      ChannelID   = match(as.character(CMData$Channel), colnames(VRData)))

  # Set the initial values for sumVR, sumER and sumWR
  CMData    <- mutate(CMData, sumVR = 0, sumER = 0, sumWR = 0)
  #CMData = CMData[,1:9]
  # Calculate sumVI, sumEI and sum AI for each row
  for (i in 1:nrow(CMData)){
    print(i)
    CMData$sumVR[i] <- sum(VRData[CMData$StartTimeID[i]:(CMData$StartTimeID[i] + CMData$Duration[i] -1), 
                                  CMData$ChannelID[i]],na.rm = T)
    CMData$sumER[i] <- sum(ERData[CMData$StartTimeID[i]:(CMData$StartTimeID[i] + CMData$Duration[i] -1), 
                                  CMData$ChannelID[i]],na.rm = T)
    CMData$sumWR[i] <- sum(WRData[CMData$StartTimeID[i]:(CMData$StartTimeID[i] + CMData$Duration[i] -1), 
                                  CMData$ChannelID[i]],na.rm = T)
  }
  
  # Aggregate data for each CM and calculate VI, EI and AI for each CM
  CMData    <- group_by(CMData, CM_ID)
  CMVI      <- summarize(CMData, VI = (sum(sumVR)/sum(sumWR))/VIOrigin[k])
  CMEI      <- summarize(CMData, EI = (sum(sumER)/sum(sumWR))/EIOrigin[k])
  CMAI      <- data.frame(CM_ID = CMVI$CM_ID)
  CMAI      <- mutate(CMAI, AI = CMEI$EI / CMVI$VI)

  # Save data
  write.csv(CMVI, paste("CMVI_Week",k,".csv",sep=""),row.names = FALSE)
  write.csv(CMEI, paste("CMEI_Week",k,".csv",sep=""),row.names = FALSE)
  write.csv(CMAI, paste("CMAI_Week",k,".csv",sep=""),row.names = FALSE)
}

