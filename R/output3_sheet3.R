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
####################################################################

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

############ Calculate each metric for 2520 rows ############
# VR
VR_res = c()
for(i in 2: 8) {
  temp = c()
  for(j in 1: 360) {
    channel = VRData[[i]]
    #temp[length(temp) + 1] = sum(channel[(1+(j-1)*3600):(3600*j)], na.rm = T) / 
    #                       length(channel[(1+(j-1)*3600):(3600*j)][!is.na(channel[(1+(j-1)*3600):(3600*j)])])
    temp[length(temp) + 1] = sum(channel[(1+(j-1)*3600):(3600*j)], na.rm = T)
  }
  VR_res[(length(VR_res)+1):(length(VR_res) + 360)] = temp
}

# ER
ER_res = c()
for(i in 2: 8) {
  temp = c()
  for(j in 1: 360) {
    channel = ERData[[i]]
    #temp[length(temp) + 1] = sum(channel[(1+(j-1)*3600):(3600*j)], na.rm = T) / 
    #                              length(channel[(1+(j-1)*3600):(3600*j)][!is.na(channel[(1+(j-1)*3600):(3600*j)])])
    temp[length(temp) + 1] = sum(channel[(1+(j-1)*3600):(3600*j)], na.rm = T)
  }
  ER_res[(length(ER_res)+1):(length(ER_res) + 360)] = temp
}

# WR
WR_res = c()
for(i in 2: 8) {
  temp = c()
  for(j in 1: 360) {
    channel = WRData[[i]]
#     temp[length(temp) + 1] = sum(channel[(1+(j-1)*3600):(3600*j)], na.rm = T) / 
#                              length(channel[(1+(j-1)*3600):(3600*j)][!is.na(channel[(1+(j-1)*3600):(3600*j)])])
    temp[length(temp) + 1] = sum(channel[(1+(j-1)*3600):(3600*j)], na.rm = T)
  }
  WR_res[(length(WR_res)+1):(length(WR_res) + 360)] = temp
}

# HHW
HHW_res = c()
for(i in 2: 8) {
  temp = c()
  for(j in 1: 360) {
    channel = HHWData[[i]]
    temp[length(temp) + 1] = sum(channel[(1+(j-1)*3600):(3600*j)], na.rm = T) / 
                             length(channel[(1+(j-1)*3600):(3600*j)][channel[(1+(j-1)*3600):(3600*j)] != 0])
  }
  HHW_res[(length(HHW_res)+1):(length(HHW_res) + 360)] = temp
}

# VWB
VWB_res = c()
for(i in 2: 8) {
  temp = c()
  for(j in 1: 360) {
    channel = VWBData[[i]]
    temp[length(temp) + 1] = sum(channel[(1+(j-1)*3600):(3600*j)], na.rm = T) / 
      length(channel[(1+(j-1)*3600):(3600*j)][channel[(1+(j-1)*3600):(3600*j)] != 0])
  }
  VWB_res[(length(VWB_res)+1):(length(VWB_res) + 360)] = temp
}

# EGM
EGM_res = c()
for(i in 2: 8) {
  temp = c()
  for(j in 1: 360) {
    channel = EGMData[[i]]
    temp[length(temp) + 1] = sum(channel[(1+(j-1)*3600):(3600*j)], na.rm = T) / 
      length(channel[(1+(j-1)*3600):(3600*j)][channel[(1+(j-1)*3600):(3600*j)] != 0])
  }
  EGM_res[(length(EGM_res)+1):(length(EGM_res) + 360)] = temp
}

# Make a standard form
# Date
date = c()
temp = rep("6/7/15",15)
for(j in 8:21) {
  temp[(length(temp) + 1):(length(temp) + 24)] = rep(paste("6/",j,"/15",sep = ""),24)
}
temp[(length(temp) + 1):(length(temp) + 9)] = "6/22/15"
date = rep(temp, 7)

# Time
time = c()
for(j in 1:24) {
  x = 0
  if(j > 15) {
    x = j - 16
  } else {
    x = j + 8
  }
  time[(length(time) + 1)] = paste(x, ":00",sep="")
}
time = rep(time, 105)

# Channel
channel_name = names(VRData)
chan = c()
for(i in 2:8) {
  temp = rep(channel_name[i],360)
  chan[(length(chan) + 1) : (length(chan) + 360)] = temp
}

# Combine each column and save result
result = data.frame(date)
result = mutate(result, time = time, channel = chan, avg_HHW = HHW_res,
                        avg_VWB = VWB_res, avg_EGM = EGM_res, avg_VI = VR_res/WR_res, avg_EI = ER_res/WR_res)

#############################################
# need to recalculate when VR ER WR change

#           first week   second week      
VIOrigin = c(0.1868966, 0.1765199)
EIOrigin = c(0.03017915,0.02459273)
#############################################
a= result
index <- result$date %in% c("6/7/15","6/8/15","6/9/15","6/10/15","6/11/15","6/12/15","6/13/15","6/14/15") | (result$date == "6/15/15" & result$time %in% c("0:00","1:00","2:00","3:00","4:00","5:00","6:00","7:00","8:00"))
#head(result)                
result[index,]$avg_VI = result[index,]$avg_VI / VIOrigin[1]
result[index,]$avg_EI = result[index,]$avg_EI / EIOrigin[1]
result[!index,]$avg_VI = result[!index,]$avg_VI / VIOrigin[2]
result[!index,]$avg_EI = result[!index,]$avg_EI / EIOrigin[2]

result = mutate(result, avg_AI = result$avg_EI / result$avg_VI)

write.csv(result, "output_sheet3.csv")
