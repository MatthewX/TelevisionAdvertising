# Cleaning the workspace
rm(list=ls())

# Setting working direction
dir <- getwd()
setwd("~/Desktop")

# Including useful packages
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(xlsx))

###########################################
### Part I: Load all VR, ER and WR data ###
###########################################
# Loading all raw data files' name in the specified folder
files_full  <- list.files("VROut", full.names=TRUE)
filesNumber <- length(files_full)

# Laading all rwa VI data in one file
VRData      <- read.csv(files_full[9])
#for (i in 2:8) {
for (i in 10:filesNumber){
    VRData  <- rbind(VRData, read.csv(files_full[i]))
}

# Loading all raw data files' name in the specified folder
files_full  <- list.files("EROut", full.names=TRUE)
filesNumber <- length(files_full)

# Laading all rwa VR data in one file
ERData      <- read.csv(files_full[9])
#for (i in 2:8) {
for (i in 10:filesNumber){
    ERData  <- rbind(ERData, read.csv(files_full[i]))
}

# Loading all raw data files' name in the specified folder
files_full  <- list.files("WROut", full.names=TRUE)
filesNumber <- length(files_full)

# Laading all rwa WR data in one file
WRData      <- read.csv(files_full[9])
#for(i in 2:8) {
for (i in 10:filesNumber){
    WRData  <- rbind(WRData, read.csv(files_full[i]))
}

#####################################################
### Part II: Calculate AI for each CM and Product ###
#####################################################
# Get VI and EI standard value
VIOrigin  <- sum(VRData[,2:length(VRData[1,])], na.rm = TRUE)/sum(WRData[,2:length(VRData[1,])], na.rm = TRUE)
EIOrigin  <- sum(ERData[,2:length(VRData[1,])], na.rm = TRUE)/sum(WRData[,2:length(VRData[1,])], na.rm = TRUE)

####################################################
### Part III: calculate VI, EI and AI for Second ###
####################################################
# Get the files name for VR, ER and WR data
files_full_VR <- list.files("VROut", full.names=TRUE)
files_full_ER <- list.files("EROut", full.names=TRUE)
files_full_WR <- list.files("WROut", full.names=TRUE)
filesNumber   <- length(files_full)

# Loading VR data for each day
for (i in 1:8){
#for (i in 9:filesNumber){
    print(i)
    VRData <- data.frame()
    ERData <- data.frame()
    WRData <- data.frame()
    
    VRData <- read.csv(files_full_VR[i])
    ERData <- read.csv(files_full_ER[i])
    WRData <- read.csv(files_full_WR[i])

    VIData <- VRData
    EIData <- ERData
    AIData <- VRData
    
    # Calculate VI, EI and AI
    VIData[ ,2:length(VRData[1,])] <- VRData[ ,2:length(VRData[1,])]/WRData[ ,2:length(WRData[1,])]/VIOrigin
    EIData[ ,2:length(ERData[1,])] <- ERData[ ,2:length(ERData[1,])]/WRData[ ,2:length(WRData[1,])]/EIOrigin
    AIData[ ,2:length(AIData[1,])] <- EIData[ ,2:length(ERData[1,])]/VIData[ ,2:length(VRData[1,])]

    # Save single day househould VI/EI/AI data
    write.csv(VIData, paste("VIOut/", substr(files_full_VR[i], 7, 16), "_VI.csv", sep=""), row.names = FALSE)
    write.csv(EIData, paste("EIOut/", substr(files_full_ER[i], 7, 16), "_EI.csv", sep=""), row.names = FALSE)
    write.csv(AIData, paste("AIOut/", substr(files_full_VR[i], 7, 16), "_AI.csv", sep=""), row.names = FALSE)
}