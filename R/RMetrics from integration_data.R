### Author: Zhenguo Li
### Time:   2015-May-10
### Update: 2015-May-26

# Cleaning the workspace
rm(list=ls())

# Setting working direction
getwd()
setwd("~/Desktop")

# Including useful packages
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

week_path <- c("2015-05-11", "2015-05-18")

for (n in 1:length(week_path)){
    
    ##############################################################
    ### Part I: Load all data file name and split for each day ###
    ##############################################################
    # Loading all rwa data files' name in the specified folder
    files_full      <- list.files(paste("output_20150607-0614", sep=""), full.names=TRUE)
    files_full      <- list.files(paste("output_20150615-0621", sep=""), full.names=TRUE)
    
    # Get single day file names' ID
    filesNumber     <- c(1:length(files_full))
    files_date      <- substr(files_full, nchar(files_full)-9, nchar(files_full))
    #singleDayFileID <- split(filesNumber, files_date)
    
    ############################################
    ### Part II: Load watching rate raw data ###
    ############################################
    # Define rating items
    #ratingItems  <- c("Time", "Skeleton", "Watch", "Engagement", "Emotion", "Channel",
    #                  "Household_ID", "Individual_ID")
    ratingItems  <- c(1, 2, 3, 4, 5, 8, 23, 24)
    # For every day, generate an standard table  2:length(singleDayFileID)  
    for (i in 1:length(filesNumber)){
        # Get the number of household  
      i=1
        singleDay = list.files(files_full[i])
        NumberViewer <- length(singleDay)
        
        RawData  <- data.frame()
        # Load all data for the day with rating items
        for (j in 1:NumberViewer){
          #j=1
            tmpData              <- read.csv(paste(files_full[i],singleDay[j],sep = "/"),header=F,fileEncoding="UTF-8")[ratingItems]
            print(j)
            names(tmpData) = c("Time", "Skeleton", "Watch", "Engagement", "Emotion", "Channel",
                               "Household_ID", "Individual_ID")
            # Change class of "Time", "Channel" amd "Household_ID"
            #head(tmpData)
            #tempTime   <- as.POSIXct(paste(files_date[i], "09:00:00", sep=' ')): 
            #              (as.POSIXct(paste(files_date[i], "09:00:00", sep=' '))+86400-1)
            #tempTime <- as.POSIXct(tempTime, origin = "1970-01-01")
            #tmpData$Time         <- tempTime
            tmpData$Time      <- as.character(tmpData$Time)
            tmpData$Channel      <- as.character(tmpData$Channel)
            tmpData$Household_ID <- as.character(tmpData$Household_ID)
            
            # Delete whether Channel = NA and Channel = ""
            tmpData              <- tmpData[tmpData$Channel!='',]
            tmpData              <- tmpData[!is.na(tmpData$Channel),]
            
            # Combind all files for the day
            RawData  <- rbind(RawData, tmpData)
        }
        #usedata = RawData
        if (length(RawData[,1]) != 0){
            ################################################
            ### Part III: Generate the standard WR table ###
            ################################################
            # Get the TV channels' name
            #ChannelNames <- read.csv("Audio_Channel_List_353.csv")
            
            HHnumber <- length(unique(RawData$Household_ID))
            
            ChannelNames <- c("NHK","ETV","NTV","EX","TBS","TX","CX")
            
            # Generate an standard table for one day watching rate
            m              <- matrix(0, nrow=86400, ncol=length(ChannelNames)+1)
            StandardTable  <- data.frame(m)
            colnames(StandardTable) <- c("Time", as.character(ChannelNames))
            
            TimeData           <- read.csv(paste(files_full[i],singleDay[1],sep="/"), header = F)[[1]]
            #head(StandardTable)
            StandardTable$Time <- TimeData
            #length(unique(RawData$Time))
            ######################################################################
            ### Part IV: Calculate every second WR, VR and ER for all Channels ###
            ######################################################################
            # Group RawData by Time and Channel
            RawData <- group_by(RawData, Time, Channel)
            
            #?group_by
            # Calculate WR, VR and ER second by second
            RatingData <- summarise(RawData, 
                                    WR=length(unique(Household_ID))/HHnumber,
                                    VR=sum(!is.na(Skeleton))/NumberViewer,
                                    ER=sum(Watch==1, na.rm=TRUE)/NumberViewer)
                                    #ER=length(Watch==1)
            
            # Initialize the WR, VR and ER table
            WR <- StandardTable
            VR <- StandardTable
            ER <- StandardTable
            
            # Get the unique Channel
            uniChannel <- unique(RatingData$Channel)
            ChannelID  <- match(uniChannel, colnames(StandardTable))

            for(k in 1:length(uniChannel)){
                #k=1
                # Get the Channel ID and its correponding Time ID
                TimeID    <- match(RatingData$Time[RatingData$Channel == uniChannel[k]], 
                                   StandardTable$Time)
                print(length(ChannelID[k]))
                print(length(TimeID))
                if (!is.na(ChannelID[k])){
                    # Tansfer WR, VR and ER data to standard table
                    WR[TimeID, ChannelID[k]] <- RatingData$WR[RatingData$Channel == uniChannel[k]]
                    VR[TimeID, ChannelID[k]] <- RatingData$VR[RatingData$Channel == uniChannel[k]]
                    ER[TimeID, ChannelID[k]] <- RatingData$ER[RatingData$Channel == uniChannel[k]]
                }
            }
            
            # Save single day househould WR data
            write.csv(WR, paste("WROut/",files_date[i], "_WR.csv", sep=""), row.names = FALSE)
            write.csv(VR, paste("VROut/",files_date[i], "_VR.csv", sep=""), row.names = FALSE)
            write.csv(ER, paste("EROut/",files_date[i], "_ER.csv", sep=""), row.names = FALSE)
        }
        else next
    }
}



