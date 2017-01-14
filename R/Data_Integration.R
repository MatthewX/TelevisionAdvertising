# Cleaning the workspace
rm(list=ls())

# Setting working direction
dir <- getwd()
setwd(dir)

# Including useful packages
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(xlsx))

########################################################
### Part I: Load both TVI and Audio Fingerprint Data ###
########################################################
# Load commercial data 
CMData       <- read.csv("ExpandCMData.csv", colClasses=c("character", "character", 
                                                        "character", "character"))

### Load TVI data ###
# Load all files' name in the specified folder
path        <- "Prep/#5/"
familyID    <- substr(path, 6, nchar(path)-1)
files_full  <- list.files(paste(path, "TVI Panel", sep=''), full.names=TRUE)

# Split file based on Date
files_date     <- substr(files_full, nchar(files_full[1])-16, nchar(files_full[1])-7)
singleDayFiles <- split(files_full, files_date)
#
for (k in 1:length(singleDayFiles)){

    ########################################
    ### Part I: Laod and Adjust TVI Data ###
    ########################################
    AllTVIData  <- data.frame()
    for (i in 1:length(singleDayFiles[[k]])) {
        tmpData    <- read.csv(singleDayFiles[[k]][i])
        colnames(tmpData) <- c("Frame_count", "Timestamp", "Skeleton", "Skeleton_id2", 
                               "Skeleton_pos1", "Head_pos", "Viewer_id", "Watch", 
                               "Engagement", "Emotion", "Face_detection_level")
        AllTVIData <- rbind(AllTVIData, tmpData)
    }

    # Add new column time character
    AllTVIData <- mutate(AllTVIData, 
                         TimeCharacter = as.character(as.POSIXct(AllTVIData$Timestamp, 
                                                                 origin="1970-01-01", 
                                                                 tz="UTC")))
    AllTVIData <- AllTVIData[c(1,2,12,3,4,5,6,7,8,9,10,11)]
    
    # Adjust Viewer_id == 0
    AdjustID   <- grep(TRUE, AllTVIData$Viewer_id==0)
    
    # Add new column for adjusted Viewer_id
    AllTVIData <- mutate(AllTVIData, Adjust_Viewer_id = Viewer_id)
    AllTVIData <- AllTVIData[c(1,2,3,4,5,6,7,8,13,9,10,11,12)]
    
    # Get the corresponding ViewerID for every SkeletonID
    uniSkeID      <- unique(AllTVIData$Skeleton)[-1]
    uniSkeID      <- as.data.frame(uniSkeID)
    Ske_Viewer    <- mutate(uniSkeID, ViewerID=0)
    
    for (i in 1:length(uniSkeID[,1])){
        SkeletonRowID <- grep(TRUE, AllTVIData$Skeleton==uniSkeID[i,1])
        
        uniViewerID   <- unique(AllTVIData$Viewer_id[SkeletonRowID])
        uniViewerID   <- uniViewerID[uniViewerID != 0]
        
        # Get the ViewerID for adjust row
        if (length(uniViewerID) > 1){
            ViewerID      <- uniViewerID[1]
            for (j in 2:length(uniViewerID)){
                if (sum(AllTVIData$Viewer_id[SkeletonRowID] == uniViewerID[j]) > 
                        sum(AllTVIData$Viewer_id[SkeletonRowID] == uniViewerID[j-1]))
                    ViewerID <- uniViewerID[j]
            }
            # Give the value to the targeted row for ViewerID
            Ske_Viewer$ViewerID[i] <- ViewerID
        }   
    }
    
    # Adjust Viewer_id based on Skeleton
    AllTVIData$Adjust_Viewer_id[AdjustID] <- Ske_Viewer$ViewerID[match(AllTVIData$Skeleton[AdjustID], Ske_Viewer$uniSkeID)]
    
    ########################################
    ### Part I: Laod and Adjust Audio Data ###
    ########################################
    ### Load Audio Fingerprint Data ###
    # Load all files' name in the specified folder
    Audio_files_full <- list.files(paste(path, "Audio", sep=''), full.names=TRUE)
    Audio_files_Date <- substr(Audio_files_full, nchar(Audio_files_full[1])-13, 
                               nchar(Audio_files_full[1]))
    Audio_files_Date <- chartr("_", "-", Audio_files_Date)
    
    # find the match audio data for the corresponding TVI data
    DateID <- grep(as.character(names(singleDayFiles)[k]), Audio_files_Date)

    if ( length(DateID) == 0) next
    else{
        AllAudioData  <- data.frame()
        for (i in 1:length(Audio_files_full)) {
            tmpData      <- read.csv(Audio_files_full[i], skip = 1, colClasses=rep("character", 8))
            AllAudioData <- rbind(AllAudioData, tmpData)
        }
        
        # Add one column for duration
        NumTimestamp <- as.numeric(as.POSIXlt(AllAudioData$timestamp, origin="1970-01-01", tz="UTC"))
        AllAudioData <- mutate(AllAudioData, NumTimestamp=NumTimestamp)
        AllAudioData <- AllAudioData[match(unique(AllAudioData$timestamp), AllAudioData$timestamp), 1:length(AllAudioData[1,])]
        AllAudioData <- AllAudioData[order(AllAudioData$timestamp), 1:length(AllAudioData[1,])]
        AllAudioData <- mutate(AllAudioData, Duration=c(pmin(diff(NumTimestamp), 600), 0))
        nonZeroID    <- grep(TRUE, AllAudioData$Duration > 0)
        AllAudioData <- AllAudioData[nonZeroID, ]
        
        # Increase more rows Data
        AllAudioExp  <- data.frame()
        for (i in 1:length(AllAudioData[,1])){
            TmpData <- AllAudioData$NumTimestamp[i] : (AllAudioData$NumTimestamp[i]+min((AllAudioData$Duration[i]-1), 600))
            TmpData <- data.frame(TmpData)
            colnames(TmpData) <- "Timestamp"

            TmpData <- mutate(TmpData, Channel=AllAudioData$channel[i], 
                              Channel_ID=AllAudioData$channel_GnUId[i],    
                              Program=AllAudioData$program[i],
                              Program_ID=AllAudioData$program_GnUId[i])
            AllAudioExp <- rbind(AllAudioExp, TmpData)
        }
        AllAudioExp$Timestamp <- as.character(as.POSIXlt(AllAudioExp$Timestamp, 
                                                         origin="1970-01-01", tz="UTC"))

        ##########################################################################
        ### Part III: Create standard data frame and integrate with Audio Data ###
        ##########################################################################
        # Build a new data with standard format
        Time   <- as.POSIXct(paste(names(singleDayFiles)[k], "00:00:00", sep=' '), tz="UTC"): 
                  (as.POSIXct(paste(names(singleDayFiles)[k], "00:00:00", sep=' '), tz="UTC")+86400-1)
        Time   <- as.character(as.POSIXlt(Time, origin="1970-01-01", tz="UTC"))        

        # Integrat the Audio data
        FormatData <- data.frame(Time)
        JoinData1  <- left_join(FormatData, AllAudioExp, by=c("Time"="Timestamp"))
        print(length(JoinData1$Time))
        # Integrat the CM data
        JoinData2  <- left_join(JoinData1, CMData, by=c("Time"="Timestamp", 
                                                                     "Channel"="Channel"))
        print(length(JoinData2$Time))
        ############################################################
        ### Part V: Separate based on Viewer and Save the result ###
        ############################################################
        # Delete Adjust_Viewer_id == NA 
        RealViewerID <- grep(TRUE, !(is.na(AllTVIData$Adjust_Viewer_id)))
        RealTVIData  <- AllTVIData[RealViewerID,]
        
        # Delete Adjust_Viewer_id == 0 | Adjust_Viewer_id == -1
        RealViewerID <- grep(TRUE, !(RealTVIData$Adjust_Viewer_id == 0 | RealTVIData$Adjust_Viewer_id == -1))
        RealTVIData  <- RealTVIData[RealViewerID,]
        
        # Get each Viewer' ID
        UniViewer    <- unique(RealTVIData$Adjust_Viewer_id)

        # Get the data for each Viewer   
        for (i in 1:length(UniViewer)){
            SingleViewerID   <- grep(TRUE, RealTVIData$Adjust_Viewer_id==UniViewer[i])
            SingleViewerData <- RealTVIData[SingleViewerID,]
            SingleViewerData <- SingleViewerData[c(3, 4, 9, 10, 11, 12)]
            
            JoinData3        <- left_join(JoinData2, SingleViewerData,      
                                          by=c("Time"="TimeCharacter"))
            JoinData3        <- JoinData3[match(unique(JoinData3$Time),JoinData3$Time), ]
            
            print(sum(JoinData3$Channel[!is.na(JoinData3$Channel)]  == "ESPN"))

            print(length(JoinData3$Time))
            # Add several more columns
            JoinData3        <- mutate(JoinData3, Scene='', Scene_ID='', 
                                       Household_ID=familyID, Individual_ID=UniViewer[i],
                                       Gender='', Age_Age_segment='')
            
            # Clean NA data
            JoinData3[is.na(JoinData3)] <- ''
            
            # Get the final data            
            JoinData3 <- JoinData3[c(1, 8, 10, 11, 2:7, 13:18)]
            
            # Save the data
            write.csv(JoinData3, paste("Integration_Output/", names(singleDayFiles)[k],      
                                        '-', familyID, '-', UniViewer[i], ".csv", sep=''), 
                      row.names=FALSE)
        }
    }
}
