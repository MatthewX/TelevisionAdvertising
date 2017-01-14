#need to specify accordingly
setwd("~/Desktop")
path <- "2015_04_15.csv"
############################
con <- file(path, open = "r")
lines <- readLines(con)
close(con)
length(lines)
N <- length(lines) - 2
#create an empty data frame
data <- data.frame(timestamp = rep("", N), channel = rep("", N), program = rep("", N),
                   position = rep("", N), channel_GnUId = rep("", N), channel_ProductId = rep("", N),
                   program_GnUid = rep("", N), program_ProductId = rep("", N),stringsAsFactors=FALSE)
for(i in 3: length(lines)){
    current_line = strsplit(lines[i], ",")[[1]]
    if(length(current_line) == 8){
        data[i - 2,] = current_line
    } else {
        num = length(current_line)
        temp_line = c()
        temp_line[1:2] = current_line[1:2]
        temp_line[3] = paste(current_line[3:(num - 5)],collapse=",")
        temp_line[4:8] = current_line[(num - 4): num]
        data[i - 2,] = temp_line
    }
}