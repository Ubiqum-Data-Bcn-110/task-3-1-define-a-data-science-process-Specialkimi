####uploading dataset

setwd("~/Desktop/tasks /M3T1")

####readingtxtfile
dataframe1 <-read.table ("M3T1data.txt", header = TRUE, sep = ";", dec = ".")

View(dataframe1)
# Create DateTime

dataframe1 <- cbind(dataframe1,paste(dataframe1$Date, dataframe1$Time), stringsAsFactors=FALSE)
colnames (dataframe1)[10] <- "DateTime"
dataframe1 <- dataframe1[,c(ncol(dataframe1), 1:(ncol(dataframe1)-1))]
View(dataframe1)

dataframe1$DateTime <- strptime(dataframe1$DateTime, "%d/%m/%Y %H:%M:%S")
dataframe1$Date <- as.Date(dataframe1$Date, "%d/%m/%Y")
View(dataframe1)
