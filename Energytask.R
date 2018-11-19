library(lubridate)
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

# We create a variable for Year
dataframe1$Year <- year(dataframe1$DateTime)

# We create a variable for month
dataframe1$Month <- month(dataframe1$DateTime)
dataframe1$Month <- factor(dataframe1$Month, levels = c(1:12),
                           labels = c( "January", "February","March","April","May","June","July","August","September","October","November","December"))


# We create a variable for day
dataframe1$Day <- wday(dataframe1$DateTime)
dataframe1$Day <- factor(dataframe1$Day, levels = c(1:7),labels = c("Sunday","Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# We create a variable for seasons
dataframe1$Season <- "0"

# We create a function to determine the season of the year from an input in "Date" format
Seasonfunc <- function (fecha) {
  p <- 0    
   
  if ( '2006-03-20 < fecha < 2006-06-20' ) {
     p <-"Spring"
  }
  if (" 2006-06-21"< fecha < "2006-09-21") {
    p <- "Summer"
  }  
  if (  "2006-09-22" < fecha < "2006-12-22") {
    p <- "Autum"
  }  
  if (  "2006-12-23" < fecha < "2007-03-19") {
    p <- "Winter"
  }
  return(p)
}
Seasonfunc(dataframe1[1,]$Date)
print(dataframe1$Season)















