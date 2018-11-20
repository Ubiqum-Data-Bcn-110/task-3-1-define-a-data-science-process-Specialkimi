library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
####uploading dataset


setwd("~/Desktop/tasks /M3T1")

####readingtxtfile
library(readr)
dataframe1 <- read_delim("M3T1data.txt", ";", 
                       escape_double = FALSE, trim_ws = TRUE)


View(dataframe1)
# Create DateTime

dataframe1 <- cbind(dataframe1,paste(dataframe1$Date, dataframe1$Time), stringsAsFactors=FALSE)
colnames (dataframe1)[10] <- "DateTime"
dataframe1 <- dataframe1[,c(ncol(dataframe1), 1:(ncol(dataframe1)-1))]
View(dataframe1)

dataframe1$DateTime <- strptime(dataframe1$DateTime, "%d/%m/%Y %H:%M:%S")
dataframe1$Date <- as.Date(dataframe1$Date, "%d/%m/%Y")
View(dataframe1)

# changing energetic metrics to double
dataframe1$Sub_metering_1 <- as.double(dataframe1$Sub_metering_1)
dataframe1$Sub_metering_2 <- as.double(dataframe1$Sub_metering_2) 
Global_active_power <- as.double(dataframe1$Global_active_power)
Global_reactive_power <- as.double(dataframe1$Global_reactive_power)
Voltage <- as.double(dataframe1$Voltage)
Global_intensity <- as.double(dataframe1$Global_intensity)


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
Seasonfunc <- function(fecha) {
  p <- 0
  dia <- day(fecha)
  mes <- month(fecha)
  if ( (mes == 3 & dia >= 20) | (mes %in% c(4,5)) | (mes == 6 & dia < 20) ) {
    p <-"Spring"
  }
  if ( (mes == 6 & dia >= 20) | (mes %in% c(7,8)) | (mes == 9 & dia < 20) ) {
    p <-"Summer"
  }  
  if ( (mes == 9 & dia >= 20) | (mes %in% c(10,11)) | (mes == 12 & dia < 20) ) {
    p <-"Autum"
  }
  if ( (mes == 12 & dia >= 20) | (mes %in% c(1,2)) | (mes == 3 & dia < 20) ) {
    p <-"Winter"
  }  
  return(p)
}

# How to compute the season for each record of our dataset (Basic approach)
#for (i in 1:nrow(dataframe1)) {
#    dataframe1[i,]$Season <- Seasonfunc(dataframe1[i,]$Date) 
#} 

dataframe1$Season <- sapply(dataframe1$Date,function(x) Seasonfunc(x))

Seasonfunc(dataframe1$Date)
print(dataframe1$Season)

# We create a variable for partofweek
dataframe1$Partofweek <- "0"

# We create a function to determine the season of the year from an input in "Date" format
Partofweekfunc <- function (Dia) {
  p <- 0    
  if ( Dia %in% c("Monday","Tuesday", "Wednesday", "Thursday" , "Friday"))  {
    p <- "Workly"
  }    
  if ( Dia %in% c("Sunday" , "Saturday" )) {
      p <-"Weekend"
  }
  return(p)
}
Partofweekfunc(dataframe1$Day)

print(dataframe1$Partofweek)

# we get the mean for seasons, workly and weekend

#starting splitting by year + submeters

year2006sub <- which(dataframe1$Year == "2006") 

year2006sub_df <- dataframe1[year2006sub,]

# group_by doesn't like POSIX.lt, therefore we need to switch to POSIX.ct as in the example below:
# https://stackoverflow.com/questions/44428319/how-to-groupby-in-dplyr-with-dataframe-containing-posixlt-datatypes-in-r
#vessel_data %>% 
#  mutate(arrival_pilot_station = as.POSIXct(arrival_pilot_station)) %>%
#  group_by(Service) %>% 
#  dplyr::summarise(average_time <- mean(diff_pilot_alongside)) %>% 
#  as.data.frame()

#dataframe1[,c(11,12,14,8,9,10)] %>% group_by(Year,Season) %>% summarise(media=mean(Sub_metering_1)) %>% as.data.frame()

aveyear2006sub <- mean(year2006)

year2007sub <- which(dataframe1$Year == "2007")
dataframe1[year2007sub,c(8:10)]

year2008sub <- which(dataframe1$Year == "2008")
dataframe1[year2008sub,c(8:10)]

year2009sub <- which(dataframe1$Year == "2009")
dataframe1[year2009sub,c(8:10)]

year2010sub <- which(dataframe1$Year == "2010")
dataframe1[year2010sub,c(8:10)]


#starting splitting by year + part of week 

#2006
year2006workly <- which(dataframe1$Partofweek== "Workly" & dataframe1$Year == "2006" )

dataframe1[year2006workly,c(8:10)]

year2006weekend <- which(dataframe1$Partofweek== "Weekend" & dataframe1$Year=="2006" & dataframe1$Sub_metering_1 
                       & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3)
#2007
year2007workly <- which(dataframe1$Partofweek== "Workly" & dataframe1$Year=="2007" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3)
year2007weekend <- which(dataframe1$Partofweek== "Weekend" & dataframe1$Year=="2007" & dataframe1$Sub_metering_1 
                         & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3)
#2008
year2008workly <- which(dataframe1$Partofweek== "Workly" & dataframe1$Year=="2008" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3)
year2008weekend <- which(dataframe1$Partofweek== "Weekend" & dataframe1$Year=="2008" & dataframe1$Sub_metering_1 
                         & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3)
#2009
year2009workly <- which(dataframe1$Partofweek== "Workly" & dataframe1$Year=="2009" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3)
year2009weekend <- which(dataframe1$Partofweek== "Weekend" & dataframe1$Year=="2009" & dataframe1$Sub_metering_1 
                         & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3)
#2010
year20010workly <- which(dataframe1$Partofweek== "Workly" & dataframe1$Year=="2010" & dataframe1$Sub_metering_1 
                         & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3)
year2010weekend <- which(dataframe1$Partofweek== "Weekend" & dataframe1$Year=="2010" & dataframe1$Sub_metering_1 
                         & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3)



# splitting by winter season and all energy atributes 
year2006Winter <- which(dataframe1$Season== "Winter" & dataframe1$Year=="2006")

dataframe1[year2006Winter,c(8:10)]

year2007Winter <- which(dataframe1$Season== "Winter" & dataframe1$Year=="2007" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2008Winter <- which(dataframe1$Season== "Winter" & dataframe1$Year=="2008" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2009Winter <- which(dataframe1$Season== "Winter" & dataframe1$Year=="2009" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2010Winter <- which(dataframe1$Season== "Winter" & dataframe1$Year=="2010" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )

# splitting by Spring season and all energy atributes 
year2006Spring <- which(dataframe1$Season== "Spring" & dataframe1$Year=="2006" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2007Spring <- which(dataframe1$Season== "Spring" & dataframe1$Year=="2007" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2008Spring <- which(dataframe1$Season== "Spring" & dataframe1$Year=="2008" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2009Spring <- which(dataframe1$Season== "Spring" & dataframe1$Year=="2009" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2010Spring <- which(dataframe1$Season== "Spring" & dataframe1$Year=="2010" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )

# splitting by Summer season and all energy atributes 
year2006Summer <- which(dataframe1$Season== "Summer" & dataframe1$Year=="2006" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2007Summer <- which(dataframe1$Season== "Summer" & dataframe1$Year=="2007" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2008Summer <- which(dataframe1$Season== "Summer" & dataframe1$Year=="2008" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2009Summer <- which(dataframe1$Season== "Summer" & dataframe1$Year=="2009" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2010Summer <- which(dataframe1$Season== "Summer" & dataframe1$Year=="2010" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )

# splitting by Autum season and all energy atributes 
year2006Autum <- which(dataframe1$Season== "Autum" & dataframe1$Year=="2006" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2007Autum <- which(dataframe1$Season== "Autum" & dataframe1$Year=="2007" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2008Autum <- which(dataframe1$Season== "Autum" & dataframe1$Year=="2008" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2009Autum <- which(dataframe1$Season== "Autum" & dataframe1$Year=="2009" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )
year2010Autum <- which(dataframe1$Season== "Autum" & dataframe1$Year=="2010" & dataframe1$Sub_metering_1 
                        & dataframe1$Sub_metering_2 & dataframe1$Sub_metering_3 )

#groupby

#ggplot2
#plot consumtion with all features for days, weeks, months

 

# Area plot
ggplot(year2007sub_df, aes(x = Year, y = value)) + 
  geom_area(aes(color = variable, fill = variable), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))










