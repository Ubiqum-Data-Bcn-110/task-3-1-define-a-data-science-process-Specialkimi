#### Index #### 

#### Libraries  ####
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(forecast)
library(reshape2)



setwd("~/Desktop/tasks /M3T1")

#### Loading ####
Consumption <- read.csv("~/Desktop/tasks /M3T1/M3T1data.txt", row.names=1, sep=";",stringsAsFactors = T)
Consumption <- as.data.frame(Consumption)



View(Consumption)

for (i in 3:length(Consumption)) {
   Consumption[,i] <- as.double(Consumption[,i])
}

View(Consumption)
summary(Consumption)
str(Consumption)

##### Create DateTime + new variables ####
Consumption$DateTime<-dmy_hms(paste(Consumption$Date, Consumption$Time))

#### How many NAs ####
prop.table(table(is.na(Consumption)))
is.na(Consumption)

# We cont the number of NA by column
apply(Consumption[,c(3:9)],2,function(x) sum(is.na(x)))

#### Creating new variables ####
Consumption$Year  <- year(Consumption$DateTime)
Consumption$Month <- month(Consumption$DateTime,label = T,abbr = F)
Consumption$Day   <- weekdays(Consumption$DateTim,abbreviate = F)

#barplot(table(Consumption2[rowSums(is.na(Consumption2)) >= 1 & rowSums(is.na(Consumption2)) < length(colnames(Consumption2))-1, 1]),
#        ylab = "Amount of NA values" ,
#        xlab = "Date",
#        main = "Distribution of NA values",
#        col = "lightblue")

#### create new atributes for month and day ####

#Consumption2 <-cbind (Consumption, dataframe_month$Month)


#####changing energetic metrics to double####
#dataframe1$Sub_metering_1 <- as.double(dataframe1$Sub_metering_1)
#dataframe1$Sub_metering_2 <- as.double(dataframe1$Sub_metering_2) 
#Global_active_power <- as.double(dataframe1$Global_active_power)
#Global_reactive_power <- as.double(dataframe1$Global_reactive_power)
#Voltage <- as.double(dataframe1$Voltage)
#Global_intensity <- as.double(dataframe1$Global_intensity)


####We create a variable for Year and month #### 

#Consumption2 <- Consumption %>% select (Global_active_power, Global_reactive_power,Voltage,Sub_metering_1,Sub_metering_2,Sub_metering_3,DateTime)%>% 
#  mutate(Month = month(DateTime, label = TRUE),Year = year(DateTime ))
#%>%
#                                #group_by(Year, Month) %>% dplyr::summarise_all(sum)



####Aggregated all years ####

Agregate_all_years <- Consumption[,c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Year","Month","TotalConsumption")] %>% 
  group_by(Year, Month) %>% summarise(sub1=mean(na.omit(Consumption$Sub_metering_1)),
                                      sub2=mean(na.omit(Consumption$Sub_metering_2)),
                                      sub3=mean(na.omit(Consumption$Sub_metering_3)),
                                      tot=mean(na.omit((Consumption$TotalConsumption))))


ggplot(data = Agregate_all_years, aes(x=Month)) + 
  geom_point(aes(y=sub1,color="yellow")) + 
  geom_point(aes(y=sub2,color="red")) + 
  geom_point(aes(y=sub3,color="green")) + facet_grid(~Year)  

total.ts = ts(Consumption$TotalConsumption, frequency = 4, start = c(2007,)
plot(total.ts, col = "darkblue", lwd = 3, main = "Total energy consumption Time Series")

#### Aggregated all with total consumption #### 

Consumption$TotalConsumption <- Consumption$Global_active_power + Consumption$Global_reactive_power



#### Aggregated 2006#### 
year2006 <- which(Consumption$Year=="2006")
Agregate_all_years_2006 <- Consumption[year2006,c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Year","Month")] %>% 
  group_by(Year, Month) %>% 
  summarise(sub1_6=mean(na.omit(Consumption$Sub_metering_1)),
            sub2_6=mean(na.omit(Consumption$Sub_metering_2)),
            sub3_6=mean(na.omit(Consumption$Sub_metering_3)))

ggplot(data = Agregate_all_years_2006, aes(x=Month)) + 
  geom_point(aes(y=sub1_6,color="yellow")) + 
  geom_point(aes(y=sub2_6,color="red")) + 
  geom_point(aes(y=sub3_6,color="green")) + facet_grid(~Year)

total.ts = ts(df.mean$total_consump, frequency = 12, start = 2007)
plot(, col = "darkblue", lwd = 3, main = "Total energy consumption Time Series")

#### Aggregated 2007#### 

Agregate_all_years_2007 <- Consumption[year2007,c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Year","Month")] %>% 
  group_by(Year, Month) %>% 
  summarise(sub1_7=mean(na.omit(Consumption$Sub_metering_1)),
            sub2_7=mean(na.omit(Consumption$Sub_metering_2)),
            sub3_7=mean(na.omit(Consumption$Sub_metering_3)))

ggplot(data = Agregate_all_years_2007, aes(x=Month)) + 
  geom_point(aes(y=sub1_7,color="yellow")) + 
  geom_point(aes(y=sub2_7,color="red")) + 
  geom_point(aes(y=sub3_7,color="green")) + facet_grid(~Year)

total.ts = ts(df.mean$total_consump, frequency = 12, start = 2007)
plot(, col = "darkblue", lwd = 3, main = "Total energy consumption Time Series")

#### Aggregated 2008 ####
year2008 <- which(Consumption$Year=="2008")
Agregate_all_years_2008 <- Consumption[year2008,c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Year","Month")] %>% 
  group_by( Year, Month) %>% summarise(sub1_8=mean(na.omit(Consumption[year2008,]$Sub_metering_1)),
                                      sub2_8=mean(na.omit(Consumption[year2008,]$Sub_metering_2)),
                                      sub3_8=mean(na.omit(Consumption[year2008,]$Sub_metering_3)))

ggplot(data = Agregate_all_years_2008, aes(x=Month)) + 
  geom_point(aes(y=sub1_8,color="yellow")) + 
  geom_point(aes(y=sub2_8,color="red")) + 
  geom_point(aes(y=sub3_8,color="green")) + facet_grid(~Year)

#### Aggregated 2009 ####
year2009 <- which(Consumption$Year=="2009")
Agregate_all_years_2009 <- Consumption[year2009,c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Year","Month")] %>% 
  group_by(Year, Month) %>% summarise(sub1_9=mean(na.omit(Consumption$Sub_metering_1)),
                                      sub2_9=mean(na.omit(Consumption$Sub_metering_2)),
                                      sub3_9=mean(na.omit(Consumption$Sub_metering_3)))

ggplot(data = Agregate_all_years_2009, aes(x=Month)) + 
  geom_point(aes(y=sub1_9,color="yellow")) + 
  geom_point(aes(y=sub2_9,color="red")) + 
  geom_point(aes(y=sub3_9,color="green")) + facet_grid(~Year)

total.ts = ts(df.mean$total_consump, frequency = 12, start = 2007)
plot(, col = "darkblue", lwd = 3, main = "Total energy consumption Time Series")

#### Aggregated 2010 ####
year2010 <- which(Consumption$Year=="2010")
Agregate_all_years_2010 <- Consumption[year2010,c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Year","Month")] %>% 
  group_by(Year, Month) %>% summarise(sub1_1=mean(na.omit(Consumption$Sub_metering_1)),
                                      sub2_1=mean(na.omit(Consumption$Sub_metering_2)),
                                      sub3_1=mean(na.omit(Consumption$Sub_metering_3)))

ggplot(data = Agregate_all_years_2010, aes(x=Month)) + 
  geom_point(aes(y=sub1_1,color="yellow")) + 
  geom_point(aes(y=sub2_1,color="red")) + 
  geom_point(aes(y=sub3_1,color="green")) + facet_grid(~Year)

total.ts = ts(df.mean$total_consump, frequency = 12, start = 2007)
plot(, col = "darkblue", lwd = 3, main = "Total energy consumption Time Series")







Agregate_all_years$Year <-as.numeric(Agregate_all_years$Year)
year2006 <- which(Consumption$Year=="2006")







Agregate_all_years_2007 <- Consumption[year2006,c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Year","Month")] %>% 
  group_by(Year, Month) %>% summarise(media1=mean(na.omit(Consumption$Sub_metering_1)),
                                                media2=mean(na.omit(Consumption$Sub_metering_2)),
                                                media3=mean(na.omit(Consumption$Sub_metering_3)))



Agregate_all_years_2009 <- Consumption[,c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Year","Month")] %>% 
  group_by(Year == "2009", Month) %>% summarise(media1=mean(na.omit(Consumption$Sub_metering_1)),
                                                media2=mean(na.omit(Consumption$Sub_metering_2)),
                                                media3=mean(na.omit(Consumption$Sub_metering_3)))

Agregate_all_years_2010 <- Consumption[,c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Year","Month")] %>% 
  group_by(Year == "2010", Month) %>% summarise(media1=mean(na.omit(Consumption$Sub_metering_1)),
                                                media2=mean(na.omit(Consumption$Sub_metering_2)),
                                                media3=mean(na.omit(Consumption$Sub_metering_3)))

#### ggplots #### 

ggplot(data = Agregado_year_mes_sub1_2_3, aes(x=Month, y=media,colour = Year)) + geom_point() + facet_grid(~Year)



# We create a variable for day
Consumption2$Day <- wday(Consumption2$DateTime)
Consumption2$Day <- factor(Consumption2$Day, levels = c(1:7),labels = c("Sunday","Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
Consumption2$Month <- as.factor(Consumption2$Month)
#Consumption2$Month <- cbind(Consumption2$month, levels = c(1:12),labels = c("January","February","March", "April", "May", "June", "July","August", "September", "October", "November", "December"))

#### Splitting ####
Agregado_year_mes_sub1_2_3 <- dataframe2[,c ("Sub_metering_1","Sub_metering_2", "Sub_metering_3", "Year","Month")] %>% group_by (Year, Month) %>% 
  summarise(media=mean(na.omit( Sub_metering_1, Sub_metering_2, Sub_metering_3)))


#### Seasons ####
dataframe2$Season <- "0"
dataframe2$Season <- sapply(dataframe2$Date,function(x) Seasonfunc(x))
#### We create a function to determine the season of the year from an input in "Date" format ####
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

Seasonfunc(dataframe2$Date)
print(dataframe2$Season)

# How to compute the season for each record of our dataset (Basic approach)
#for (i in 1:nrow(dataframe1)) {
#    dataframe1[i,]$Season <- Seasonfunc(dataframe1[i,]$Date) 
#} 

#aplicarlo manñana





#### function for partofweek ####
# dataframe2$Partofweek <- sapply(dataframe1$Date,function(x) Partofweekfunc(x))
# dataframe2$Partofweek <- "0"

# We create a function to determine the season of the year from an input in "Date" format
#Partofweekfunc <- function (Dia) {
  p <- 0    
  if ( Day == "Monday","Tuesday", "Wednesday", "Thursday" , "Friday")  {
    p <- "Workly"
  }    
  if ( Dia %in% c("Sunday" , "Saturday" )) {
      p <-"Weekend"
  }
  return(p)
}
Partofweekfunc(dataframe2$Day)

print(dataframe2$Partofweek)

# we get the mean for seasons, workly and weekend



# group_by doesn't like POSIX.lt, therefore we need to switch to POSIX.ct as in the example below:
# https://stackoverflow.com/questions/44428319/how-to-groupby-in-dplyr-with-dataframe-containing-posixlt-datatypes-in-r
#vessel_data %>% 
#  mutate(arrival_pilot_station = as.POSIXct(arrival_pilot_station)) %>%
#  group_by(Service) %>% 
#  dplyr::summarise(average_time <- mean(diff_pilot_alongside)) %>% 
#  as.data.frame()


#### splitting by year + submeters ####

year2006sub <- which(dataframe2$Year == "2006") 
year2006sub_df <- dataframe2[year2006sub,c(4,5,6,8,9)]

year2007sub <- which(dataframe2$Year == "2007")
year2007sub_df <- dataframe2[year2007sub,c(4,5,6,8,9)]

year2008sub <- which(dataframe2$Year == "2008")
year2008sub_df <- dataframe2[year2008sub,c(4,5,6,8,9)]

year2009sub <- which(dataframe2$Year == "2009")
year2009sub_df<- dataframe2[year2009sub,c(4,5,6,8,9)]

year2010sub <- which(dataframe2$Year == "2010")
year2010sub_df <- dataframe2[year2010sub,c(4,5,6,8,9)]


#### splitting by year + part of week ####

#2006
year2006workly <- which(dataframe1$Partofweek== "Workly" & dataframe1$Year == "2006" )

year2006workly_df <- dataframe1[year2006workly,c(8:10)]

year2006weekend <- which(dataframe1$Partofweek== "Weekend" & dataframe1$Year=="2006" )

year2006weekend_df <- dataframe1[year2006weekend,c(8:10)]

#2007
year2007workly <- which(dataframe1$Partofweek== "Workly" & dataframe1$Year == "2007" )

year2007workly_df <- dataframe1[year2007workly,c(8:10)]

year2007weekend <- which(dataframe1$Partofweek== "Weekend" & dataframe1$Year=="2007" )

year2007weekend_df <- dataframe1[year2007weekend,c(8:10)]

                         
#2008
year2008workly <- which(dataframe1$Partofweek== "Workly" & dataframe1$Year == "2008" )

year2008workly_df <- dataframe1[year2008workly,c(8:10)]

year2008weekend <- which(dataframe1$Partofweek== "Weekend" & dataframe1$Year=="2008" )

year2008weekend_df <- dataframe1[year2008weekend,c(8:10)]

#2009
year2009workly <- which(dataframe1$Partofweek== "Workly" & dataframe1$Year == "2009" )

year2009workly_df <- dataframe1[year2009workly,c(8:10)]

year2009weekend <- which(dataframe1$Partofweek== "Weekend" & dataframe1$Year=="2009" )

year2009weekend_df <- dataframe1[year2009weekend,c(8:10)]

#2010
year2010workly <- which(dataframe1$Partofweek== "Workly" & dataframe1$Year == "2010" )

year2010workly_df <- dataframe1[year2010workly,c(8:10)]

year2010weekend <- which(dataframe1$Partofweek== "Weekend" & dataframe1$Year=="2010" )

year2010weekend_df <- dataframe1[year2010weekend,c(8:10)]
                         

#### splitting by season and all energy atributes ####
year2006Winter <- which(dataframe1$Season== "Winter" & dataframe1$Year=="2006")
year2006Winter_df <- dataframe1[year2006Winter,c(8:10)]

year2007Winter <- which(dataframe1$Season== "Winter" & dataframe1$Year=="2007" )
year2007Winter_df <- dataframe1[year2007Winter,c(8:10)]

year2008Winter <- which(dataframe1$Season== "Winter" & dataframe1$Year=="2008" )
year2008Winter_df <- dataframe1[year2008Winter,c(8:10)]

year2009Winter <- which(dataframe1$Season== "Winter" & dataframe1$Year=="2009")
year2009Winter_df <- dataframe1[year2009Winter,c(8:10)]

year2010Winter <- which(dataframe1$Season== "Winter" & dataframe1$Year=="2010" )
year2010Winter_df <- dataframe1[year2010Winter,c(8:10)]


# splitting by Spring season and all energy atributes 
year2006Spring <- which(dataframe1$Season== "Spring" & dataframe1$Year=="2006")
year2006Spring_df <- dataframe1[year2006Spring,c(8:10)]

year2007Spring <- which(dataframe1$Season== "Spring" & dataframe1$Year=="2007" )
year2007Spring_df <- dataframe1[year2007Spring,c(8:10)]

year2008Spring <- which(dataframe1$Season== "Spring" & dataframe1$Year=="2008" )
year2008Spring_df <- dataframe1[year2008Spring,c(8:10)]

year2009Spring <- which(dataframe1$Season== "Spring" & dataframe1$Year=="2009")
year2009Spring_df <- dataframe1[year2009Spring,c(8:10)]

year2010Spring <- which(dataframe1$Season== "Spring" & dataframe1$Year=="2010" )
year2010Spring_df <- dataframe1[year2010Spring,c(8:10)]


# splitting by Summer season and all energy atributes 
year2006Summer <- which(dataframe1$Season== "Summer" & dataframe1$Year=="2006")
year2006Summer_df <- dataframe1[year2006Summer,c(8:10)]

year2007Summer <- which(dataframe1$Season== "Summer" & dataframe1$Year=="2007" )
year2007Summer_df <- dataframe1[year2007Summer,c(8:10)]

year2008Summer <- which(dataframe1$Season== "Summer" & dataframe1$Year=="2008" )
year2008Summer_df <- dataframe1[year2008Summer,c(8:10)]

year2009Summer <- which(dataframe1$Season== "Summer" & dataframe1$Year=="2009")
year2009Summer_df <- dataframe1[year2009Summer,c(8:10)]

year2010Summer <- which(dataframe1$Season== "Summer" & dataframe1$Year=="2010" )
year2010Summer_df <- dataframe1[year2010Summer,c(8:10)]


# splitting by Autum season and all energy atributes 

year2006Autum_df <- dataframe1[year2006Autum,c(8:10)]

year2007Autum <- which(dataframe1$Season== "Autum" & dataframe1$Year=="2007" )
year2007Autum_df <- dataframe1[year2007Autum,c(8:10)]

year2008Autum <- which(dataframe1$Season== "Autum" & dataframe1$Year=="2008" )
year2008Autum_df <- dataframe1[year2008Autum,c(8:10)]

year2009Autum <- which(dataframe1$Season== "Autum" & dataframe1$Year=="2009")
year2009Autum_df <- dataframe1[year2009Autum,c(8:10)]

year2010Autum <- which(dataframe1$Season== "Autum" & dataframe1$Year=="2010" )
year2010Autum_df <- dataframe1[year2010Autum,c(8:10)]

#### groupby seasons #### 

#dataframe1[,c(11,12,14,8,9,10)] %>% group_by(Year,Season) %>% summarise(media=mean(Sub_metering_1)) %>% as.data.frame()

# Ejemplo que funciona!!!
# dataframe1[,c("Sub_metering_1","Year","Month")] %>% group_by(Year,Month) %>% summarise(media=mean(na.omit(Sub_metering_1)))

Agregado_year_mes_sub1_2_3 <- dataframe2[,c ("Sub_metering_1","Sub_metering_2", "Sub_metering_3", "Year","Month")] %>% group_by (Year, Month) %>% 
  summarise(media=mean(na.omit( Sub_metering_1, Sub_metering_2, Sub_metering_3)))

Agregado_year_mes_sub1_2_3 <- dataframe2[,c ("Sub_metering_1","Sub_metering_2", "Sub_metering_3", "Year","Month")] %>% group_by (Year, Month) %>% 
  summarise(media=mean(na.omit( month)))



Agregado_year_GAP<- dataframe1[,c ("Global_active_power","Global_reactive_power", "Year","Month")] %>% group_by (Year,Month) %>% 
  summarise(media=mean(na.omit(Global_active_power, Global_reactive_power, Year,Month)))

Agregado_year_GAP<- dataframe1[,c ("Global_active_power", "Year","Month")] %>% group_by (Year,Month) %>% 
  summarise(media=mean(na.omit(Global_active_power,Year,Month)))


#### GGplot2 #### 
ggplot(data = Agregado_year_mes_sub1_2_3, aes(x=Month, y=media,colour = Year)) + geom_point()

ggplot(data = Agregado_year_mes_sub1_2_3, aes(x=Month, y=media)) + geom_line()
ggplot(data = Agregado_year_2006_mes_sub1_2_3 , aes(x=months, y=average)) + geom_line()

ggplot(data = Agregado_year_mes_sub1_2_3, aes(x=Month, y=media,colour = Year)) + geom_point()











