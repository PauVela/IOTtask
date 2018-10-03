#### Install Packages ####

install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("arulesViz",dependencies = c("Depends", "Suggests"))
pacman::p_load( corrplot, data.table, party, caret, dplyr, arules, prabclus, trimcluster)
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("tidyr")){install.packages("tidyr")}
install.packages("dplyr")
install.packages("plotly")
install.packages("tidyr")
install.packages("chron")
install.packages("ggplot2")
install.packages("ggplot")
####Load Data####
library(dplyr)
library(caret)
library(arules)
library(arulesViz)
library(RColorBrewer)
library(tidyr)
library(plotly)
setwd("C:/Users/pauve/Documents/UBIQUM/SCANS/PRACTICA9")
library(readr)
library(ggplot2)
library(chron)
library(lubridate)
library(readr)

library(readr)

household_power_consumption <- read_delim("household_power_consumption.txt", 
                                          ";", escape_double = FALSE, 
                                          trim_ws = TRUE)
View(household_power_consumption)
View(household_power_consumption)



####pre-process####

##creating a column with date+time##
HPC<- household_power_consumption
HPC <-cbind(HPC,paste(HPC$Date,HPC$Time), stringsAsFactors=FALSE)
colnames(HPC)[10] <-"DateTime"

HPC <- HPC[,c(ncol(HPC), 1:(ncol(HPC)-1))]
head(HPC)
HPC$DateTime <- strptime(HPC$DateTime, "%d/%m/%Y %H:%M:%S")
HPC$Date <- as.Date(HPC$Date, "%d/%m/%Y")
str(HPC)
HPC$DateTime <- as.POSIXct(HPC$DateTime,
                            format = "%Y-%m-%d %H:%M%:%S", tz = "Europe/Paris")

HPC$Date_Time<-ymd_hms(paste(HPC$Date,HPC$Time))
str(HPC)
##Create a column with Month and Year###
HPC$Month <- month(HPC$Date_Time)
HPC<-HPC[,c(ncol(HPC),1:(ncol(HPC)-1))]
HPC$Year <- year(HPC$Date_Time)
HPC<-HPC[,c(ncol(HPC),1:(ncol(HPC)-1))]
HPC$Hour<-hour(HPC$Time)
HPC$Day<-day(HPC$Date)
HPC$WeekDay<-weekdays(HPC$Date)
HPC$month<-month(HPC$Date)
HPC$months<-months(HPC$Date)


##Hour Change##
HPCHOURCHANGE<-HPC
HPCpart2007march<-HPC %>% select(Date_Time,Date,Hour)  %>%
  filter(Date >= "2007-03-25" & Date <="2007-03-26") ##hour change at 2am

HPCHOURCHANGE<-mutate(HPCHOURCHANGE, Date_Time=
ifelse(Date_Time >= as_datetime('2007-03-25 02:00:00') & Date_Time <= as_datetime('2007-10-28 01:59:00'),Date_Time+ hours(1),
ifelse(Date_Time >= as_datetime('2008-03-30 02:00:00') & Date_Time <= as_datetime('2008-10-26 01:59:00'),Date_Time+ hours(1),
ifelse(Date_Time >= as_datetime('2009-03-29 02:00:00') & Date_Time <= as_datetime('2009-10-29 01:59:00'),Date_Time+ hours(1),
ifelse(Date_Time >= as_datetime('2010-03-28 02:00:00') & Date_Time <= as_datetime('2010-10-31 01:59:00'),Date_Time+ hours(1),Date_Time )))))



##GETSEASON##

getSeason<- function(DATES){
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") #Winter Solstice
  SE <- as.Date("2012-3-15", format = "%Y-%m-%d") #Spring Equinox
  SS <- as.Date("2012-6-15", format = "%Y-%m-%d") #Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from the others years into 2012 dates
  d<- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall"))) 
}


##Data info##
View(HPC)
summary(HPC) ##na's problems##
which(is.na(HPC))
##removing NA##
HPC<-na.locf(HPC,na.rm=FALSE,maxgap=1440)
HPC[is.na(HPC)] <- 0
summary(HPC)
length(HPC)
summary(HPC$Global_active_power)
str(HPC)
options(scipen = 999)
with(HPC, hist(Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", col = "red"))
plot(x = HPC$Date_Time, y = HPC$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")

##HeatMap to look at NA's##
source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
source("calendarHeat.R")
calendarHeat2<-calendarHeat
calendarHeat(HPC$Date, HPC$Global_reactive_power, varname="Global_reActive_Power")
##look at numbers with a small part of a database##
HPCDay<-HPC[HPC$Date>="2009-02-26" & HPC$Date<="2009-03-03",]
plot(x = HPCDay$Date_Time, y = HPCDay$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
####summarise####
##pre-process##
HPC<- HPC %>%
   mutate(Sub_metering1KW=((HPC$Sub_metering_1/1000)))
HPC<- HPC %>%
  mutate(Sub_metering2KW=((HPC$Sub_metering_2/1000)))
HPC<- HPC %>%
  mutate(Sub_metering3KW=((HPC$Sub_metering_3/1000)))
HPC<- HPC %>%
  mutate(Global_active_powerKWh=((HPC$Global_active_power/60)))
HPC<- HPC %>%
  mutate(Global_reactive_powerKWh=((HPC$Global_reactive_power/60)))

HPC$Global_powerKwh <- HPC$Global_active_powerKWh+HPC$Global_reactive_powerKWh


####Database creation####

##year##
HPC_2<-HPC
HPC_y <- HPC_2 %>% select(Year,month,Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW)%>%
  group_by(Year)%>%
  filter(Year!=2006)%>%
  summarise_at(vars(Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW),
               funs(sum))
##month and year##
HPC_my <- HPC_2 %>% select(Year,month,months,Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW)%>%
  group_by(Year,month)%>%
  filter(Year!=2006)%>%
  summarise_at(vars(Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW),
               funs(sum))
##transform month in english and with order##
HPC_my<- transform(HPC_my, MonthAbb = month.abb[month])
HPC_my$MonthAbb <-factor(HPC_my$MonthAbb,
                             levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
##Day##
HPC_d<-HPC_2 %>% select(Year,month,months,Day,WeekDay,Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW)%>%
  group_by(Year,Day)%>%
  filter(Year!=2006)%>%
  summarise_at(vars(Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW),
               funs(sum))
##Hour##
HPC_hour<-HPC_2 %>% select(Year,month,months,Day,Hour,WeekDay,Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW)%>%
  group_by(Year,Hour)%>%
  filter(Year!=2006)%>%
  summarise_at(vars(Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW),
               funs(sum))

##Seasonal##

##PLOTS##
####Year####
##global power monthly by year##
ggplot(data=HPC_my, aes(x=MonthAbb,y=Global_powerKwh, group=Year,colour=Year)) +
  geom_line()+theme_bw()+xlab("Month")+ylab("Global Power in KW/h")+ggtitle("Global Power by Months")+
  geom_point()+facet_wrap(facets = Year ~ .)

ggplot(data=HPC_my, aes(x=MonthAbb, y=Global_powerKwh, group=Year,colour=Year)) +
  geom_line()+theme_bw()+
  geom_point()+facet_grid(facets = Year ~ ., margins = FALSE)

####Month####
##Reactive and Active power by month##
ggplot(data=HPC_my, aes(HPC_my$MonthAbb,group=1))+
geom_line(aes(y=HPC_my$Global_reactive_powerKWh,color="Global Reactive power"))+
  geom_line(aes(y=HPC_my$Global_active_powerKWh,color="Global Active power"))+
  theme_bw()+
xlab("Month")+
ylab("Global Active and Reactive in KW/h")+
ggtitle("Global Active and Reactive by Months")+
facet_wrap(facets = Year ~ .)
##Reactive by month##
ggplot(data=HPC_my, aes(HPC_my$MonthAbb,group=1))+
  geom_line(aes(y=HPC_my$Global_reactive_powerKWh,color="Global Reactive power"))+
  theme_bw()+
  xlab("Month")+
  ylab("Reactive in KW/h")+
  ggtitle("Reactive by Months")+
  facet_wrap(facets = Year ~ .)

##sub metters 1,2,3 by month##

ggplot(data=HPC_my, aes(HPC_my$MonthAbb,group=1))+
  geom_line(aes(y=HPC_my$Sub_metering1KW,color="Kitchen"))+
  geom_line(aes(y=HPC_my$Sub_metering2KW,color="Laundry Room"))+
  geom_line(aes(y=HPC_my$Sub_metering3KW,color="Water Heater and Air-Conditioner"))+
  theme_bw()+
  xlab("Month")+
  ylab("Global Consumption in KW/h")+
  ggtitle("Global Consumption Monthly by Sub Metering")+
  facet_wrap(facets = Year ~ .)

####Day####
##Reactive and Active power by Day##
ggplot(data=HPC_d, aes(HPC_d$Day))+
  geom_line(aes(y=HPC_d$Global_reactive_powerKWh,color="Global Reactive power"))+
  geom_line(aes(y=HPC_d$Global_active_powerKWh,color="Global Active power"))+
  theme_bw()+
  xlab("Day")+
  ylab("Global Active and Reactive in KW/h")+
  ggtitle("Global Active and Reactive by Day")+
  facet_wrap(facets = Year ~ .)
##reactive alone##
ggplot(data=HPC_d, aes(HPC_d$Day))+
  geom_line(aes(y=HPC_d$Global_reactive_powerKWh,color="Global Reactive power"))+
  theme_bw()+
  xlab("Day")+
  ylab("Global Reactive in KW/h")+
  ggtitle("Global Reactive by Day")+
  facet_wrap(facets = Year ~ .)
##active alone##
ggplot(data=HPC_d, aes(HPC_d$Day))+
  geom_line(aes(y=HPC_d$Global_active_powerKWh,color="Global Active power"))+
  theme_bw()+
  xlab("Day")+
  ylab("Global Active in KW/h")+
  ggtitle("Global Active by Day")+
  facet_wrap(facets = Year ~ .)
##sub metters 1,2,3 by Day##

ggplot(data=HPC_d, aes(HPC_d$Day))+
  geom_line(aes(y=HPC_d$Sub_metering1KW,color="Kitchen"))+
  geom_line(aes(y=HPC_d$Sub_metering2KW,color="Laundry Room"))+
  geom_line(aes(y=HPC_d$Sub_metering3KW,color="Water Heater and Air-Conditioner"))+
  theme_bw()+
  xlab("Day")+
  ylab("Global Consumption in KW/h")+
  ggtitle("Global Consumption Daily by Sub Metering")+
  facet_wrap(facets = Year ~ .)
####Hour####
####Reactive and Active power by Hour####
ggplot(data=HPC_hour, aes(HPC_hour$Hour))+
  geom_line(aes(y=HPC_hour$Global_reactive_powerKWh,color="Global Reactive power"))+
  geom_line(aes(y=HPC_hour$Global_active_powerKWh,color="Global Active power"))+
  theme_bw()+
  xlab("Hour")+
  ylab("Global Active and Reactive in KW/h")+
  ggtitle("Global Active and Reactive by Day")+
  facet_wrap(facets = Year ~ .)
##Reactive by Hour##
ggplot(data=HPC_hour, aes(HPC_hour$Hour))+
  geom_line(aes(y=HPC_hour$Global_reactive_powerKWh,color="Global Reactive power"))+
  theme_bw()+
  xlab("Hour")+
  ylab("Global Reactive in KW/h")+
  ggtitle("Global Reactive by Hour")+
  facet_wrap(facets = Year ~ .)
##Active by Hour##
ggplot(data=HPC_hour, aes(HPC_hour$Hour))+
  geom_line(aes(y=HPC_hour$Global_active_powerKWh,color="Global Active power"))+
  theme_bw()+
  xlab("Hour")+
  ylab("Global Active in KW/h")+
  ggtitle("Global Active by Hour")+
  facet_wrap(facets = Year ~ .)
##Sub meters by Hour##
ggplot(data=HPC_hour, aes(HPC_hour$Hour))+
  geom_line(aes(y=HPC_hour$Sub_metering1KW,color="Kitchen"))+
  geom_line(aes(y=HPC_hour$Sub_metering2KW,color="Laundry Room"))+
  geom_line(aes(y=HPC_hour$Sub_metering3KW,color="Water Heater and Air-Conditioner"))+
  theme_bw()+
  xlab("Hour")+
  ylab("Global Consumption in KW/h")+
  ggtitle("Global Consumption Hourly by Sub Metering")+
  facet_wrap(facets = Year ~ .)

##submeters Hour##
ggplot(data=HPC_hour, aes(HPC_hour$Hour))+
  geom_line(aes(y=HPC_hour$Sub_metering1KW,color="Kitchen"))+
  geom_line(aes(y=HPC_hour$Sub_metering2KW,color="Laundry Room"))+
  geom_line(aes(y=HPC_hour$Sub_metering3KW,color="Water Heater and Air-Conditioner"))+
  theme_bw()+
  xlab("Hour")+
  ylab("Global Consumption in KW/h")+
  ggtitle("Global Consumption Hourly by Sub Metering")+
  facet_wrap(facets = Year ~ .)
