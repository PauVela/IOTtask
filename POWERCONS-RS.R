#### Install Packages ####

install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("arulesViz",dependencies = c("Depends", "Suggests"))
pacman::p_load( corrplot, data.table, party, caret, dplyr, arules, prabclus, trimcluster)
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("tidyr")){install.packages("tidyr")}
install.packages("plotly")
install.packages("tidyr")
install.packages("chron")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("zoo")
install.packages("DescTools")
####Load Data####
library(DescTools)
library(dplyr)
library(zoo)
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

household_power_consumption <- read_delim("household_power_consumption.txt", 
                                          ";", escape_double = FALSE, trim_ws = TRUE)
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

HPC %>% select(Date_Time,Date,Hour)  %>%
  filter(Date >= "2007-03-25" & Date <="2007-03-26") ##hour change at 2am

HPC<-mutate(HPC, Date_Time=
ifelse(Date_Time >= as_datetime('2007-03-25 02:00:00') & Date_Time <= as_datetime('2007-10-28 01:59:00'),Date_Time+ hours(1),
ifelse(Date_Time >= as_datetime('2008-03-30 02:00:00') & Date_Time <= as_datetime('2008-10-26 01:59:00'),Date_Time+ hours(1),
ifelse(Date_Time >= as_datetime('2009-03-29 02:00:00') & Date_Time <= as_datetime('2009-10-29 01:59:00'),Date_Time+ hours(1),
ifelse(Date_Time >= as_datetime('2010-03-28 02:00:00') & Date_Time <= as_datetime('2010-10-31 01:59:00'),Date_Time+ hours(1),Date_Time )))))



##GETSEASON##

Winter <- as_date("2008-12-21") #Winter Solstice
Spring <- as_date("2008-3-20") #Spring Equinox
Summer <- as_date("2008-6-21") #Summer Solstice
Autumn <- as_date("2008-9-22") # Fall Equinox

seasondates<-as_date(format(HPC$Date,"%2008-%m-%d"))

HPC$Season<-ifelse(seasondates>=as.Date(Autumn) &  seasondates<=as.Date(Winter), "Autumn",
                          ifelse(seasondates>=as.Date(Spring) & seasondates<=as.Date(Summer), "Spring",
                                 ifelse(seasondates>=as.Date(Summer) & seasondates<=as.Date(Autumn), "Summer","Winter")))

HPC$Season<-as.factor(HPC$Season)

####Data info####
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
#####box-plot####
#outliers
boxplot(HPC$Global_active_powerKWh)
Outlier(HPC$Global_active_powerKWh, method = c("boxplot"), na.rm = FALSE)
boxplot(HPC$Global_reactive_powerKWh)
Outlier(HPC$Global_reactive_powerKWh, method = c("boxplot"), na.rm = FALSE)
boxplot(HPC$Sub_metering1KW)
Outlier(HPC$Sub_metering1KW, method = c("boxplot"), na.rm = FALSE)
boxplot(HPC$Sub_metering2KW)
Outlier(HPC$Sub_metering2KW, method = c("boxplot"), na.rm = FALSE)
boxplot(HPC$Sub_metering3KW)
Outlier(HPC$Sub_metering3KW, method = c("boxplot"), na.rm = FALSE)
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
  group_by(Year,month,Day)%>%
  filter(Year!=2006)%>%
  summarise_at(vars(Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW),
               funs(sum))
##Hour##
HPC_hour<-HPC_2 %>% select(Year,month,months,Day,Hour,WeekDay,Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW)%>%
  group_by(Year,month,Day,Hour)%>%
  filter(Year!=2006)%>%
  summarise_at(vars(Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW),
               funs(sum))

##Seasonal##

HPC_2$Season <-factor(HPC_2$Season,
                           levels = c("Winter","Spring","Summer","Autumn"))
HPC_season<-HPC_2 %>% select(Year,month,months,Season,Day,Hour,WeekDay,Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW)%>%
  group_by(Year,Season)%>%
  filter(Year!=2006)%>%
  summarise_at(vars(Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW),
               funs(sum))

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
##Reactive and Active power by Hour##
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

####Season####
##Reactive and Active by Season##
ggplot(data=HPC_season, aes(HPC_season$Season))+
  geom_line(aes(y=HPC_season$Global_reactive_powerKWh,color="Global Reactive power"))+
  geom_line(aes(y=HPC_season$Global_active_powerKWh,color="Global Active power"))+
  theme_bw()+
  xlab("Month")+
  ylab("Global Active and Reactive in KW/h")+
  ggtitle("Global Active and Reactive by Season")+
  facet_wrap(facets = Year ~ .)
####TimeSeries####

install.packages("forecast")
library("forecast")
##month##
HPCMONTHLY <- ts(HPC_my, frequency = 12, start=c(2007,1),end=c(2011))
HPCMONTHLY
plot(HPCMONTHLY)
HPCMONTHLYDesc<-decompose(HPCMONTHLY[,3])
plot(HPCMONTHLYDesc)

HPCDAILY<- ts(HPC_d, frequency= 365, start=c(2007,1))
HPCDAILY
plot(HPCDAILY)
