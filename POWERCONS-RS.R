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
                                          ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                                                       Time = col_time(format = "%H:%M:%S")), 
                                          trim_ws = TRUE)
View(household_power_consumption)
View(household_power_consumption)



####pre-process####

##creating a column with date+time##
HPC<- household_power_consumption
View(HPC)
HPC$Date_Time <- paste(HPC$Date,HPC$Time)
HPC$Date_Time <- strptime(HPC$Date_Time, "%Y-%m-%d %H:%M:%S")
HPC <- HPC[,c(ncol(HPC), 1:(ncol(HPC)-1))]
class(HPC$Date_Time)


##Create a column with Month and Year###
HPC$Month <- month(HPC$Date_Time)
HPC<-HPC[,c(ncol(HPC),1:(ncol(HPC)-1))]
HPC$Year <- year(HPC$Date_Time)
HPC<-HPC[,c(ncol(HPC),1:(ncol(HPC)-1))]
HPC$Hour<-hour(HPC$Time)
HPC$month<-month(HPC$Date)
HPC$months<-months(HPC$Date)


##Data info##
View(HPC)
summary(HPC) ##na's problems##
length(HPC)
summary(HPC$Global_active_power)
str(HPC)
options(scipen = 999)
with(HPC, hist(Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", col = "red"))
plot(x = HPC$Date_Time, y = HPC$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
HPC$Date_Time <- as.POSIXct(HPC$Date_Time,
                           format = "%Y-%m-%d %H:%M%:%S", tz = "Europe/Paris")
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

##removing NA##

HPC_2<- HPC
HPC_2<- na.omit(HPC)

##info by year##
####2007 info####

HPC_y <- HPC_2 %>% select(Year,month,Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW)%>%
  group_by(Year)%>%
  filter(Year!=2006)%>%
  summarise_at(vars(Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW),
               funs(sum))

HPC_my <- HPC_2 %>% select(Year,month,months,Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW)%>%
  group_by(Year,month)%>%
  filter(Year!=2006)%>%
  summarise_at(vars(Global_powerKwh,Global_active_powerKWh,Global_reactive_powerKWh,Sub_metering1KW,Sub_metering2KW,Sub_metering3KW),
               funs(sum))

HPC_my<- transform(HPC_my, MonthAbb = month.abb[month])
HPC_my$MonthAbb <-factor(HPC_my$MonthAbb,
                             levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


ggplot(data=HPC_my, aes(x=MonthAbb,y=Global_powerKwh, group=Year,colour=Year)) +
  geom_line()+theme_bw()+xlab("Month")+ylab("Global Power in KW/h")+ggtitle("Global Power by Months")+
  geom_point()+facet_wrap(facets = Year ~ .)

ggplot(data=HPC_my, aes(x=MonthAbb, y=Global_powerKwh, group=Year,colour=Year)) +
  geom_line()+theme_bw()+
  geom_point()+facet_grid(facets = Year ~ ., margins = FALSE)

ggplot(data=HPC_my, aes(x=MonthAbb,y=Sub_metering1KW, group=Year,colour=Year)) +
  geom_line()+theme_bw()+xlab("Month")+ylab("Sub meterings")+ggtitle("Sub meterings")+
  geom_point()+facet_wrap(facets = Year ~ .)


