library(forecast)
library(dygraphs)
library(tidyverse)
library(ggplot2)
data(AirPassengers)

police_shootings <- read.csv(file = 'fatal-police-shootings-data.csv')
police_shootings$case = 1
police_shootings$date <- as.Date(police_shootings$date)
police_shootings$months <- format(police_shootings$date, "%Y-%m")


monthly_shootings_state <- police_shootings %>% group_by(months) %>% summarize(monthly_shootings = sum(case)) %>% print()


monthly_shootings_state_ts<-ts(monthly_shootings_state$monthly_shootings,start=c(2019,1), end=c(2022,4), frequency=12) 
monthly_shootings_state_ts


ggseasonplot(x = monthly_shootings_state_ts, polar = TRUE)+
  ylab("Number of Shootings")+
  ggtitle("Seasonal plot : Monthly fatal police shootings in USA.")

monthly_shootings_state_race <- police_shootings %>% group_by(months,race) %>% summarize(monthly_shootings = sum(case)) %>% print()

monthly_shootings_state_race%>%subset(race == "B")


monthly_shootings_state_race_ts<-ts(monthly_shootings_state_race$monthly_shootings,start=c(2019,1), end=c(2022,4), frequency=12) 


ggseasonplot(x = monthly_shootings_state_race_ts, polar = TRUE)+
  ylab("Number of Shootings")+
  ggtitle("Seasonal plot : Monthly fatal police shootings of Black ethnicity in USA.")

ggsubseriesplot(monthly_shootings_state_race_ts)

library(TSA)
acf(as.vector(monthly_shootings_state$monthly_shootings), main = "autocorrelation of the Monthly fatal police shootings")

auto.arima(monthly_shootings_state$monthly_shootings)
