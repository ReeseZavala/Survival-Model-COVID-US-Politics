rm(list = ls())
library(survival)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(survminer)
library(broom)
library(knitr)

coughTweet <- read.csv("C:\\Users\\rzava\\Box Sync\\Fall 2020\\QPMII\\Project\\ConvergingData\\TeamCOVID_MergedFull.csv")

#Decay Function
decay_func <- function(N = NULL,t = NULL,h = NULL){
 {return(N*(1/2)^(t/h))}
}

# Create TVCs
coughTweet <- coughTweet %>%
  mutate(days = as.Date(Date) - as.Date("2020-02-01") + 1) %>%
  mutate(totalCases.dec = decay_func(N = as.numeric(totalCases), t = as.numeric(days), h = 182)) %>%
  mutate(dailyCases.dec = decay_func(N = as.numeric(dailyCases), t = as.numeric(days), h = 182)) %>%
  mutate(totalDeaths.dec = decay_func(N = as.numeric(totalDeaths), t = as.numeric(days), h = 182)) %>%
  mutate(dailyDeaths.dec = decay_func(N = as.numeric(dailyDeaths), t = as.numeric(days), h = 182)) 
  
# Model 2:







#class(coughTweet$Date[1])

# Alternate Option

# decay_func <- function(N = startvalue,t = time(day?),h = halflife){
#  {return(N*(1/2)^(t/h))}
# }
# data$case_decay <- NULL
# for (i in data$cases) {
#  decay <- decay_func(data$cases[i], t, h)
#  data$case_decay <- c(data$case_decay, decay)
# }
