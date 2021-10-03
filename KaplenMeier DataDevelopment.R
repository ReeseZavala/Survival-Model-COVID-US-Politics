rm(list = ls())

#install.packages("survival")
library(survival)
library(lubridate)
library(tidyverse)
library(stringr)

Peeps <- read.csv("C:\\Users\\rzava\\Box Sync\\Fall 2020\\QPMII\\Project\\COVID data\\HandleInfo.csv")
CData <- read.csv("C:\\Users\\rzava\\Box Sync\\Fall 2020\\QPMII\\Project\\COVID data\\CA_cases.csv")
CitCount <- read.csv("C:\\Users\\rzava\\Box Sync\\Fall 2020\\QPMII\\Project\\COVID data\\list-cities-ca.csv")
RNTweet <- read.csv("C:\\Users\\rzava\\Box Sync\\Fall 2020\\QPMII\\Project\\ConvergingData\\GOP_ready.csv")

# Loading the Data:
RNTweet.GOP <- read.csv("C:\\Users\\rzava\\Box Sync\\Fall 2020\\QPMII\\Project\\ConvergingData\\GOP_ready.csv",comment.char="#")
RNTweet.DEM <- read.csv("C:\\Users\\rzava\\Box Sync\\Fall 2020\\QPMII\\Project\\ConvergingData\\DEM_ready.csv",comment.char="#")


####################################################################################################
######################################   STANDARDIZING DATA    #####################################
####################################################################################################

# Fixing Peeps
Peeps$Handle <- tolower(Peeps$Handle)
Problems <- rbind(c(0, "City of San Diego", 0, 0, 0, 0, 0, "@chrisjcate", 0, 0),
                  c(0, "City of San Diego", 0, 0, 0, 0, 0, "@kevin_faulconer", 0, 0),
                  c(0, "City of San Diego", 0, 0, 0, 0, 0, "@shermansd7", 0, 0),
                  c(0, "City of San Francisco", 0, 0, 0, 0, 0, "@aaronpeskin", 0, 0),
                  c(0, "City of San Francisco", 0, 0, 0, 0, 0, "@ahsha_safai", 0, 0),
                  c(0, "City of San Diego", 0, 0, 0, 0, 0, "@barbarabryd1", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@bobblumenfield", 0, 0),
                  c(0, "City of San Diego", 0, 0, 0, 0, 0, "@cd4monica", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@cd6nury", 0, 0),
                  c(0, "City of San Diego", 0, 0, 0, 0, 0, "@chriswardd3", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@cityattorneyla", 0, 0),
                  c(0, "City of San Diego", 0, 0, 0, 0, 0, "@cityattorneysd", 0, 0),
                  c(0, "City of San Diego", 0, 0, 0, 0, 0, "@cmjencampbell", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@currendpricejr", 0, 0),
                  c(0, "City of San Francisco", 0, 0, 0, 0, 0, "@d4gordonmar", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@davideryu", 0, 0),
                  c(0, "City of San Francisco", 0, 0, 0, 0, 0, "@deanpreston", 0, 0),
                  c(0, "City of San Diego", 0, 0, 0, 0, 0, "@ggomezd9", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@gilcedillo", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@herbjwesson", 0, 0),
                  c(0, "City of San Francisco", 0, 0, 0, 0, 0, "@hillaryronen", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@joebuscaino", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@kdeleon", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@keirjonesagency", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@lacontroller", 0, 0),
                  c(0, "City of San Francisco", 0, 0, 0, 0, 0, "@londonbreed", 0, 0),
                  c(0, "City of San Francisco", 0, 0, 0, 0, 0, "@matthaneysf", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@mayorofla", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@mhdcd8", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@mikeboninla", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@mitchofarrell", 0, 0),
                  c(0, "City of San Francisco", 0, 0, 0, 0, 0, "@mohlermargie", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@mrodcd7", 0, 0),
                  c(0, "City of San Francisco", 0, 0, 0, 0, 0, "@normanyeesf", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@paulkoretzcd5", 0, 0),
                  c(0, "City of Los Angeles", 0, 0, 0, 0, 0, "@paulkrekorian", 0, 0),
                  c(0, "City of San Francisco", 0, 0, 0, 0, 0, "@rafaelmandelman", 0, 0),
                  c(0, "City of San Francisco", 0, 0, 0, 0, 0, "@sandraleefewer", 0, 0),
                  c(0, "City of San Francisco", 0, 0, 0, 0, 0, "@shamannwalton", 0, 0),
                  c(0, "City of San Francisco", 0, 0, 0, 0, 0, "@supstefani", 0, 0),
                  c(0, "City of San Diego", 0, 0, 0, 0, 0, "@vivianmorenosd", 0, 0))

colnames(Problems) <- colnames(Peeps)
Peeps <- rbind(Peeps,Problems)

#Match Peeps format to dupRT
Peeps <- Peeps %>%
  mutate(City = str_remove_all(City, "City"))%>%
  mutate(City = str_remove_all(City, "Town"))%>%
  mutate(City = str_remove_all(City, "of"))%>%
  mutate(City = tolower(City))%>%
  mutate(City = str_squish(City))

CitCount <- CitCount %>%
  mutate(Name = str_remove_all(Name, " City"))%>%
  mutate(Name = str_remove_all(Name, " Town"))%>%
  mutate(Name = tolower(Name))

#Matching format
CData <- CData %>%
  mutate(county = str_remove_all(county, " County"))

# Data Managing:
colnames(RNTweet.GOP)[16] <- "DV"
colnames(RNTweet.DEM)[18] <- "DV"
RNTweet.GOP <- RNTweet.GOP[,-c(15)] 
RNTweet.DEM <- RNTweet.DEM[,-c(1,16,17)] 


# Merging Both DEM and GOP data to RNTweet:
colnames(RNTweet.GOP)==colnames(RNTweet.DEM) #  Cool!
RNTweet <- rbind(RNTweet.GOP, RNTweet.DEM)


# Remove unnecessary variables
Rtweets <- RNTweet[,-c(1,4:6,8,11:14)]
Rtweets$Date <- as.Date(paste(RNTweet$year, RNTweet$month, RNTweet$day, sep = "-"), format="%Y-%B-%d")
Rtweets <- Rtweets[which(!is.na(Rtweets$party)),]


# Redefine data:
Rtweets <- Rtweets %>%
  group_by(handle) %>%
  arrange(Date) %>%
  mutate(entry = Date[1]) %>%
  mutate(first1 = min(which(DV == 1 | row_number() == n())))%>%
  filter(row_number() <= first1)%>%
  mutate(exit = if_else(DV == 1, Date, Date[n()]))%>%
  arrange(handle, Date) %>%                                            
  slice(n()) %>%                  #take last observation in group
  mutate(duration = exit - entry) %>%
  mutate(duration2 = as.numeric(exit - entry)+1) %>%
  mutate(white = if_else((race == "white" | race == "white?"), 1, 0)) %>%
  mutate(pos.fac = as.factor(position)) %>%
  mutate(male = gender)


levels(Rtweets$pos.fac)[levels(Rtweets$pos.fac)=="mayor pro tem"] <- "mayor"
levels(Rtweets$pos.fac)[levels(Rtweets$pos.fac)=="city attorney"] <- "city council"
levels(Rtweets$pos.fac)[levels(Rtweets$pos.fac)=="city controller"] <- "city council"
levels(Rtweets$pos.fac)[levels(Rtweets$pos.fac)=="city manager"] <- "city council"



# Get rid of those without party:
Rtweets <- Rtweets[complete.cases(Rtweets),]

#adding cities and counties
Rtweets$city <- c(rep(NA, nrow(Rtweets)))
Rtweets$county <- c(rep(NA, nrow(Rtweets)))
Rtweets$logPop <- c(rep(NA, nrow(Rtweets)))

for (i in 1:nrow(Rtweets)) {
  n <- which(Peeps$Handle == Rtweets$handle[i])
  Rtweets$city[i] <- Peeps$City[n]
  k <- which(CitCount$Name == Rtweets$city[i])
  Rtweets$county[i] <- CitCount$County[k]
  Rtweets$logPop[i] <- log10(as.numeric(gsub(",","", CitCount$Population..2010.[k])))
}

#Adding cases/100k people on day of exit

Rtweets$totalCases <- c(rep(NA, nrow(Rtweets)))
Rtweets$totalDeaths <- c(rep(NA, nrow(Rtweets)))
for (i in 1:nrow(Rtweets)) {
  if (length(which(CData$date == Rtweets$exit[i] & CData$county == Rtweets$county[i])) == 0){
    #COVID data starts 3/18, could also treat this data as 0
    Rtweets$totalCases[i] <- 0
    Rtweets$totalDeaths[i] <- 0
  }
  else{
    c <- which(CData$date == Rtweets$exit[i] & CData$county == Rtweets$county[i])
    Rtweets$totalCases[i] <- CData$totalConfirmedProp[c]
    Rtweets$totalDeaths[i] <- CData$totalDeadProp[c]
  }
}

write.csv(Rtweets,  "C:\\Users\\rzava\\Box Sync\\Fall 2020\\QPMII\\Project\\ConvergingData\\KaplenMeier_FULLandCases.csv")
