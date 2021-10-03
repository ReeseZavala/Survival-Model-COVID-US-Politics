rm(list = ls())

##Read in data
countyDat <- read.csv("C:\\Users\\rzava\\Box Sync\\Fall 2020\\QPMII\\Project\\COVID Data\\CA_gov_statewide_cases.csv", header = T)
poppN <- read.csv("C:\\Users\\rzava\\Box Sync\\Fall 2020\\QPMII\\Project\\COVID Data\\Counties.csv", header = F)
##make copy
CA <- countyDat

CA[date >= "2020-03-01" & date <= "2020-07-31"]

CA <- CA[c(CA$date >= "2020-03-01" & CA$date <= "2020-07-31"), ]

##pasting County to match poppN data
for (i in 1:nrow(CA)){
  CA[i,1] <- paste0(CA[i,1], " County", sep = "")
}
##Removing variables with no population data
CA <- CA[!CA$county == "Out Of Country County",]
CA <- CA[!CA$county == "Unassigned County",]

##Declare new columns
CA$population <- c(rep(NA, nrow(CA)))
CA$newConfirmedProp <- c(rep(NA, nrow(CA)))
CA$newDeadProp <- c(rep(NA, nrow(CA)))
CA$totalConfirmedProp <- c(rep(NA, nrow(CA)))
CA$totalDeadProp <- c(rep(NA, nrow(CA)))

##Addin proportional values using population
for (i in 1:nrow(CA)){
  CA[i, 7] <- poppN[poppN$V2 == CA[i,1],3]
  CA$newConfirmedProp[i] <- CA$newcountconfirmed[i]/CA$population[i] * 100000
  CA$newDeadProp[i] <- CA$newcountdeaths[i]/CA$population[i] * 100000
  CA$totalConfirmedProp[i] <- CA$totalcountconfirmed[i]/CA$population[i] * 100000
  CA$totalDeadProp[i] <- CA$totalcountdeaths/CA$population[i] * 100000
}



write.csv(CA, file = "C:\\Users\\rzava\\Box Sync\\Fall 2020\\QPMII\\Project\\COVID Data\\CA_cases.csv")

