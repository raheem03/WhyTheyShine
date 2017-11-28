########################################################################################
# File: BlackDisenfranchisement.R
# Date: 11/27/2017
# Author: Raheem Chaudhry
# Description: Data analysis related to blog post on disenfranchisement of formerly
# convicted.
# Notes:
########################################################################################

########################################################################################
# 0. SET ENVIRONMENT
########################################################################################

# Set environment
#Sys.setenv(censuskey="YOUR_CENSUS_KEY")

# Clear environment
rm(list=ls())

# Import packages
library(tidyverse)
library(dplyr)
library(jsonlite)

########################################################################################
# 1. DOWNLOAD AND LOAD DATA
########################################################################################

# The Sentencing Project
url <- "http://www.sentencingproject.org/wp-content/uploads/2016/10/6-Million-Lost-Voters.pdf"
download.file(url, "Lost-Voters.pdf", mode="wb")
# Manually extract pp 15-16; R packages not working here
blkDis <- read_csv("Disenfranchised-Black.csv")
colnames(blkDis) <- c("State", "DisBlk", "BlkVAP", "BlkPctDis")

totDis <- read_csv("Disenfranchised-Total.csv")
colnames(totDis) <- c("State", "TotDis", "TotVAP", "totPctDis")

# BJS
url <- "https://www.bjs.gov/content/pub/pdf/p15.pdf"
download.file(url, "Prisoners-By-Sex.pdf", mode="wb")
# Manually extract p5; R packages not working here
prisonersBySex <- read_csv("Prisoners-By-Sex.csv")
colnames(prisonersBySex) <- c("State", "TotPrisoners", "Male", "Female")

# ACS
url <- paste("https://api.census.gov/data/2015/acs/acs5?get=B01001B_001E,B01001B_002E,B01001B_003E,B01001B_004E,B01001B_005E,B01001B_006E,B01001B_017E,B01001B_018E,B01001B_019E,B01001B_020E,B01001B_021E,NAME&for=state:*&key=",Sys.getenv("censuskey"), sep='')
acs <- fromJSON(url)
colnames(acs) <- as.character(unlist(acs[1,]))
acs <- acs[-1,]
acs <- data.frame(acs)
acs[1:11] <- lapply(acs[1:11], function(x) as.numeric(as.character(x)))
acs <- rename(acs, blkMale = B01001B_002E, blkFemale = B01001B_017E, statefips = state, State = NAME)
acs[1:11] <- lapply(acs[1:11], function(x) as.numeric(as.character(x)))

acs$u18BlkMale <- acs$B01001B_003E + acs$B01001B_004E + acs$B01001B_005E + acs$B01001B_006E 
acs$u18BlkFemale <- acs$B01001B_018E + acs$B01001B_019E + acs$B01001B_020E + acs$B01001B_021E
acs$blkMaleVAP <- acs$blkMale - acs$u18BlkMale
acs$blkFemaleVAP <- acs$blkFemale - acs$u18BlkFemale
acs$blkVAP <- acs$blkMaleVAP + acs$blkFemaleVAP
varlist <- c("blkVAP", "blkMaleVAP", "blkFemaleVAP", "State", "statefips")
acs <- acs[varlist]

write_csv(acs, "ACS-Pop-By-Sex.csv")

########################################################################################
# 1. CREATE DATASET
########################################################################################

rm(df)
df <- inner_join(blkDis, totDis, by = "State")
df <- inner_join(df, prisonersBySex, by = "State")
df <- inner_join(df, acs, by = "State")

slaveStates <- c("Alabama", "Arkansas", "Delaware", "Florida", "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri", "New Mexico", "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia")
df$slaveStates <- ifelse(df$State %in% slaveStates, 1, 0)

df$pctBlkMen <- df$blkMaleVAP/df$blkVAP
df$pctBlkMenIncarcerated <- df$Male/df$TotPrisoners

df$blkMenVAP <- df$BlkVAP * df$pctBlkMen
df$blkMenDis <- df$DisBlk * df$pctBlkMenIncarcerated

df$TotDis <- as.numeric(gsub(",", "", df$TotDis))
df$TotDis <- as.numeric(df$TotDis)
df$TotDis[is.na(df$TotDis)] <- 0

df$allOthersDis <- df$TotDis - df$blkMenDis
df$allOthersVAP <- df$TotVAP - df$blkMenVAP

df$blkMenPctDis <- df$blkMenDis/df$blkMenVAP
df$allOthersPctDis <- df$allOthersDis/df$allOthersVAP

df$ratioBlkVoteToOthers <- (1 - df$blkMenPctDis)/(1-df$allOthersPctDis)

write_csv(df, "Disenfranchised-Final-Data.csv")

########################################################################################
# 1. SAMPLE VIZ
########################################################################################

rm(list=ls())
df <- read_csv("Disenfranchised-Final-Data.csv")
df <- rename(df, State = state)

attach(df)

total <- df %>%
  summarize(blkMenDis = sum(blkMenDis, na.rm=TRUE), blkMenVAP = sum(blkMenVAP,na.rm=TRUE), allOthersDis = sum(allOthersDis,na.rm=TRUE), allOthersVAP=sum(allOthersVAP,na.rm=TRUE)) %>%
  mutate(pctblkMenDis = blkMenDis/blkMenVAP, pctAllOthersDis = allOthersDis/allOthersVAP, ratio = (1-pctblkMenDis)/(1-pctAllOthersDis)) %>%
  select(pctblkMenDis, pctAllOthersDis, ratio)

total$id <- 1
total.long <- total[c("pctblkMenDis", "pctAllOthersDis", "id")] %>%
  reshape2::melt(id.vars="id")

ggplot(total.long[]) +
  geom_bar(mapping = aes(variable, value), stat = "identity")

slavestates <- df %>%
  group_by(slaveStates) %>%
  summarize(blkMenDis = sum(blkMenDis, na.rm=TRUE), blkMenVAP = sum(blkMenVAP,na.rm=TRUE), allOthersDis = sum(allOthersDis,na.rm=TRUE), allOthersVAP=sum(allOthersVAP,na.rm=TRUE)) %>%
  mutate(pctblkMenDis = blkMenDis/blkMenVAP, pctAllOthersDis = allOthersDis/allOthersVAP, ratio = (1-pctblkMenDis)/(1-pctAllOthersDis)) %>%
  select(slaveStates, pctblkMenDis, pctAllOthersDis, ratio)

slavestates$id <- 1
slavestates.long <- slavestates[c("pctblkMenDis", "pctAllOthersDis", "id")] %>%
  reshape2::melt(id.vars="id")

ggplot(slavestates.long[]) +
  geom_bar(mapping = aes(variable, value), stat = "identity")

########################################################################################
# END OF FILE
########################################################################################