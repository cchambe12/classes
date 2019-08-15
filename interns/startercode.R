## Housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

### Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Set Working Directory
setwd("~/Documents/git/wildhellgarden/analyses/")
obs19 <-read.csv("2019_data/2019_CG_datasheet.csv", header=TRUE) 

obs19$id <- paste(obs19$Ind, obs19$Plot, sep="_")
obs19$Ind<-NULL
obs19$Plot<-NULL
clean <- gather(obs19, "date", "bbch", -id, -Phase)
clean <- na.omit(clean)
clean <- clean[!(clean$bbch==""),]

clean$date <- ifelse(clean$date=="4.3.19", "4.3.2019", foo$date)
clean$date <- gsub("X", "", clean$date)
foo <- clean
foo$date <- as.Date(foo$date, format="%m.%d.%Y")
foo$doy <- yday(foo$date)
