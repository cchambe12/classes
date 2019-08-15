## 16 July 2019 - Cat
# Aim is to investigate the relationship between growth traits and the duration of vegetative risk
# Below is code for 2018 and then initial code for 2019

## Housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

### Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

### First start with 2018 data...
# Set Working Directory
setwd("~/Documents/git/CGtraits/analyses")
cg18 <-read.csv("input/2018_CG_datasheet.csv", header=TRUE)

## Clean data
cg18<-gather(cg18, "date","bbch", -Ind, -Plot)
cg18<-na.omit(cg18)
cg18$date<-substr(cg18$date, 2,8)
cg18$date<-as.character(as.Date(cg18$date,"%m.%d.%y"))
cg18$doy<-yday(cg18$date)
cg18$species<-substr(cg18$Ind, 0,6)
cg18<-dplyr::select(cg18, -date)
cg18$species<-ifelse(cg18$species=="betpap", "BETPAP", cg18$species)
cg18$bbch<-gsub(",", " ", cg18$bbch, fixed=TRUE)


cg18<-cg18[!(cg18$bbch==""),]
dx<-separate(cg18, bbch, into = c("first", "second"), sep = " (?=[^ ]+$)")
dx<-separate(dx, first, into = c("first", "third"), sep = " (?=[^ ]+$)")

dx$first <- substr(dx$first, 0, 2)
dx$second <- substr(dx$second, 0, 2)
dx$third <- substr(dx$third, 0, 2)

dx$bb<-NA
dx$bb<-ifelse(dx$first=="9" | dx$first=="9-" | dx$first=="11" | dx$second=="9" | dx$second=="9-" |
                dx$second=="11" | dx$third=="9" | dx$third=="9-" | dx$third=="11", dx$doy, dx$bb)
dx$lo<-NA
dx$lo<-ifelse(dx$first=="19" | dx$second=="19" | dx$third=="19", dx$doy, dx$lo)

drisk<-dx%>%dplyr::select(Ind, Plot, bb, lo, species)
drisk<-drisk[!(is.na(drisk$bb) & is.na(drisk$lo)),]

bb<-drisk[!is.na(drisk$bb),]
bb$budburst<-ave(bb$bb, bb$Ind, bb$Plot, FUN=min)
bb<-subset(bb, select=c("Ind", "Plot", "budburst"))
bb<-bb[!duplicated(bb),]
lo<-drisk[!is.na(drisk$lo),]
lo$leafout<- ave(lo$lo, lo$Ind, lo$Plot, FUN=min) 
lo<-subset(lo, select=c("Ind", "Plot", "leafout"))
lo<-lo[!duplicated(lo),]

dvr<-full_join(bb, lo)
dvr$risk<-dvr$leafout-dvr$budburst 

dvr<-na.omit(dvr)
### Starting here, need to check code... ####
dvr<- dvr[order(dvr$Ind, dvr$risk), ]
dvr$Ind<-paste(dvr$Ind, dvr$Plot, sep="_")
dvr$ind.risk<-paste(dvr$Ind, dvr$risk, sep=",")
days.btw <- Map(seq, dvr$budburst, dvr$leafout, by = 1)

dxx <- data.frame(ind.risk = rep.int(dvr$ind.risk, vapply(days.btw, length, 1L)), 
                  doy = do.call(c, days.btw))

dxx$Ind<-gsub(",.*", "", dxx$ind.risk)
dxx$risk<-gsub(".*,", "", dxx$ind.risk)
dxx<-dplyr::select(dxx, -ind.risk)
dxx$budburst<-ave(dxx$doy, dxx$Ind, FUN=min)
dxx$leafout<-ave(dxx$doy, dxx$Ind, FUN=max)

dvr<-dplyr::select(dvr, Ind, Plot)

dvr<-full_join(dxx, dvr)
dvr$doy <- NULL
dvr <- dvr[!duplicated(dvr),]
dvr$year <- 2018
dvr <- dvr %>% rename(id = Ind)

dvr <- separate(data = dvr, col = id, into = c("spp", "site", "ind", "plot"), sep = "\\_")

## fix warning
dvr[is.na(dvr$plot), c("ind", "plot")] <- dvr[is.na(dvr$plot), c("plot", "ind")] 

dvr$year <- 2018
#dvr$last.obs <- 274
dvr$Plot <- NULL
dvr$risk <- as.numeric(dvr$risk)

#write.csv(dvr, file="output/dvr_cg_2018.csv", row.names=FALSE)

### Let's add in traits data now!### 
traits<-read.csv("output/clean_traits.csv", header=TRUE)

traits$d.index<-traits$perim/(2*sqrt(traits$area*pi))
traits$Ind<-paste(traits$species, traits$site, traits$ind, traits$plot, sep="_")
traits$d.index<-ave(traits$d.index, traits$Ind)
traits$plot <- as.character(traits$plot)

traits.clean<-full_join(dvr, traits)
traits.clean$sla<-traits.clean$area/traits.clean$dr.wt

traits.clean<-subset(traits.clean, select=c("id","budburst", "leafout", "risk", "d.index", "species", "area", "sla"))
traits.clean<-traits.clean[!duplicated(traits.clean),]
traits.clean<-traits.clean[!(traits.clean$risk<0),]
traits.clean<-na.omit(traits.clean)
traits.clean$area<-ave(traits.clean$area, traits.clean$id)
traits.clean$sla<-ave(traits.clean$sla, traits.clean$id)
traits.clean$d.index<-ave(traits.clean$d.index, traits.clean$id)
traits.clean<-traits.clean[!duplicated(traits.clean),]


### Now some starter code for 2019!
# Set Working Directory
setwd("~/Documents/git/wildhellgarden/analyses/") ## adjust as necessary!
cg19 <-read.csv("2019_data/2019_CG_datasheet.csv", header=TRUE) 

## Now let's clean 2019 data
cg19$id <- paste(cg19$Ind, cg19$Plot, sep="_")
cg19$Ind<-NULL
cg19$Plot<-NULL
cg19 <- gather(cg19, "date", "bbch", -id, -Phase)
cg19 <- na.omit(cg19)
cg19 <- cg19[!(cg19$bbch==""),]

cg19$date <- gsub("X", "", cg19$date)
cg19$date <- as.Date(cg19$date, format="%m.%d.%Y")
cg19$doy <- yday(cg19$date)

cg19 <- cg19[(cg19$Phase=="Leaves"),]

cg19 <- subset(cg19, select=c("id", "doy", "bbch"))
cg19 <- separate(data = cg19, col = id, into = c("spp", "site", "ind", "plot"), sep = "\\_")
cg19$ind <- ifelse(is.na(cg19$ind), substr(cg19$spp, 7,8), cg19$ind)
cg19$ind <- ifelse(cg19$ind=="", "XX", cg19$ind)
cg19$spp <- substr(cg19$spp, 0, 6)
cg19$year <- 2019

cg19$bbch <- ifelse(cg19$bbch==10, 9, cg19$bbch)

cg19 <-cg19%>% 
  group_by(spp, site, ind, plot, bbch, year) %>% 
  slice(which.min(doy))
cg19<-cg19[!duplicated(cg19),]

cg19$budburst <- ifelse(cg19$bbch==9, cg19$doy, NA)
cg19$leafout <- ifelse(cg19$bbch==19, cg19$doy, NA)

cg19 <- subset(cg19, select=c("spp", "year", "site", "ind", "budburst", "leafout", "plot"))
cg19$plot <- as.character(cg19$plot)

cg19 <- cg19[!duplicated(cg19),]

cg19 <- cg19 %>% 
  group_by(spp, site, ind, plot, year) %>% 
  summarise_all(list(~first(na.omit(.))))
cg19$risk <- cg19$leafout - cg19$budburst

cg <- full_join(cg19, dvr)

cg$provenance.lat <- NA
cg$provenance.long <- NA

cg$provenance.lat <- ifelse(cg$site == "HF", 42.531705, cg$provenance.lat)
cg$provenance.long <- ifelse(cg$site == "HF", -72.189920, cg$provenance.long)
cg$provenance.lat <- ifelse(cg$site == "WM", 44.112337, cg$provenance.lat)
cg$provenance.long <- ifelse(cg$site == "WM", -71.230138, cg$provenance.long)
cg$provenance.lat <- ifelse(cg$site == "GR", 44.794942, cg$provenance.lat)
cg$provenance.long <- ifelse(cg$site == "GR", -71.146683, cg$provenance.long)
cg$provenance.lat <- ifelse(cg$site == "SH", 45.932675, cg$provenance.lat)
cg$provenance.long <- ifelse(cg$site == "SH", -74.025070, cg$provenance.long)


#write.csv(cg, file="~/Documents/git/CGtraits/analyses/output/clean_cgandtraits.csv", row.names=FALSE)



