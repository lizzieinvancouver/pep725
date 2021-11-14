### Started 27 Feb 2020 ###
### By Lizzie ###

## This code mainly tries to find sites along a latitudinal gradient ##
## I started with maps in Cat's regional risk paper, then searched for sites with enough years of data ##
## I write out the sites, and then eventually the phen data ##

## There's also some earlier code (see within FALSE{}) where I was just chekcing Cat's work ##

# safety feature(s)
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# set up
setwd("~/Documents/git/projects/misc/pep725/")

# get the PEP data -- the main bit 
phendata <- read.csv2("PEP725_records/PEP725_data.csv", skip=1, header=FALSE,col.names=c("PEP_ID", "PLANT_ID", "CULT_ID", "BBCH", "YEAR", "DAY"))
# station data, plant ID etc
statz <- read.csv2("PEP725_records/PEP725_stations.csv", header=TRUE)
plantz <- read.csv2("PEP725_records/PEP725_plant.csv", header=TRUE)
bbch <- read.csv2("PEP725_records/PEP725_BBCH.csv", header=TRUE)
cult <- read.csv2("PEP725_records/PEP725_cultivar.csv", header=TRUE)

sort(unique(plantz$sci_name))
plantz[grep("Betula", plantz$sci_name),] # 106

## Pull Cat's work ....
# NEEDS TO BE UPDATED based on new files ...
if(FALSE){
bp1 <- read.csv("/Users/Lizzie/Documents/git/projects/treegarden/decsens/analyses/pep_analyses/output/betpen_allchillsandgdds_45sites_mat_tntx_forsims.csv")
bp2 <- read.csv("/Users/Lizzie/Documents/git/projects/treegarden/decsens/analyses/pep_analyses/output/betpen_decsens_1950-2000.csv")

## Find a corresponding station ..
subset(statz, LAT==49.2667 & LON==8.75) # 3091
onestat <- subset(phendata, PEP_ID=="3091" & PLANT_ID==106 & BBCH==11)
subset(onestat, YEAR==1951 | YEAR==1954 | YEAR==2002)
## And compare ...
subset(bp1, lat==49.2667 & long==8.75) # looks good, but does not appear in bp2 so try another station ...
subset(statz, LAT==50.5167 & LON==8.81667) # 2009
twostat <- subset(phendata, PEP_ID=="2009" & PLANT_ID==106 & BBCH==11)
subset(twostat, YEAR==1951 | YEAR==1954 | YEAR==2002)
subset(bp2, lat==50.5167 & long==8.81667 & (year==1951|year==1954|year==2002))
}

## Update in Feb 2021
# Try to see how much data we have along latitudinal gradient into Scandanavia
# This is messy code where I try out different things by just commenting them in and out (sorry future Lizzie)

statz$latnum <- as.numeric(statz$LAT)
statz$lonnum <- as.numeric(statz$LON)

# use betpen
# as that's the only angiosperm with any data there based on looking at Cat's regional risk paper
# critical decision on which stage to use! Or should we just use BOTH? Shocking idea....
bpdat <- subset(phendata, PLANT_ID==106 & BBCH==10) # this stage best for getting a latitudinal gradient
# bpdat <- subset(phendata, PLANT_ID==106 & BBCH==11) # this stage best for longer term data
# bpdat <- rbind(bpdat10, bpdat11)

whichsites <- statz[which(statz$PEP_ID %in% unique(bpdat$PEP_ID)),]

whichsitesnorth <- subset(whichsites, latnum>54 & lonnum>3)
whichsitesfarnorth <- subset(whichsites, latnum>57 & lonnum>3)

library("maptools")
data(wrld_simpl)
EuropeList <- c('Germany', 'France', 'Sweden', 'Finland', 'Norway', 'Austria', 'Switzerland')
my_map <- wrld_simpl[wrld_simpl$NAME %in% EuropeList, ]
plot(my_map)

points(whichsites$LON, whichsites$LAT, pch=16, col="darkred", cex=0.5)
points(whichsitesnorth$LON, whichsitesnorth$LAT, pch=16, col="dodgerblue", cex=0.5)
points(whichsitesfarnorth$LON, whichsitesfarnorth$LAT, pch=16, col="skyblue", cex=0.5)

bpdatnorth <- bpdat[which(bpdat$PEP_ID %in% whichsitesnorth$PEP_ID),]
# bpdatnorth <- bpdat[which(bpdat$PEP_ID %in% whichsitesfarnorth$PEP_ID),]

table(bpdatnorth$PEP_ID) # hmm, 18 rows is often fewer years ...

library(plyr)
library(dplyr)
bpdatnorthsumm <-
      ddply(bpdatnorth, c("PEP_ID", "CULT_ID"), summarise,
      nyear = length(unique(YEAR)))

nrow(subset(bpdatnorthsumm, nyear>10)) # farnorth: 43 for BBCH 10 # 10 for BBCH 11
nrow(subset(bpdatnorthsumm, nyear>15)) # farnorth: 0 for BBCH 10 # 8 for BBCH 11
nrow(subset(bpdatnorthsumm, nyear>20)) # farnorth: 0 for BBCH 10 # # 8 for BBCH 11

siteztotrylist <- subset(bpdatnorthsumm, nyear>10)
siteztotry <- statz[which(statz$PEP_ID %in% unique(siteztotrylist$PEP_ID)),]
plot(my_map)
points(siteztotry$LON, siteztotry$LAT, pch=16, col="darkred", cex=1.5)

siteztotrybp <- bpdat[which(bpdat$PEP_ID %in% siteztotrylist$PEP_ID),]
sort(unique(siteztotrybp$YEAR)) # need 1959-2011

write.csv(siteztotry, "..//..//treegarden/decsens/analyses/pep_analyses/input/sitespeplatBBCH10.csv", row.names=FALSE)
write.csv(siteztotrybp, "..//..//treegarden/decsens/analyses/pep_analyses/input/sitespeplatBBCH10phendat.csv", row.names=FALSE)

# Check other BBCH available ...
northernstatz <- subset(statz, latnum>58 & lonnum>3)
bpdatall <- subset(phendata, PLANT_ID==106)
bpdatscan <- bpdatall[which(bpdatall$PEP_ID %in% northernstatz$PEP_ID),]
table(bpdatscan$BBCH) # 11 seems pretty good

