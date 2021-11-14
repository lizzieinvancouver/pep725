## Started 14 November 2021 ##
## Trying to see if we can use the PEP 725 data for OSPREE ##
## Specifically, whether we can use it to correlate cues with leafout order ##

# safety feature(s)
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# set up
setwd("~/Documents/git/projects/misc/pep725/")
library(plyr)
library(ggplot2)
library(rgdal)

# which species and phasecodes show up?
phendata <- read.csv2("PEP725_records/PEP725_data.csv", skip=1, header=FALSE,col.names=c("PEP_ID", "PLANT_ID", "CULT_ID", "BBCH", "YEAR", "DAY"))

goo <- ddply(phendata, c("PLANT_ID", "CULT_ID", "BBCH"), summarise,
             N=length(PLANT_ID),
             meanyr=mean(YEAR)
             )

# station data 
statz <- read.csv2("PEP725_records/PEP725_stations.csv", header=TRUE)

# merge in cultivar and BBCH
plantz <- read.csv2("PEP725_records/PEP725_plant.csv", header=TRUE)
bbch <- read.csv2("PEP725_records/PEP725_BBCH.csv", header=TRUE)
cult <- read.csv2("PEP725_records/PEP725_cultivar.csv", header=TRUE)

dater <- merge(goo, bbch, by.x="BBCH", by.y="bbch", all.x=TRUE)
dater <- merge(dater, plantz, by.x="PLANT_ID", by.y="plant_id", all.x=TRUE)
dater <- merge(dater, cult, by.x="CULT_ID", by.y="cult_id", all.x=TRUE)

## above all copied from pep725spp.R 

# get the stage, then look for the species matches....
daterstage11 <- subset(dater, BBCH==11)
traitors.sp <- c("Acer_pensylvanicum",
                 "Acer_pseudoplatanus",
                 "Acer_saccharum",
                 "Aesculus_hippocastanum",
                 "Alnus_glutinosa",
                 "Alnus_incana",
                 "Betula_pendula",
                 "Betula_populifolia",
                 "Corylus_avellana",
                 "Fagus_grandifolia",
                 "Fagus_sylvatica",
                 "Fraxinus_excelsior",
                 "Juglans_regia",
                 "Populus_tremula",
                 "Prunus_padus",
                 "Prunus_serotina",
                 "Quercus_alba",
                 "Quercus_coccifera",
                 "Quercus_ilex",
                 "Quercus_petraea",
                 "Quercus_robur",
                 "Quercus_rubra",
                 "Quercus_velutina",
                 "Rhamnus_cathartica",
                 "Sorbus_aucuparia",
                 "Ulmus_pumila")
sort(traitors.sp)
sort(unique(daterstage11$sci_name)) 
sort(unique(paste(daterstage11$sci_name, daterstage11$cult_name)))
# this is a mess, let's just grab them by hand
peptraitorsp <- c("Acer Acer pseudoplatanus",
                  "Aesculus hippocastanum no specification",
                  "Alnus Alnus glutinosa",
                  "Alnus Alnus incana",
                  "Betula Betula pendula (B. verrucosa| B. alba)",
                  "Corylus avellana no specification",
                  "Fagus Fagus sylvatica",
                  "Fraxinus excelsior no specification",
                  "Juglans no specification",
                  "Populus Populus tremula",
                  "Quercus robur (Q.peduncula) no specification",
                  "Sorbus aucuparia no specification",
                  "Ulmus no specification")
# should CHECK: Fagus, Juglans, Prunus padus and the whole list really!
# not surprised the Quercus are missing as French data are generally not in PEP725 (at least not as of 2016)

daterstage11spp <- daterstage11[which(paste(daterstage11$sci_name, daterstage11$cult_name) %in% peptraitorsp),]
daterstage11spplist <- unique(paste(daterstage11spp$PLANT_ID, daterstage11spp$CULT_ID))

# now get the phendata!
phendata11 <- subset(phendata, BBCH==11)
phendata11$spcult <- paste(phendata11$PLANT_ID, phendata11$CULT_ID)
peptraitorsdat <- phendata11[which(phendata11$spcult %in% daterstage11spplist),]

# CHECK: here there are MUCH better ways to find a site x year combo with lots of data ... this is poor of me (but all I have time for)
peptraitorsdatsite <- aggregate(peptraitorsdat["DAY"], peptraitorsdat["PEP_ID"], FUN=length)
peptraitorsdatsite[order(-peptraitorsdatsite$DAY),][1:20,]
# here's one option ... 4495; 97361150; 10.4167; 48.4333; 460;BURGAU
onesite <- subset(peptraitorsdat, PEP_ID=="4495") 
aggregate(onesite["spcult"], onesite["YEAR"], FUN=length)
onesiteyear <- subset(onesite, YEAR==2000)


# and another ...
anothersite <- subset(peptraitorsdat, PEP_ID=="427") 
aggregate(anothersite["spcult"], anothersite["YEAR"], FUN=length)
anothersiteyear <- subset(anothersite, YEAR==2000)
    
# hand-coding below, someone should CHECK very closely!
lookuppeptraitorsp <- c("115 31"="Acer_pseudoplatanus",
                        "101 0"="Aesculus_hippocastanum",
                        "102 40"="Alnus_glutinosa",
                        "102 41"="Alnus_incana",
                        "106 20"="Betula_pendula",
                        "107 0"="Corylus_avellana",
                        "108 10"="Fagus_sylvatica",
                        "120 0"="Fraxinus_excelsior",
                        "203 0"="Juglans_regia",
                        "669 60"="Populus_tremula",
                        "111 0"="Quercus_robur",
                        "126 0"="Sorbus_aucuparia",
                        "147 0"="Ulmus_pumila")

onesiteyear$latbi <- unname(lookuppeptraitorsp[onesiteyear$spcult])
anothersiteyear$latbi <- unname(lookuppeptraitorsp[anothersiteyear$spcult])

onesiteyear[order(onesiteyear$DAY),]
anothersiteyear[order(anothersiteyear$DAY),]


if(FALSE){
# can use modelstraitors_stan.R
    # set runzscoremodel <- FALSE
    # and run through to end of ...
    # `Side bar to look at cues versus leafout day'
    
pepdat <- onesiteyear # anothersiteyear
# break down and build dataframe ...
dfhere <- data.frame(latbi=unique(bb.traitors$latbi),
                     intercept=a_sp[,"mean"],
                     chill=b_chill[,"mean"],
                     force=b_force[,"mean"],
                     photo=b_photo[,"mean"])
dfwpep <- merge(dfhere, pepdat, by="latbi")

par(mfrow=c(1,3))
plot(chill~DAY, data=dfwpep)
text(y=dfwpep$chill, x=dfwpep$DAY, labels=dfwpep$latbi)
plot(force~DAY, data=dfwpep)
text(y=dfwpep$force, x=dfwpep$DAY, labels=dfwpep$latbi)
plot(photo~DAY, data=dfwpep)
text(y=dfwpep$photo, x=dfwpep$DAY, labels=dfwpep$latbi)

}
    

