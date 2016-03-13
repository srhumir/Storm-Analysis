if (!dir.exists("data")) dir.create("data")
if (!file.exists("./data/Storm-Data.csv.bz2")) {
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                      destfile = "./data/Storm-Data.csv.bz2")
}
storms <- read.csv("./data/Storm-Data.csv.bz2")
storms$BGN_DATE <- as.Date(storms$BGN_DATE, "%m/%d/%Y")


convert <- data.frame(exp = c(as.character(levels(storms$PROPDMGEXP)),"k"), 
                     value = c(0,0,0,10,10,10,10,10,10,10,10,10,10,1000000000,100,100,1000,1000000,100000, 1000))
propexp <- relable(storms$PROPDMGEXP, convert)
cropexp <- relable(storms$CROPDMGEXP, convert)
library(plyr)
library(dplyr)
library(data.table)
storms <- as.data.table(storms)
storms[, PropDamage := propexp * PROPDMG]
storms[, CropDamage := cropexp * CROPDMG]
storms[, TotalDamage := PropDamage + CropDamage]
storms$EVTYPE <- as.character(storms$EVTYPE)

stormsDamage <- storms[TotalDamage > 0 | FATALITIES > 0 | INJURIES,]

DamageType <- ddply(stormsDamage, .(EVTYPE), summarise, 
                    TotalDamage = sum(TotalDamage), 
                    TotalFatality = sum(FATALITIES),
                    TotalInjuries = sum(INJURIES))
#DamageType$EVTYPE <- as.character(DamageType$EVTYPE)
DamageType[DamageType$EVTYPE == "TORNAO", 1] <- "TORNADO"


#the names are too much. There are replica due to difference in writing and 
#and too detailed categorfification.
namelist <- c("Flood", "Thunderstorm", "TSTM", "Rain", "Tornado", "Fire",
              "Squall", "Freez", "Hurricane", "Ice", "Hail", "waterspout", "Heat",
              "Glaze", "precipitation", "microburst", "Wet", "winter", "Cold",
              "Tsunami", "Snow", "Stream", "Lightning", "Dust", "Wintry Mix",
              "Urban", "Wind")
for ( x in namelist){
       DamageType <- sumduplicate(x, expensetable = DamageType)
}
library(Hmisc)
DamageType$EVTYPE <- capitalize(tolower(DamageType$EVTYPE))
#Thunderstorm and TSTM are the same
index <- which(DamageType$EVTYPE ==  "Thunderstorm" |
               DamageType$EVTYPE == "Tstm")
sum <- apply(DamageType[index,c(names(DamageType)[2:4])],2,sum)
new <- data.frame("Thunderstorm", t(sum))
names(new) <- names(DamageType)
DamageType <- rbind(DamageType[-index,], new)



#plot damages
#omit too small damages
SevereDamage <- subset(DamageType, TotalDamage >= 10000000)
SevereDamage <- arrange(SevereDamage, desc(TotalDamage))

newdamage <- SevereDamage$TotalDamage/1e7
newdamage <- c(newdamage[4:length(newdamage)], newdamage[1:3])
l <- length(newdamage)

barplot(c(newdamage[1:(length(newdamage)-3)],0,0,0), 
        names.arg = c(SevereDamage$EVTYPE[-(1:3)], SevereDamage$EVTYPE[1:3]),
        las =2)
barplot(tail(newdamage,3)/15, las =2, add = T, col = "red", 
        space = c(l-3+(l-2)*.2,.2,.2))
title(main = "Sever damages made by natural disasters", 
      sub = "Red bars are deviated data",
      ylab = "Total Damage (10 Million $)")
text(16,1300,"Red bars are associated to outliers")
for (i in 14:(length(newdamage)-3)){
       text(i*1.2-.5, newdamage[i]+130, round(newdamage[i], digits = 0), srt = 90)
}
for (i in (length(newdamage)-2):length(newdamage)){
       text(i*1.2-.5, newdamage[i]/15+150, round(newdamage[i], digits = 0), srt = 90)
}


#plot injuries and fatalities
FatInj <- subset(DamageType, TotalFatality > 30 | TotalInjuries > 100 )
FatInj <- arrange(FatInj, desc(TotalFatality), desc(TotalInjuries))

barplot(t(as.matrix(rbind(FatInj[-1,c("TotalFatality", "TotalInjuries")],c(0,0)))),
        horiz = F, names.arg = c(FatInj$EVTYPE[-1],FatInj$EVTYPE[1]), las = 2, 
        col = c("blue", "gray"), beside = F,
        legend.text = T, args.legend = c(xjust = 1.3))
barplot(t(as.matrix(FatInj[1,c("TotalFatality", "TotalInjuries")]))/7, add = T, 
        las =2, names.arg = "", space = dim(FatInj)[1]*1.2 -1, 
        col = c("red", "gray28"), beside = F)  
text(dim(FatInj)[1]*1.2 -.5, FatInj$TotalFatality[1]/10 + 1400,
     FatInj$TotalFatality[1], 
     srt = 90, col = "red")
text(dim(FatInj)[1]*1.2 -.5, FatInj$TotalInjuries[1]/10 + 1200,
     FatInj$TotalInjuries[1] + FatInj$TotalFatality[1], 
     srt = 90, col = "white")




library(tm)
stormCorpus <- Corpus(VectorSource(as.character(DamageType$EVTYPE)))
stormCorpus <- tm_map(stormCorpus, PlainTextDocument)
stormCorpus <- tm_map(stormCorpus, removePunctuation)
stormCorpus <- tm_map(stormCorpus, removeWords, stopwords("english"))
stormCorpus <- tm_map(stormCorpus, stemDocument)
stormCorpus <- tm_map(stormCorpus, content_transformer(toupper))
library(wordcloud)
wordcloud(stormCorpus)


