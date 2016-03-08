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
stormsDamage <- storms[TotalDamage > 0,]

DamageType <- ddply(stormsDamage, .(EVTYPE), summarise, 
                    TotalDamage = sum(TotalDamage))
DamageType$EVTYPE <- as.character(DamageType$EVTYPE)
DamageType[DamageType$EVTYPE == "TORNAO", 1] <- "TORNADO"
plot(DamageType[DamageType$TotalDamage > 2e10,"EVTYPE"],
     DamageType[DamageType$TotalDamage > 2e10,"TotalDamage"],
     type = "p", pch = "16")


boxplot(DamageType$TotalDamage)

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
sum <- sum(DamageType[DamageType$EVTYPE == "Thunderstorm" |
                      DamageType$EVTYPE == "TSTM",2])
new <- data.frame("Thunderstorm", sum)
names(new) <- names(DamageType)
DamageType <- rbind(DamageType, new)


#omit too small damages
SevereDamage <- subset(DamageType, TotalDamage >= 10000000)
SevereDamage <- arrange(SevereDamage, desc(TotalDamage))
#plot
#newdamage <- ifelse(SevereDamage$TotalDamage/1e7 > 4000, 
#                    SevereDamage$TotalDamage/1e7 - 2500, SevereDamage$TotalDamage/1e7)
#newdamage <- ifelse(newdamage > 5500, newdamage - 5500, newdamage)
newdamage <- SevereDamage$TotalDamage/1e7
newdamage <- c(newdamage[4:length(newdamage)], newdamage[1:3])
l <- length(newdamage)

barplot(c(newdamage[1:(length(newdamage)-3)],0,0,0), 
        names.arg = c(SevereDamage$EVTYPE[-(1:3)], SevereDamage$EVTYPE[1:3]), las =2)
barplot(tail(newdamage,3)/15, las =2, add = T, col = "red", 
        space = c(l-3+(l-2)*.2,.2,.2))
title(ylab = "Total Damage (10 Million $)")

for (i in 14:(length(newdamage)-3)){
       text(i*1.2-.5, newdamage[i]+130, round(newdamage[i], digits = 0), srt = 90)
}
for (i in (length(newdamage)-2):length(newdamage)){
       text(i*1.2-.5, newdamage[i]/15+230, round(newdamage[i], digits = 0), srt = 90)
}



abline(h = 1300)
barplot(log(SevereDamage$TotalDamage))


library(tm)
stormCorpus <- Corpus(VectorSource(as.character(DamageType$EVTYPE)))
stormCorpus <- tm_map(stormCorpus, PlainTextDocument)
stormCorpus <- tm_map(stormCorpus, removePunctuation)
stormCorpus <- tm_map(stormCorpus, removeWords, stopwords("english"))
stormCorpus <- tm_map(stormCorpus, stemDocument)
stormCorpus <- tm_map(stormCorpus, content_transformer(toupper))
library(wordcloud)
wordcloud(stormCorpus)
c




library(ggplot2)
boxplot(DamageType$TotalDamage, ylim = c(1000, 4000000)), data = DamageType[1:5,])



DamageType[DamageType$TotalDamage == max(DamageType$TotalDamage), ]
DamageType[DamageType$TotalDamage > quantile(DamageType$TotalDamage, probs = .99),]
DamageType[DamageType$TotalDamage > 2e10,]

plot(stormsDamage1q$BGN_DATE, stormsDamage1q$TotalDamage)
which(stormsDamage1q$TotalDamage == max(stormsDamage1q$TotalDamage))
stormsDamage1q[113875,]
