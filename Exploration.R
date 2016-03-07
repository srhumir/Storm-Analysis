if (!dir.exists("data")) dir.create("data")
if (!file.exists("./data/Storm-Data.csv.bz2")) {
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                      destfile = "./data/Storm-Data.csv.bz2")
}
storms <- read.csv("./data/Storm-Data.csv.bz2")
storms$BGN_DATE <- as.Date(data$BGN_DATE, "%m/%d/%Y")


convert <- data.frame(exp = c(as.character(levels(storms$PROPDMGEXP)),"k"), 
                     value = c(0,0,0,10,10,10,10,10,10,10,10,10,10,1000000000,100,100,1000,1000000,100000, 1000))
propexp <- relable(storms$PROPDMGEXP, convert)
cropexp <- relable(storms$CROPDMGEXP, convert)

library(data.table)
storms <- as.data.table(storms)
storms[, PropDamage := propexp * PROPDMG]
storms[, CropDamage := cropexp * CROPDMG]
storms[, TotalDamage := PropDamage + CropDamage]
stormsDamage <- storms[TotalDamage >0,]
stormsDamage1q <- stormsDamage[TotalDamage > 3000,]
DamageType <- ddply(stormsDamage, .(EVTYPE), summarise, 
                    TotalDamage = sum(TotalDamage))
DamageType$EVTYPE <- as.character(DamageType$EVTYPE)
plot(as.factor(DamageType[DamageType$TotalDamage > 2e10,"EVTYPE"]),
     DamageType[DamageType$TotalDamage > 2e10,"TotalDamage"],
     type = "p", pch = "16")




DamageType[DamageType$TotalDamage == max(DamageType$TotalDamage), ]
DamageType[DamageType$TotalDamage > quantile(DamageType$TotalDamage, probs = .99),]
DamageType[DamageType$TotalDamage > 2e10,]

plot(stormsDamage1q$BGN_DATE, stormsDamage1q$TotalDamage)
which(stormsDamage1q$TotalDamage == max(stormsDamage1q$TotalDamage))
stormsDamage1q[113875,]
