# SELECTIVITY FROM LENGTH TO AGES
#################################
rm(list=ls())

library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)

NS_alk <- read.csv ("ALK/ALK_NS_IBTS.csv")
head(NS_alk)
NS_alk[c(paste("Age",seq(0,10),sep="_"))] <- sapply(NS_alk[c(paste("Age",seq(0,10),sep="_"))], as.numeric)


setDT(NS_alk)
NS_alk <- melt(NS_alk, measure.vars = c(paste("Age",seq(0,10),sep="_")))
names(NS_alk)[names(NS_alk)=="variable"]="age"; names(NS_alk)[names(NS_alk)=="value"]="number"

NS_alk = NS_alk %>% 
  group_by(Survey, Year, Quarter, Area, AphiaID, Species, LngtClass, age) %>% 
  summarise(number = sum(number, na.rm=T))

laa_NS = NS_alk %>% 
  group_by(Survey, Year, Quarter, Area, AphiaID, Species, age) %>% 
  summarise(lgt_at_age = weighted.mean(x=LngtClass, w=number)) %>% # WEIGHTED MEAN!!
  filter(!is.na(lgt_at_age))


all_laa_NS <- data.frame()

for (i in unique(laa_NS$Species)){
  model <- mgcv::gam(lgt_at_age ~ age,
                     data = laa_NS[laa_NS$Species== i,])
  
  temp <- cbind(laa_NS[laa_NS$Species==i,],fitted=predict(model))
  temp <- temp[order(temp$age),]
  
  new <- data.frame(age=unique(temp$age))
  
  new$mean_length_at_age = predict(model, new, se.fit = TRUE)$fit/10
  
  new$species <- i
  
  all_laa_NS <- rbind(all_laa_NS, new)
  
  assign (paste0(i, "_laa_NS"), new)
}


ggplot(all_laa_NS, aes(x= as.numeric(age), y = mean_length_at_age, colour = species))+
  geom_line()+
  ggtitle("my NS")

####################################################################################
####################################################################################
###############     Try the BTS alk

BTS_alk <- read.csv ("ALK/ALK_BTS.csv")
head(BTS_alk)
BTS_alk[c(paste("Age",seq(0,10),sep="_"))] <- sapply(BTS_alk[c(paste("Age",seq(0,10),sep="_"))], as.numeric)


setDT(BTS_alk)
BTS_alk <- melt(BTS_alk, measure.vars = c(paste("Age",seq(0,10),sep="_")))
names(BTS_alk)[names(BTS_alk)=="variable"]="age"; names(BTS_alk)[names(BTS_alk)=="value"]="number"

BTS_alk = BTS_alk %>% 
  group_by(Survey, Year, Quarter, AphiaID, Species, LngtClass, age) %>% 
  summarise(number = sum(number, na.rm=T))

laa_BTS = BTS_alk %>% 
  group_by(Survey, Year, Quarter,AphiaID, Species, age) %>% 
  summarise(lgt_at_age = weighted.mean(x=LngtClass, w=number)) %>% # WEIGHTED MEAN!!
  filter(!is.na(lgt_at_age))


all_laa_BTS <- data.frame()

for (i in unique(laa_BTS$Species)){
  model <- mgcv::gam(lgt_at_age ~ age,
                     data = laa_BTS[laa_BTS$Species== i,])
  
  temp <- cbind(laa_BTS[laa_BTS$Species==i,],fitted=predict(model))
  temp <- temp[order(temp$age),]
  
  new <- data.frame(age=unique(temp$age))
  
  new$mean_length_at_age = predict(model, new, se.fit = TRUE)$fit/10
  
  new$species <- i
  
  all_laa_BTS <- rbind(all_laa_BTS, new)
  
  assign (paste0(i, "_laa_BTS"), new)
}


ggplot(all_laa_BTS, aes(x= as.numeric(age), y = mean_length_at_age, colour = species))+
  geom_line()+
  ggtitle("BTS")

## Compare NS Plaice and BTS
NS_Ple <- all_laa_NS[all_laa_NS$species == "Pleuronectes platessa", ]
NS_Ple$alk <- "NS"
BTS_Ple <- all_laa_BTS[all_laa_BTS$species == "Pleuronectes platessa", ]
BTS_Ple$alk <- "BTS"

ple <- rbind (NS_Ple, BTS_Ple)

ggplot(ple, aes(x= as.numeric(age), y = mean_length_at_age, colour = alk))+
  geom_line()

### Can we try for the Celtic Sea?

CS_alk <- read.csv ("ALK/ALK_2020_EVHOE.csv")

head(CS_alk)
CS_alk[c(paste("Age",seq(0,10),sep="_"))] <- sapply(CS_alk[c(paste("Age",seq(0,10),sep="_"))], as.numeric)

setDT(CS_alk)
CS_alk <- melt(CS_alk, measure.vars = c(paste("Age",seq(0,10),sep="_")))
names(CS_alk)[names(CS_alk)=="variable"]="age"; names(CS_alk)[names(CS_alk)=="value"]="number"

head(CS_alk)


CS_alk2 = CS_alk %>% 
  group_by(Year, Quarter, Area, AphiaID, Species, LngtClass, age) %>% 
  summarise(number = sum(number, na.rm=T))

head(CS_alk2)
CS_laa = CS_alk2 %>% 
  group_by(Species, age) %>% 
  summarise(lgt_at_age = weighted.mean(x=LngtClass, w=number)) %>% # WEIGHTED MEAN!!
  filter(!is.na(lgt_at_age))

# laa$age <- as.numeric(substring(laa$age, 5,5))


all_laa_CS <- data.frame()

for (i in unique(CS_laa$Species)){
  model <- mgcv::gam(lgt_at_age ~ age,
                     data = CS_laa[CS_laa$Species== i,])
  
  
  temp <- cbind(CS_laa[CS_laa$Species==i,],fitted=predict(model))
  temp <- temp[order(temp$age),]
  
  new <- data.frame(age=unique(temp$age))
  
  new$mean_length_at_age = predict(model, new, se.fit = TRUE)$fit/10
  
  new$species <- i
  
  all_laa_CS <- rbind(all_laa_CS, new)
  
  assign (paste0(i, "_CS_laa"), new)
}


ggplot(CS_all_laa, aes(x= as.numeric(age), y = mean_length_at_age, colour = species))+
  geom_line()

### Compile the relevant ALKs

ALK_laa <- rbind(all_laa_BTS, all_laa_BTS, all_laa_NS)  
