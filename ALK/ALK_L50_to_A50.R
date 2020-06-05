# SELECTIVITY FROM LENGTH TO AGES
#################################
rm(list=ls())

library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(mgcv)

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
  
  temp = as.data.frame(NS_alk[NS_alk$Species == i, ] %>% 
    group_by(Species, age) %>% 
    summarise(lgt_at_age = weighted.mean(x=LngtClass, w=number)) %>% # WEIGHTED MEAN!!
    filter(!is.na(lgt_at_age)))
  
  temp$model_laa <- predict(model, temp)/10

  all_laa_NS <- rbind(all_laa_NS, temp)
}


ggplot(all_laa_NS, aes(x= as.numeric(age), y = model_laa, colour = Species))+
  geom_line()+
  ggtitle("NS")

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

BTS_alk$age <- as.numeric(BTS_alk$age)

laa_BTS = BTS_alk %>% 
  group_by(Survey, Year, Quarter,AphiaID, Species, age) %>% 
  summarise(lgt_at_age = weighted.mean(x=LngtClass, w=number)) %>% # WEIGHTED MEAN!!
  filter(!is.na(lgt_at_age))

all_laa_BTS <- data.frame()

for (i in unique(laa_BTS$Species)){
  # model <- mgcv::gam(lgt_at_age ~ age,
  #                    data = laa_BTS[laa_BTS$Species== i,])

  model <- nls(lgt_at_age ~ SSlogis(age, phi1, phi2, phi3),
      data = laa_BTS[laa_BTS$Species== i,])
  
  temp = as.data.frame(BTS_alk[BTS_alk$Species == i, ] %>% 
                         group_by(Species, age) %>% 
                         summarise(lgt_at_age = weighted.mean(x=LngtClass, w=number)) %>% # WEIGHTED MEAN!!
                         filter(!is.na(lgt_at_age)))
  
  
  temp$model_laa <- predict(model, temp)/10
  
  all_laa_BTS <- rbind(all_laa_BTS, temp)
}


ggplot(all_laa_BTS, aes(x= as.numeric(age), y = model_laa, colour = Species))+
  geom_line()+
  ggtitle("BTS")


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

CS_alk2$age <- as.numeric(CS_alk2$age)

CS_laa = CS_alk2 %>% 
  group_by(Species, age) %>% 
  summarise(lgt_at_age = weighted.mean(x=LngtClass, w=number)) %>% # WEIGHTED MEAN!!
  filter(!is.na(lgt_at_age))

# laa$age <- as.numeric(substring(laa$age, 5,5))


all_laa_CS <- data.frame()

for (i in unique(CS_laa$Species)){
#  model <- mgcv::gam(lgt_at_age ~ age,
#                     data = CS_laa[CS_laa$Species== i,])

  model <- nls(lgt_at_age ~ SSlogis(age, phi1, phi2, phi3), 
       data = CS_laa[CS_laa$Species== i,])

  temp <- as.data.frame(cbind(CS_laa[CS_laa$Species==i,],model_laa=predict(model)/10))
  
  all_laa_CS <- rbind(all_laa_CS, temp)
}


ggplot(all_laa_CS, aes(x= as.numeric(age), y = model_laa, colour = Species))+
  geom_line()

### Compile the relevant ALKs
all_laa_BTS$ALK <- "BTS"
all_laa_NS$ALK <- "NS"
all_laa_NS$age <- as.numeric(all_laa_NS$age)
all_laa_CS$ALK <- "CS"

ALK_laa <- rbind(all_laa_BTS, all_laa_CS, all_laa_NS)  
save(ALK_laa, file = "ALK_laa.RData")

ggplot(ALK_laa, aes(x= as.numeric(age), y = model_laa, colour = ALK))+
  facet_wrap(~Species)+
  geom_line()

save(BTS_alk, CS_alk2, NS_alk, file = "ALK/raw_alk.RData")


