# SELECTIVITY FROM LENGTH TO AGES
#################################
rm(list=ls())


library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)

alk <- read_excel(("ALK//ALK_2020-04-27 20_45_03_NS-IBTS.xlsx"),sheet="ALK_2020-04-27 20_45_03")
head(alk)
alk[c(paste("Age",seq(0,10),sep="_"))] <- sapply(alk[c(paste("Age",seq(0,10),sep="_"))], as.numeric)


setDT(alk)
alk <- melt(alk, measure.vars = c(paste("Age",seq(0,10),sep="_")))
names(alk)[names(alk)=="variable"]="age"; names(alk)[names(alk)=="value"]="number"

alk = alk %>% 
  group_by(Survey, Year, Quarter, Area, AphiaID, Species, LngtClass, age) %>% 
  summarise(number = sum(number, na.rm=T))

lgt_at_age = alk %>% 
  group_by(Survey, Year, Quarter, Area, AphiaID, Species, age) %>% 
  summarise(lgt_at_age = weighted.mean(x=LngtClass, w=number)) %>% # WEIGHTED MEAN!!
  filter(!is.na(lgt_at_age))

laa <- lgt_at_age
# laa$age <- as.numeric(substring(laa$age, 5,5))
# 
# i = "Gadus morhua"
# # model <- mgcv::gam(lgt_at_age ~ age,
# #                    data = laa[laa$Species== i,])
# 
# test <- laa[laa$Species== i,]
# 
# model <- mgcv::gam(lgt_at_age ~ age,
#                    data = test)
# model2 <- mgcv::gam(lgt_at_age ~ age,
#                    data = laa[laa$Species=="Gadus morhua",])
# 
# temp <- cbind(test,fitted=predict(model))
# temp2 <- cbind(test,fitted=predict(model2))
# 
# 
# new <- data.frame(age=levels(test$age))
# 
# mean_length_at_age = predict(model, new, se.fit = TRUE)$fit/10
# mean_length_at_age
# 
# mean_length_at_age2 = predict(model2, new, se.fit = TRUE)$fit/10
# mean_length_at_age2
# 
# plot (mean_length_at_age)

all_laa <- data.frame()

for (i in unique(laa$Species)){
model <- mgcv::gam(lgt_at_age ~ age,
                   data = laa[laa$Species== i,])


temp <- cbind(laa[laa$Species==i,],fitted=predict(model))
temp <- temp[order(temp$age),]

new <- data.frame(age=unique(temp$age))

new$mean_length_at_age = predict(model, new, se.fit = TRUE)$fit/10

new$species <- i

all_laa <- rbind(all_laa, new)

assign (paste0(i, "_laa"), new)


}


ggplot(all_laa, aes(x= as.numeric(age), y = mean_length_at_age, colour = species))+
  geom_line()

###########################################################################
###########################################################################
### that is from the north sea. Can we try for the Celtic Sea?

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


CS_all_laa <- data.frame()

for (i in unique(CS_laa$Species)){
  model <- mgcv::gam(lgt_at_age ~ age,
                     data = CS_laa[CS_laa$Species== i,])
  
  
  temp <- cbind(CS_laa[CS_laa$Species==i,],fitted=predict(model))
  temp <- temp[order(temp$age),]
  
  new <- data.frame(age=unique(temp$age))
  
  new$mean_length_at_age = predict(model, new, se.fit = TRUE)$fit/10
  
  new$species <- i
  
  CS_all_laa <- rbind(CS_all_laa, new)
  
  assign (paste0(i, "_CS_laa"), new)
}


ggplot(CS_all_laa, aes(x= as.numeric(age), y = mean_length_at_age, colour = species))+
  geom_line()

##############################################################
### Combine both laa results

all_laa$area = "NS"
CS_all_laa$area = "EVHOE"

laa_both <- rbind(all_laa, CS_all_laa)

ggplot(laa_both, aes(x= as.numeric(age), y = mean_length_at_age, colour = area))+
  geom_line()+
  facet_wrap(~ species, ncol = 3)



