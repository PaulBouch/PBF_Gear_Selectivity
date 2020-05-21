# SELECTIVITY FROM LENGTH TO AGES
#################################
rm(list=ls())

localdir <- "F:/backup_to_H/R_gitlab/Follow_Others/ALKr/"

library(readxl)
alk <- read_excel(paste0(localdir,"user_jochen/ALK_2020-04-27 20_45_03_NS-IBTS.xlsx"),sheet="ALK_2020-04-27 20_45_03")
head(alk)
library(data.table)
alk[c(paste("Age",seq(0,10),sep="_"))] <- sapply(alk[c(paste("Age",seq(0,10),sep="_"))], as.numeric)
setDT(alk)
alk <- melt(alk, measure.vars = c(paste("Age",seq(0,10),sep="_")))
names(alk)[names(alk)=="variable"]="age"; names(alk)[names(alk)=="value"]="number"
library(dplyr)
alk = alk %>% 
  group_by(Survey, Year, Quarter, Area, AphiaID, Species, LngtClass, age) %>% 
  summarise(number = sum(number, na.rm=T))
library(ggplot2)
lgt_at_age = alk %>% 
  group_by(Survey, Year, Quarter, Area, AphiaID, Species, age) %>% 
  summarise(lgt_at_age = weighted.mean(x=LngtClass, w=number)) %>% # WEIGHTED MEAN!!
  filter(!is.na(lgt_at_age))
ggplot(data=lgt_at_age[lgt_at_age$Species=="Gadus morhua",], 
       aes(y=lgt_at_age/10, x=age)) + 
  geom_boxplot() + 
  geom_smooth(method = "loess", se=TRUE, aes(group=1)) +
  theme_bw()
model <- mgcv::gam(lgt_at_age ~ age,
                   data = lgt_at_age[lgt_at_age$Species=="Gadus morhua",])
cbind(lgt_at_age[lgt_at_age$Species=="Gadus morhua",],fitted=predict(model))
new <- data.frame(age=levels(lgt_at_age$age))
mean_length_at_age = predict(model, new, se.fit = TRUE)$fit/10
mean_length_at_age
names(mean_length_at_age) = levels(lgt_at_age$age)

selogive_cod <- function(alpha,beta,fishsize_cm=seq(0:150)){
  alpha=rep(alpha,length=length(fishsize_cm))
  X=alpha+beta*fishsize_cm
  Y=exp(X)/(1+exp(X))
  selogive=data.frame(fishsize_cm=fishsize_cm,retention=Y)
  return(selogive)
}
plot(NULL,xlim=c(0,80), ylim=c(0,1),xlab="fish size (cm)",ylab="retention probability")
cod = c(-3.62126206896552, 0.15151724137931)
ogstan = selogive_cod(cod[1],cod[2])
lines(ogstan)
ogstan

ogstan_age = selogive_cod(cod[1],cod[2],fishsize_cm = mean_length_at_age)
lines(ogstan_age,col="red")
axis(side=3, at=mean_length_at_age,col="red", 
     labels=names(mean_length_at_age))
mean_length_at_age
