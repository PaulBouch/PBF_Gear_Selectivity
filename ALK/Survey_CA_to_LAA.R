library(icesDatras)
library(ggplot2)
library(mgcv)
library(dplyr)
library(reshape)

Species <- c("Gadus morhua", "Melanogrammus aeglefinus", "Pollachius virens", "Solea solea", 
             "Pleuronectes platessa", "Merlangius merlangus", "Scophthalmus maximus",
             "Glyptocephalus cynoglossus", "Platichthys flesus", "Scophthalmus rhombus",
             "Limanda limanda", "Molva molva", "Squalus acanthias", "Raja clavata",
             "Leucoraja naevus", "Raja montagui", "Hippoglossus hippoglossus",
             "Chelidonichthys cuculus", "Scyliorhinus canicula", "Merluccius merluccius",
             "Lepidorhombus whiffiagonis", "Trachurus trachurus", "Scomber scombrus",
             "Micromesistius poutassou", "Dicentrarchus labrax", "Mullus surmuletus",
             "Lophius piscatorius", "Lophius budegassa", "Conger conger", "Lepidorhombus boscii")

Valid_Aphia = icesVocab::findAphia(c("Gadus morhua", "Melanogrammus aeglefinus", "Pollachius virens", "Solea solea", 
                                "Pleuronectes platessa", "Merlangius merlangus", "Scophthalmus maximus",
                                "Glyptocephalus cynoglossus", "Platichthys flesus", "Scophthalmus rhombus",
                                "Limanda limanda", "Molva molva", "Squalus acanthias", "Raja clavata",
                                "Leucoraja naevus", "Raja montagui", "Hippoglossus hippoglossus",
                                "Chelidonichthys cuculus", "Scyliorhinus canicula", "Merluccius merluccius",
                                "Lepidorhombus whiffiagonis", "Trachurus trachurus", "Scomber scombrus",
                                "Micromesistius poutassou", "Dicentrarchus labrax", "Mullus surmuletus",
                                "Lophius piscatorius", "Lophius budegassa", "Conger conger", "Lepidorhombus boscii"), latin = T)

Aphia_Sp = data.frame(Species, Valid_Aphia)  


#### IGFS

years_survey <- getSurveyYearList("IE-IGFS")

quarters_survey <- do.call(rbind,lapply(years_survey,
                                        function(year){
                                          quarters <- getSurveyYearQuarterList(survey = "IE-IGFS", year = year)
                                          return(data.frame(year=rep(year,length(quarters)),quarter = quarters))}))
igfs_raw <-  do.call(rbind,
                   lapply(unique(quarters_survey$quarter),function(quarter){
                     do.call(rbind,lapply(quarters_survey$year[quarters_survey$quarter==quarter],
                                          function(year) getCAdata(survey = "IE-IGFS", year = year, quarter = quarter)))}))

head(igfs_raw)
igfs <- igfs_raw[!is.na (igfs_raw$Age),]

igfs <- merge(igfs, Aphia_Sp)

# remove the year zero for ple and sol

plesol <- igfs[igfs$Species %in% c("Solea solea", "Pleuronectes platessa"), ]
plesol <- plesol[plesol$Age != 0, ]

ifgs_noplesol <- igfs[igfs$Species != "Solea solea", ]
unique(ifgs_noplesol$Species)
ifgs_noplesol <- ifgs_noplesol[ifgs_noplesol$Species != "Pleuronectes platessa", ]

igfs <- rbind(ifgs_noplesol, plesol)


igfs$Age[igfs$Age>10]<- 10

igfs$Age <- as.numeric(igfs$Age)

igfs_laa <- igfs %>% 
   group_by(Species, Age) %>% 
   summarise(lgt_at_age = weighted.mean(x=LngtClass, w=NoAtALK)/10)

all_laa_igfs <- data.frame()

for (i in unique(igfs$Species)){
  model <-  nls(lgt_at_age ~ SSlogis(Age, phi1, phi2, phi3), 
                data = igfs_laa[igfs_laa$Species== i,])
  
  temp <- as.data.frame(cbind(igfs_laa[igfs_laa$Species==i,],model_laa=predict(model)))
  
  all_laa_igfs <- rbind(all_laa_igfs, temp)
}

all_laa_igfs$Survey <- "IGFS"

### test plots
ggplot(all_laa_igfs, aes(x=Age, y = model_laa, colour = Species))+
  geom_line()

test <- melt(all_laa_igfs, id.vars = c("Species", "Age", "Survey"))

ggplot(test, aes(x= Age, y = value, colour = variable))+
  geom_line()+
  facet_wrap(~Species)


#### NS IBTS

years_survey <- getSurveyYearList("NS-IBTS")

quarters_survey <- do.call(rbind,lapply(years_survey,
                                        function(year){
                                          quarters <- getSurveyYearQuarterList(survey = "NS-IBTS", year = year)
                                          return(data.frame(year=rep(year,length(quarters)),quarter = quarters))}))
NS_IBTS_raw <-  do.call(rbind,
                     lapply(unique(quarters_survey$quarter),function(quarter){
                       do.call(rbind,lapply(quarters_survey$year[quarters_survey$quarter==quarter],
                                            function(year) getCAdata(survey = "NS-IBTS", year = year, quarter = quarter)))}))

head(NS_IBTS_raw)
NS_IBTS <- NS_IBTS_raw[!is.na (NS_IBTS_raw$Age),]

NS_IBTS <- merge(NS_IBTS, Aphia_Sp)
unique(NS_IBTS$Species)

unique(NS_IBTS$Age)
NS_IBTS$Age[NS_IBTS$Age >10] <- 10

NS_IBTS <- NS_IBTS[NS_IBTS$LngtCode %in% c(".", "1"), ]
NS_IBTS$LngtClass[NS_IBTS$LngtCode %in% "1"] <- NS_IBTS$LngtClass[NS_IBTS$LngtCode %in% "1"] *10 

NS_IBTS_laa <-NS_IBTS %>% 
  group_by(Species, Age) %>% 
  summarise(lgt_at_age = weighted.mean(x=LngtClass, w=NoAtALK)/10)

ggplot(NS_IBTS_laa, aes(x= Age, y = lgt_at_age, colour = Species))+
  geom_line()



all_laa_NS <- data.frame()

for (i in unique(NS_IBTS$Species)){
  try(model <-  nls(lgt_at_age ~ SSlogis(Age, phi1, phi2, phi3), 
                data = NS_IBTS_laa[NS_IBTS_laa$Species== i,]))
  
  try(temp <- as.data.frame(cbind(NS_IBTS_laa[NS_IBTS_laa$Species==i,],model_laa=predict(model))))
  
  try(all_laa_NS <- rbind(all_laa_NS, temp))
}

all_laa_NS$Survey <- "NS_IBTS"

ggplot(all_laa_NS, aes(x=Age, y = model_laa, colour = Species))+
  geom_line()

test2 <- melt(all_laa_NS, id.vars = c("Species", "Age", "Survey"))

ggplot(test2, aes(x= Age, y = value, colour = variable))+
  geom_line()+
  facet_wrap(~Species)


Survey_laa <- rbind(all_laa_NS, all_laa_igfs)  
save(Survey_laa, file = "Survey_laa.RData")

ggplot(Survey_laa, aes(x= as.numeric(Age), y = model_laa, colour = Survey))+
  facet_wrap(~Species)+
  geom_line()

### raw survey data
save(igfs, NS_IBTS, file = "ALK/raw_survey.RData")


