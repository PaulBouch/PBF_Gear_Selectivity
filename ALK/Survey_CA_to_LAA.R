library(icesDatras)
library(ggplot2)

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
unique(igfs$Species)


igfs_laa <- igfs %>% 
  group_by(Species, Age) %>% 
  summarise(lgt_at_age = weighted.mean(x=LngtClass, w=NoAtALK)/10)


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

NS_IBTS_laa <-NS_IBTS %>% 
  group_by(Species, Age) %>% 
  summarise(lgt_at_age = weighted.mean(x=LngtClass, w=NoAtALK)/10)

ggplot(NS_IBTS_laa, aes(x= Age, y = lgt_at_age, colour = Species))+
  geom_line()

####################################################
#### needs fixing!

all_laa_NS <- data.frame()

for (i in unique(NS_IBTS$Species)){
  model <- mgcv::gam(LngtClass ~ Age,
                     data = NS_IBTS[NS_IBTS$Species== i,])
  
  temp <- cbind(NS_IBTS[NS_IBTS$Species==i,],fitted=predict(model))
  temp <- temp[order(temp$Age),]
  
  new <- data.frame(Age=unique(temp$Age))
  
  new$mean_length_at_age = predict(model, new, se.fit = TRUE)$fit/10
  
  new$species <- i
  
  all_laa_NS <- rbind(all_laa_NS, new)
  
  assign (paste0(i, "_laa_NS"), new)
}

ggplot(all_laa_NS, aes(x= as.numeric(Age), y = mean_length_at_age, colour = species))+
  geom_line()

