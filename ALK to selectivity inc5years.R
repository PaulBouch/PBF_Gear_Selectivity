library(dplyr)
library(ggplot2)

load("Survey_laa.RData")
load("ALK_laa.RData")

head(ALK_laa)
head(Survey_laa)

ALK_laa$method <- paste0("ALK_", ALK_laa$ALK)
ALK_laa$ALK <- NULL

Survey_laa$method <- paste0("Survey_", Survey_laa$Survey)
Survey_laa$Survey <- NULL

colnames(Survey_laa)[2] <- "age" 

load("Survey_laa5.RData")
load("ALK_laa5.RData")


ALK_laa5$method <- paste0("ALK_", ALK_laa5$ALK)
ALK_laa5$ALK <- NULL


Survey_laa5$Survey <- paste0(Survey_laa5$Survey, "5")
Survey_laa5$method <- paste0("Survey_", Survey_laa5$Survey)
Survey_laa5$Survey <- NULL

colnames(Survey_laa5)[2] <- "age" 

laa <- rbind(ALK_laa, Survey_laa, ALK_laa5, Survey_laa5)

#### sort gear and mods
TR2 <- readRDS("TR2_stan-70_sep20_stan-140_vFinal.RDS")
TR1 <- readRDS("TR1_stan-100_stan-140_vFinal.RDS")
BT <- readRDS("BT2_stan-80_t90-100_t90-100-SMT_vFinal.RDS")

TR2$Gear <- "TR2"
TR1$Gear <- "TR1"
BT$Gear <- "BT"

unique(TR2$mod)
unique(TR1$mod)
unique(BT$mod)

all_gear <- rbind(TR1, TR2, BT)

unique(all_gear$species)
unique(laa$Species)

all_gear$species <- as.character(all_gear$species)
all_gear$species [all_gear$species %in% "cod"] <- "Gadus morhua"
all_gear$species [all_gear$species %in% "dogfish"] <- "Squalus acanthias"
all_gear$species [all_gear$species %in% "haddock"] <- "Melanogrammus aeglefinus"
all_gear$species [all_gear$species %in% "hake"] <- "Merluccius merluccius"
all_gear$species [all_gear$species %in% "horse mackerel"] <- "Trachurus trachurus"
all_gear$species [all_gear$species %in% "megrim"] <- "Lepidorhombus whiffiagonis"
all_gear$species [all_gear$species %in% "monkfish"] <- "Lophius piscatorius"
all_gear$species [all_gear$species %in% "red mullet"] <- "Mullus surmuletus"
all_gear$species [all_gear$species %in% "plaice"] <- "Pleuronectes platessa"
all_gear$species [all_gear$species %in% "saithe"] <- "Pollachius virens"
all_gear$species [all_gear$species %in% "sole"] <- "Solea solea"
all_gear$species [all_gear$species %in% "whiting"] <- "Merlangius merlangus"
all_gear$species [all_gear$species %in% "witch"] <- "Glyptocephalus cynoglossus"
all_gear$species [all_gear$species %in% "dab"] <- "Limanda limanda"
all_gear$species [all_gear$species %in% "gurnard"] <- "Chelidonichthys cuculus"


lopbud <- all_gear[all_gear$species %in% "Lophius piscatorius",]
lopbud$species <- "Lophius budegassa"
all_gear <- rbind (all_gear, lopbud)

all_gear$species <- as.factor(all_gear$species)

all_gear$retention[is.na(all_gear$retention) && all_gear$laa_round <10] <- 0
all_gear$retention[is.na(all_gear$retention) && all_gear$laa_round >10] <- 1



##############################################
#### So now loop through by species
laa$laa_round <- round(laa$model_laa)

colnames(all_gear)[1] <- "Species"
colnames(all_gear)[3] <- "laa_round"
retentions <- merge (laa, all_gear, all = F)

retentions$gear_mod <- paste(retentions$Gear, retentions$mod, sep = "_")

for (i in unique(retentions$method)){
  df <- retentions[retentions$method == i, ]
  
  myplot <-  ggplot(df, aes(x= age, y = retention, colour = gear_mod))+
    geom_line()+
    facet_wrap(~Species, scales = "free")
 

  pdf(paste0("Plots/Retentions_",i, ".pdf"))
  print(myplot)
  dev.off()
}

retentions5 <- retentions

save(retentions5, file = "gear_selectivity5.RData")


