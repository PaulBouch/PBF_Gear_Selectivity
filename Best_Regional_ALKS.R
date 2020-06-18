load("gear_selectivity5.RData")
unique(retentions5$method)
unique(retentions5$Species)

## modelled species
CS_Sp <- c("Gadus morhua", "Melanogrammus aeglefinus", "Merlangius merlangus",
           "Lophius piscatorius", "Merluccius merluccius", "Pleuronectes platessa",
           "Solea solea", "Lepidorhombus whiffiagonis")

NS_Sp <- c("Gadus morhua", "Melanogrammus aeglefinus", "Pollachius virens", "Solea solea", "Pleuronectes platessa",
           "Merlangius merlangus","Nephrops norvegicus","Scophthalmus maximus", "Glyptocephalus cynoglossus",
           "Platichthys flesus", "Scophthalmus rhombus", "Limanda limanda", "Lophius budegassa", "Lophius piscatorius",
           "Molva molva", "Squalus acanthias", "Raja clavata", "Leucoraja naevus", "Raja montagui", "Anarhicas lupus",
           "Hippoglossus hippoglossus")

BB_Sp <- c("Merluccius merluccius", "Lepidorhombus whiffiagonis", "Lophius piscatorius", "Trachurus trachurus",
           "Scomber scombrus", "Solea solea", "Micromesistius poutassou", "Scyliorhinus canicula", "Squalus acanthias",
           "Raja clavata", "Lophius budegassa", "Nephrops norvegicus", "Dicentrarchus labrax", "Mullus surmuletus",
           "Conger conger")

#### CS species ####

CS_sel <- retentions5[retentions5$Species %in% CS_Sp, ]

cod <- CS_sel[CS_sel$method == "ALK_CS" & CS_sel$Species == "Gadus morhua", ]
whg <- CS_sel[CS_sel$method == "ALK_CS5" & CS_sel$Species == "Merlangius merlangus", ]
hke <- CS_sel[CS_sel$method == "ALK_CS" & CS_sel$Species == "Merluccius merluccius", ]
mon <- CS_sel[CS_sel$method == "ALK_CS" & CS_sel$Species == "Lophius piscatorius", ]
had <- CS_sel[CS_sel$method == "Survey_IGFS" & CS_sel$Species == "Melanogrammus aeglefinus", ]
meg <- CS_sel[CS_sel$method == "Survey_IGFS" & CS_sel$Species == "Lepidorhombus whiffiagonis", ]
sol <- CS_sel[CS_sel$method == "ALK_BTS5" & CS_sel$Species == "Solea solea", ]
ple <- CS_sel[CS_sel$method == "ALK_BTS5" & CS_sel$Species == "Pleuronectes platessa", ]


CS_ret <- rbind(cod, had, whg, hke, mon, meg, ple, sol)
save(CS_ret, file = "CS_Retentions.RData")

#### NS species ####
NS_sel <- retentions5[retentions5$Species %in% NS_Sp, ]
unique(NS_sel$Species)

cod <- NS_sel[NS_sel$method == "ALK_NS" & NS_sel$Species == "Gadus morhua", ]
had <- NS_sel[NS_sel$method == "ALK_NS5" & NS_sel$Species == "Melanogrammus aeglefinus", ]
whg <- NS_sel[NS_sel$method == "ALK_NS" & NS_sel$Species == "Merlangius merlangus", ]
lopp <- NS_sel[NS_sel$method == "Survey_NS_IBTS5" & NS_sel$Species == "Lophius piscatorius", ]
lopb <- NS_sel[NS_sel$method == "Survey_NS_IBTS" & NS_sel$Species == "Lophius budegassa", ]
sol <- NS_sel[NS_sel$method == "ALK_BTS5" & NS_sel$Species == "Solea solea", ]
ple <- NS_sel[NS_sel$method == "ALK_NS5" & NS_sel$Species == "Pleuronectes platessa", ]
pol <- NS_sel[NS_sel$method == "ALK_NS5" & NS_sel$Species == "Pollachius virens", ]
wit <- NS_sel[NS_sel$method == "ALK_NS5" & NS_sel$Species == "Glyptocephalus cynoglossus", ]
dab <- NS_sel[NS_sel$method == "Survey_NS_IBTS" & NS_sel$Species == "Limanda limanda", ]

NS_ret <- rbind(cod, had, whg, lopb, lopp, ple, sol, pol, wit, dab)
save(NS_ret, file = "NS_Retentions.RData")

#### BB species ####
BB_sel <- retentions5[retentions5$Species %in% BB_Sp, ]
unique(BB_sel$Species)


lopp <- BB_sel[BB_sel$method == "ALK_CS" & BB_sel$Species == "Lophius piscatorius", ]
lopb <- BB_sel[BB_sel$method == "ALK_CS" & BB_sel$Species == "Lophius budegassa", ]
hke <- BB_sel[BB_sel$method == "ALK_CS" & BB_sel$Species == "Merluccius merluccius", ]
meg <- BB_sel[BB_sel$method == "Survey_IGFS5" & BB_sel$Species == "Lepidorhombus whiffiagonis", ]
sol <- BB_sel[BB_sel$method == "ALK_BTS5" & BB_sel$Species == "Solea solea", ]
sdg <- BB_sel[BB_sel$method == "Survey_IGFS" & BB_sel$Species == "Squalus acanthias", ]
hom <- BB_sel[BB_sel$method == "Survey_IGFS5" & BB_sel$Species == "Trachurus trachurus", ]
mur <- BB_sel[BB_sel$method == "Survey_NS_IBTS" & BB_sel$Species == "Mullus surmuletus", ]

BB_ret <- rbind(lopb, lopp, hke, meg, sdg, sol, hom, mur)
save(BB_ret, file = "BB_Retentions.RData")

