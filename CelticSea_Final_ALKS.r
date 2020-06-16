load("gear_selectivity.RData")

### CS model
CS_Sp <- c("Gadus morhua", "Melanogrammus aeglefinus", "Merlangius merlangus",
           "Lophius piscatorius", "Merluccius merluccius", "Pleuronectes platessa",
           "Solea solea", "Lepidorhombus whiffiagonis")

CS_sel <- retentions[retentions$Species %in% CS_Sp, ]

cod <- CS_sel[CS_sel$method == "ALK_CS" & CS_sel$Species == "Gadus morhua", ]
whg <- CS_sel[CS_sel$method == "ALK_CS" & CS_sel$Species == "Merlangius merlangus", ]
hke <- CS_sel[CS_sel$method == "ALK_CS" & CS_sel$Species == "Merluccius merluccius", ]
mon <- CS_sel[CS_sel$method == "ALK_CS" & CS_sel$Species == "Lophius piscatorius", ]

had <- CS_sel[CS_sel$method == "Survey_IGFS" & CS_sel$Species == "Melanogrammus aeglefinus", ]
meg <- CS_sel[CS_sel$method == "Survey_IGFS" & CS_sel$Species == "Lepidorhombus whiffiagonis", ]
sol <- CS_sel[CS_sel$method == "Survey_IGFS" & CS_sel$Species == "Solea solea", ]
ple <- CS_sel[CS_sel$method == "Survey_IGFS" & CS_sel$Species == "Pleuronectes platessa", ]

CS_ret <- rbind(cod, had, whg, hke, mon, meg, ple, sol)

save(CS_ret, file = "CS_Retentions.RData")
