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

laa <- rbind(ALK_laa, Survey_laa)

ggplot(laa, aes(x=age, y = model_laa, colour = method))+
  geom_line()+
  facet_wrap(~Species, scales = "free")


