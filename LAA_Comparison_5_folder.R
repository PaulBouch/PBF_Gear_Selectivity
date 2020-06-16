library(ggplot2); theme_set (theme_bw())
library(gridExtra)
library (cowplot)

load("Survey_laa.RData")
load("ALK_laa.RData")
load("Survey_laa5.RData")
load("ALK_laa5.RData")


ALK_laa$method <- paste0("ALK_", ALK_laa$ALK)
ALK_laa$ALK <- NULL

Survey_laa$method <- paste0("Survey_", Survey_laa$Survey)
Survey_laa$Survey <- NULL

colnames(Survey_laa)[2] <- "age" 

ALK_laa5$method <- paste0("ALK_", ALK_laa5$ALK)
ALK_laa5$ALK <- NULL

Survey_laa5$Survey <- paste0(Survey_laa5$Survey, "5")
Survey_laa5$method <- paste0("Survey_", Survey_laa5$Survey)
Survey_laa5$Survey <- NULL
colnames(Survey_laa5)[2] <- "age" 


laa <- rbind(ALK_laa, Survey_laa, ALK_laa5, Survey_laa5)
unique(laa$method)

# pdf("plots/ALK Comprisons5.pdf")
# ggplot(laa, aes(x=age, y = model_laa, colour = method))+
#   geom_line()+
#   facet_wrap(~Species, scales = "free")
# dev.off()

#####################
### plot underlying model data
load("ALK/raw_ALK.RData")
load("ALK/raw_survey.RData")

load("ALK/raw_ALK5.RData")
load("ALK/raw_survey_5.RData")

# for (i in unique(laa$Species)){
#   sp_laa <- laa[laa$Species == i, ]
# 
#   myplot <- ggplot(sp_laa, aes(x=age, y = model_laa, colour = method), size = 2)+
#     geom_line()+
#     ggtitle(paste0("Lenth Age Models - ", i))+
#     theme(legend.title = element_blank())
#   
# pdf(paste0("Plots/LAA5_Comparison_",i, ".pdf"))    
# print(myplot)
# dev.off()
# }  

########   big species plots
i = "Limanda limanda"

for (i in unique(laa$Species)){
  sp_laa <- laa[laa$Species == i, ]

all_laa <- ggplot(sp_laa, aes(x=age, y = model_laa, colour = method), size = 2)+
  geom_line()+
  ggtitle(paste0("LAA Models - ", i))+
  theme(legend.title = element_blank(), legend.position = c(0.6, 0.3))

pdf(paste0("LAA_Models/", i, "/All_models.pdf"))  
print(all_laa)
dev.off()


df_ns <- NS_alk[NS_alk$Species == i & NS_alk$number >0,]

if(sum(df_ns$number)>0){
plot_ALK_NS <- ggplot(df_ns, aes(age, LngtClass/10))+
  geom_point()+
  ylab("Length")+
  ggtitle("NS_IBTS_ALK")+
  geom_line(data = sp_laa[sp_laa$method == "ALK_NS", ], 
            aes(x=age, y = model_laa), colour = "red", size = 2 )+
  annotate("text", 5, 15, label = paste0("samples = ", sum(df_ns$number)))

pdf(paste0("LAA_Models/", i, "/ALK_NS.pdf")) 
print(plot_ALK_NS)
dev.off()
}


df_cs <- CS_alk2[CS_alk2$Species == i & CS_alk2$number >0,]
if(sum(df_cs$number)>0){
  plot_ALK_CS <- ggplot(df_cs, aes(age, LngtClass/10))+
  geom_point()+
  ylab("Length")+
  ggtitle("CS_EVHOE_ALK")+
  geom_line(data = sp_laa[sp_laa$method == "ALK_CS", ], 
            aes(x=age, y = model_laa), colour = "red", size = 2 )+
  annotate("text", 5, 15, label = paste0("samples = ", sum(df_cs$number)))

pdf(paste0("LAA_Models/", i, "/ALK_CS.pdf")) 
print(plot_ALK_CS)
dev.off()
}

if( sum(NS_IBTS$NoAtALK[NS_IBTS$Species == i])>0){
  plot_survey_NS <- ggplot(NS_IBTS[NS_IBTS$Species == i,], aes(Age, LngtClass/10))+
  geom_point()+
  ylab("Length")+
  ggtitle("NS_IBTS_Survey")+
  geom_line(data = sp_laa[sp_laa$method == "Survey_NS_IBTS", ], 
            aes(x=age, y = model_laa), colour = "red", size = 2 )+
  annotate("text", 5, 15, label = paste0("samples = ", sum(NS_IBTS$NoAtALK[NS_IBTS$Species == i])))

pdf(paste0("LAA_Models/", i, "/Survey_IBTS.pdf")) 
print(plot_survey_NS)
dev.off()
}

if(sum(igfs$NoAtALK[igfs$Species == i])>0){

  plot_survey_IGFS <- ggplot(igfs[igfs$Species == i,], aes(Age, LngtClass/10))+
  geom_point()+
  ylab("Length")+
  ggtitle("IGFS Survey")+
  geom_line(data = sp_laa[sp_laa$method == "Survey_IGFS", ], 
            aes(x=age, y = model_laa), colour = "red", size = 2 )+
  annotate("text", 5, 15, label = paste0("samples = ", sum(igfs$NoAtALK[igfs$Species == i])))

pdf(paste0("LAA_Models/", i, "/Survey_IGFS.pdf"))  
print(plot_survey_IGFS)
dev.off()
}


##### Now the last 5 year data sets

df_ns_5 <- NS_alk_5[NS_alk_5$Species == i & NS_alk_5$number >0,]

if(sum(df_ns_5$number)>0){
plot_ALK_NS_5 <- ggplot(df_ns_5, aes(age, LngtClass/10))+
  geom_point()+
  ylab("Length")+
  ggtitle("NS_IBTS_ALK 5 years")+
  geom_line(data = sp_laa[sp_laa$method == "ALK_NS5", ], 
            aes(x=age, y = model_laa), colour = "red", size = 2 )+
  annotate("text", 5, 15, label = paste0("samples = ", sum(df_ns_5$number)))

pdf(paste0("LAA_Models/", i, "/ALK_NS_5.pdf"))  
print(plot_ALK_NS_5)
dev.off()
}

df_cs_5 <- CS_alk_5[CS_alk_5$Species == i & CS_alk_5$number >0,]

if(sum(df_cs_5$number)>0){
plot_ALK_CS_5 <- ggplot(df_cs_5, aes(age, LngtClass/10))+
  geom_point()+
  ylab("Length")+
  ggtitle("CS_EVHOE ALK Data - last 5")+
  geom_line(data = sp_laa[sp_laa$method == "ALK_CS5", ], 
            aes(x=age, y = model_laa), colour = "red", size = 2 )+
  annotate("text", 5, 15, label = paste0("samples = ", sum(df_cs_5$number)))

pdf(paste0("LAA_Models/", i, "/ALK_CS_5.pdf"))  
print(plot_ALK_CS_5)
dev.off()

}

if(sum(NS_IBTS_5$NoAtALK[NS_IBTS_5$Species == i])>0){
plot_survey_NS_5 <- ggplot(NS_IBTS_5[NS_IBTS_5$Species == i,], aes(Age, LngtClass/10))+
  geom_point()+
  ylab("Length")+
  ggtitle("NS_IBTS_Survey last 5")+
  geom_line(data = sp_laa[sp_laa$method == "Survey_NS_IBTS5", ], 
            aes(x=age, y = model_laa), colour = "red", size = 2 )+
  annotate("text", 5, 15, label = paste0("samples = ", sum(NS_IBTS_5$NoAtALK[NS_IBTS_5$Species == i])))
pdf(paste0("LAA_Models/", i, "/Survey_IBTS_5.pdf"))  
print(plot_survey_NS_5)
dev.off()
}


if(sum(igfs_5$NoAtALK[igfs_5$Species == i])>0){
plot_survey_IGFS_5 <- ggplot(igfs_5[igfs_5$Species == i,], aes(Age, LngtClass/10))+
  geom_point()+
  ylab("Length")+
  ggtitle("IGFS Survey Data - last 5")+
  geom_line(data = sp_laa[sp_laa$method == "Survey_IGFS5", ], 
            aes(x=age, y = model_laa), colour = "red", size = 2 )+
  annotate("text", 5, 15, label = paste0("samples = ", sum(igfs_5$NoAtALK[igfs_5$Species == i])))
pdf(paste0("LAA_Models/", i, "/Survey_IGFS_5.pdf"))  
print(plot_survey_IGFS_5)
dev.off()
}

df_bts <- BTS_alk[BTS_alk$Species == i & BTS_alk$number >0,]

if (sum(df_bts$number)>0){  
plot_ALK_BTS <- ggplot(df_bts, aes(age, LngtClass))+
    geom_point()+
    ylab("Length")+
    ggtitle("BTS_ALK")+
    geom_line(data = sp_laa[sp_laa$method == "ALK_BTS", ], 
              aes(x=age, y = model_laa), colour = "red", size = 2 )+
    annotate("text", 5, 15, label = paste0("samples = ", sum(df_bts$number)))
pdf(paste0("LAA_Models/", i, "/ALK_BTS.pdf"))  
print(plot_ALK_BTS)
dev.off()
}

df_bts_5 <- BTS_alk_5[BTS_alk_5$Species == i & BTS_alk_5$number >0,]
  
if (sum(df_bts_5$number)>0){  
  plot_ALK_BTS_5 <- ggplot(df_bts_5, aes(age, LngtClass))+
    geom_point()+
    ylab("Length")+
    ggtitle("BTS_ALK last 5")+
    geom_line(data = sp_laa[sp_laa$method == "ALK_BTS5", ], 
              aes(x=age, y = model_laa), colour = "red", size = 2 )+
    annotate("text", 5, 15, label = paste0("samples = ", sum(df_bts_5$number)))
  pdf(paste0("LAA_Models/", i, "/ALK_BTS_5.pdf"))  
  print(plot_ALK_BTS_5)
  dev.off()
}


}




