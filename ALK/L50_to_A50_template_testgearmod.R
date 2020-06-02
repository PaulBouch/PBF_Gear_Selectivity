
library(dplyr)

temp3=readRDS("F:/backup_to_H/R_gitlab/trawl_selectivity/output/TR2_stan-70_sep20_stan-140_vFinal.RDS")


cod = temp3%>%
  filter(species=="cod") #  codend_mm %in% c("70", "70_sep20cmheight_140")
cod = cod[!is.na(cod$retention),]
codmod <- cod %>%
  filter(codend_mm %in% c("70_sep20cmheight_140"))

ggplot(codmod,
       aes(x=as.numeric(as.character(fishsize_cm)), y=retention)) +
  geom_line()

# the dataset comes from the L50_at_A50_template: lgt_at_age

model <- mgcv::gam(lgt_at_age ~ age,
                   data = lgt_at_age[lgt_at_age$Species=="Gadus morhua",])
cbind(lgt_at_age[lgt_at_age$Species=="Gadus morhua",],fitted=predict(model))
new <- data.frame(age=levels(lgt_at_age$age))
mean_length_at_age = predict(model, new, se.fit = TRUE)$fit/10
mean_length_at_age


codmoda <- data.frame(approx(x=codmod$fishsize_cm, y=codmod$retention,
                  xout=mean_length_at_age))

ggplot(codmod,
       aes(x=as.numeric(as.character(fishsize_cm)), y=retention)) +
  geom_line() + 
  geom_line(data=codmoda, aes(x=x, y=y), col="red", lwd=1.5, lty=2)

