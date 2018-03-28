library(tidyverse)
class(eddypro)
eddypro=read_csv("C:/Pokoeva_125/MathMod/eddypro.csv", skip = 1,na=c("","NA","-9999","-9999.0"),comment =c("["))
View(eddypro)
eddypro=eddypro [-1,]
eddypro
glimpse(eddypro)
names(eddypro)= str_replace_all(names(eddypro), "[!]","_emph_")
nov_dec=filter[eddypro, DOY > 133 & DOY < 150,c(l:ncol(eddypro))]
eddypro = select(eddypro, -(roll))
eddypro = eddypro %>% mutate_if(is.character, factor)
night = filter(nov_dec, daytime == "FALSE")
sapply(eddypro,is.numeric)
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric)]
cor_eddypro = cor(eddypro_numeric)
cor_eddypro
cor_eddypro=cor(na.omit(eddypro_numeric))
cor_eddypro
cor_eddypro= cor(na.omit(eddypro_numeric)) %>% as.data.frame %>% select(h2o_flux)
vars = row.names(cor_eddypro)[cor_eddypro$h2o_flux^2 > .7] %>% na.exclude
formula=as.formula(paste("h2o_flux~",paste(vars,collapse="+"),sep = ""))
formula
model1=lm(h2o_flux~DOY +Tau + rand_err_Tau + H + LE + qc_LE + rand_err_LE + co2_flux+ qc_h2o_flux + rand_err_h2o_flux + H_strg +  h2o_time_lag + sonic_temperature + air_temperature + air_density +  air_molar_volume + es + RH + VPD+un_Tau+un_H+un_LE+un_co2_flux+un_h2o_flux+w_var+h2o_var+flowrate,data=eddypro)
anova(model1)
summary(model1)
model2=lm(h2o_flux~DOY +Tau  + H + LE + qc_LE + rand_err_LE + co2_flux+ rand_err_h2o_flux + H_strg +  h2o_time_lag+ air_temperature + air_density +  air_molar_volume + es + RH + VPD+un_Tau+un_H+un_LE+un_co2_flux+un_h2o_flux+h2o_var,data=eddypro)
anova(model2)
summary(model2)
model3=lm(h2o_flux~DOY +Tau  + H + LE  + rand_err_LE + co2_flux+ rand_err_h2o_flux  + air_temperature + air_density +  air_molar_volume + es + RH + VPD+un_Tau+un_H+un_LE+un_co2_flux+un_h2o_flux,data=eddypro)
anova(model3)
summary(model3)

