library("tidyverse") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
class(eddypro)
eddypro=read_csv("C:/Pokoeva_125/MathMod/eddypro.csv", skip = 1,na=c("","NA","-9999","-9999.0"),comment =c("["))
View(eddypro)
eddypro=eddypro [-1,]
eddypro
glimpse(eddypro)
eddypro = select(eddypro, -(roll))
eddypro<-eddypro[,c(-1,-3,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)] 
eddypro = eddypro %>% mutate_if(is.character, factor)
names(eddypro)= str_replace_all(names(eddypro), "[!]","_emph_")
names(eddypro) = names(eddypro) %>% str_replace_all("[!]","_emph_") %>%
  str_replace_all("[?]","_quest_") %>%  
  str_replace_all("[*]","_star_") %>%  
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(eddypro) 
sapply(eddypro,is.numeric) 
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric)]
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ] 
eddypro1$daytime = as.logical(eddypro$daytime) 
eddypro= eddypro[eddypro, DOY > 133 & DOY < 150,]
night = filter(daytime == "FALSE")
cor_td = cor(eddypro_numeric) 
cor_td 
cor_td = cor(drop_na(eddypro_numeric)) %>% as.data.frame %>% select(h2o_flux) 
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude 
vars 
formula=as.formula(paste("h2o_flux~",paste(vars,collapse="+"),sep = ""))
formula
model1=lm((h2o_flux ~ DOY + Tau + rand_err_Tau + H + rand_err_H + LE + rand_err_LE + co2_flux + rand_err_co2_flux + h2o_flux + rand_err_h2o_flux + H_strg + co2_molar_density + h2o_time_lag + sonic_temperature +  air_temperature + air_density + air_molar_volume + es + RH + VPD + u_star_ + TKE + T_star_ + un_Tau + un_H + un_LE + un_co2_flux + un_h2o_flux + mean_value + u_var + v_var + w_var + h2o_var +  w_div_ts_cov + w_div_co2_cov + w_div_h2o_cov + co2_signal_strength_7200 +  h2o_signal_strength_7200 + flowrate)^2,data=eddypro)
coef(model1) 
resid(model1) 
confint(model1) 
summary(model1)
anova(model1)
model2=lm((h2o_flux ~ DOY + Tau + H + LE + co2_flux + h2o_flux + co2_molar_density  +  air_temperature + air_density + air_molar_volume es + RH + VPD + u. + TKE + T )^2,data=eddypro)
coef(model2) 
resid(model2) 
confint(model2) 
summary(model2)
anova(model2)
model3 =lm((h2o_flux ~ DOY + Tau + H + LE + co2_flux + h2o_flux + co2_molar_density  +  air_temperature + air_density + air_molar_volume es + RH + VPD + u. + TKE + T )^2-Tau:H -Tau:LE -Tau:co2_flux - Tau:co2_molar_density-Tau:H_strg-Tau:h2o_time_lag-Tau:air_density -  Tau:air_molar_volume-Tau:u.-Tau:T.-H:LE-H:co2_molar_density -H:h2o_time_lag -H:air_density-H_strg:es-H_strg:RH-H_strg:VPD-H_strg:u.-co2_molar_density:h2o_time_lag - co2_molar_density:u.-co2_molar_density:T.-h2o_time_lag:u. -h2o_time_lag:T.-sonic_temperature:RH -air_density:VPD-air_density:u.-air_molar_volume:u.-es:RH-air_molar_volume:es ,data=eddypro) 
coef(model3)#вытаскиваем коэффииенты из модели
resid(model3)  #вытаскиваем остатки
confint(model3) 
summary(model3)
anova(model3)



