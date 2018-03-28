library("tidyverse") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
class(eddypro)
#загружаем файл,считываем
eddypro=read_csv("C:/Pokoeva_125/MathMod/eddypro.csv", skip = 1,na=c("","NA","-9999","-9999.0"),comment =c("["))
View(eddypro)
eddypro=eddypro [-1,]
eddypro
glimpse(eddypro)
#удаляем лишние колонки
eddypro = select(eddypro, -(roll))
eddypro<-eddypro[,c(-1,-3,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)] 
eddypro = eddypro %>% mutate_if(is.character, factor)
#Убираем проблему со знаками в переменных 
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
#оставляем численные данные
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric)]
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ] 
#меняем тип колонки
eddypro$daytime = as.logical(eddypro$daytime) 
#выбираем весенний ночной период
eddypro<-eddypro[eddypro$DOY>133 & eddypro$DOY<150 & eddy$daytime == FALSE, c(1:ncol(eddypro))] 
#проводим корреляцию
cor_td = cor(eddypro_numeric) 
cor_td 
cor_td = cor(drop_na(eddypro_numeric)) %>% as.data.frame %>% select(h2o_flux) 
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude 
vars 
 #Собираем все переменные в одну формулу 
formula=as.formula(paste("h2o_flux~",paste(vars,collapse="+"),sep = ""))
formula
#создаем обучающую выборку
row_numbers = 1:length(eddypro$date) 
teach = sample(row_numbers, floor(length(eddypro$date)*.7)) 
test = row_numbers[-teach] 
teaching_eddypro_unq = eddypro[teach,] 
testing_eddypro_unq = eddypro[test,] 
#взаимодействие переменных
model1 = lm(h2o_flux ~ (rand_err_Tau + H + LE + rand_err_LE + h2o_flux + rand_err_h2o_flux +  co2_molar_density + co2_mixing_ratio + RH + VPD + max_speed +  u_star_ + TKE + T_star_ + un_H + un_LE + un_h2o_flux + u_var +  v_var + w_div_ts_cov + w_div_h2o_cov + co2_signal_strength_7200)^2, data = eddypro)
model1
coef(model1) #вытаскиваем коэффииенты из модели
resid(model1) #вытаскиваем остатки
confint(model1) 
summary(model1)
anova(model1)
model2=lm(h2o_flux ~(Tau + rand_err_Tau + H + rand_err_H + LE + rand_err_LE + co2_flux + rand_err_co2_flux + h2o_flux + rand_err_h2o_flux + H_strg + co2_molar_density + h2o_time_lag + sonic_temperature +  air_temperature + air_density + air_molar_volume + es + RH + VPD + u_star_ + TKE + T_star_ + un_Tau + un_H + un_LE + un_co2_flux + un_h2o_flux + mean_value + u_var + v_var + w_var + h2o_var +  w_div_ts_cov + w_div_co2_cov + w_div_h2o_cov + co2_signal_strength_7200 +  h2o_signal_strength_7200 + flowrate)^2,data=eddypro)
model2
coef(model2) 
resid(model2) 
confint(model2) 
summary(model2)
anova(model2)



