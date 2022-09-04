#################################Punto 1########################################
# Cleaning Data
rm(list = ls())

setwd(r'(C:\Users\rasantofimior\Documents\GitHub\Problem-Set1)')

## install pacman
if(!require(pacman)) install.packages("pacman") ; require(pacman)
## Loading required package: pacman

## Load relevant packages
require(pacman)
p_load(  rio, # import/export data
         tidyverse, # tidy-data
         skimr, # summary data
         caretidyverse,
         here,
         jtools, ## summ function
         ggstance,
         broom, ## tidy function
         broom.mixed,
         skimr,
         stargazer,sandwich, kable, kableExtra,ggplot2,GGally, ggcorrplot,stargazer, xtable, hrbrthemes,boot, dplyr)

## Import dataset

df <- import(here("./stores/data.csv"))

## Filter by age >18 & Cleaning Data set
##NOTE: Confirm if this should be '>' or '>=' 
df <- df[df$age >=18,]
df <- df[df$ocu ==1,]

data <- df %>% select(!matches(c(("^p[0-9]"), "^cc", "^io", "^y_", "^fex", "^hours"))) %>% select(-(ina:ingtotes), -c(depto,dominio, V1, wap, directorio, secuencia_p, pet, orden, fweight, informal, cuentaPropia, pea, microEmpresa, ocu, dsi, inac, clase)) %>% replace_na(list(oficio = 0, relab = 0, totalHoursWorked = 0))

y_salary_m = df$y_salary_m

y_ingLab_m = df$y_ingLab_m

y_total_m =  df$y_total_m

data$y_ingLab_m = y_ingLab_m

data$y_salary_m = y_salary_m

data$y_total_m = y_total_m

categoricas <- c('estrato1','oficio','relab','maxEducLevel','regSalud','cotPension')
for (v in categoricas) {
  data[,v] <- as.factor(data[,v,drop=T])
} 

##Missing Values in categorical replacing with mode

table(data$maxEducLevel)

data$maxEducLevel[is.na(data$maxEducLevel)]<-7

table(data$regSalud)

data$regSalud[is.na(data$regSalud)]<-1

## Descriptives
skim(data)

stargazer(data)

# Check correlations (as scatterplots), distribution and print corrleation coefficient

#db <- as_tibble(df) ## from dataframe to tibble
#data2 <-df %>% select(matches("y_")) %>% colSums(na.rm=TRUE)

data_plot <- data %>% select(-c('estrato1','oficio','relab','maxEducLevel','regSalud','cotPension'))

corr <- round(cor(data_plot), 1)

ggcorrplot(corr, method = 'circle', type = 'lower', lab = TRUE) +
  ggtitle("Correlograma de base de ingresos") +
  theme_minimal() +
  theme(legend.position="none")

##Histogram Ingreso

ggplot(data, aes(x=ingtot))+ geom_histogram(bins=20, fill = "darkorange")+
  ggtitle("Ingresos Totales") + labs(x="Ingreso Total", y = "Cantidad")+
  theme_bw() 

##Bar chart for categorical variables

ggplot(data, aes(x = `estrato1`)) +
  geom_bar( fill = "darkorange") +
  xlab("Estrato") +
  theme_bw()

#################################Punto 3########################################
# a

lnincome <- log(y_salary_m)
mod_female <- lm("lnincome ~ sex" , data= data)
summary(mod_female)
results2 = tidy(mod_female)
stargazer(mod_female, dep.var.labels=c("Ln(income)") , out="./views/Modelo_sex_gap.tex")

#b
mod_Pfemale <- lm("lnincome ~ sex + (sex*age) + (sex*I(age^2)) + age + I(age^2)" , data= data)
summary(mod_Pfemale)
results3 = tidy(mod_Pfemale)
stargazer(mod_Pfemale, dep.var.labels=c("Ln(income)") , out="./views/Modelo_age_earnings_profile.tex")


# c

## install fastDummies
if(!require(fastDummies)) install.packages("fastDummies") ; require(fastDummies)
require(fastDummies)
data <- dummy_cols(data, select_columns = c('mes', 'oficio', 'relab', 'maxEducLevel', 'sizeFirm'), remove_first_dummy = TRUE, remove_selected_columns = TRUE)
data2 <- data %>% select(sex, age, formal, totalHoursWorked, mes_2:maxEducLevel_7)
data2$lnincome2 <- log(y_salary_m)
data2 <- data2 %>% replace_na(list(lnincome2 = mean(data2$lnincome2, na.rm = TRUE)))

mod_PPfemale <- lm(lnincome2 ~ sex + (sex*age) + (sex*I(age^2)) + age + I(age^2)+., data= data2)
summary(mod_PPfemale)
results4 = tidy(mod_PPfemale)
stargazer(mod_PPfemale, dep.var.labels=c("Ln(income)") , out="./views/Modelo_age_earnings_gap_controls.tex")

names_vars <- data2 %>% dplyr::select(-c(2)) %>% colnames()
independiente <- " ~ age"
formulas <- paste0(names_vars, independiente)

for(x in formulas) { 
  data2[x] <- lm(x, data2)$residuals
}

data3 <- data2 %>% dplyr::select(matches("~ age$"))
reg2<-lm(`lnincome2 ~ age` ~ . - 1, data = data3)

# 0.0806
