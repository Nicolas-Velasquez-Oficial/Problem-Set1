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

#### sd and if sex
library(ggplot2)

ggplot(data, aes(y=predict(mod_Pfemale), x= age))+
  geom_point() +
  geom_line(aes(y=lwr), color="red", linetype="dashed") +
  geom_line(aes(y=upr), color="red", linetype="dashed") +
  geom_smooth(method=lm, formula= lnincome ~ sex + (sex*age) + (sex*I(age^2)) + age + I(age^2), se=TRUE, level=0.95, col='blue', fill='pink2') +
  theme_light()

#plot predicted vs. actual values
data$UperCI <- predict(mod_age) +(1.96*12527.290)
data$LowerCI <- predict(mod_age) -(1.96*12527.290)

ggplot(data, aes(y=predict(mod_age), x= age)) + 
  labs(x='Edad', y='Salario predicho', title='Salarios Predichos vs. Edad (95% IC)')+ 
  geom_point()+geom_line(aes(y = UperCI), color = "red", linetype = "dashed")+
  geom_line(aes(y = LowerCI), color = "red", linetype = "dashed")

# c

## install fastDummies
if(!require(fastDummies)) install.packages("fastDummies") ; require(fastDummies)
require(fastDummies)

data <- dummy_cols(data, select_columns = c('mes', 'oficio', 'relab', 'maxEducLevel', 'sizeFirm'), remove_first_dummy = TRUE, remove_selected_columns = TRUE)
data2 <- data %>% select(sex, age, formal, totalHoursWorked, mes_2:maxEducLevel_7)

lnincome2 <- log(y_salary_m)
mod_PPfemale <- lm(lnincome2 ~ sex + (sex*age) + (sex*I(age^2)) + age + I(age^2)+., data= data2)
summary(mod_PPfemale)
results4 = tidy(mod_PPfemale)
stargazer(mod_PPfemale, dep.var.labels=c("Ln(income)") , out="./views/Modelo_age_earnings_gap_controls.tex")


data2<-cbind(data2,lnincome2, I(age^2))

mod_summaries <- list()

for(i in 2:ncol(data2)) {                 # Head of for-loop
  
  predictors_i <- colnames(data2)[2:i]    # Create vector of predictor names
  mod_summaries[[i - 1]] <- summary(     # Store regression model summary in list
    lm(sex ~., data[ , c("sex", predictors_i)]))
  
}
