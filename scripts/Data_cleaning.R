#################################Punto 1########################################
# Cleaning Data
rm(list = ls())

setwd(r'(C:\Users\juan.velasquez\OneDrive - Universidad de los Andes\Maestria\Semestres\2022-2\BIG DATA & MACHINE LEARNING FOR APPLIED ECONOMICS\Talleres\Problem-Set1)')

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

data <- df %>% select(!matches(c(("^p[0-9]"), "^cc", "^io", "^y_", "^fex", "^hours"))) %>% select(-(ina:ingtotes), -c(depto,dominio ,clase, V1, wap, directorio, secuencia_p, pet, orden, mes, fweight, informal, cuentaPropia, pea, microEmpresa, ocu, dsi, inac)) %>% replace_na(list(oficio = 0, relab = 0, totalHoursWorked = 0)) 

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

ggplot(data, aes(x=estrato1))+ geom_histogram(bins=20, fill = "darkorange")+
  ggtitle("Estrato") + labs(x="Estrato", y = "Cantidad")+
  theme_bw() 



#################################Punto 2########################################

mod_age <- lm("ingtot ~ age + I(age^2)" , data= data)

summary(mod_age)
results = tidy(mod_age)
stargazer(mod_age, dep.var.labels=c("Earnings") , out="./views/Modelo_Age.tex")


beta.fn<-function(data,index){
  coef(lm(ingtot ~ age + I(age^2) , data = data, subset = index))
}

plotbot <- boot(data, beta.fn, R = 1000)

plot(plotbot)

boot.ci(plotbot, type="bca")

#plot predicted vs. actual values

ggplot(data, aes(y=predict(mod_age), x= age)) + 
  labs(x='Age', y='Predicted Values', title='Predicted Values vs. Age')+ geom_point() 

  
  
  