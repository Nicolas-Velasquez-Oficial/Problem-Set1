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
         stargazer,sandwich, kable, kableExtra)

## Import dataset

df <- import(here("./stores/data.csv"))

db <- as_tibble(df) ## from dataframe to tibble

## Filter by age >18
##NOTE: Confirm if this should be '>' or '>=' 
db <- db[db$age >=18,]
## Descriptives
head(db)
tail(db)

skim(db)

## Handeling with missing values

