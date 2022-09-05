################################# Punto 1########################################
# Cleaning Data
rm(list = ls())

setwd(r'(C:\Users\rasantofimior\Documents\GitHub\Problem-Set1)')

## install pacman
if (!require(pacman)) install.packages("pacman")
require(pacman)
## Loading required package: pacman

## Load relevant packages
require(pacman)
p_load(
  rio, # import/export data
  tidyverse, # tidy-data
  skimr, # summary data
  caretidyverse,
  here,
  jtools, ## summ function
  ggstance,
  broom, ## tidy function
  broom.mixed,
  skimr,
  stargazer, sandwich, kable, kableExtra, ggplot2, GGally, ggcorrplot, stargazer, xtable, hrbrthemes, boot, dplyr
)

library("tidyverse")
library("stargazer")
library("boot")

## Import dataset

df <- read.csv("./stores/data.csv")

## Filter by age >18 & Cleaning Data set
df <- df[df$age >= 18, ]
df <- df[df$ocu == 1, ]
df <- df %>% mutate(female = ifelse(sex == 1, 0, 1))
y_salary_m <- df$y_salary_m
y_ingLab_m <- df$y_ingLab_m
y_total_m <- df$y_total_m

data <- df %>%
  select(!matches(c(("^p[0-9]"), "^cc", "^io", "^y_", "^fex", "^hours"))) %>%
  select(
    -(ina:ingtotes),
    -c(
      depto, dominio, clase, V1, wap, directorio, secuencia_p, pet, orden,
      mes, fweight, informal, cuentaPropia, pea, microEmpresa, ocu, dsi,
      inac, sex
    )
  ) %>%
  replace_na(list(oficio = 0, relab = 0, totalHoursWorked = 0))

data$y_ingLab_m <- y_ingLab_m
data$y_salary_m <- y_salary_m
data$y_total_m <- y_total_m

categoricals <- data %>%
  dplyr::select(
    -c(
      age,
      ingtot,
      totalHoursWorked,
      y_ingLab_m,
      y_salary_m,
      y_total_m
    )
  ) %>%
  colnames()

data[, categoricals] <- lapply(data[, categoricals], factor)

data <- data %>%
  replace_na(
    list(y_salary_m = mean(data$y_salary_m, na.rm = TRUE))
  )
## Missing Values in categorical replacing with mode

data$maxEducLevel[is.na(data$maxEducLevel)] <- 7
data$regSalud[is.na(data$regSalud)] <- 1

# a
# Modelo básico
mod_female <- lm(log(y_salary_m) ~ female, data = data)
summary(mod_female)

data$lnincome <- log(y_salary_m)

stargazer(mod_female, dep.var.labels = c("Ln(income)"), out = "./views/Modelo_sex_gap.tex")

# b
# Modelo gender GAP interaccion edad
formula <- "log(y_salary_m) ~ female + female*age + female*I(age^2) + age + I(age^2)"
mod_peak_female <- lm(formula, data)
summary(mod_peak_female)


stargazer(
  mod_peak_female,
  dep.var.labels = c("Ln(income)"),
  out = "./views/Modelo_age_earnings_profile.tex"
)

# Grafico

sd2 <- results3[3, 3]

predict_mod_peak_female <- predict(mod_peak_female, interval = "confidence")
predict_mod_peak_female <- as_tibble(predict_mod_peak_female)

data %>% ggplot(aes(y = predict_mod_peak_female$fit, x = age, color = female)) +
  geom_point() +
  labs(
    x = "Edad",
    y = "Salario mensual predicho",
    title = "Salarios Predichos vs. Edad (95% IC)"
  ) +
  geom_ribbon(
    aes(ymin = predict_mod_peak_female$lwr, ymax = predict_mod_peak_female$upr),
    alpha = 0.2
  )

# c

data_with_dummies <- data %>% select(
  y_salary_m,
  age,
  totalHoursWorked,
  female,
  sizeFirm,
  relab,
  maxEducLevel,
  oficio
)


variables <- data_with_dummies %>%
  select(-c(y_salary_m, female)) %>%
  colnames()
formula <- paste0(variables, collapse = " + ")
formula1 <- paste("log(y_salary_m) ~", formula)
formula2 <- paste("as.numeric(female) ~", formula)

mod_res0 <- lm(log(y_salary_m) ~ ., data_with_dummies)
mod_res1 <- lm(formula1, data_with_dummies)$residuals
mod_res2 <- lm(formula2, data_with_dummies)$residuals
mod_res3 <- lm(mod_res1 ~ mod_res2 - 1)

data$mod_res1 <- lm(formula1, data_with_dummies)$residuals
data$mod_res2 <- lm(formula2, data_with_dummies)$residuals

# Bootstrap
beta_est <- function(data, index) {
  coef(lm(mod_res1 ~ mod_res2 - 1, data = data, subset = index))
}

set.seed(10)
boot_fwl <- boot(data, beta_est1, R = 1000)
# Bootstrap estimate

boot_est <- boot_fwl$t0
boot_est_se <- apply(boot_fwl$t, 2, sd)
