#Atajo para conectar los otros scriots que contienen data y filtros
source("../scripts/data_cleaning.R")
#Librerias para generar tablas a latex y realizar bootstrap respectivamente
library("stargazer")
library("boot")

# Codigo para item a
# Modelo de regresion  b√°sico (M1)
mod_female <- lm(log(y_salary_m) ~ female, data = data)
summary(mod_female)

stargazer(mod_female, dep.var.labels = c("Ln(income)"), out = "../views/Modelo_sex_gap.tex")

# Codigo para item b
# Modelo gender GAP interaccion edad
# Se crea formula para automatizar el proceso de regresion
formula <- "log(y_salary_m) ~ female + female*age + female*I(age^2) + age + I(age^2)"
mod_peak_female <- lm(formula, data)
summary(mod_peak_female)

stargazer(
  mod_peak_female,
  dep.var.labels = c("Ln(income)"),
  out = "../views/Modelo_age_earnings_profile.tex"
)

# Grafico perfil de ingresos sexo y edad
# se predice el modelo y su respectivo IC
predict_mod_peak_female <- predict(mod_peak_female, interval = "confidence")
predict_mod_peak_female <- as_tibble(predict_mod_peak_female)

# con ggplot se grafica el modelo predicio y se crea la senda de IC
# geom_ribbon permite printar el intervalo de confianza estimado
data %>% ggplot(aes(y = predict_mod_peak_female$fit, x = age, color = female)) +
  geom_point() +
  labs(
    x = "Edad",
    y = "Log Salario mensual predicho",
    title = "log Salarios mensual vs. Edad (95% IC)"
  ) +
  geom_ribbon(
    aes(ymin = predict_mod_peak_female$lwr, ymax = predict_mod_peak_female$upr),
    alpha = 0.2
  )

# Codigo para item c
# primero se convierten las variables categoricas en dummies y se meten en una lista
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

#Se crea formulas que contienen las variables de inteteres con el animo de acortar los atributos a asignar a las regresiones
variables <- data_with_dummies %>%
  select(-c(y_salary_m, female)) %>%
  colnames()
formula <- paste0(variables, collapse = " + ")
formula1 <- paste("log(y_salary_m) ~", formula)
formula2 <- paste("as.numeric(female) ~", formula)

# Se estima el modelo grande, luego los residuales dejando a edad quieta 
mod_res0 <- lm(log(y_salary_m) ~ ., data_with_dummies)
mod_res1 <- lm(formula1, data_with_dummies)$residuals
mod_res2 <- lm(formula2, data_with_dummies)$residuals
mod_res3 <- lm(mod_res1 ~ mod_res2 - 1)

data$mod_res1 <- lm(formula1, data_with_dummies)$residuals
data$mod_res2 <- lm(formula2, data_with_dummies)$residuals

stargazer(
  mod_res0, mod_res3,
  dep.var.labels = c("Ln(income)", "FWL"), 
  keep = c("female", "mod_res2"),
  out = "../views/Modelo_age_earnings_control_FWL.tex"
)

stargazer(
  mod_res0,
  dep.var.labels = c("Ln(income)", "FWL"), 
  keep = c("female", "mod_res2"),
  out = "../views/Modelo_age_earnings_control.tex"
)

summary(mod_res0)

# Bootstrap
# Finalmente se define la funciÛn de boost para estimar o costruir la desviaciÛn estandar

beta_est <- function(data, index) {
  coef(lm(mod_res1 ~ mod_res2 - 1, data = data, subset = index))
}

# Se hace 1000 replicas de la funciÛn beta que contiene los coeficientes
boot_fwl <- boot(data, beta_est, R = 1000)
# Bootstrap estimate

# Se genera un objeto para extraer y almacenar el S.d. del parametro estimado
boot_est <- boot_fwl$t0
boot_est_se <- apply(boot_fwl$t, 2, sd)
