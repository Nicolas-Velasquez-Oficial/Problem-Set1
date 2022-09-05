source("./scripts/data_cleaning.R")
library("stargazer")
library("boot")

# a
# Modelo básico
mod_female <- lm(log(y_salary_m) ~ female, data = data)
summary(mod_female)

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
boot_fwl <- boot(data, beta_est, R = 1000)
# Bootstrap estimate

boot_est <- boot_fwl$t0
boot_est_se <- apply(boot_fwl$t, 2, sd)
