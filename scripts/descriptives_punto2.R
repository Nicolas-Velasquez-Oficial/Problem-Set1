source("./scripts/data_cleaning.R")
library("skimr")
library("ggcorrplot")
library("stargazer")
library("boot")

################################### Descriptives#################################
skim(data)
glimpse(data)
stargazer(data)

# Check correlations (as scatterplots), distribution and print corrleation coefficient
data_plot_vardep <- data %>% select(c("y_salary_m", "y_ingLab_m", "y_total_m", "ingtot"))
data_plot_vardep <- na.omit(data_plot_vardep)
corr <- round(cor(data_plot_vardep), 1)

ggcorrplot(corr, method = "circle", type = "lower", lab = TRUE) +
  ggtitle("Correlograma de base de variables de ingresos") +
  theme_minimal() +
  theme(legend.position = "none")

data_plot <- data %>% select(-c("estrato1", "oficio", "relab", "maxEducLevel", "regSalud", "cotPension"))
corr <- round(cor(data_plot), 1)

ggcorrplot(corr, method = "circle", type = "lower", lab = TRUE) +
  ggtitle("Correlograma de base de ingresos") +
  theme_minimal() +
  theme(legend.position = "none")
############################## Histogramas y gráficos de barras##################

ggplot(data, aes(x = ingtot)) +
  geom_histogram(bins = 20, fill = "darkorange") +
  ggtitle("Ingresos Totales") +
  labs(x = "Ingreso Total", y = "Cantidad") +
  theme_bw()

## Bar chart for categorical variables

# plotting Estrato
ggplot(data, aes(x = `estrato1`)) +
  geom_bar(fill = "darkorange") +
  xlab("Estrato") +
  theme_bw()

# plotting Max Education Level
ggplot(data, aes(x = `maxEducLevel`)) +
  geom_bar(fill = "darkorange") +
  xlab("Nivel de educación") +
  theme_bw()
################################# Punto 2########################################

mod_age <- lm("ingtot ~ age + I(age^2)", data = data)

summary(mod_age)
results <- tidy(mod_age)
stargazer(mod_age, dep.var.labels = c("Earnings"), out = "./views/Modelo_Age.tex")


beta.fn <- function(data, index) {
  coef(lm(ingtot ~ age + I(age^2), data = data, subset = index))
}

set.seed(1010)

boot(data, beta.fn, R = 1000)

plotbot <- boot(data, beta.fn, R = 1000)

sd <- apply(plotbot$t, 2, sd)

intervalos <- as_tibble(predict(mod_age, interval = "confidence"))

intervalos$lwr <- intervalos$lwr - (1.96 * sd[2])
intervalos$upr <- intervalos$upr + (1.96 * sd[2])

ggplot(data, aes(y = predict(mod_age), x = age)) +
  labs(x = "Edad", y = "Salario predicho", title = "Salarios Predichos vs. Edad (95% IC)") +
  geom_point() +
  geom_ribbon(
    aes(ymin = intervalos$lwr, ymax = intervalos$upr),
    alpha = 0.2
  )


### Peak Age = -b1/2*b2
lm_summary <- summary(mod_age)$coefficients

Peak_age <- -(lm_summary[2, 1]) / (2 * lm_summary[3, 1])

Peak_age
