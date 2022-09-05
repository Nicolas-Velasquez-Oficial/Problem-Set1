library("tidyverse")

df <- read.csv("./stores/data.csv")

## Filter by age >18 & Cleaning Data set
df <- df[df$age >= 18, ]
df <- df[df$ocu == 1, ]
df <- df %>% mutate(female = ifelse(sex == 1, 0, 1))

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

y_salary_m <- df$y_salary_m
y_ingLab_m <- df$y_ingLab_m
y_total_m <- df$y_total_m
data$y_ingLab_m <- y_ingLab_m
data$y_salary_m <- y_salary_m
data$y_total_m <- y_total_m

categoricals <- data %>%
    dplyr::select(-c(age, ingtot, totalHoursWorked, y_ingLab_m, y_salary_m, y_total_m)) %>%
    colnames()

data[, categoricals] <- lapply(data[, categoricals], factor)

data <- data %>%
    replace_na(
        list(y_salary_m = mean(data$y_salary_m, na.rm = TRUE))
    )

data$maxEducLevel[is.na(data$maxEducLevel)] <- 7
data$regSalud[is.na(data$regSalud)] <- 1

## NOTE: Confirm if this should be '>' or '>='
df <- df[df$age >= 18, ]
df <- df[df$ocu == 1, ]
df <- df %>% mutate(female = if_else(sex == 0, 1, 0))

data <- df %>%
    select(!matches(c(("^p[0-9]"), "^cc", "^io", "^y_", "^fex", "^hours"))) %>%
    select(-(ina:ingtotes), -c(depto, sex, dominio, clase, V1, wap, directorio, secuencia_p, pet, orden, fweight, informal, cuentaPropia, pea, microEmpresa, ocu, dsi, inac)) %>%
    replace_na(list(oficio = 0, relab = 0, totalHoursWorked = 0))


y_salary_m <- df$y_salary_m
y_ingLab_m <- df$y_ingLab_m
y_total_m <- df$y_total_m
data$y_ingLab_m <- y_ingLab_m
data$y_salary_m <- y_salary_m
data$y_total_m <- y_total_m
categoricas <- c("estrato1", "oficio", "relab", "maxEducLevel", "regSalud", "cotPension")
for (v in categoricas) {
    data[, v] <- as.factor(data[, v, drop = T])
}
skim(data)
###################### Missing Values in categorical replacing with mode#########
table(data$maxEducLevel)
data$maxEducLevel[is.na(data$maxEducLevel)] <- 7
table(data$regSalud)
data$regSalud[is.na(data$regSalud)] <- 1
