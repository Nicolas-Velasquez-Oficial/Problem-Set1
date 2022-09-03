library("tidyverse")
library("caret")
library("doSNOW")
library("foreach")

df <- read.csv("./stores/data.csv")

## Filter by age >18 & Cleaning Data set
df <- df[df$age >= 18, ]
df <- df[df$ocu == 1, ]

data <- df %>%
    select(!matches(c(("^p[0-9]"), "^cc", "^io", "^y_", "^fex", "^hours"))) %>%
    select(
        -(ina:ingtotes),
        -c(
            depto, dominio, clase, V1, wap, directorio, secuencia_p, pet, orden,
            mes, fweight, informal, cuentaPropia, pea, microEmpresa, ocu, dsi,
            inac
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
    dplyr::select(-c(age, ingtot, totalHoursWorked)) %>%
    colnames()

data[, categoricals] <- lapply(data[, categoricals], factor)

set.seed(10)

# create ID column
data$id <- 1:nrow(data)
# use 70% of dataset as training set and 30% as test set
train <- data %>% dplyr::sample_frac(0.70)
test <- data %>% dplyr::anti_join(train, by = "id")
data <- dplyr::select(data, -id)
train <- dplyr::select(train, -id)
test <- dplyr::select(test, -id)

# Solve issue when fitting a regression
test <- test %>% semi_join(train, by = "relab")

formulas <- c(
    "estrato1",
    "sex",
    "age",
    "relab",
    "cotPension",
    "maxEducLevel",
    "regSalud",
    "totalHoursWorked"
)

dependent <- "ingtot ~ "

formulas <- paste0(dependent, formulas)



lm_fit <- function(formula) {
    fit_reg <- lm(formula, train)
    preds <- predict(fit_reg, test)
    rmse <- caret::RMSE(preds, test$ingtot, na.rm = TRUE)
    rmse
}

errors <- numeric()

for (i in formulas) {
    error <- lm_fit(i)
    errors[i] <- error
}

best_models <- function(n) {
    sorted <- sort(errors)
    best <- sorted[1:n]
    attributes(best)$names
}

influences <- numeric()

cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

for (i in best_models(1)) {
    foreach(j = 1:nrow(test), .combine = c) %dopar% {
        temp_set <- rbind(train, test[j, ])
        fit_best <- lm(i, temp_set)
        infl <- dffits(fit_best)
        influence <- infl[length(infl)]
        influences[j] <- influence
        return(influences)
    }
}

stopCluster(cl)

for (i in best_models(1)) {
    for (j in 1:nrow(test)) {
        temp_set <- rbind(train, test[j, ])
        fit_best <- lm(i, temp_set)
        infl <- dffits(fit_best)
        influence <- infl[length(infl)]
        influences[j] <- influence
    }
}

boxplot(influences)
summary(influences)
temp_set %>% tail()
