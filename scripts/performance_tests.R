source("./scripts/data_cleaning.R")
library("caret")
library("doSNOW")
library("foreach")

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
test <- test %>% semi_join(train, by = c("relab", "oficio"))

formulas <- c(
    "age + I(age ^ 2)",
    "female*age + I(age ^ 2) + female:I(age ^ 2)",
    "female + age + totalHoursWorked + sizeFirm + relab + maxEducLevel + oficio",
    "poly(totalHoursWorked, 3)*sizeFirm + female",
    "poly(age, 3) + maxEducLevel",
    "poly(log(age), 3)*poly(log(totalHoursWorked), 3)"
)

dependent <- "log(y_salary_m) ~ "
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

cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

for (i in best_models(1)) {
    influences <- foreach(j = 1:nrow(test), .combine = c) %dopar% {
        temp_set <- rbind(train, test[j, ])
        fit_best <- lm(i, temp_set)
        infl <- dffits(fit_best)
        influence <- infl[length(infl)]
        influence
    }
}

histogram(influences)
boxplot(influences)
summary(influences)

train_control <- trainControl(method = "LOOCV")


trainer <- function(formula) {
    caret::train(
        as.formula(formula),
        data = data,
        method = "lm",
        trControl = train_control,
        na.action = na.omit
    )
}

model1 <- trainer(best_models(2)[1])
model2 <- trainer(best_models(2)[2])
stopCluster(cl)

str(model1)
model1$results
model2$results
