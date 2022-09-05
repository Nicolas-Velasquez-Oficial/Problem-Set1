source("../scripts/data_cleaning.R")
library("caret")
library("doSNOW")
library("foreach")
library("stargazer")


# create ID column
data$id <- 1:nrow(data)
# use 70% of dataset as training set and 30% as test set

train <- data %>% dplyr::slice_sample(prop = 0.70)
test <- data %>% dplyr::anti_join(train, by = "id")
data <- dplyr::select(data, -id)
train <- dplyr::select(train, -id)
test <- dplyr::select(test, -id)

# Solve issue when fitting a regression
test <- test %>% semi_join(train, by = c("oficio", "relab"))

# Generate formulas to be used in the model comparison
formulas <- c(
    "age + I(age ^ 2)",
    "female*age + I(age ^ 2) + female:I(age ^ 2)",
    "female + age + totalHoursWorked + relab + sizeFirm + maxEducLevel + oficio",
    "poly(totalHoursWorked, 3)*sizeFirm + female",
    "poly(age, 3) + maxEducLevel",
    "poly(log(age), 3)*poly(log(totalHoursWorked), 3)"
)

dependent <- "log(y_salary_m) ~ "
formulas <- paste0(dependent, formulas)

# Fit a model and compute RMSE with respect to the test set
lm_fit <- function(formula) {
    fit_reg <- lm(formula, train)
    preds <- predict(
        fit_reg,
        test
    )
    rmse <- caret::RMSE(preds, log(test$y_salary_m), na.rm = TRUE)
    rmse
}

errors <- numeric()

# Use lm_fit with each specification in formulas
for (i in formulas) {
    error <- lm_fit(i)
    errors[i] <- error
}

stargazer(errors, summary = FALSE, rownames = FALSE)

# Select best models with regards to RMSE value (smallest)
best_models <- function(n) {
    sorted <- sort(errors)
    best <- sorted[1:n]
    attributes(best)$names
}

cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

# Compute influences for observations in test set (parallelized)
for (i in best_models(1)) {
    influences <- foreach(j = 1:nrow(test), .combine = c) %dopar% {
        temp_set <- rbind(train, test[j, ])
        fit_best <- lm(i, temp_set)
        infl <- dffits(fit_best)
        influence <- infl[length(infl)]
        influence
    }
}

# Summarize influences over train and test sets
histogram(influences)
boxplot(influences)
boxplot(dffits(lm(best_models(1), train)))
summary(influences)

# Cross-validate third and fifth specifications with LOOCV
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

# Stop parallel cluster
stopCluster(cl)

stargazer(model1$results, title = "Desempeño del modelo 3", summary = FALSE, rownames = FALSE)
stargazer(model2$results, title = "Desempeño del modelo 5", summary = FALSE, rownames = FALSE)
