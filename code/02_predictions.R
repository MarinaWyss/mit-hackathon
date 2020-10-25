library(tidyverse)
library(xgboost)
library(Metrics)

set.seed(2342)

# 01 load data -------------------------------------------------------
demographics <- read.csv("externalData/censusData.csv")
politicalData <- read.csv("externalData/politicalData.csv")
prisonData <- read.csv("externalData/prisonData.csv")

externalData <- demographics %>% 
  left_join(politicalData) %>% 
  left_join(prisonData)

covidData <- read.csv("externalData/covid.csv")

# 02 data prep -------------------------------------------------------
covidDataAgg <- covidData %>% 
  group_by(State) %>% 
  summarise(confirmed_cases_residents = sum(
    Confirmed.Cases...Residents., na.rm = TRUE)) %>% 
  rename("state" = State) %>% 
  mutate(state = tolower(state))

data <- externalData %>% 
  left_join(covidDataAgg) %>% 
  filter(!is.na(confirmed_cases_residents)) %>% 
  mutate(cases_per_population = confirmed_cases_residents/total_incarcerated_persons)

indVars <- c("population_estimates_july_1_2019__v2019",
             "persons_65_years_and_over_percent",
             "white_alone_percent",
             "black_or_african_american_alone_percent",
             "american_indian_and_alaska_native_alone_percent",
             "asian_alone_percent",
             "native_hawaiian_and_other_pacific_islander_alone_percent",
             "two_or_more_races_percent",
             "hispanic_or_latino_percent",
             "white_alone_not_hispanic_or_latino_percent",
             "veterans_2014.2018",
             "foreign_born_persons_percent_2014.2018",
             "persons_per_household_2014.2018",
             "language_other_than_english_spoken_at_home_percent_of_persons_age_5_years._2014.2018",
             "households_with_a_computer_percent_2014.2018",
             "households_with_a_broadband_internet_subscription_percent_2014.2018",
             "high_school_graduate_or_higher_percent_of_persons_age_25_years._2014.2018",
             "bachelor.s_degree_or_higher_percent_of_persons_age_25_years._2014.2018",
             "with_a_disability_under_age_65_years_percent_2014.2018",
             "persons__without_health_insurance_under_age_65_years_percent",
             "median_household_income_in_2018_dollars_2014.2018",
             "persons_in_poverty_percent",
             "republican_governor",
             "voted_trump_2016_percent",
             "prison_rate_per_100k",
             "total_incarcerated_persons",
             "cases_per_population")

dataFiltered <- data[ , which((names(data) %in% indVars) == TRUE)]
# 03 train-test-split -------------------------------------------------------
smp_size <- floor(0.75 * nrow(dataFiltered))

train_ind <- sample(seq_len(nrow(dataFiltered)), size = smp_size)

train <- dataFiltered[train_ind, ]
test <- dataFiltered[-train_ind, ]

# 04 XGBoost -------------------------------------------------------
X_df <- train[setdiff(names(train), c("cases_per_population"))]
X <- as.matrix(sapply(X_df, as.numeric))  
Y <- train$cases_per_population

# hyperparameters
hyper_grid <- expand.grid(
  eta = 0.01,
  max_depth = 3, 
  min_child_weight = 3,
  subsample = 0.5, 
  colsample_bytree = 0.5,
  gamma = c(0, 1, 10, 100, 1000),
  lambda = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  alpha = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  error = 0,          
  trees = 0          
)

for(i in seq_len(nrow(hyper_grid))) {
  set.seed(123)
  m <- xgb.cv(
    data = X,
    label = Y,
    nrounds = 4000,
    objective = "reg:squarederror",
    early_stopping_rounds = 50, 
    nfold = 10,
    verbose = 0,
    params = list( 
      eta = hyper_grid$eta[i], 
      max_depth = hyper_grid$max_depth[i],
      min_child_weight = hyper_grid$min_child_weight[i],
      subsample = hyper_grid$subsample[i],
      colsample_bytree = hyper_grid$colsample_bytree[i],
      gamma = hyper_grid$gamma[i], 
      lambda = hyper_grid$lambda[i], 
      alpha = hyper_grid$alpha[i]
    ) 
  )
  hyper_grid$error[i] <- min(m$evaluation_log$test_error_mean)
  hyper_grid$trees[i] <- m$best_iteration
}

hyper_grid %>%
  filter(error > 0) %>%
  arrange(error) %>%
  glimpse()

params <- list(
  eta = 0.01,
  max_depth = 3,
  min_child_weight = 3,
  subsample = 0.5,
  colsample_bytree = 0.5, 
  alpha = 0.0
)

xgbTrain <- xgboost(
  params = params,
  data = X,
  label = Y,
  nrounds = 56,
  objective = "reg:squarederror",
  verbose = 0
)

# prediction 
X_df_test <- test[setdiff(names(test), c("cases_per_population", "state"))]
X_test <- as.matrix(sapply(X_df_test, as.numeric))  
Y_test <- test$cases_per_population

preds <- predict(xgbTrain, as.matrix(X_test))

# evaluate
cbind(Y_test, preds)
rmse(Y_test, preds)

# feature importance
vip::vip(xgbTrain) 
