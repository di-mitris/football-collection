getDataFromFootballData <- function(season = "1415") {
  temp <- read.csv(paste0("https://www.football-data.co.uk/mmz4281/",
                          season, "/E0.csv"))
  subset(temp, select = c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG"))
}

library(Countr)
library(copula)
library(dplyr)

#Pre-processing start
Seasons <- paste0(10:14, 11:15)
theData <- data.frame()
for (i in seq(along = Seasons))
  theData <- rbind(theData, getDataFromFootballData(Seasons[i]))

theData <- na.omit(theData)
# Convert Date to a Date object
theData$Date <- as.Date(theData$Date, format = "%d/%m/%y")

# Sort data by Date
theData <- theData %>% arrange(Date)

# Split data: First 4.5 seasons for training, last 0.5 season for testing
n <- nrow(theData)
split_index <- round(4.5 / 5 * n)

train_data <- theData[1:split_index, ]
test_data <- theData[(split_index + 1):n, ]

# Fit Weibull model for Home Goals
weibull_home <- renewalCount(
  formula = FTHG ~ HomeTeam + AwayTeam,
  data = train_data,
  dist = "weibull",
  weiMethod = "series_acc"  # Use series acceleration for efficient computation
)

# Fit Weibull model for Away Goals
weibull_away <- renewalCount(
  formula = FTAG ~ AwayTeam + HomeTeam,
  data = train_data,
  dist = "weibull",
  weiMethod = "series_acc"
)

# CUT
# extract names?

pseudo_data <- cbind(train_data$FTHG, train_data$FTAG)

# Fit Frank copula
frank_cop <- fitCopula(
  copula = frankCopula(),
  data = pobs(pseudo_data),
  method = "ml"
)

# Extract the copula parameter
theta <- coef(frank_cop)

# DEBUG
simulate_match <- function(HomeTeam, AwayTeam, n = 1000) {
  # Access coefficients using proper formatting
  home_lambda <- exp(
    weibull_home$coefficients[paste0("scale_HomeTeam", HomeTeam)] +
      weibull_home$coefficients[paste0("scale_AwayTeam", AwayTeam)]
  )
  away_lambda <- exp(
    weibull_away$coefficients[paste0("scale_AwayTeam", AwayTeam)] +
      weibull_away$coefficients[paste0("scale_HomeTeam", HomeTeam)]
  )
  
  # Simulate from the Frank copula
  copula_samples <- rCopula(n, frankCopula(param = coef(frank_cop)))
  home_goals <- qpois(copula_samples[, 1], lambda = home_lambda)
  away_goals <- qpois(copula_samples[, 2], lambda = away_lambda)
  
  # Determine the most probable match outcome
  outcome_freq <- table(ifelse(home_goals > away_goals, "1",
                               ifelse(home_goals < away_goals, "2", "X")))
  
  # Handle cases with no valid outcomes
  if (length(outcome_freq) == 0) {
    return("X")  # Default to draw
  } else {
    return(names(outcome_freq)[which.max(outcome_freq)])
  }
}

test_data$Predicted_Result <- mapply(
  simulate_match,
  HomeTeam = test_data$HomeTeam,
  AwayTeam = test_data$AwayTeam
)



simulate_total_goals <- function(HomeTeam, AwayTeam, n = 1000) {
  # Access coefficients using proper formatting
  home_lambda <- exp(
    weibull_home$coefficients[paste0("scale_HomeTeam", HomeTeam)] +
      weibull_home$coefficients[paste0("scale_AwayTeam", AwayTeam)]
  )
  away_lambda <- exp(
    weibull_away$coefficients[paste0("scale_AwayTeam", AwayTeam)] +
      weibull_away$coefficients[paste0("scale_HomeTeam", HomeTeam)]
  )
  
  # Simulate from the Frank copula
  copula_samples <- rCopula(n, frankCopula(param = coef(frank_cop)))
  home_goals <- qpois(copula_samples[, 1], lambda = home_lambda)
  away_goals <- qpois(copula_samples[, 2], lambda = away_lambda)
  
  total_goals <- home_goals + away_goals
  
  # Return probability of over 2.5 goals
  return(mean(total_goals > 2.5))
}

test_data$Predicted_Over_2_5 <- mapply(
  simulate_total_goals,
  HomeTeam = test_data$HomeTeam,
  AwayTeam = test_data$AwayTeam
)

table(test_data$Predicted_Result)
summary(test_data$Predicted_Over_2_5)


na_rows <- which(is.na(test_data$Predicted_Over_2_5))
test_data[na_rows, ]
#END DEBUG

simulate_match <- function(HomeTeam, AwayTeam, n = 1000) {
  # Handle baseline case for HomeTeam
  home_team_effect <- if (HomeTeam == "Arsenal") 0 else weibull_home$coefficients[paste0("scale_HomeTeam", HomeTeam)]
  
  # Handle baseline case for AwayTeam
  away_team_effect <- if (AwayTeam == "Arsenal") 0 else weibull_home$coefficients[paste0("scale_AwayTeam", AwayTeam)]
  
  # Compute home and away lambdas
  home_lambda <- exp(home_team_effect + away_team_effect)
  away_lambda <- exp(
    (if (AwayTeam == "Arsenal") 0 else weibull_away$coefficients[paste0("scale_AwayTeam", AwayTeam)]) +
      (if (HomeTeam == "Arsenal") 0 else weibull_away$coefficients[paste0("scale_HomeTeam", HomeTeam)])
  )
  
  # Simulate from the Frank copula
  copula_samples <- rCopula(n, frankCopula(param = coef(frank_cop)))
  home_goals <- qpois(copula_samples[, 1], lambda = home_lambda)
  away_goals <- qpois(copula_samples[, 2], lambda = away_lambda)
  
  # Determine the most probable match outcome
  outcome_freq <- table(ifelse(home_goals > away_goals, "1",
                               ifelse(home_goals < away_goals, "2", "X")))
  
  if (length(outcome_freq) == 0) {
    return("X")  # Default to draw if no valid outcomes
  } else {
    return(names(outcome_freq)[which.max(outcome_freq)])
  }
}

test_data$Predicted_Result <- mapply(
  simulate_match,
  HomeTeam = test_data$HomeTeam,
  AwayTeam = test_data$AwayTeam
)

simulate_total_goals <- function(HomeTeam, AwayTeam, n = 1000) {
  # Handle baseline case for HomeTeam
  home_team_effect <- if (HomeTeam == "Arsenal") 0 else weibull_home$coefficients[paste0("scale_HomeTeam", HomeTeam)]
  
  # Handle baseline case for AwayTeam
  away_team_effect <- if (AwayTeam == "Arsenal") 0 else weibull_home$coefficients[paste0("scale_AwayTeam", AwayTeam)]
  
  # Compute home and away lambdas
  home_lambda <- exp(home_team_effect + away_team_effect)
  away_lambda <- exp(
    (if (AwayTeam == "Arsenal") 0 else weibull_away$coefficients[paste0("scale_AwayTeam", AwayTeam)]) +
      (if (HomeTeam == "Arsenal") 0 else weibull_away$coefficients[paste0("scale_HomeTeam", HomeTeam)])
  )
  
  # Simulate from the Frank copula
  copula_samples <- rCopula(n, frankCopula(param = coef(frank_cop)))
  home_goals <- qpois(copula_samples[, 1], lambda = home_lambda)
  away_goals <- qpois(copula_samples[, 2], lambda = away_lambda)
  
  total_goals <- home_goals + away_goals
  return(mean(total_goals > 2.5))  # Probability of over 2.5 goals
}

test_data$Predicted_Over_2_5 <- mapply(
  simulate_total_goals,
  HomeTeam = test_data$HomeTeam,
  AwayTeam = test_data$AwayTeam
)

# VAL
test_data[test_data$HomeTeam == "Arsenal" | test_data$AwayTeam == "Arsenal", ]
sum(is.na(test_data$Predicted_Result))
sum(is.na(test_data$Predicted_Over_2_5))




# Create actual results for 1X2
test_data$Actual_Result <- ifelse(test_data$FTHG > test_data$FTAG, "1",
                                  ifelse(test_data$FTHG < test_data$FTAG, "2", "X"))

# Accuracy for 1X2 predictions
accuracy_1X2 <- mean(test_data$Predicted_Result == test_data$Actual_Result, na.rm = TRUE)
print(paste("Accuracy for 1X2 predictions:", round(accuracy_1X2 * 100, 2), "%"))

# Actual results for Over/Under
test_data$Actual_Over_2_5 <- test_data$FTHG + test_data$FTAG > 2.5

# Brier Score for Over/Under
brier_score <- mean((test_data$Predicted_Over_2_5 - as.numeric(test_data$Actual_Over_2_5))^2, na.rm = TRUE)
print(paste("Brier Score for Over/Under predictions:", round(brier_score, 4)))

# Team-specific analysis
team_analysis <- test_data %>%
  group_by(HomeTeam) %>%
  summarise(
    Total_Home_Games = n(),
    Correct_Home_Predictions = sum(Predicted_Result == Actual_Result, na.rm = TRUE),
    Accuracy_Home = round(100 * Correct_Home_Predictions / Total_Home_Games, 2)
  )

print(team_analysis)

# Visualization

library(ggplot2)

# Prepare data for heatmap
heatmap_data <- test_data %>%
  mutate(Correct = Predicted_Result == Actual_Result) %>%
  group_by(HomeTeam, AwayTeam) %>%
  summarise(Accuracy = mean(Correct, na.rm = TRUE)) %>%
  na.omit()

# Heatmap
ggplot(heatmap_data, aes(x = AwayTeam, y = HomeTeam, fill = Accuracy)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0.5) +
  labs(title = "Prediction Accuracy by Team (Home vs Away)",
       x = "Away Team", y = "Home Team", fill = "Accuracy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Histogram of Predicted Over 2.5 probabilities
ggplot(test_data, aes(x = Predicted_Over_2_5)) +
  geom_histogram(bins = 20, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Predicted Over 2.5 Goals Probabilities",
       x = "Predicted Probability (Over 2.5 Goals)", y = "Frequency") +
  theme_minimal()

# Scatter plot of actual vs predicted
test_data$TotalGoals <- test_data$FTHG + test_data$FTAG

ggplot(test_data, aes(x = Predicted_Over_2_5, y = TotalGoals)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Predicted Over 2.5 Probabilities vs Actual Total Goals",
       x = "Predicted Probability (Over 2.5 Goals)", y = "Actual Total Goals") +
  theme_minimal()


























































#wtf
HG <- theData$FTHG
AG <- theData$FTAG

# Encode teams as factors
team_list <- unique(c(theData$HomeTeam, theData$AwayTeam))
theData <- theData %>% 
  mutate(
    HomeTeam = factor(HomeTeam, levels = team_list),
    AwayTeam = factor(AwayTeam, levels = team_list)
  )

# Fit Weibull margins for HG
weibull_HG <- glm(
  FTHG ~ HomeTeam + AwayTeam + 1,  # Home attack, Away defense, and home advantage
  data = theData,
  family = poisson(link = "log")
)

# Fit Weibull margins for AG
weibull_AG <- glm(
  FTAG ~ AwayTeam + HomeTeam,  # Away attack and Home defense
  data = theData,
  family = poisson(link = "log")
)

# Extract coefficients for attack and defense strengths
home_attack <- coef(weibull_HG)[grepl("HomeTeam", names(coef(weibull_HG)))]
away_defense <- coef(weibull_HG)[grepl("AwayTeam", names(coef(weibull_HG)))]
away_attack <- coef(weibull_AG)[grepl("AwayTeam", names(coef(weibull_AG)))]
home_defense <- coef(weibull_AG)[grepl("HomeTeam", names(coef(weibull_AG)))]

data_copula <- cbind(HG, AG)

# Fit a Frank copula with manually specified start value
frank_cop <- fitCopula(
  copula = frankCopula(),
  data = pobs(data_copula),
  method = "ml"
)

theta <- coef(frank_cop)
print(theta)

# Simulate using Frank copula
simulated_copula <- rCopula(1000, frankCopula(param = theta))

# Transform back to HG and AG scales
simulated_HG <- qweibull(simulated_copula[, 1], shape = 1, scale = exp(predict(weibull_HG)))
simulated_AG <- qweibull(simulated_copula[, 2], shape = 1, scale = exp(predict(weibull_AG)))

# Combine results
results <- data.frame(
  HomeGoals = simulated_HG,
  AwayGoals = simulated_AG
)

# Calculate match outcomes
results <- results %>%
  mutate(
    MatchResult = ifelse(HomeGoals > AwayGoals, "1",
                         ifelse(HomeGoals < AwayGoals, "2", "X"))
  )

# Display result probabilities
table(results$MatchResult) / nrow(results)
table(simulated_HG == simulated_AG)


