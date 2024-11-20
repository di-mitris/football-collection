getDataFromFootballData <- function(season = "1415") {
  temp <- read.csv(paste0("https://www.football-data.co.uk/mmz4281/",
                          season, "/E0.csv"))
  subset(temp, select = c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG"))
}

library(Countr)

Seasons <- paste0(10:14, 11:15)
theData <- data.frame()
for (i in seq(along = Seasons))
  theData <- rbind(theData, getDataFromFootballData(Seasons[i]))

theData <- na.omit(theData)

write.csv(theData,"C:\\Users\\Mitsos\\Desktop\\Projects\\Sports\\RProjects\\epldata.csv", row.names = FALSE)

HG <- theData$FTHG
AG <- theData$FTAG
sum(is.nan(HG))  # Counts the number of missing values in HG
# which(is.na(HG))  # Returns the indices of missing values
# tail(theData, 10)

# Plot 1: HG
hg_freq <- table(HG)
plot(hg_freq, xlab = 'Number of Home Team Goals', ylab = 'Count')
c(Mean = mean(HG), Var = var(HG))

# Plot 2: O/E
hg_n <- length(HG)
lambdaHat <- mean(HG)
hg_count <- as.numeric(names(hg_freq))
hg_E <- hg_n * dpois(hg_count, lambda = lambdaHat)
hg_eo <- rbind(hg_freq, hg_E)
row.names(hg_eo) <- c("Observed", "Expected")
plot(hg_freq, xlab = 'Observed and Expected frequencies of Home Team Goals', ylab = 'Count')
points(hg_count, hg_E)

print(hg_eo, digits = 4)

# Wrapper function for dWeibullCount
dweibullcount <- function(x, lambda, c) {
  scale <- 1 / lambda  # Convert rate (lambda) to scale
  dWeibullCount(x, shape = c, scale = scale, method = "series_acc")
}

# Fit the Weibull count distribution using the wrapper
weibull_fit <- fitdistr(
  x = HG,
  densfun = dweibullcount,
  start = list(lambda = 1, c = 1)  # Initial parameter guesses
  # lower = c(0.001, 0.001) 
)

print(weibull_fit)
lambda_hat <- weibull_fit$estimate["lambda"]
c_hat <- weibull_fit$estimate["c"]

weibull_count_pmf <- function(x, lambda, c) {
  scale <- 1 / lambda  # Convert rate (lambda) to scale
  dWeibullCount(x, shape = c, scale = scale, method = "series_acc")
}

# Compute expected frequencies
hg_E_weibull <- sapply(hg_count, weibull_count_pmf, lambda = lambda_hat, c = c_hat) * hg_n

# Plot observed vs. expected
plot(hg_freq, xlab = "Number of Home Goals", ylab = "Count", main = "Observed vs Expected Frequencies")
points(hg_count, hg_E_weibull, col = "red", pch = 19, type = "b")
legend("topright", legend = c("Observed", "Weibull Count"), col = c("black", "red"), pch = c(1, 19))

# Poisson model: Fit Poisson distribution to the data
poisson_fit <- fitdistr(HG, densfun = "poisson")

# AIC values
aic_weibull <- 2 * length(weibull_fit$estimate) - 2 * weibull_fit$loglik
aic_poisson <- 2 * length(poisson_fit$estimate) - 2 * poisson_fit$loglik

cat("AIC for Weibull count distribution:", aic_weibull, "\n")
cat("AIC for Poisson distribution:", aic_poisson, "\n")

# Likelihood ratio test for goodness-of-fit comparison
loglik_weibull <- weibull_fit$loglik
loglik_poisson <- poisson_fit$loglik
lr_statistic <- 2 * (loglik_weibull - loglik_poisson)
df_diff <- length(weibull_fit$estimate) - length(poisson_fit$estimate)
p_value <- 1 - pchisq(lr_statistic, df = df_diff)

# Print the goodness-of-fit comparison results
cat("Log-likelihood ratio statistic:", lr_statistic, "\n")
cat("Degrees of freedom difference:", df_diff, "\n")
cat("p-value:", p_value, "\n")

# Interpretation
if (p_value < 0.05) {
  cat("Weibull count model fits significantly better than Poisson (p < 0.05).\n")
} else {
  cat("No significant difference between Weibull count and Poisson models.\n")
}


