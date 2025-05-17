ccpp=read.csv("D:/assignment/data/dataset.csv", header = TRUE)

#Get number of samples
n <- nrow(ccpp)


# RSS values for each model (replace with your actual values)
rss_values <- c(
  Model1 = 657248.2,
  Model2 = 602347.1,
  Model3 = 549034.9,
  Model4 = 603630.7,
  Model5 = 365625.0
)

# Number of parameters (k) in each model including bias
# Model 1: x4, x3^2, bias b 3 params
# Model 2: x4, x3^2, x5, bias b 4 params
# Model 3: x3, x4, x5^3 b 3 params
# Model 4: x4, x3^2, x5^3, bias b 4 params
# Model 5: x4, x1^2, x3^2, bias b 4 params
k_values <- c(
  model1 = 3,
  model2 = 4,
  model3 = 3,
  model4 = 4,
  model5 = 4
)

# Compute sigma^2 (variance of residuals)
sigma2 <- rss_values / (n - 1)

# Compute log-likelihood
log_likelihood <- -n/2 * log(2*pi) - n/2 * log(sigma2) - rss_values / (2 * sigma2)

# Compute AIC and BIC
aic <- 2 * k_values - 2 * log_likelihood
bic <- log(n) * k_values - 2 * log_likelihood

# Combine results in a data frame
results <- data.frame(
  #Model = names(rss_values),
  #RSS = rss_values,
  k = k_values,
  #logLik = log_likelihood,
  AIC = aic,
  BIC = bic
)

# Print results
print(results)
