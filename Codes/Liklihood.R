ccpp=read.csv("D:/assignment/data/dataset.csv", header = TRUE)

#Get number of samples
n <- nrow(ccpp)


# Creating log-likelihood function
log_likelihood <- function(rss, n) {
  delta_hat_sq <- rss / (n - 1)
  ll <- - (n / 2) * log(2 * pi) -
    (n / 2) * log(delta_hat_sq) -
    (1 / (2 * delta_hat_sq)) * rss
  return(ll)
}

#Define the RSS values (replace with your actual computed values)
rss_values <- c(
  Model1 = 657248.2,  # Replace with actual RSS
  Model2 = 602347.1,
  Model3 = 549034.9,
  Model4 = 603630.7,
  Model5 = 365625.0
)


log_likelihoods <- sapply(rss_values, function(rss) log_likelihood(rss, n))


log_likelihoods
