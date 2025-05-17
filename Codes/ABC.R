library(ggplot2)
library(gridExtra)

ccpp=read.csv("D:/assignment/data/dataset.csv", header = TRUE)

# Dependent variable
y <- ccpp$x2

# Fit model with squared terms
model5 <- lm(x2 ~ x4 + I(x1^2) + I(x3^2), data = ccpp)

# Extract coefficients
theta_hat5 <- coef(model5)

# Design matrix
X5 <- cbind(ccpp$x4, ccpp$x1^2, ccpp$x3^2, rep(1, length(y)))


# Constants
theta_x4 <- 0.227168908     # selected
thata_bias <- 251.671653131 # selected
theta_x1 <- -0.036521863    # constant
theta_x3 <- -0.003650664    # constant

rss5 <- 365625

epison <- rss5*1.1
num <- 10000

accepted_samples <- data.frame(param1 = numeric(), param2 = numeric())
temp <- 0


# Step 8: Define reasonable uniform priors around MLEs
r1 <- 0.2 * abs(theta_x4)         # B120% around MLE
r2 <- 0.2 * abs(thata_bias)

lower1 <- theta_x4 - r1
upper1 <- theta_x4 + r1

lower2 <- thata_bias - r2
upper2 <- thata_bias + r2

# Monte Carlo loop
for (i in 1:num) {
  range1 <-  runif(1, lower1, upper1)
  range2 <- runif(1, lower2, upper2)
  
  new_theta <- c(range1,  theta_x1, theta_x3,range2)
  new_y_hat <- X5 %*% new_theta
  new_RSS <- sum((y - new_y_hat)^2)
  
  if(new_RSS < epison){
    temp = temp+1
    accepted_samples[temp, ] <- c(range1, range2)
  }
}


# Create data frame for easier plotting
posterior_df <- accepted_samples
names(posterior_df) <- c("param1", "param2")



# Marginal of param1 (x4)
p1 <- ggplot(posterior_df, aes(x = param1)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "black", alpha = 0.6) +
  geom_density(color = "darkblue", size = 1) +
  labs(title = "Posterior of Parameter 1 (x4)", x = "Parameter 1", y = "Density") +
  theme_minimal()

# Marginal of param2 (intercept/bias)
p2 <- ggplot(posterior_df, aes(x = param2)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", color = "black", alpha = 0.6) +
  geom_density(color = "darkgreen", size = 1) +
  labs(title = "Posterior of Parameter 2 (bias)", x = "Parameter 2", y = "Density") +
  theme_minimal()

# Joint distribution
p3 <- ggplot(posterior_df, aes(x = param1, y = param2)) +
  geom_point(alpha = 0.4, color = "blue") +
  labs(title = "Joint Posterior of Parameters", x = "Parameter 1", y = "Parameter 2") +
  theme_minimal()

# Arrange plots
grid.arrange(p1, p2, p3, ncol = 2)


