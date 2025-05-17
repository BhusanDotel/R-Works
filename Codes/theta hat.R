ccpp=read.csv("D:/assignment/data/dataset.csv", header = TRUE)

# Output vector
y <- ccpp$x2

a <-ccpp$x1
c <-ccpp$x3
d <-ccpp$x4
e <-ccpp$x5

# Design matrix
X1 <- cbind(d, c^2, rep(1, length(y)))
colnames(X1) <- c("x4", "x3_squared", "bias")

# Least Squares estimation
theta_hat1 <- solve(t(X1) %*% X1) %*% t(X1) %*% y
theta_hat1

#compute RSS
y_hat1 <- X1 %*% theta_hat1
rss1 <- sum((y - y_hat1)^2)
rss1


##########################################################


# Design matrix
X2 <- cbind(d, c^2, e, rep(1, length(y)))
colnames(X2) <- c("x4", "x3_squared", "x5", "bias")

# Least Squares estimation
theta_hat2 <- solve(t(X2) %*% X2) %*% t(X2) %*% y
theta_hat2

#compute RSS
y_hat2 <- X2 %*% theta_hat2
rss2 <- sum((y - y_hat2)^2)
rss2

##########################################################


# Design matrix
X3 <- cbind(c, d, e^3)
colnames(X3) <- c("x3", "x4", "x5_cubed")

# Least Squares estimation
theta_hat3 <- solve(t(X3) %*% X3) %*% t(X3) %*% y
theta_hat3

#compute RSS
y_hat3 <- X3 %*% theta_hat3
rss3 <- sum((y - y_hat3)^2)
rss3

##########################################################


# Design matrix
X4 <- cbind(d, c^2, e^3, rep(1, length(y)))
colnames(X4) <- c("x4", "x3_squared", "x5_cubed", "bias")

# Least Squares estimation
# due to "system is computationally singular" issue we use qr.solve()
#theta_hat4 <- solve(t(X4) %*% X4) %*% t(X4) %*% y
theta_hat4 <- qr.solve(X4,y)
theta_hat4

#compute RSS
y_hat4 <- X4 %*% theta_hat4
rss4 <- sum((y - y_hat4)^2)
rss4

##########################################################


# Design matrix
X5 <- cbind(d, a^2, c^2, rep(1, length(y)))
colnames(X5) <- c("x4", "x1_squared", "x3_squared", "bias")

# Least Squares estimation
theta_hat5 <- solve(t(X5) %*% X5) %*% t(X5) %*% y
theta_hat5

#compute RSS
y_hat5 <- X5 %*% theta_hat5
rss5 <- sum((y - y_hat5)^2)
rss5

##########################################################

names(theta_hat1) <- c("theta_x4", "theta_x3_squared", "theta_bias")
names(theta_hat2) <- c("theta_x4", "theta_x3_squared", "theta_x5", "theta_bias")
names(theta_hat3) <- c("theta_x3", "theta_x4", "theta_x5_cubed")
names(theta_hat4) <- c("theta_x4", "theta_x3_squared", "theta_x5_cubed", "theta_bias")
names(theta_hat5) <- c("theta_x4", "theta_x1_squared", "theta_x3_squared", "theta_bias")


all_names <- unique(c(
  names(theta_hat1),
  names(theta_hat2),
  names(theta_hat3),
  names(theta_hat4),
  names(theta_hat5)
))

pad_theta <- function(theta, all_names) {
  padded <- setNames(rep(NA, length(all_names)), all_names)
  padded[names(theta)] <- theta
  return(padded)
}

padded_theta_hat1 <- pad_theta(theta_hat1, all_names)
padded_theta_hat2 <- pad_theta(theta_hat2, all_names)
padded_theta_hat3 <- pad_theta(theta_hat3, all_names)
padded_theta_hat4 <- pad_theta(theta_hat4, all_names)
padded_theta_hat5 <- pad_theta(theta_hat5, all_names)

theta_table <- data.frame(
  "Model 1" = padded_theta_hat1,
  "Model 2" = padded_theta_hat2,
  "Model 3" = padded_theta_hat3,
  "Model 4" = padded_theta_hat4,
  "Model 5" = padded_theta_hat5,
  row.names = all_names
)


theta_table


###########################################################3


# Combine into a data frame
rss_table <- data.frame(
  Model = paste0("Model ", 1:5),
  RSS = c(rss1, rss2, rss3, rss4, rss5)
)

# Print the table
print(rss_table)


