ccpp=read.csv("D:/assignment/data/dataset.csv", header = TRUE)

library(ggplot2)



model1 <- lm(x2 ~ x4 + I(x3^2), data = ccpp)
model2 <- lm(x2 ~ x4 + I(x3^2) + x5, data = ccpp)
model3 <- lm(x2 ~ x3 + x4 + I(x5^3), data = ccpp)
model4 <- lm(x2 ~ x4 + I(x3^2) + I(x5^3), data = ccpp)
model5 <- lm(x2 ~ x4 + I(x1^2) + I(x3^2), data = ccpp)

# Set up 2x3 plot layout
par(mfrow = c(2, 3))  # 2 rows, 3 columns
par(mar = c(4, 4, 2, 1))  # Adjust margins

# Plot Q-Q plots
qqnorm(residuals(model1), main = "Model 1")
qqline(residuals(model1), col = "red")

qqnorm(residuals(model2), main = "Model 2")
qqline(residuals(model2), col = "green")

qqnorm(residuals(model3), main = "Model 3")
qqline(residuals(model3), col = "blue")

qqnorm(residuals(model4), main = "Model 4")
qqline(residuals(model4), col = "orange")

qqnorm(residuals(model5), main = "Model 5")
qqline(residuals(model5), col = "purple")


plot.new()

