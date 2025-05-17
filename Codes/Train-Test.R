ccpp=read.csv("D:/assignment/data/dataset.csv", header = TRUE)

# Load necessary packages
library(ggplot2)

# Step 1: Split the data into training (70%) and testing (30%)
set.seed(123)  # for reproducibility
sample_size <- floor(0.7 * nrow(ccpp))
train_indices <- sample(seq_len(nrow(ccpp)), size = sample_size)

train_data <- ccpp[train_indices, ]
test_data <- ccpp[-train_indices, ]

# Step 2: Fit Model 5 using training data
# Model 5: x2 ~ x4 + I(x1^2) + I(x3^2)
model5 <- lm(x2 ~ x4 + I(x1^2) + I(x3^2), data = train_data)

# Step 3: Predict on testing data (with 95% confidence intervals)
pred <- predict(model5, newdata = test_data, interval = "confidence", level = 0.95)

pred_df <- data.frame(
  actual = test_data$x2,
  predicted = pred[, "fit"],
  lower = pred[, "lwr"],
  upper = pred[, "upr"]
)


# Step 4: Plot predictions vs actual values with confidence intervals
ggplot(pred_df, aes(x = predicted, y = actual)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 2, color = "gray") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Model 5 Predictions vs Actual",
       x = "Predicted x2",
       y = "Actual x2") +
  theme_minimal()


#actual and predicted values for y
y_actual=pred_df$actual
y_predicted=pred_df$predicted

#calculating correlation between values
ys_cor <- cor(y_actual, y_predicted)
ys_cor


# Mean Absolute Percentage Error (MAPE) calculation
mape <- mean(abs((y_actual - y_predicted) / y_actual)) * 100
mape
