ccpp=read.csv("D:/assignment/data/dataset.csv", header = TRUE)
attach(ccpp)



library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

#add new column t as time signal as first column
ccpp=ccpp %>% mutate(t=row_number()) %>% select(t,everything())




#For Plotting heatmap for correlation matrix between signals
#
#
# Select input and output signals
selected_vars <- ccpp %>% select(x1, x2, x3, x4, x5)

# Compute correlation matrix
corr_matrix <- round(cor(selected_vars, use = "complete.obs"), 2)

# Convert to long format for heatmap
corr_melt <- melt(corr_matrix)

# Plot heatmap
ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Matrix All Signals", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))





#For Plotting heatmap for correlation matrix between input signal and output signal only
#
#
# Select signals
selected_vars <- ccpp %>% select(x1, x2, x3, x4, x5)

# Compute correlation matrix
corr_matrix <- round(cor(selected_vars, use = "complete.obs"), 2)

# Melt the matrix
corr_melt <- melt(corr_matrix)

# Filter only input vs output (x2)
input_signals <- c("x1", "x3", "x4", "x5")
corr_filtered <- corr_melt %>%
  filter(Var2 == "x2" & Var1 %in% input_signals)

# Plot heatmap: x2 on x-axis, inputs on y-axis
ggplot(corr_filtered, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), name = "Correlation") +
  labs(title = "Correlation: Inputs (y-axis) vs Output x2 (x-axis)", x = "Output Signal (x2)", y = "Input Signals") +
  theme_minimal()






#For Plotting  scatter plots (between different combination of input and output signals) to examine their dependencies
#
#
# Reshape the data to long format
scatter_data <- ccpp %>%
  select(x1, x2, x3, x4, x5) %>%
  pivot_longer(cols = c(x1, x3, x4, x5), names_to = "Input", values_to = "InputValue")

# Plot scatter plots with different colors for each input signal
ggplot(scatter_data, aes(x = InputValue, y = x2, color = Input)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Input, scales = "free_x") +
  labs(
    title = "Scatter Plots: Input Signals(x1,x3,x4,x5) vs Output (x2)",
    x = "Input Signal Value",
    y = "Output Signal (x2)"
  ) +
  scale_color_manual(values = c("x1" = "red", "x3" = "blue", "x4" = "green", "x5" = "purple")) +
  theme_minimal()
