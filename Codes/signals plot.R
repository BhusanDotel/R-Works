ccpp=read.csv("D:/assignment/data/dataset.csv", header = TRUE)
attach(ccpp)


library(dplyr)
library(ggplot2)
library(tidyr)

#adding time "t" column as first column
ccpp = ccpp %>% mutate(t = row_number()) %>%  select(t, everything())

# Reshape to long format for plotting multiple input x1
inputs_x1 = ccpp %>%
  select(t, x1) %>%
  pivot_longer(cols = c(x1), names_to = "Signal", values_to = "Value")

# Time series plot for input signals
ggplot(inputs_x1, aes(x = t, y = Value)) +
  geom_line(color = "pink") +
  labs(title = "Input Signals over Time (x1)", x = "Time", y = "Signal Value") +
  theme_minimal()


inputs_x3 = ccpp %>%
  select(t, x3) %>%
  pivot_longer(cols = c(x3), names_to = "Signal", values_to = "Value")


ggplot(inputs_x3, aes(x = t, y = Value)) +
  geom_line(color = "red") +
  labs(title = "Input Signals over Time (x3)", x = "Time", y = "Signal Value") +
  theme_minimal()

inputs_x4 = ccpp %>%
  select(t, x4) %>%
  pivot_longer(cols = c(x4), names_to = "Signal", values_to = "Value")


ggplot(inputs_x4, aes(x = t, y = Value)) +
  geom_line(color = "orange") +
  labs(title = "Input Signals over Time (x4)", x = "Time", y = "Signal Value") +
  theme_minimal()

inputs_x5 = ccpp %>%
  select(t, x5) %>%
  pivot_longer(cols = c(x5), names_to = "Signal", values_to = "Value")


ggplot(inputs_x5, aes(x = t, y = Value)) +
  geom_line(color = "black") +
  labs(title = "Input Signals over Time (x5)", x = "Time", y = "Signal Value") +
  theme_minimal()


output_x2 = ccpp %>%
  select(t, x2) %>%
  pivot_longer(cols = c(x2), names_to = "Signal", values_to = "Value")


ggplot(output_x2, aes(x = t, y = Value)) +
  geom_line(color = "green") +
  labs(title = "Output Signals over Time (x2)", x = "Time", y = "Signal Value") +
  theme_minimal()



#overall all signals in single plot
# Reshape to long format for plotting multiple inputs
inputs_long <- ccpp %>%
  select(t, x1, x2, x3,x4,x5) %>%
  pivot_longer(cols = c(x1, x2, x3,x4,x5), names_to = "Signal", values_to = "Value")

# Time series plot for input signals
ggplot(inputs_long, aes(x = t, y = Value, color = Signal)) +
  geom_line() +
  labs(title = "All Signals over Time", x = "Time", y = "Signal Value") +
  theme_minimal()

