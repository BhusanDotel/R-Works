ccpp=read.csv("D:/assignment/data/dataset.csv", header = TRUE)
attach(ccpp)

#add new column t as time signal as first column
library(dplyr)
ccpp=ccpp %>% mutate(t=row_number()) %>% select(t,everything())
names(ccpp)

library(dplyr)
library(tidyr)
library(ggplot2)
library(e1071)  # for skewness()

# 1. Reshape input signals to long format
input_long = ccpp %>%
  select(x1,x2, x3, x4, x5) %>%
  pivot_longer(cols = everything(), names_to = "Signal", values_to = "Value")

# 2. Compute skewness for each signal
skew_data = input_long %>%
  group_by(Signal) %>%
  summarise(skew = round(skewness(Value, na.rm = TRUE), 3)) %>%
  mutate(label = paste0(Signal, "\nskewness: ", skew))  # custom facet label

# 3. Create named vector for labeller
label_map = setNames(skew_data$label, skew_data$Signal)

# 4. Plot with custom facet labels
ggplot(input_long, aes(x = Value, fill = Signal)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.4, color = "black") +
  geom_density(alpha = 0.7, color = "black") +
  facet_wrap(~ Signal, scales = "free", labeller = labeller(Signal = label_map)) +
  labs(title = "Density Plot of Each Signal",
       x = "Value", y = "Density") +
  theme_minimal()





