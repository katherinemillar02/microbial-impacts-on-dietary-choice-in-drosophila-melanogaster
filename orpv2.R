library(ggplot2)
library(tibble)
library(patchwork)

# Male fly measurement 1 and 2
d1_2 <- tibble(
  weight1 = c(0.937, 0.733, 0.739, 0.782, 0.861),
  weight2 = c(0.920, 0.717, 0.716, 0.773, 0.852)
)

# Linear model and R-squared calculation
model1 <- lm(weight1 ~ weight2, data = d1_2)
r_squared1 <- summary(model1)$r.squared
r_squared_text1 <- paste("R-squared:", round(r_squared1, 3))

# First plot
m1_2 <- ggplot(d1_2, aes(x = weight1, y = weight2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ylim(0.6, 1) +
  xlim(0.6, 1) +
  theme_minimal() +
  annotate("text", x = 0.7, y = 0.9, label = r_squared_text1, size = 5, color = "blue")

# Male fly measurement 2 and 3
d2_3 <- tibble(
  weight2 = c(0.920, 0.717, 0.716, 0.773, 0.852),
  weight3 = c(0.833, 0.699, 0.718, 0.762, 0.712)
)

# Linear model and R-squared calculation
model2 <- lm(weight2 ~ weight3, data = d2_3)
r_squared2 <- summary(model2)$r.squared
r_squared_text2 <- paste("R-squared:", round(r_squared2, 3))

# Second plot
m2_3 <- ggplot(d2_3, aes(x = weight2, y = weight3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ylim(0.6, 1) +
  xlim(0.6, 1) +
  theme_minimal() +
  annotate("text", x = 0.7, y = 0.9, label = r_squared_text2, size = 5, color = "blue")

# Male fly measurement 3 and 1
d3_1 <- tibble(
  weight3 = c(0.833, 0.699, 0.718, 0.762, 0.712),
  weight1 = c(0.937, 0.733, 0.739, 0.782, 0.861)
)

# Linear model and R-squared calculation
model3 <- lm(weight3 ~ weight1, data = d3_1)
r_squared3 <- summary(model3)$r.squared
r_squared_text3 <- paste("R-squared:", round(r_squared3, 3))

# Third plot
m3_1 <- ggplot(d3_1, aes(x = weight3, y = weight1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ylim(0.6, 1) +
  xlim(0.6, 1) +
  theme_minimal() +
  annotate("text", x = 0.7, y = 0.9, label = r_squared_text3, size = 5, color = "blue")

# Combine the male plots using patchwork
male_plot <- m1_2 + m2_3 + m3_1

male_plot <- male_plot + ggtitle("Male")

# Female fly measurement 1 and 2
df1_2 <- tibble(
  weight1 = c(1.496, 1.286, 1.391, 1.454),
  weight2 = c(1.457, 1.325, 1.416, 1.388)
)

# Linear model and R-squared calculation
model_f1_2 <- lm(weight1 ~ weight2, data = df1_2)
r_squared_f1_2 <- summary(model_f1_2)$r.squared
r_squared_text_f1_2 <- paste("R-squared:", round(r_squared_f1_2, 3))

# First plot
f1_2 <- ggplot(df1_2, aes(x = weight1, y = weight2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ylim(1, 1.6) +
  xlim(1, 1.6) +
  theme_minimal() +
  annotate("text", x = 1.2, y = 1.5, label = r_squared_text_f1_2, size = 5, color = "blue")

# Female fly measurement 2 and 3
df2_3 <- tibble(
  weight2 = c(1.457, 1.325, 1.416, 1.388),
  weight3 = c(1.477, 1.398, 1.369, 1.387)
)

# Linear model and R-squared calculation
model_f2_3 <- lm(weight2 ~ weight3, data = df2_3)
r_squared_f2_3 <- summary(model_f2_3)$r.squared
r_squared_text_f2_3 <- paste("R-squared:", round(r_squared_f2_3, 3))

# Second plot
f2_3 <- ggplot(df2_3, aes(x = weight2, y = weight3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ylim(1, 1.6) +
  xlim(1, 1.6) +
  theme_minimal() +
  annotate("text", x = 1.2, y = 1.5, label = r_squared_text_f2_3, size = 5, color = "blue")

# Female fly measurement 3 and 1
df3_1 <- tibble(
  weight3 = c(1.477, 1.398, 1.369, 1.387),
  weight1 = c(1.496, 1.286, 1.391, 1.454)
)

# Linear model and R-squared calculation
model_f3_1 <- lm(weight3 ~ weight1, data = df3_1)
r_squared_f3_1 <- summary(model_f3_1)$r.squared
r_squared_text_f3_1 <- paste("R-squared:", round(r_squared_f3_1, 3))

# Third plot
f3_1 <- ggplot(df3_1, aes(x = weight3, y = weight1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ylim(1, 1.6) +
  xlim(1, 1.6) +
  theme_minimal() +
  annotate("text", x = 1.2, y = 1.5, label = r_squared_text_f3_1, size = 5, color = "blue")

# Combine the female plots using patchwork
female_plot <- f1_2 + f2_3 + f3_1

female_plot <- female_plot + ggtitle("Female")


# Combine male and female plots using patchwork
combined_plot <- female_plot / male_plot

# Display the combined plot
print(combined_plot)
