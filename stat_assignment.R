#Question 1

plate_colors <- c("r", "r", "o", "o", "g", "r", "o", "o", "o", "r", "r", "b", "g", "r", "g", "g", "b", "g", "r", "r", "b", "r", "o", "g", "r", "r", "o", "o", "b", "r", "g", "r", "g", "b", "r", "b", "r", "b", "r", "g")


library(ggplot2)
library(dplyr)

data <- data.frame(PlateColor = plate_colors)
#calculate frequencies and relative frequencies in plates
frequency_table <- data %>%
  group_by(PlateColor) %>%
  summarize(Frequency = n(), RelativeFrequency = n() / nrow(data))

#calculate the mode
mode_value <- frequency_table %>%
  filter(Frequency == max(Frequency)) %>%
  select(PlateColor) %>%
  pull()

# Print the frequency table, mode, and create the bar chart
print(frequency_table)
print(paste("Mode: ", mode_value))

ggplot(data, aes(x = PlateColor)) +
  geom_bar() +
  labs(title = "Plate Color Distribution",
       x = "Plate Color",
       y = "Frequency")


#question 2
#part 1,2
evaluation <- c("b", "g", "n", "n", "b", "n", "g", "b", "g", "b", "b", "b", "rg", "n", "b", "n", "b", "n", "g", "n", "n", "b", "b", "b", "b", "g", "n", "b", "n", "b", "b", "b", "g", "b", "b", "b", "n", "b", "n", "g")

data_evaluation <- data.frame(evaluation = evaluation)

frequency_table <- data_evaluation %>%
  group_by(evaluation) %>%
  summarize(Frequency = n(), RelativeFrequency = n() / nrow(data_evaluation))

mode_value_evaluation <- frequency_table %>%
  filter(Frequency == max(Frequency)) %>%
  select(evaluation) %>%
  pull()

print(frequency_table)
print(paste("Mode: ", mode_value_evaluation))

ggplot(data_evaluation, aes(x = evaluation)) +
  geom_bar() +
  labs(title = "Evaluation Distribution",
       x = "Evaluation",
       y = "Frequency")


#part 3
contingency_table <- table(plate_colors, evaluation)
print(contingency_table)

# You can also create a bar chart to visualize the relationship
barplot(contingency_table, beside = TRUE, legend = TRUE, 
        main = "Plate Color vs. Evaluation",
        xlab = "Plate Color",
        ylab = "Frequency")


#question3

mass_fries <- c(109.12, 94.78, 118.70, 119.92, 76.82, 110.26, 121.61, 157.26, 117.18, 69.99, 131.10, 123.65, 79.50, 117.26, 118.27, 93.98, 151.28, 112.90, 117.72, 120.82, 107.42, 80.51, 101.66, 114.54, 127.24, 119.97, 130.40, 117.23, 133.83, 112.55, 87.69, 105.10, 97.67, 98.60, 122.07, 98.61, 116.57, 121.90, 104.91, 112.45)

#part 2
# Mean
mean_mass <- mean(mass_fries)

# Median
median_mass <- median(mass_fries)

cat("Mean:", mean_mass, "grams\n")
cat("Median:", median_mass, "grams\n")


#part 3
n <- length(mass_fries)
k <- 10
class_width <- (max(mass_fries) - min(mass_fries)) / k

# histogram
histogram <- hist(mass_fries, breaks = seq(min(mass_fries), max(mass_fries) + class_width, by = class_width),
                  main = "Histogram of Mass of Fries",
                  xlab = "Mass (grams)",
                  ylab = "Frequency",
                  col = "lightblue")

# cumulative relative frequency polygon
cumulative_freq <- cumsum(histogram$counts) / n
plot(histogram$mids, cumulative_freq, type = "b", pch = 16, col = "red", xlab = "Mass (grams)", ylab = "Cumulative Relative Frequency", main = "Cumulative Relative Frequency Polygon")


#part 4
percentile_95 <- quantile(mass_fries, 0.95)
cat("95th Percentile:", percentile_95, "grams\n")


#part 5
below_120 <- sum(mass_fries < 120)
percentage_below_120 <- (below_120 / length(mass_fries)) * 100
cat("Percentage of values below 120g:", percentage_below_120, "%\n")


#question 4
# data
population_std_dev <- 10  
alpha <- 0.05 

# Sample data
mass_salad <- c(70.79, 73.01, 63.83, 65.96, 82.66, 56.49, 67.03, 44.17, 61.03, 81.95, 56.55, 62.59, 73.81, 63.69, 61.49, 70.33, 48.13, 58.96, 57.66, 74.85, 71.12, 83.92, 74.38, 68.39, 55.68, 61.98, 67.00, 69.40, 53.68, 74.17, 81.69, 68.52, 74.77, 71.13, 68.82, 68.65, 68.55, 56.19, 74.41, 68.86)

# Sample size
n <- length(mass_salad)

#part 1
standard_error <- population_std_dev / sqrt(n)

# margin of error
critical_value <- qnorm(1 - alpha / 2)  # Z-value for a 95% confidence interval
margin_of_error <- critical_value * standard_error

sample_mean <- mean(mass_salad)
confidence_interval <- c(sample_mean - margin_of_error, sample_mean + margin_of_error)


cat("Sample Mean:", sample_mean, "g\n")
cat("standard error:",standard_error, "g\n")
cat("95% Confidence Interval:", confidence_interval[1], "to", confidence_interval[2], "g\n")


# part 2
H0 <- 70  # Null hypothesis
H1 <- 70  # Alternative hypothesis

#one-sample t-test
t_test_result <- t.test(mass_salad, mu = H0, alternative = "less")


cat("t-Statistic:", t_test_result$statistic, "\n")
cat("p-value:", t_test_result$p.value, "\n")

# compare p-value with alpha
if (t_test_result$p.value < alpha) {
  cat("Reject the null hypothesis: Population mean is less than 70g\n")
} else {
  cat("Fail to reject the null hypothesis: No strong evidence that the population mean is less than 70g\n")
}


#question5

# part 1
boxplot(mass_fries, main = "Boxplot of Fries Portion", ylab = "Fries Mass (g)")

# part 2
boxplot(mass_salad, main = "Boxplot of Lettuce Portion", ylab = "Lettuce Mass (g)")


#part 3
# correlation coefficient (Pearson)
correlation_coeff <- cor(mass_fries, mass_salad)


cat("Correlation Coefficient:", correlation_coeff, "\n")


#visualize the relationship using a scatter plot:
plot(mass_fries, mass_salad, xlab = "Mass of Fries (grams)", ylab = "Mass of Salad (grams)", main = "Scatter Plot")


