getwd()

library(readr)
songs_normalize <- read_csv("songs_normalize.csv")
head(songs_normalize)
str(songs_normalize)

# TASK 1

#creating sample sizes of 1%, 5%, and 50% of data
set.seed(123) 
sample_df_1 <- songs_normalize[sample(nrow(songs_normalize), round(nrow(songs_normalize)) * 0.01), ]
sample_df_5 <- songs_normalize[sample(nrow(songs_normalize), round(nrow(songs_normalize)) * 0.05), ]
sample_df_50 <- songs_normalize[sample(nrow(songs_normalize), round(nrow(songs_normalize)) * 0.50), ]

# Determine the critical value for a 95% confidence level
confidence_level <- 0.95
alpha <- 1 - confidence_level
critical_value95 <- qnorm(1 - alpha / 2)

# Confidence Interval for mean of N for sample_df_1

#mean and sd
sample_liveness_mean <- mean(songs_normalize$liveness, na.rm = TRUE)
sample_liveness_sd <- sd(songs_normalize$liveness, na.rm = TRUE)

#sample size
n <- nrow(sample_df_1)

#standard error of mean
sem <- sample_liveness_sd / sqrt(n)

# Compute the margin of error
margin_of_error <- critical_value95 * sem

# Compute the confidence interval
lower_bound <- sample_liveness_mean - margin_of_error
upper_bound <- sample_liveness_mean + margin_of_error

# Print the results
cat("Sample Mean:", sample_liveness_mean, "\n")
cat("Sample Standard Deviation:", sample_liveness_sd, "\n")
cat("Sample Size (1%):", n, "\n")
cat("Standard Error of the Mean:", sem, "\n")
cat("Critical Value:", critical_value, "\n")
cat("Margin of Error:", margin_of_error, "\n")
cat("95% Confidence Interval: [", lower_bound, ",", upper_bound, "]\n")
  
# Confidence Interval for mean of N for sample_df_5

#mean and sd
sample_liveness_mean <- mean(songs_normalize$liveness, na.rm = TRUE)
sample_liveness_sd <- sd(songs_normalize$liveness, na.rm = TRUE)

#sample size
n <- nrow(sample_df_5)

#standard error of mean
sem <- sample_liveness_sd / sqrt(n)

# Compute the margin of error
margin_of_error <- critical_value95 * sem

# Compute the confidence interval
lower_bound <- sample_liveness_mean - margin_of_error
upper_bound <- sample_liveness_mean + margin_of_error

# Print the results
cat("Sample Mean:", sample_liveness_mean, "\n")
cat("Sample Standard Deviation:", sample_liveness_sd, "\n")
cat("Sample Size (5%):", n, "\n")
cat("Standard Error of the Mean:", sem, "\n")
cat("Critical Value:", critical_value, "\n")
cat("Margin of Error:", margin_of_error, "\n")
cat("95% Confidence Interval: [", lower_bound, ",", upper_bound, "]\n")

# Confidence Interval for mean of N for sample_df_50

#mean and sd
sample_liveness_mean <- mean(songs_normalize$liveness, na.rm = TRUE)
sample_liveness_sd <- sd(songs_normalize$liveness, na.rm = TRUE)

#sample size
n <- nrow(sample_df_50)

#standard error of mean
sem <- sample_liveness_sd / sqrt(n)

# Compute the margin of error
margin_of_error <- critical_value95 * sem

# Compute the confidence interval
lower_bound <- sample_liveness_mean - margin_of_error
upper_bound <- sample_liveness_mean + margin_of_error

# Print the results
cat("Sample Mean:", sample_liveness_mean, "\n")
cat("Sample Standard Deviation:", sample_liveness_sd, "\n")
cat("Sample Size (50%):", n, "\n")
cat("Standard Error of the Mean:", sem, "\n")
cat("Critical Value:", critical_value, "\n")
cat("Margin of Error:", margin_of_error, "\n")
cat("95% Confidence Interval: [", lower_bound, ",", upper_bound, "]\n")


# Confidence Interval for proportion of C for sample_df_1

# get sample size
n <- nrow(sample_df_1)

#
p_hat <- sum(songs_normalize$artist == "Christina Aguilera") / n

# Standard error
SE <- sqrt((p_hat * (1 - p_hat)) / n)

# Critical value for 95% confidence level
z <- 1.96

# Margin of error
MOE <- z * SE

# Confidence interval
lower_bound <- p_hat - MOE
upper_bound <- p_hat + MOE

# Print the results
cat("P-hat for songs by Christina Aguilera: ", p_hat, "\n")
cat("Standard Error:", SE, "\n")
cat("Margin of Error:", MOE, "\n")
cat("95% Confidence Interval: [", lower_bound, ",", upper_bound, "]\n")

# Confidence Interval for proportion of C for sample_df_5

# get sample size
n <- nrow(sample_df_5)

# Christina Aguilera p-hat
p_hat <- sum(songs_normalize$artist == "Christina Aguilera") / n

# Standard error
SE <- sqrt((p_hat * (1 - p_hat)) / n)

# Critical value for 95% confidence level
z <- 1.96

# Margin of error
MOE <- z * SE

# Confidence interval
lower_bound <- p_hat - MOE
upper_bound <- p_hat + MOE

# Print the results
cat("P-hat for songs by Christina Aguilera:", p_hat, "\n")
cat("Standard Error:", SE, "\n")
cat("Margin of Error:", MOE, "\n")
cat("95% Confidence Interval: [", lower_bound, ",", upper_bound, "]\n")

# Confidence Interval for proportion of C for sample_df_50

# get sample size
n <- nrow(sample_df_50)

# Christina Aguilera p-hat
p_hat <- sum(songs_normalize$artist == "Christina Aguilera") / n

# Standard error
SE <- sqrt((p_hat * (1 - p_hat)) / n)

# Critical value for 95% confidence level
z <- 1.96

# Margin of error
MOE <- z * SE

# Confidence interval
lower_bound <- p_hat - MOE
upper_bound <- p_hat + MOE

# Print the results
cat("P-hat for songs by Christina Aguilera:", p_hat, "\n")
cat("Standard Error:", SE, "\n")
cat("Margin of Error:", MOE, "\n")
cat("95% Confidence Interval: [", lower_bound, ",", upper_bound, "]\n")


# TASK 2

#5% sample size
n <- nrow(sample_df_5)

#mean, sd, MOE I chose - NUMERICAL (mean)
sample_liveness_mean <- mean(songs_normalize$liveness, na.rm = TRUE)
sample_liveness_sd <- sd(songs_normalize$liveness, na.rm = TRUE)
margin_of_error_mean_task2 <- 0.01

#determining the confidence level for the mean
z_score <- (margin_of_error_mean_task2 * sqrt(n)) / sample_liveness_sd
p_value <- pnorm(z_score)
confidence_level <- 2 * p_value - 1

#calculate the sample size needed to achieve MER with 90% confidence.
confidence_level <- 0.90
alpha <- 1 - confidence_level
critical_value90 <- qnorm(1 - alpha / 2)

sample_size_mean <- ((critical_value90 * sample_liveness_sd) / 0.01 )^2
cat("The sample size needed to achieve a MER of 0.01 for the mean with 90% confidence: ", sample_size_mean)

  
#p_hat, MOE I chose for CATEGORICAL (prop)
p_hat <- sum(songs_normalize$artist == "Christina Aguilera") / nrow(sample_df_5)
margin_of_error_proportion_task2 <- 0.04

#now, determining confidence level for the proportion
n <- nrow(sample_df_5)
p_hat <- sum(songs_normalize$artist == "Christina Aguilera") / n
z <- margin_of_error_proportion_task2 / sqrt((p_hat * (1 - p_hat)) / n)
p_value <- pnorm(z)
confidence_level <- 2 * p_value - 1

#calculate the sample size needed to achieve MER with 90% confidence.
sample_size_prop <- ((critical_value90)^2 * p_hat * (1-p_hat)) / (0.04^2)
cat("The sample size needed to achieve a MER of 0.04 for the proportion with 90% confidence: ", sample_size_prop)


# TASK 3: C1: EXPLICIT (FALSE) AND C2: GENRE (POP)

task_3 <- subset(songs_normalize, explicit == FALSE & genre == "pop")

total_count <- nrow(songs_normalize)
subset_count <- nrow(task_3)
frac <- subset_count / total_count

std_error <- sqrt(frac * (1 - frac) / total_count)
z_score <- qnorm(0.5 + 0.9 / 2)

margin_of_error <- z_score * std_error
confidence_interval <- c(frac - margin_of_error, frac + margin_of_error)

cat("Fraction:", frac, "\n")
cat("90% Confidence Interval:", confidence_interval, "\n")

# TASK 4: NUMERICAL VARIABLE (N): POPULARITY

#taking subsets for different ranges of popularity
subset_1 <- subset(songs_normalize, popularity >= 0 & popularity < 25)
subset_2 <- subset(songs_normalize, popularity >= 25 & popularity < 50)
subset_3 <- subset(songs_normalize, popularity >= 50 & popularity < 75)
subset_4 <- subset(songs_normalize, popularity >= 75 & popularity < 100)

#critical value for 90% confidence level
critical_value90

#SUBSET 1
n1 <- nrow(subset_1)

#getting mean and sd
mean1 <- mean(subset_1$popularity)
sd1 <- sd(subset_1$popularity)

# Compute the standard error of the mean
sem1 <- sd1 / sqrt(n1)

# Compute the margin of error
margin_of_error1 <- critical_value90 * sem1

# Compute the confidence interval
lower_bound1 <- mean1 - margin_of_error1
upper_bound1 <- mean1 + margin_of_error1


#SUBSET 2
n2 <- nrow(subset_2)

#getting mean and sd
mean2 <- mean(subset_2$popularity)
sd2 <- sd(subset_2$popularity)

# Compute the standard error of the mean
sem2 <- sd2 / sqrt(n2)

# Compute the margin of error
margin_of_error2 <- critical_value90 * sem2

# Compute the confidence interval
lower_bound2 <- mean2 - margin_of_error2
upper_bound2 <- mean2 + margin_of_error2


#SUBSET 3
n3 <- nrow(subset_3)

#getting mean and sd
mean3 <- mean(subset_3$popularity)
sd3 <- sd(subset_3$popularity)

# Compute the standard error of the mean
sem3 <- sd3 / sqrt(n3)

# Compute the margin of error
margin_of_error3 <- critical_value90 * sem3

# Compute the confidence interval
lower_bound3 <- mean3 - margin_of_error3
upper_bound3 <- mean3 + margin_of_error3


#SUBSET 4
n4 <- nrow(subset_4)

#getting mean and sd
mean4 <- mean(subset_4$popularity)
sd4 <- sd(subset_4$popularity)

# Compute the standard error of the mean
sem4 <- sd4 / sqrt(n4)

# Compute the margin of error
margin_of_error4 <- critical_value90 * sem4

# Compute the confidence interval
lower_bound4 <- mean4 - margin_of_error4
upper_bound4 <- mean4 + margin_of_error4


#Printing the narrowest confidence interval
cat("The narrowest confidence interval is: [", lower_bound4, ",", upper_bound4, "]")






