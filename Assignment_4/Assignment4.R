#####################
# Assignment 4
# Name & Surname: Tiyiso Hlungwani
# Student number: u********
#####################

# Import the data
coffee <- read.csv("CoffeeTruck.csv")

# Import the necessary packages
library(nortest)
# Do not import any other packages

# Question 1

# Perform Anderson-Darling test
result <- ad.test(coffee$Profit)

# Save the p-value into a variable called Q1
Q1 <- result$p.value



# Question 2

# Set seed value
set.seed(15)

# Define the number of iterations
num_iterations <- 1000

# Initialize an empty vector to store the sample means
sample_means <- numeric(num_iterations)

# Perform the simulation
for (i in 1:num_iterations) {
  # Take a random sample from the Profit variable
  bootstrap_sample <- sample(coffee$Profit, replace = TRUE)
  
  # Calculate the mean of the random sample
  sample_means[i] <- mean(bootstrap_sample)
}

# Save the response into a variable called Q2
Q2 <- sample_means



# Question 3

# Calculate the mean and standard error of the sampling distribution
sampling_mean <- mean(Q2)
sampling_sd <- sd(Q2)
n <- length(Q2)

# Calculate the standard error of the mean
sem <- sampling_sd / sqrt(n)

# Calculate z-scores for 326 and 347
z_326 <- (326 - sampling_mean) / sem
z_347 <- (347 - sampling_mean) / sem

# Calculate the cumulative probabilities
p_326 <- pnorm(z_326)
p_347 <- pnorm(z_347)

# Calculate the theoretical probability
Q3a <- p_347 - p_326


# Calculate the cumulative probabilities
p_326 <- pnorm(z_326)
p_347 <- pnorm(z_347)

# Calculate the theoretical probability
Q3a <- p_347 - p_326


# Count the number of sample means between 326 and 347
num_means_between_326_and_347 <- sum(Q2 >= 326 & Q2 <= 347)

# Calculate the empirical probability
Q3b <- num_means_between_326_and_347 / length(Q2)



# Question 4

# Perform t-test and calculate confidence interval
result <- t.test(coffee$Profit, conf.level = 0.90)

# Save the upper limit of the confidence interval into Q4
Q4 <- result$conf.int[2]



# Question 5
# Calculate the overall average profit
#overall_avg_profit <- mean(coffee$Profit)

# Assuming 'City Hall' is one of the locations in your 'Location' variable
#city_hall_data <- subset(coffee, Location == "City Hall")

# Perform t-test
#result <- t.test(city_hall_data$Profit, mu = overall_avg_profit, alternative = "two.sided")

# Save the p-value into Q5
#Q5 <- result$p.value



# Question 6

# Perform one-way ANOVA
result_anova <- aov(Profit ~ Location, data = coffee)

# Calculate mean square error (MSE)
mse <- deviance(result_anova) / result_anova$df.residual

# Save the MSE into Q6a
Q6a <- mse


# Calculate the F-statistic
#f_statistic <- summary(result_anova)[[1]]$Pr[1]

# Save the test statistic value into Q6b
#Q6b <- f_statistic




# Question 7
# Assuming 'Alternative' and 'HipHop' are music genres in your dataset
alternative_data <- subset(coffee, Music == "Alternative")
hiphop_data <- subset(coffee, Music == "HipHop")

# Calculate sample sizes and variances
n1 <- length(alternative_data$Profit)
n2 <- length(hiphop_data$Profit)
s1_sq <- var(alternative_data$Profit)
s2_sq <- var(hiphop_data$Profit)

# Calculate the pooled sample variance
sp_sq <- ((n1 - 1) * s1_sq + (n2 - 1) * s2_sq) / (n1 + n2 - 2)

# Calculate the standard error
SE <- sqrt(sp_sq / n1 + sp_sq / n2)

# Save the standard error into Q7a
Q7a <- SE


# Calculate sample means
x1_bar <- mean(alternative_data$Profit)
x2_bar <- mean(hiphop_data$Profit)

# Calculate the test statistic
t_statistic <- (x1_bar - x2_bar) / SE

# Save the test statistic value into Q7b
Q7b <- t_statistic


# Calculate the degrees of freedom
df <- n1 + n2 - 2

# Calculate the p-value
p_value <- 2 * (1 - pt(abs(t_statistic), df))

# Save the p-value into Q7c
Q7c <- p_value
