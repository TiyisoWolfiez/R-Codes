#####################
# Assignment 3
# Name & Surname: Tiyiso Hlungwani
# Student number: u********
#####################

# Import the data
racing <- read.csv("RacingGameData.csv")

# Question 1
speed <- racing$speed <- ifelse(racing$FinishTime < 25, "fast", "not fast")

# Question 2
fast_races <- sum(racing$FinishTime < 25)
total_races <- nrow(racing)
point_estimate <- fast_races / total_races

Q2a <- point_estimate

standard_error <- sqrt(0.5 * (1 - 0.5) / total_races)

# Q2b
Q2b <- standard_error

hypothesized_proportion <- 0.5
test_statistic <- (Q2a - hypothesized_proportion) / Q2b

# Q2c
Q2c <- test_statistic

# Question 3
# Convert the `speed` variable such that the entries are 1s and 0s.
speed_ind <- ifelse(speed == "fast", 1, 0)

Gauss_1 <- subset(speed_ind, racing$Engine == "Gauss")
Nightingale_1 <- subset(speed_ind, racing$Engine == "Nightingale")

set.seed(30)

n <- 1000
bootstrap_samples <- n

for(k in 1:1000){
  Night_bootstrap_sample <- sample(Nightingale_1, replace = TRUE)
  Gauss_bootstrap_sample <- sample(Gauss_1, replace = TRUE)
  
  p_hat_Night <- mean(Night_bootstrap_sample)
  p_hat_Gauss <- mean(Gauss_bootstrap_sample)
  
  bootstrap_samples[k] <- p_hat_Night - p_hat_Gauss
}

Q3a <- bootstrap_samples

confidence_interval <- quantile(Q3a, c(0.05, 0.95))
upper_bound <- confidence_interval[2]
Q3b <- upper_bound


# Question 4

p1 <- mean(speed_ind[racing$Track == "StraightTrack"])
n_1 <- sum(racing$Track == "StraightTrack")
p2 <- mean(speed_ind[racing$Track == "OvalTrack"])
n_2 <- sum(racing$Track == "OvalTrack")

Q4a <- sqrt((p1 * (1 - p1) / n_1) + (p2 * (1-p2) / n_2))



# Question 5
Q5a <- table(racing$Engine, racing$speed)
Q5b <- table(racing$Track, racing$speed)

# Question 6
Bayes_N <- subset(racing, Engine %in% c("Bayes","Nightingale"))

set.seed(35)
num_samples <- 1000

boostrap_sample_Q6 <- num_samples

for(j in 1: num_samples){
  r_speed <- sample(speed_ind)
  
  p_hat_Bayes <- mean(r_speed[Bayes_N$Engine == "Bayes"])
  p_hat_N <- mean(r_speed[Bayes_N$Engine == "Nightingale"])
  
  boostrap_sample_Q6[j] <- p_hat_Bayes - p_hat_N
  
}
Q6a <- mean(boostrap_sample_Q6)

observed_difference <- mean(speed_ind[Bayes_N$Engine == "Bayes"]) - mean(speed_ind[Bayes_N$Engine == "Nightingale"])

# Calculate p-value
p_value <- mean(boostrap_sample_Q6 >= observed_difference)

# Save the p-value into a variable called Q6b
Q6b <- p_value


# Question 7
chi_square <- chisq.test(Q5a)

Q7a <- chi_square$statistic
Q7b <- chi_square$p.value




