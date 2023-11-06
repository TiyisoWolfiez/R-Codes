########################
# Assignment 5
# Name and surname: Tiyiso Hlungwani
# Student number: u********
########################

# Import the data
lego <- read.csv("lego_sample.csv")

# Question 1

Q1 <- cor(lego$Amazon_Price, lego$Pieces)

# Question 2

model <- lm(Amazon_Price ~ Pieces, data = lego)
Q2a <- coef(model)
Q2b <- predict(model, newdata = data.frame(Pieces = 350))
Q2c <- summary(model)$r.squared * 100

# Question 3

plot(lego$Pieces, lego$Amazon_Price, main = "Scatterplot of Amazon Price vs. Number of Pieces",
     xlab = "Number of Pieces", ylab = "Amazon Price", pch = 16, col = "blue")

abline(model, col = "red")
legend("topright", legend = c("Regression Line"), col = c("red"), lty = 1, cex = 0.8)

# Question 4

Q4 <- resid(model)

# Question 5

# Fit the linear regression model
model <- lm(Amazon_Price ~ Pieces, data = lego)

# Extract the coefficient for the 'Pieces' variable
beta_pieces <- coef(model)["Pieces"]

# Calculate the standard error of the coefficient
se_beta_pieces <- summary(model)$coefficients["Pieces", "Std. Error"]

# Define the number of additional pieces (1 in this case)
additional_pieces <- 1

# Calculate the t-statistic
t_statistic <- (beta_pieces) / se_beta_pieces

# Calculate the p-value
p_value <- 2 * pt(-abs(t_statistic), df = length(lego$Pieces) - 2)

# Save the point estimate
Q5a <- beta_pieces

# Save the test statistic value
Q5b <- t_statistic




# Question 6

# Filter the dataset for "Lego Friends" theme
lego_friends <- subset(lego, Theme == "Friends")

# Fit a linear regression model for Lego Friends sets
model_friends <- lm(Amazon_Price ~ Pieces, data = lego_friends)

# Extract the slope parameter for Lego Friends sets
Q6a <- coef(model_friends)["Pieces"]

# Filter the dataset for "Lego City" theme
lego_city <- subset(lego, Theme == "City")

# Fit a linear regression model for Lego City sets
model_city <- lm(Amazon_Price ~ Pieces, data = lego_city)

# Extract the slope parameter for Lego City sets
Q6b <- coef(model_city)["Pieces"]



# Question 7

# Fit a linear regression model using the theme to explain the Amazon price
model_theme <- lm(Amazon_Price ~ Theme, data = lego)

# Save the parameter estimates
Q7a <- coef(model_theme)

# Perform an F-test to test the significance of the regression model
f_test <- summary(model_theme)$fstatistic

# Extract the p-value from the F-test
p_value <- pf(f_test[1], f_test[2], f_test[3], lower.tail = FALSE)

# Save the p-value
Q7b <- p_value


#View(lego$Theme)
