########################
# Assignment 2
# Name and surname: Tiyiso Hlungwani
# Student number: u*********
########################

# Import packages
library(openintro)
# Do not import any other packages!

# Question 1
X <- rep(1/6, 6)

# Question 2
Q2 <- sum(1:6) * (1/6)

# Question 3
Q3a <- 1/6
Q3b <- 0.5

# Question 4 


# Question 5
# Consider the code below
# Change the code where necessary, so that it performs the simulation correctly
set.seed(12)

dice_sum <- 0
Q6 <- 0

while (dice_sum < 36) {
  die <- sample(1:6, size = 1, replace = TRUE)
  dice_sum <- dice_sum + die
  Q6 <- Q6 + 1
}

Q5 <- Q6

# Question 6
# Consider the code below
# Change the code where necessary, so that it performs the simulation correctly
set.seed(17)
nsim <- 100

Q6 <- vector("integer", nsim)
for(i in 1:nsim){
  dice_sum <- 0
  step <- 0
  
  while (dice_sum < 36) {
    die <- sample(1:6, size = 1, replace = TRUE)
    dice_sum <- dice_sum + die
    step <- step + 1
  }
  
  Q6[i] <- step
}

Q6a <- mean(Q6)
Q6b <- sd(Q6)
barplot(table(Q6))


# Question 7
# Consider the code below
# Change the code where necessary, so that it performs the simulation correctly

set.seed(20)

dice_sum <- 0
Q7 <- 0

while (dice_sum < 36) {
  die <- sample(1:6, size = 1, replace = TRUE)
  dice_sum <- dice_sum + die
  Q7 <- Q7 + 1
    
  dice_sum <- if(dice_sum == 5) 18 else dice_sum # Ladder: If the token lands on 5, it immediately moves to 18
  dice_sum <- if(dice_sum == 9) 3 else dice_sum
  dice_sum <- if(dice_sum == 10) 15 else dice_sum
  dice_sum <- if(dice_sum == 14) 2 else dice_sum
  dice_sum <- if(dice_sum == 19) 32 else dice_sum
  dice_sum <- if(dice_sum == 28) 15 else dice_sum
  dice_sum <- if(dice_sum == 35) 13 else dice_sum
  dice_sum <- if(dice_sum == 16) 7 else dice_sum # Snake: If the token lands on 16, it immediately moves to 7
}


# Question 8
set.seed(25)

ntimes <- 100
Q8 <- vector("integer", ntimes)
for(k in 1:ntimes){
  sum <- 0
  rolls <- 0

while (sum < 36) {
  die <- sample(1:6, size = 1, replace = TRUE)
  sum <- sum + die
  #Q7 <- Q7 + 1
  
  sum <- if(sum == 5) 18 else sum # Ladder: If the token lands on 5, it immediately moves to 18
  sum <- if(sum == 9) 3 else sum
  sum <- if(sum == 10) 15 else sum
  sum <- if(sum == 14) 2 else sum
  sum <- if(sum == 19) 32 else sum
  sum <- if(sum == 28) 15 else sum
  sum <- if(sum == 35) 13 else sum
  sum <- if(sum == 16) 7 else sum # Snake: If the token lands on 16, it immediately moves to 7
  sum <- if(sum == 27) 33 else sum
  rolls <- rolls + 1
  }
  Q8[k] <- rolls
}


Q8a <- mean(Q8)
Q8b <- sd(Q8)
barplot(table(Q8))



