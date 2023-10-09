#####################
# Assignment 1
# Name & Surname: Tiyiso Hlungwani
# Student number: u********
#####################

# Import the data
titles <- read.csv("titles.csv")

# Question 1
Q1 <- titles$title[which.min(titles$imdb_score)]

#titles$title selects the title of the movies which has
#the lowest score when we use which.min(titles$imdb_score)
#it finds the index of the row where the imdb_score is the lowest

# Question 2
Q2 <- subset(titles, type == "MOVIE")
#Create a subset containing only the movies. Save response into variable Q2
#With Subsets you choose which Data Frame to choose from, and the choose the 
#column name you want to extra your specific data and the select what kind of
#information to be displayed.

# Question 3

Q3 <- Q2$title[which.max(Q2$tmdb_popularity)]


# Question 4
Q4 <- names(table(titles$age_certification[titles$type == "MOVIE"]))[which.max(table(titles$age_certification[titles$type == "MOVIE"]))]

# Question 5
# Create a histogram with breaks = 5
hist(titles$imdb_score,
     breaks = 5,
     main = "Histogram of IMDB Ratings (Breaks = 5)",
     xlab = "IMDB Ratings",
     border = "midnightblue", 
     col = "hotpink")

# Create a histogram with breaks = 10
hist(titles$imdb_score,
     breaks = 10,
     main = "Histogram of IMDB Ratings (Breaks = 10)",
     xlab = "IMDB Ratings",
     border = "midnightblue", 
     col = "hotpink")

# Create a histogram with breaks = 50
hist(titles$imdb_score,
     breaks = 50,
     main = "Histogram of IMDB Ratings (Breaks = 50)",
     xlab = "IMDB Ratings",
     border = "midnightblue", 
     col = "hotpink")

# Create a histogram with breaks = 100
hist(titles$imdb_score,
     breaks = 100,
     main = "Histogram of IMDB Ratings (Breaks = 100)",
     xlab = "IMDB Ratings",
     border = "midnightblue", 
     col = "hotpink")


# Question 6
Q6 <- mean(titles$runtime)

# Question 7
Q7 <- sd(titles$runtime)

# Question 8
#lower_bound <- Q6 - 1.4 * Q7
#upper_bound <- Q6 + 1.4 * Q7

Q8 <- mean(titles$runtime >= Q6 - 1.4 * Q7 & titles$runtime <= Q6 + 1.4 * Q7) * 100
#Q15 <- mean(titles$runtime >= lower_bound & titles$runtime <= upper_bound) * 100

# Question 9
Q9 <- table(titles$age_certification, titles$type)


# Question 10

boxplot(imdb_score ~ type, data = titles,
        horizontal = TRUE,
        main = "Comparison of IMDB Ratings for Movies and TV Shows",
        xlab = "Type of Release",
        ylab = "IMDB Ratings",
        col = c("hotpink", "midnightblue"),
        legend = TRUE)

# Adding a legend
legend("topleft", legend = c("Movie", "TV Show"),
       col = c("hotpink", "midnightblue"), pch = 15, pt.cex = 1.5)


# Question 11
barplot(table(titles$age_certification),
        main = "Bar Chart of Age Certifications",
        xlab = "Age Certification",
        ylab = "Count",
        col = "hotpink")

#table(titles$age_certification) creates a frequency table of the 
#age certifications in the 'titles' data frame


