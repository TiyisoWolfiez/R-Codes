
#Creating a Matrix:

(A <- matrix(c(1,1,-0.2,-0.5), nrow = 2, ncol = 2))

##     [,1] [,2]
## [1,] 1 -0.2
## [2,] 1 -0.5

(B <- matrix(c(2,1,1,0.25), nrow = 2, ncol = 2))

##    [,1] [,2]
## [1,] 2   1
## [2,] 1 0.25

# Matrix Addition:
A + B

#       [,1]  [,2]
#[1,]    3  0.80
#[2,]    2 -0.25


#Matrix Subtraction
A - B
#      [,1]  [,2]
#[1,]   -1 -1.20
#[2,]    0 -0.75

#Matrix Multiplication but this is 'Elementwise Multiplication Operator'
A * B
#       [,1]   [,2]
#[1,]    2   -0.200
#[2,]    1   -0.125

#Matrix multiplication operators:
#Creates a new matrix by multiplying matrix1 with matrix2

A %*% B
#      [,1]  [,2]
#[1,]  1.8  0.950
#[2,]  1.5  0.875

#Division Operator
A / B
#      [,1] [,2]
#[1,]  0.5  -0.2
#[2,]  1.0  -2.0

#Elementwise Power Operator
A ^ 3
#       [,1]   [,2]
#[1,]    1   -0.008
#[2,]    1   -0.125

#Horizontal Concatenation
cbind(A,B)
##     [,1] [,2] [,3] [,4]
##[1,] 1 - 0.2   2  1.00
##[2,] 1 -0.5  1   0.25

#Vertical Concatenation
rbind(A, B)
#      [,1]  [,2]
#[1,]    1  -0.20
#[2,]    1  -0.50
#[3,]    2  1.00
#[4,]    1  0.25

#Transpose Matrix
t(A)
##      [,1] [,2]
## [1,] 1.0   1.0
## [2,] -0.2 -0.5

#Scalar Multiplication
5 * A
##     [,1] [,2]
## [1,] 5  -1.0
## [2,] 5  -2.5

#Determinant
det(A)
#[1]  = -0.3

#Extract the diagonal entries from a square matrix
diag(A)

#[1]  1.0 -0.5

#Dimensions of a Matrix
dim(A)
##[1] 2 2



# --->   Types Of Matrices   <---

# 1. Identity Matrix:
diag(3)

#       [,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1

#2. Scalar Matrix:
diag(7,4)
##    [,1] [,2] [,3] [,4]
## [1,] 7   0    0    0
## [2,] 0   7    0    0
## [3,] 0   0    7    0
## [4,] 0   0    0    7


# --->   Inverse Of A Matrix   <---

(invA <- solve(A))
##        [,1]       [,2]
## [1,] 1.666667 -0.6666667
## [2,] 3.333333 -3.3333333


#Showing that multiplying matrix A and the inverse of the matrix, A^-1
#is equal to the identity matrix

A %*% invA
#      [,1] [,2]
#[1,]    1    0
#[2,]    0    1


# --->   System Of Linear Equations   <---

 #  2x + 5y = 4
 #  8x + 3y = 7

#Create the matrices in R:

(A <- matrix(c(2,8,5,3), nrow = 2, ncol = 2))
#      [,1] [,2]
#[1,]    2    5
#[2,]    8    3


(b <- matrix(c(4,7),nrow = 2, ncol = 1))
#      [,1]
#[1,]    4
#[2,]    7


#Solve for x = A^-1b
(invA <- solve(A))
##            [,1]      [,2]
## [1,] -0.08823529  0.14705882
## [2,] 0.23529412 - 0.05882353


(x <- invA %*% b)

#      [,1]
#[1,] 0.6764706
#[2,] 0.5294118
