#####Problem 1#####
# Write the following functions:
# 1.1. A function which replicates the SMA function. It should calculate the SMA
# for a vector to the left:
# 1.2. A function, which calculates the correlation coefficient between two vectors.
# It should replicate the cor function from the base package.
#####Problem 1#####
###1.1
vector1 <- c(5,2,7,3,6,4)
TTR::SMA(m, n=2)
sma <-NULL
n <- 3
SMAFunction <- function(inputVector,n){
  for (i in n:base::length(inputVector)){
    sma <- c(sma, base::mean(inputVector[i:(i-n+1)]))
  }
  return(sma)
}

SMAFunction(vector1,n)

###1.2
vector2 <- c(7,2,4,9,5,1)
cor <- stats::cor(vector2, vector1)

CorFunction <- function(inputVector1, inputVector2){
  First = inputVector1 - base::mean(inputVector1)
  Second = inputVector2 - base::mean(inputVector2)
  Nominator = base::sum(First*Second)/(base::length(inputVector1) - 1)
  Denominator = stats::sd(inputVector1)*stats::sd(inputVector2)
  Final = Nominator/Denominator
  return(Final)
}

CorFunction(vector2, vector1)


#####Problem 2#####
# Find all prime numbers less than 100, using for/while loops.
#####Problem 2#####
n=100
x = seq(1, n)

prime_numbers -> c()
composite_numbers -> c()
for (i in seq(2, n)) {
  if (any(x == i)) {
    prime_numbers = c(prime_numbers, i)
    x = c(x[(x %% i) != 0], i)
  }
  else{
    composite_numbers = c(composite_numbers, i)
  }
}

print(prime_numbers)