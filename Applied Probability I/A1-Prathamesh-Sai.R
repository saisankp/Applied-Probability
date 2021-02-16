#STU22004 Lab Assignment 1
#Student Number: 19314123
install.packages("gtools")
library(gtools)
install.packages("numbers") #use the numbers package so we can use the function coprime()
library(numbers)
iterations <- 200 #We need to calculate the mean of 200 iterations

#I used the replicate() function which uses nonstandard evaluation to re-evaluate the expression 200 times.
#I also used the coprime() function from the "numbers" package to see if 2 randomly chosen integers from a sample of 10,000 are coprime)
#I store the result into "answer" (which should consist of 200 of TRUE/FALSE)
answer <- replicate(iterations, coprime(sample(1:10000, 1), sample(1:10000, 1)))


#finally, I get the mean and return it as an output
finalAnswer = sum(answer/iterations)
finalAnswer

#The mean of results for 200 iterations: 0.605


