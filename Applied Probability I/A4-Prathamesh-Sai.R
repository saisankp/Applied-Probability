#STU22004 Lab Assignment 4
#Student Number: 19314123

#Q1
probabilityKCustomersArrive = function(samples){
  finalAnswer <- c(0)
  for(i in 1:samples){
    #Calculate the length of time that the customer spends in the shop 
    #I used rexp() because this has an exponential distribution.
    
    # N=1 since we need one observation,
    # rate=1 for example (the question states any parameter)
    y <- rexp(1)
    
    #Calculate how many customers arrive during the time that a customer spends in the shop
    #We want 1 observation (N=1), and the rate is the answer from rexp() i.e. the variable "y"
    z <- rpois(1, y)
    
    #get all the answers and store them into a vector
    finalAnswer <- append(finalAnswer, z, after = length(finalAnswer))
  }
  
  #Since the question revolves around when "maximum k other customer arrive"
  #I then calculated the probability when 2 or less customers arrived. 
  count <- c(0) 
  for(i in 1:samples){
    if(finalAnswer[i] <= 2){
      #increment our count
      count = count + 1 
    }
  }
  
  #get the probability
  percentage <- count/samples
  return (percentage)
}

#we need 10,000 samples for this question
samples <- 10000
answerFromIterations <- c(0)
answer <- c(0)

#get the mean of 200 iterations as asked in the question.
answerFromIterations <- replicate(200, probabilityKCustomersArrive(samples))
answer <- mean(answerFromIterations)
answer 

#results
#The probability that while a particular customer is shopping, maximum k other customer arrive:
#ANSWER = 0.8751175


#Q2

findExpectedValue = function(a, samples){
randomVariable <- c(0)
y <- c(0)

for(i in 1:samples){
  #use runif() for the uniform distribution U(0, a)
  x <- runif(1, min = 0, max = a)
  
  #store the appropriate value into the y vector according to the value of X
  if(x < a/2){
    y[i] <- x
  }
  else {
    y[i] <- a/2
  }
}

#get the mean of y to get the expected value
answer <- mean(y)
return (answer)
}

#we need 10,000 samples for this question
samples <- 10000

#find the expected values of 200 iterations for a = 1,2,3,4,5
expectedValuesFor1 <- replicate(200, findExpectedValue(1, samples))
expectedValuesFor2 <- replicate(200, findExpectedValue(2, samples))
expectedValuesFor3 <- replicate(200, findExpectedValue(3, samples))
expectedValuesFor4 <- replicate(200, findExpectedValue(4, samples))
expectedValuesFor5 <- replicate(200, findExpectedValue(5, samples))

expectedValues <- c(0,0,0,0,0)

#get the mean of 200 iterations and store them into a vector.
expectedValues[1] = mean(expectedValuesFor1)
expectedValues[2] = mean(expectedValuesFor2)
expectedValues[3] = mean(expectedValuesFor3)
expectedValues[4] = mean(expectedValuesFor4)
expectedValues[5] = mean(expectedValuesFor5)

expectedValues #this should now contain the results for the y-axis (dependent variable)
a <- c(1,2,3,4,5) #this will be the data on the x-axis (independent variable)

#Plot the results 
plot(a, expectedValues, main = "E[Y] for a=1,2,3,4,5", xlab="a", ylab="E[Y]")

#results
#For different values of a and using 10,000 random samples of X:
# a = 1, E[Y] = 0.3750969
# a = 2, E[Y] = 0.7499028
# a = 3, E[Y] = 1.1244971
# a = 4, E[Y] = 1.5007942
# a = 5, E[Y] = 1.8751335

#From the plot, it is visible that there is a linear relationship between a and E[Y]
