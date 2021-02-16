#STU22004 Lab Assignment 3
#Student Number: 19314123

#Q1

#parameters: numberOfLamps to test with (i.e. k), average lifetime, and X where we want to find the percentage of the time that we would use lamp X 
findPercentageForLampX = function(numberOfLamps, averageHours, X){

decayParameter = (averageHours)^-1 #which is 1/mu (i.e. 1/average)
randomSampleLamps = 10000 #the question asked us to get 10,000 samples

#use a matrix to store the lamps being tested
lampsBeingTested = matrix(ncol=numberOfLamps, nrow=randomSampleLamps)

#fill the matrix using the rexp() function.
#the rexp() function returns a vector of random numbers having an exponential distribution.
for(a in 1:numberOfLamps){
  lampsBeingTested[,a] = rexp(randomSampleLamps, decayParameter)
}
percentageOfTimeFirstLampIsUsed = c(0)
finalPercentage = c(0)

#calculate the percentage of time lamp X is being used
for(i in 1:randomSampleLamps){
  time = c(0)
  for(q in 1:numberOfLamps){
    time = time + lampsBeingTested[i,q]
  }
  percentageOfTimeLampXIsUsed = 100 * (lampsBeingTested[i,X]/time)
  finalPercentage = finalPercentage + percentageOfTimeLampXIsUsed
}
average = sum(finalPercentage/randomSampleLamps)
return(average)
}

k <- c(0,0,0,0) #create a vector to hold the results of 200 iterations for k = 2,3,4,5

iterations = 200 # the question stated that we need to have 200 iterations
for(i in 1:iterations){
  averageHours <- 24000 #i.e. mu = 24,000 hour lifetime
  #use the vector to store the values, and let X = 1 when calling the function
  #because we want to find the percentage of time that we would use the first lamp 
  k[1] = k[1] + findPercentageForLampX(2, averageHours, 1)
  k[2] = k[2] + findPercentageForLampX(3, averageHours, 1)
  k[3] = k[3] + findPercentageForLampX(4, averageHours, 1)
  k[4] = k[4] + findPercentageForLampX(5, averageHours, 1)
}

percent <- c(0,0,0,0)
percent[1] = sum(k[1]/iterations)
percent[2] = sum(k[2]/iterations)
percent[3] = sum(k[3]/iterations)
percent[4] = sum(k[4]/iterations)

percent

k <- c(2,3,4,5)

plot(k, percent, ylim=c(0,100), xlim=c(2,5), main = "% of time for first lamp with K lamps", xlab = "Values of K", ylab = "Percentage")

percent[1]
percent[2]
percent[3]
percent[4]

# results :
# k = 2 : 50.01453%
# k = 3 : 33.33771%
# k = 4 : 25.01255%
# k = 5 : 20.00208%


#Q2

variancesFromIterations= c(0)
samples <- 10000
variancesFromIterations = replicate(200, findVariance(samples, 1, 2))
finalVariance = mean(variancesFromIterations)
finalVariance

# results :
# variance for which P(a<X<b) is maximum:
# variance = 4 


findVariance = function(samples, a, b){
  mu = c(0) # average = 0 (stated in the question)
  maximumStandardDeviation = c(0)
  probabilityWithAandBwhenMaximum = c(0) 
  
  #get samples as required from the question for the normal distribution
  samplesOfStandardDeviation <- sample(1:samples, samples)
  for(a in 1:samples){
    probabilityWithA = c(0)
    probabilityWithB = c(0)
    probabilityWithAandB = c(0)
    
    #P(Z = a-mu/sd)
    probabilityWithA = pnorm(a, mu, samplesOfStandardDeviation[a])
    #P(Z = b-mu/sd)
    probabilityWithB = pnorm(b, mu, samplesOfStandardDeviation[a])
    
    #P(Z = b-mu/sd) - P(Z = a-mu/sd)
    probabilityWithAandB = (probabilityWithB) - (probabilityWithA)
    
    #keep track of the maximum probability within the P(a < X < b)
    if(probabilityWithAandBwhenMaximum < probabilityWithAandB){
      maximumStandardDeviation = samplesOfStandardDeviation[a]
      probabilityWithAandBwhenMaximum = probabilityWithAandB
    }
  }
  #finally return the variance when the probability is maximized
  #variance = (standard deviation)^2
  varianceWhenProbabilityIsMax = (maximumStandardDeviation)^2
  return(varianceWhenProbabilityIsMax)
}