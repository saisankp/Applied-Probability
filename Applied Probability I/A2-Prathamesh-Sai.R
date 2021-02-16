#STU22004 Lab Assignment 2
#Student Number: 19314123
install.packages("gtools")
library(gtools)

#Q1 Estimate the number of required dice rolling to get each face at least once. 

#function to find the number of tries to get each face at least once
numberOfSidesOnDie = function(sides) { 
  faces = numeric(0) #make the variable "faces" equal to the numeric value of 0.
  x <- sample(1:6, 10000, replace=TRUE) #get 10,000 random samples of values from 1-6.
  while(length(unique(faces)) < sides) { #while the number of unique faces is less than the number of sides.
    faces[length(faces)+1] = sample(x, 1) #add to faces, but add a random number from our sample of 10,000.
  }
  
  #after all the faces/sides have been rolled, then output the number of times it took!
  return(length(faces))
}

iterations = 200 #we need the mean of results for 200 iterations.

#I used the replicate() function which uses nonstandard evaluation to re-evaluate the expression 200 times
#I also called my own function numberOfSidesOnDie()
rollsUntilEachFaceRolled = replicate(iterations, numberOfSidesOnDie(6)) 

#get the mean of this value to estimate the number of required dice rolling to get each face at least once. 
mean(rollsUntilEachFaceRolled) 

#Q1 Answer: number of required dice rolling to get each face at least once: 14.715


#Q2

probabilityThatNoNumbersAreCorrect = function(k) { 
  
  #create cards from 1 to k, each must differ.
  cards <- sample(1:k, k, replace=FALSE)
  
  #create the matrix to contain our 10,000 guesses, 
  #An example of one guess would be [2,4,3,1,6,5] 
  guesses <- matrix(nrow=10000, ncol=k, byrow=TRUE)
  
  #fill matrix with pairs of 6 guesses in each row (total 10,000 rows)
  for(i in 1:10000){ 
    guesses[i,] = sample(1:k, k, replace = FALSE)
  }
   
  #create the extra matrix to track if the guesses are correct or not.
  #I just set them all to be wrong guesses at the start.
  guessIsWrong <- matrix(nrow=10000, ncol=k, byrow=TRUE)
  for(i in 1:10000){ 
    guessIsWrong[i,] = TRUE
  }
  
  wrongGuess <- 0
  for(i in 1:10000){ #for 10,000 guesses
 
    for(j in 1:k){ #for each row of k elements in the matrix
       if(guesses[i,j] == cards[j]){ #if our guess is correct
          guessIsWrong[i,j] = FALSE #we did not incorrectly guess, hence set that value to false.
       }
    }
    
    x <- numeric(0) #set x to 0.
    x <- guessIsWrong[i,] #transfer the data from row i into x.
    if(all(x) == TRUE) { #if all of row i is TRUE, then all the guesses were incorrect.
      wrongGuess = wrongGuess + 1 #let's increase our counter
    }
    
  }
  #return average of the outcomes from 10,000 guesses and return it
  return (wrongGuess/10000)
}

x1 <- c(6,7,8,9,10) # these will be the K values

#initialize variables to 0
kEquals6Values <- 0
kEquals6 <- 0
kEquals7Values <- 0
kEquals7 <- 0
kEquals8Values <- 0
kEquals8 <- 0
kEquals9Values <- 0
kEquals9 <- 0
kEquals10Values <- 0
kEquals10 <- 0

#Now, we can use our function that we made above.
#I used the replicate() function to call my function 200 times, and get the mean for 200 iterations.
#I also used the append() function to add all the probabilities into y1.
#Finally, I plot x1 and y1 as requested.

#when K = 6, probability = 0.3675110
kEquals6Values <- replicate(200, probabilityThatNoNumbersAreCorrect(6))
kEquals6 = mean(kEquals6Values)
y1 <- kEquals6

#when K = 7, probability = 0.3678205
kEquals7Values <- replicate(200, probabilityThatNoNumbersAreCorrect(7))
kEquals7 = mean(kEquals7Values)
y1 <- append(y1, kEquals7, after = length(y1))

#when K = 8, probability = 0.3679100
kEquals8Values <- replicate(200, probabilityThatNoNumbersAreCorrect(8))
kEquals8 = mean(kEquals8Values)
y1 <- append(y1, kEquals8, after = length(y1))

#when K = 9, probability = 0.3680535
kEquals9Values <- replicate(200, probabilityThatNoNumbersAreCorrect(9))
kEquals9 = mean(kEquals9Values)
y1 <- append(y1, kEquals9, after = length(y1))

#when K = 10, probability = 0.3683465
kEquals10Values <- replicate(200, probabilityThatNoNumbersAreCorrect(10))
kEquals10 = mean(kEquals10Values)
y1 <- append(y1, kEquals10, after = length(y1))
 # x1 = [6, 7, 8, 9, 10] i.e. our values of K
 # y1 = [0.3675110, 0.3678205, 0.3679100, 0.3680535, 0.3683465] i.e. our probabilities.

#Finally, plot the data
plot(x1, y1, main = "Probabilities vs K", xlab = "Independent Variable (K)", ylab = "Dependent Variable (Probabilities)", type="l", col = "red")





