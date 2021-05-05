
##IID - independent and identically distributed random variables
# Create a vector containing the data
data <- c(13.1, 19.6, 21.3, 11.6, 15.4, 23.7, 18.6, 16.1, 19.3, 17.4, 21.5, 16.8, 14.9)
# Print out the data
data

# Generate a histogram
hist(data)

# Find the confidence interval using t.test (default is 95% confidence interval)
t.test(data)

# We see that the 95% confidence interval is: (15.54, 19.74).
# . What is the interpretation of this confidence interval?
#   We are 95% confident that the true population mean µ lies between 15.54 and 19.74.
# . What is the hypothesis test that is also being tested in the above output?
#   H0 : µ = 0, versus H0 : µ 6= 0
# 2
# . Is this a sensible hypothesis here?
#   We have no idea! No context was given, and hypotheses should be specified based on prior belief about the
# mean of the population. By default, R will test against 0, but we can change the default. Check out all the
# options by running ?t.test
# By default, t.test returns a 95% confidence interval. We can also find a 90% confidence interval.
# . Before looking at the output below, would you expect to see a narrower or wider interval than the 95%
# interval above?
# 

# Find the 90% confidence interval using t.test
t.test(data, conf.level = 0.9)

# Create a function where x is the data and alpha to determine the level
# of confidence (100*(1-alpha)%)
CI <- function(x, alpha, round) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  se <- s/sqrt(n)
  tval <- qt(0.5*alpha, df = n-1, lower.tail=FALSE )
  round(c( m - tval * se, m + tval * se ), round)
}

# Create a function where x is the data and alpha to determine the level
# of confidence (100*(1-alpha)%)
CI(x = data, alpha = 0.05, round = 2)

# Create a function where x is the data and alpha to determine the level
# of confidence (100*(1-alpha)%)
CI(x = data, alpha = 0.1, round = 2)

# Simulate 1000 datasets of size 15 from N(35, 9) and store in a matrix
set.seed(2646537)
mean_val = 35
sd_val = 3
samples = 1000
size = 15
x_sim <- matrix(rnorm(samples*size, mean = mean_val, sd = sd_val), nrow=samples,
                ncol=size, byrow=TRUE)
head(x_sim)

# Create the function coverage
coverage <- function(X, alpha, mu, round) {
  CIs <- matrix(nrow=nrow(X), ncol=2)
  for(k in 1:nrow(X)) CIs[k, ] <- CI(X[k,], alpha, round)
  #make a binary array of which CI's contain the true mean
  z <- (CIs[,1] < mu) * (CIs[ , 2 ] > mu)
  sum(z)/nrow(X)
}
# Call the function coverage:
coverage(X = x_sim, alpha = 0.05, mu = mean_val, round = 2)

#Q1

xval <- c( 69, 74, 79, 81, 85, 86, 89, 90, 94, 97, 100, 105)
t.test(xval, conf.level = 0.99)

#Q2

# Create a function where x is the data and alpha to determine the level
# of confidence (100*(1-alpha)%)
CI <- function(x, alpha, round) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  se <- s/sqrt(n)
  tval <- qt(0.5*alpha, df = n-1, lower.tail=FALSE )
  round(c( m - tval * se, m + tval * se ), round)
}

#rounding to 2 decimal places
answerQ2 <- CI(x=xval, alpha=0.01, round=2)
answerQ2


#Q3
#should get a narrower CI here with 90% instead of 99%
#less chance of population mean being in the interval, so the interval is narrower.
xval <- c( 69, 74, 79, 81, 85, 86, 89, 90, 94, 97, 100, 105)
t.test(xval, conf.level = 0.90)

# Create a function where x is the data and alpha to determine the level
# of confidence (100*(1-alpha)%)
CI <- function(x, alpha, round) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  se <- s/sqrt(n)
  tval <- qt(0.5*alpha, df = n-1, lower.tail=FALSE )
  round(c( m - tval * se, m + tval * se ), round)
}

#rounding to 2 decimal places
answerQ3 <- CI(x=xval, alpha=0.1, round=2)
answerQ3

#Q4

# Simulate 1000 datasets of size 15 from N(50, 16) and store in a matrix
set.seed(9498)
mean_val = 50
sd_val = 4
samples = 1000
size = 20
x_sim <- matrix(rnorm(samples*size, mean = mean_val, sd = sd_val), nrow=samples,
                ncol=size, byrow=TRUE)
head(x_sim)


# Create the function coverage
coverage <- function(X, alpha, mu, round) {
  CIs <- matrix(nrow=nrow(X), ncol=2)
  for(k in 1:nrow(X)) CIs[k, ] <- CI(X[k,], alpha, round)
  z <- (CIs[,1] < mu) * (CIs[ , 2 ] > mu)
  sum(z)/nrow(X)
}

# Call the function coverage:
coverage(X = x_sim, alpha = 0.05, mu = mean_val, round = 2)

#for 1000 samples, 0.941


#now for 10,000 samples

# Simulate 1000 datasets of size 15 from N(50, 16) and store in a matrix
set.seed(9498)
mean_val = 50
sd_val = 4
samples = 10000
size = 20
x_sim <- matrix(rnorm(samples*size, mean = mean_val, sd = sd_val), nrow=samples,
                ncol=size, byrow=TRUE)
head(x_sim)


# Create the function coverage
coverage <- function(X, alpha, mu, round) {
  CIs <- matrix(nrow=nrow(X), ncol=2)
  for(k in 1:nrow(X)) CIs[k, ] <- CI(X[k,], alpha, round)
  z <- (CIs[,1] < mu) * (CIs[ , 2 ] > mu)
  sum(z)/nrow(X)
}

# Call the function coverage:
coverage(X = x_sim, alpha = 0.05, mu = mean_val, round = 2)
#coverage changes to 0.9475 for 10,000 samples



#now for 100,000 samples

# Simulate 1000 datasets of size 15 from N(50, 16) and store in a matrix
set.seed(9498)
mean_val = 50
sd_val = 4
samples = 100000
size = 20
x_sim <- matrix(rnorm(samples*size, mean = mean_val, sd = sd_val), nrow=samples,
                ncol=size, byrow=TRUE)
head(x_sim)


# Create the function coverage
coverage <- function(X, alpha, mu, round) {
  CIs <- matrix(nrow=nrow(X), ncol=2)
  for(k in 1:nrow(X)) CIs[k, ] <- CI(X[k,], alpha, round)
  z <- (CIs[,1] < mu) * (CIs[ , 2 ] > mu)
  sum(z)/nrow(X)
}

# Call the function coverage:
coverage(X = x_sim, alpha = 0.05, mu = mean_val, round = 2)
#coverage changes to 0.94925 for 100,000 samples

#now for 1,000,000 samples

# Simulate 1000 datasets of size 15 from N(50, 16) and store in a matrix
set.seed(9498)
mean_val = 50
sd_val = 4
samples = 1000000
size = 20
x_sim <- matrix(rnorm(samples*size, mean = mean_val, sd = sd_val), nrow=samples,
                ncol=size, byrow=TRUE)
head(x_sim)


# Create the function coverage
coverage <- function(X, alpha, mu, round) {
  CIs <- matrix(nrow=nrow(X), ncol=2)
  for(k in 1:nrow(X)) CIs[k, ] <- CI(X[k,], alpha, round)
  z <- (CIs[,1] < mu) * (CIs[ , 2 ] > mu)
  sum(z)/nrow(X)
}

# Call the function coverage:
coverage(X = x_sim, alpha = 0.05, mu = mean_val, round = 2)
#coverage changes to 0.94925 for 100,000 samples


#Q5
#change the mean to 25
# Simulate 1000 datasets of size 15 from N(50, 16) and store in a matrix
set.seed(9498)
mean_val = 25
sd_val = 4
samples = 1000
size = 20
x_sim <- matrix(rnorm(samples*size, mean = mean_val, sd = sd_val), nrow=samples,
                ncol=size, byrow=TRUE)
head(x_sim)


# Create the function coverage
coverage <- function(X, alpha, mu, round) {
  CIs <- matrix(nrow=nrow(X), ncol=2)
  for(k in 1:nrow(X)) CIs[k, ] <- CI(X[k,], alpha, round)
  z <- (CIs[,1] < mu) * (CIs[ , 2 ] > mu)
  sum(z)/nrow(X)
}

# Call the function coverage:
coverage(X = x_sim, alpha = 0.05, mu = mean_val, round = 2)
#coverage doesn't change with changing mean from 50 to 25.

#change the sample size to 10
# Simulate 1000 datasets of size 15 from N(50, 16) and store in a matrix
set.seed(9498)
mean_val = 50
sd_val = 4
samples = 1000
size = 10
x_sim <- matrix(rnorm(samples*size, mean = mean_val, sd = sd_val), nrow=samples,
                ncol=size, byrow=TRUE)
head(x_sim)


# Create the function coverage
coverage <- function(X, alpha, mu, round) {
  CIs <- matrix(nrow=nrow(X), ncol=2)
  for(k in 1:nrow(X)) CIs[k, ] <- CI(X[k,], alpha, round)
  z <- (CIs[,1] < mu) * (CIs[ , 2 ] > mu)
  sum(z)/nrow(X)
}

# Call the function coverage:
coverage(X = x_sim, alpha = 0.05, mu = mean_val, round = 2)
#coverage changes to 0.956 when sample size changed to 10.
