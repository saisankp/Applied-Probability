# Set the seed value
set.seed(8423)

# Simulate values from the normal distribution
simdata <- rnorm(n = 10000, mean = 100, sd = 4)

# Create a matrix with 1000 samples in each row of 10 columns
matrixdata <- matrix(simdata, nrow = 1000, ncol = 10)

# Compute the mean of each sample (each row) and print the first six means
means <- apply(matrixdata, 1, mean)
means[1:6]

# Plot the first six samples
?par
par(mfrow=c(3,2), mar=c(4,4,4,1), oma=c(0.5,0.5,0.5,0))
for (i in c(1:6)){
  hist(matrixdata[i,], main = paste0("Sample ", i), xlab = "Values",
       cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2,
       ylim = c(0,5), xlim = c(85,115))
}

# Reset the number of plots that appear in the window at a time
par(mfrow=c(1,1))

# Generate histogram (keeping the y scale the same as the earlier graphs for comparison)
hist(means, main = "Histogram of the sample means", xlab = "Sample mean", xlim = c(85,115))

# Set the seed value
set.seed(96358285)
# Simulate values from the exponential distribution
simdata <- rexp(n = 10000, rate = 0.5)
# Create a matrix with 1000 samples in each row of 10 columns
matrixdata <- matrix(simdata, nrow = 1000, ncol = 10)

#Compute the mean of each sample (each row)
means <- apply(matrixdata, 1, mean)
means[1:6]

# Plot the first six samples
par(mfrow = c(3,2), mar = c(4,4,4,1), oma = c(0.5,0.5,0.5,0))
for (i in c(1:6)){
  hist(matrixdata[i,], main = paste0("Sample ", i), xlab = "Values",
       cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2,
       ylim = c(0,8), xlim = c(0,10))
}

# Reset the number of plots that appear in the window at a time
par(mfrow = c(1,1))

# Generate histogram (keeping the y scale the same as the earlier graphs for comparison)
hist(means, main = "Histogram of the sample means", xlab = "Sample mean", xlim = c(0, 10))


means1 <- null
par(mfrow = c(1,1))

#Q1

set.seed(6594)
simdata1 <- rnorm(n=5000, mean=50, sd=3)
matrixdata1 <- matrix(simdata1, nrow=1000, ncol=5)
means1 <- apply(matrixdata1,1,mean)
hist(means1, main="Orginal samples: N(50,9), n=5", xlab="Sample Mean")


#Q2

set.seed(9278)
simdata2 <- rnorm(n=30000, mean=50, sd=3)
matrixdata2 <- matrix(simdata2, nrow=1000, ncol=30)
means2 <- apply(matrixdata2,1,mean)
hist(means2, main="Orginal samples: N(50,9), n=30", xlab="Sample Mean", xlim=c(45,55))

#Q3

set.seed(3845)
simdata3 <- rexp(n=5000, rate=0.4)
matrixdata3 <- matrix(simdata3, nrow=1000, ncol=5)
means3 <- apply(matrixdata3,1,mean)
hist(means3, main="Orginal samples: Exp(0.4), n=5", xlab="Sample Mean", xlim=c(0,8))


#Q4

set.seed(8651)
simdata4 <- rexp(n=30000, rate=0.4)
matrixdata4 <- matrix(simdata4, nrow=1000, ncol=30)
means4 <- apply(matrixdata4,1,mean)
hist(means4, main="Orginal samples: Exp(0.4), n=30", xlab="Sample Mean", xlim=c(0,8))


#Q5

?par

# Plot the 4 histograms
par(mfrow = c(2,2), mar = c(4,4,4,1), oma = c(0.5,0.5,0.5,0))
hist(means1, main="(a) Orginal samples: N(50,9), n=5", xlab="Sample Mean", xlim=c(45,55), cex.main=0.9)
hist(means2, main="(b) Orginal samples: N(50,9), n=30", xlab="Sample Mean", xlim=c(45,55), cex.main=0.9)
hist(means3, main="(c) Orginal samples: Exp(0.4), n=5", xlab="Sample Mean", xlim=c(0,8), cex.main=0.9)
hist(means4, main="(d) Orginal samples: Exp(0.4), n=30", xlab="Sample Mean", xlim=c(0,8), cex.main=0.9)


#Fo (a) and (b), (b) is less spread out, because n is greater (30 compared to 5 in (a)). 
#For the sampling distribution of Y bar, we divide the sd by n. So the greater the N, the greater the number we divide by, and the less the sd and variance will be (hence the less it will be spread out).
#For the normal distribution, as the sample size increases, x axis is not as wide (more squished in), as we are dividing by N.

#For (c) and (d), (d) is less spread out once again for the exponential distribution, but the graph starts to look a bit more normally distributed, as N is bigger.
#For the exponential distribution, as sample is larger, the curve will be more normally distributed, using the central limit theorem.

#Online info:
#The central limit theorem states that if you have a population with mean ?? and standard deviation ?? and take sufficiently large random samples from the population with replacement, 
#then the distribution of the sample means will be approximately normally distributed.

#Also, In probability theory, the central limit theorem establishes that, in many situations, when independent random variables are added,
#their properly normalized sum tends toward a normal distribution even if the original variables themselves are not normally distributed.
