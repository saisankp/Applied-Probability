#TA Email: naveedh@tcd.ie
#STU22005 Laboratory Session 1

x <- c(4.5, 6.5, 6.4, 8.9, 4.1, 6.4, 6.3, 9.1, 12.1, 1.4, 1.4, 4.6, 1.6, 9.8, 7.2, 6.5,
       4.1, 6.5, 11.6, 2.9)
x

mean(x)
sd(x)

#find the number of observations
length(x)

#find various summary statistics(stuff like max, min, mean, median, 1st and 3rd quartile)
summary(x)

#you can add '?' before a action, to get info on it.
?summary

hist(x, main="Histogram of data values in X", xlab="Values", ylab="Frequency of Values", cex.main=0.9, cex.lab=0.6)

# Set a seed for reproducibility
set.seed(6124)

# Store the value of a scalar in n
n <- 100

# Generate a vector of independent standard normal variables.
x <- rnorm(n)
hist(x, main="Histogram of vector of independent standard normal values", xlab="Independent standard normal variables", ylab="Frequency of Independent standard normal variables", cex.main=0.9, cex.lab=0.6)

#. Set the working directory for this R session
setwd("C:/Applied Probability II Labs/Lab_1")
# Read the data into R
data1 <- read.csv("Lab1a.csv")
data1

#You have now created a data frame called data1 which includes the variable var1.
#To access var1, we use: data1$var1

# for the mean of var1:

mean(data1$var1)


#Lab Questions

#Q1

y <- c(1.1, 1.8, 2, 2.4, 2.5, 2.8, 2.9, 3, 3.4, 3.4, 4)
summary(y)
hist(y, main="Histogram of pharmaceutical data", xlab="Change in temperature", ylab="Frequency of change in temperature", cex.main=0.9, cex.lab=0.6)

#Q2

# Set a seed for reproducibility
set.seed(964)

n <- 1000

y <- rnorm(n)

hist(y, main="Histogram of values from a standard normal distribution", xlab="Value from standard normal distribution", ylab="Frequency of value from standard normal distribution", cex.main=0.9, cex.lab=0.6)

#Q3

set.seed(64)
n <- 200

# since variance = 9, we can do sd = 3 (since 3^2 = 9)
y<- rnorm(n, mean = 30, sd = 3)
hist(y, main="Histogram of values from a normal distribution", xlab="Value from normal distribution", ylab="Frequency of value from normal distribution", cex.main=0.9, cex.lab=0.6)

#Q4

n <- 20000

# since variance = 9, we can do sd = 3 (since 3^2 = 9)
z<- rnorm(n, mean = 30, sd = 3)
hist(z, main="Histogram of values from a standard normal distribution", xlab="Value from standard normal distribution", ylab="Frequency of value from standard normal distribution", cex.main=0.9, cex.lab=0.6)

#observed: the max frequency went from 50's to 5000
# also, more values around the middle.

#Q5

#. Set the working directory for this R session
setwd("C:/Applied Probability II Labs/Lab_1")
# Read the data into R
data2 <- read.csv("Lab1b.csv")
head(data2)

#summary and histogram for x1
summary(x1)
# x1 seems like a normal distribution ( normally distributed)
hist(data2$x1, main="Histogram of variable x1", xlab="Value from x1", ylab="Frequency of value from variable x1", cex.main=0.9, cex.lab=0.6)

#summary and histogram for x2
summary(x2)
#x2 seems like a exponential distribution (skewed to the right)
hist(data2$x2, main="Histogram of variable x2", xlab="Value from x2", ylab="Frequency of value from variable x2", cex.main=0.9, cex.lab=0.6)

#summary and histogram for x3 (uniform)
summary(x3)
hist(data2$x3, main="Histogram of variable x3", xlab="Value from x3", ylab="Frequency of value from variable x3", cex.main=0.9, cex.lab=0.6)

#Q6

?hist

#fixed all histograms by changing titles, labels for axes, and the size of the text.

#assignment help ;P
a <- 0.5
s <- 0.2
n <- 8
xbar <- 0.6
t <- (xbar-a)/(s/sqrt(n))
t
2*pt(-abs(t),df=n-1)
