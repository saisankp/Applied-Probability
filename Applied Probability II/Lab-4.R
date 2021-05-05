#. Set the working directory for this R session
setwd("C:/Applied Probability II Labs/Lab_4")
# Read the data into R
# Read the data into R
exp <- read.csv("Lab4_ExperimentData.csv")
# View the first few lines of the data
head(exp)

# Scatterplot
plot(exp$Question, exp$Seconds, main = "Experiment time versus group",
     xlab = "Question", ylab = "Time to answer (seconds)")

# Fit and summarise the slr model fit to the data
lm1 <- lm(Seconds ~ Question, data = exp)
summary(lm1)

# Scatterplot with lm fit
plot(exp$Question, exp$Seconds, main = "Experiment time versus group",
     xlab = "Question", ylab = "Time to answer (seconds)")
abline(lm1)

# Store the residuals, standardised residuals and predicted values
resids <- resid(lm1)
sresids = rstandard(lm1)
preds <- predict(lm1)
# Plot the residuals
par(mfrow = c(1, 2), mar = c(5,6,2,2))
plot(preds, resids, xlab = "Predicted", ylab = "Residuals",
     main = "Residuals versus predicted")
abline(h = 0)
# QQ probability plot
qqnorm(resids, ylab="Residuals", xlab="Normal Scores",
       main="QQ plot")
qqline(resids)
par(mfrow = c(1,1), mar = c(5,4,4,2))


#Q1 
# Create a subset of the original dataset
exp_subset <- exp[which(exp$Question <5 & exp$Seconds < 98),]
summary(exp_subset)

#Q2 
# Scatterplot
plot(exp_subset$Question, exp_subset$Seconds, main = "Experiment time versus group",
     xlab = "Question", ylab = "Time to answer (seconds)")

# Fit and summarise the slr model fit to the data
lmQ1 <- lm(Seconds ~ Question, data = exp_subset)
summary(lmQ1)

# Scatterplot with lm fit
plot(exp_subset$Question, exp_subset$Seconds, main = "Experiment time versus group",
     xlab = "Question", ylab = "Time to answer (seconds)")
abline(lmQ1)

# Store the residuals, standardised residuals and predicted values
resids1 <- resid(lmQ1)
sresids1 = rstandard(lmQ1)
preds1 <- predict(lmQ1)
# Plot the residuals
par(mfrow = c(1, 2), mar = c(5,6,2,2))
plot(preds1, resids1, xlab = "Predicted", ylab = "Residuals",
     main = "Residuals versus predicted")
abline(h = 0)
# QQ probability plot
qqnorm(resids1, ylab="Residuals", xlab="Normal Scores",
       main="QQ plot")
qqline(resids1)
par(mfrow = c(1,1), mar = c(5,4,4,2))

#Q3
#There are two values in the restricted dataset that had Correct = 0. Is it appropriate to include these
#observations in the analysis?

# Create a subset of the original dataset
exp_subset2 <- exp_subset[which(exp_subset$Correct > 0),]
summary(exp_subset2)

# Scatterplot
plot(exp_subset2$Question, exp_subset2$Seconds, main = "Experiment time versus group",
     xlab = "Question", ylab = "Time to answer (seconds)")

# Fit and summarise the slr model fit to the data
lmQ2 <- lm(Seconds ~ Question, data = exp_subset2)
summary(lmQ2)

#The results change a bit.
#Median went from -1.819 to -1.440
#Min went from -24.549 to -24.457.
#Max went from 45.451 to 44.543.

#the biggest change seems to be in the median.
# Scatterplot with lm fit
plot(exp_subset2$Question, exp_subset2$Seconds, main = "Experiment time versus group",
     xlab = "Question", ylab = "Time to answer (seconds)")
abline(lmQ2)

# Store the residuals, standardised residuals and predicted values
resids2 <- resid(lmQ2)
sresids2 = rstandard(lmQ2)
preds2 <- predict(lmQ2)
# Plot the residuals
par(mfrow = c(1, 2), mar = c(5,6,2,2))
plot(preds2, resids2, xlab = "Predicted", ylab = "Residuals",
     main = "Residuals versus predicted")
abline(h = 0)
# QQ probability plot
qqnorm(resids2, ylab="Residuals", xlab="Normal Scores",
       main="QQ plot")
qqline(resids2)
par(mfrow = c(1,1), mar = c(5,4,4,2))


#Q4
#Weighted least squares model - account for the non constant variance

#Analyze a 


#Q5

x = c(1,2,3,4,5,6,7,8)
y = c(15,10,21,16,19,25,21,29)
plot(x, y, main = "y versus x", xlab = "x", ylab = "y")

lm5 <- lm(y ~ x)
summary(lm5)

#Verify the OLS estimates
n <- length(y)
sumx <- sum(x)
sumy <- sum(y)
xbar <- mean(x)
ybar <- mean(y)
sumx2 <- sum(x^2)
sumy2 <- sum(y^2)
sumxy <- sum(x*y)
Sxx <- sum(x^2) - n * mean(x)^2
Syy <- sum(y^2) - n * mean(y)^2
Sxy <- sum(x*y) - n * mean(x) * mean(y)
beta1_hat <- Sxy / Sxx
beta1_hat

beta0_hat <- ybar - beta1_hat * xbar
beta0_hat

resids <- resid(lm5)
preds <- predict(lm5)
testdata <- data.frame(x,y,resids,preds)
testdata

