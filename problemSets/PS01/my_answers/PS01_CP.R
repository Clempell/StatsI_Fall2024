###########################
# Load libraries
# Set wd
# Clear global environment
############################

# I remove objects
rm(list=ls())

# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", 
                      "package:grDevices", "package:utils",
                      "package:datasets", "package:methods", 
                      "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", 
                                                  search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list)
    detach(package,  character.only=TRUE)
}
detachAllPackages()

# I load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

############################
# Problem 1 - Education 
# Question 1 : 
##########################

# I Load my data as a vector : 
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 
       80, 97, 95, 111, 114, 89, 95, 126, 98)

# I calculate the mean of the sample : 
mean_y <- mean(y)
mean(y)

# I calculate the standard deviation of the sample : 
sd_y <- sd(y)
sd(y)

# I determine the size of the sample : 
n <- length(y)
n

#I find the t-value :
#Since our sample is smaller than 30, I use a t-distribution
#The confidence interval is 0.90 
alpha <- 0.10
t_value <- qt(1 - alpha/2, df = n - 1)
qt(1 - alpha/2, df = n - 1)

#I calculate the appropriate confidence interval for the mean level of students' 
#IQ in the school. Since our sample is samller than 30, I conduct a t-test. To 
#do so, I calculate the standard error of the mean
SE <- sd(y) / sqrt(n)
sd(y) / sqrt(n)

#I calculate the confidence interval :
lower_bound90 <- mean(y) - t_value * SE
mean(y) - t_value * SE
upper_bound90 <- mean(y) + t_value * SE
mean(y) + t_value * SE
lower_bound90
upper_bound90

#The results are : 93.95993 and 102.9201

# Interpretation : At least 90/100 of the time, the IQ of the population would 
#be between 93.95993 and 102.9201

############################
# Problem 1 - Education 
# Question 2
##########################

# I run a 5 steps hypothesis testing test

# Step One: Assomptions about my data : 
# 1) The data is continuous 
# 2) The sample is small, 25 
# 3) The sampling method is randomnization 

# Step Two: Formulating the hypotheses
# H0 : The average IQ in her school is equal to the average 100 IQ score
# H1: The average IQ in her school is greater than the average 100 IQ score
# The set significant level is alpha = 0.05

# Step Three : Choosing the testing methodology 
# Since the sample is small (25), I will use t-statistic 

# Step Four : Calculating t-value and performing one-sample t-test : 
  
# I start by loading  my data as vector : 
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 
       113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# I calculate the t value, to do so I use 90 as my confidence interval : 
t_value <- qt(1 - alpha/2, df = n - 1)
qt(1 - alpha/2, df = n - 1)


# I calculate the degree of freedom : 
df <- 25-1
25-1

# I start the hypothesis test by performing one-sample t-test : 
t_test_result <- t.test(y,mu=100, alternative = "greater")
t.test(y, mu = 100, alternative = "greater")

# The results are : 
# t-value = -0.59574, 
# p-value = 0.7215

# Step Five : Drawing conclusion 

# Interpretation of the results : A negative value means that the mean of the 
# sample, i.e. the IQ of the counselor's students, is less than 100. 
# On top of this, the p-value is greater than the significant level.This means 
# that the null hypotheses cannot be rejected. This means that there 
# is not enough evidence to conclude that the average IQ in the school is 
# greater than the national one, i.e.100. 


########################
# Problem 2 - Economy 
# Question 1
########################

# I load my data : 
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt"
                          , header=T)
data <- expenditure[, c("Y", "X1", "X2", "X3")]

# I start analyzing the six different relationships :

# First (outcome/predictor one): plot Y/X1 (with a regression line): 
  
# With R basis : 
plot(expenditure$X1, expenditure$Y,
     xlab = "per capita personal income in state", ylab = 
       "capita expenditure on shelters/housing assistance in state", 
     main = "Capita Expenditure/Personal Income")
model <- lm(Y ~ X1, data = expenditure)
abline(model, col = "blue", lwd = 2) 

# With ggplot: 
install.packages("ggplot2")
library(ggplot2)

ggplot(data, aes(x = X1, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Capita Expenditure/Personal Income", x = "Per capita personal 
       income in state", y = "Per capita expenditure 
       on shelters/housing assistance in state")


# Interpretation : I can see a strong positive linear correlation between 
# capital personal income (X) and capital expenditure (Y). In other words, 
# when there is a increase of personal income in the sate, there is also an
# increase of capital expenditure on shelters/housing assistance.

# Second (outcome/predictor two): plot Y/X2 (with a regression line) :

# With R basis : 
plot(expenditure$X2, expenditure$Y,
     xlab = "Number of residents per 100,000 that are financially 
       insecure in state", ylab = 
       "capita expenditure on shelters/housing assistance in state", 
     main = "Capita Expenditure/Personal Income")
model <- lm(Y ~ X2, data = expenditure)
abline(model, col = "red", lwd = 2) 

# With ggplot : 
ggplot(data, aes(x = X2, y = Y)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Capita Expenditure/Financially Insecure", 
       x = "Number of residents per 100,000 that are financially 
       insecure in state", y = "Per capita expenditure 
       on shelters/housing assistance in state")

# Interpretation : I can see no clear linear positive or negative correlation 
# between the number of residents financially unsecured (X) and capital
# expenditure (Y), the dots forming a parabola. In other words,I can see that if 
# unsecured, the capital expenditure on shelters/housing assistance decreases. 
# However, above 300 of residents financially unsecured, there is a linear 
# positive correlation between this number of residents financially unsecured and 
# the capital expenditure. 

# Third (outcome/predictor three): plot Y/X3 (with a regression line) : 

# With R basis: 
plot(expenditure$X3, expenditure$Y,
     xlab = "Number of people per thousand residing in urban areas in state", 
     ylab = "capita expenditure on shelters/housing assistance in state", 
     main = "Capita Expenditure/Personal Income")
model <- lm(Y ~ X3, data = expenditure)
abline(model, col = "green", lwd = 2) 

# With ggplot : 
ggplot(data, aes(x = X3, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Capita Expenditure/Urban Population", 
       x = "Number of people per thousand residing in urban areas in state", 
       y = "Per capita expenditure on shelters/housing assistance in state")

# Interpretation : I can see a weak linear correlation between the number of 
# people in urban areas (X) and capital expenditure (Y). In other words, when 
# there is a increase of the number of people in urban areas in the sate, there 
# is also a small tendancy of an increase of capital expenditure on 
# shelters/housing assistance.

# Fourth (predictor one/predictor two): plot X1 vs X2(with a regression line): 

# With R basis: 
plot(expenditure$X2, expenditure$X1,
     xlab = "Number of residents per 100,000 that are financially 
       insecure in state", 
     ylab = "Per capita personal income in state", 
     main = "Per capita personal income in state/Number of residents per 
     100,000 that are financially unsecured")
model <- lm(X1 ~ X2, data = expenditure)
abline(model, col = "yellow", lwd = 2) 

# With ggplot :
ggplot(data, aes(x = X2, y = X1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +
  labs(title = "Personal Income/Financially Insecure", 
       x = "Number of residents per 100,000 that are financially 
       insecure in state", y = "Per capita personal income in state")

# Interpretation : Here there is no clear linear correlation between the number 
# of residents financially unsecured (X) and capital personal income (Y), 
# the dots forming a parabola. In other words, if there is an increase from 100 to 
# 250 of the number of residents financially unsecured, the  capital personal 
# income decreases. However, above 300 of residents financially unsecured, there 
# is an increasing correlation between the number of residents financially 
# unsecured and  the  capital personal income.  

# Fifth (predictor one/predictor three): plot X1 vs X3(with a regression line) : 

# With R basis: 
plot(expenditure$X3, expenditure$X1,
     xlab = "Number of people per thousand residing in urban areas in state", 
     ylab = "Per capita personal income in state", 
     main = "Per capita personal income in state/Number of people per thousand 
     residing in urban areas in state")
model <- lm(X1 ~ X3, data = expenditure)
abline(model, col = "pink", lwd = 2) 


# With ggplot :
ggplot(data, aes(x = X3, y = X1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "pink") +
  labs(title = "Personal Income/Urban Population", 
       x = "Number of people per thousand residing in urban areas in state", 
       y = "Per capita personal income in state")

# Interpretation : I can see a strong linear correlation between the number of 
# people in urban areas (X) and capital personal income (Y). In other words, 
# when there is an increase of the number of people in urban areas in the sate, 
# there is also an increase of capital personal income.

# Sixth (predictor two/predictor three): plot X2 vs X3 (with a regression line):

# With R basis: 
plot(expenditure$X3, expenditure$X2,
     xlab = "Number of people per thousand residing in urban areas in state", 
     ylab = "Number of residents per 100,000 that are financially insecure 
       in state", 
     main = "Financially Insecure/Urban Population")
model <- lm(X2 ~ X3, data = expenditure)
abline(model, col = "orange", lwd = 2) 

# With ggplot :
ggplot(data, aes(x = X3, y = X2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(title = "Financially Insecure/Urban Population", 
       x = "Number of people per thousand residing in urban areas in state", 
       y = "Number of residents per 100,000 that are financially insecure 
       in state")

# Interpretation : I can see a weak linear correlation between the number of 
# people in urban areas (X) and  the number of residents financially unsecured 
# (Y). In other words, when there is an increase of the number of people in urban 
# areas in the sate, there is also a small tendancy of an increase of the number 
# of residents financially unsecured. 

# I complete this analyse by calculating correlations :
correlation_matrix <- cor(data[, c("Y", "X1", "X2", "X3")])

# Interpretation : 
# I know that : 
# => A value close to 1 indicates a strong positive correlation.
# => A value close to -1 indicates a strong negative correlation.
# => A value close to 0 means no correlation.

# Here we have : 
# 1) Y/X1 = 0.5317212, it is a moderate positive correlation. This means that 
# there is a not particular strong correlation between the increase of 
# personal income in the sate and the increase of capital expenditure on 
# shelters/housing assistance.
  
# 2) Y/X2 = 0.4482876, it is a moderate positive correlation.This means that 
# there is a not particular strong correlation between the increase of the number 
# of residents financially unsecured and the increase of capital 
# expenditure on shelters/housing assistance decreases.
  
# 3) Y/X3 = 0.4636787, it is a moderate positive correlation. This means that 
# there is a not particular strong correlation between the increase of the number 
# of people in urban areas and the increase of capital expenditure on 
# shelters/housing assistance decreases.
  
# 4) X1/X2 = 0.2056101, it is a weak positive correlation. This means there is a 
# low correlation between the increase of personal income in the sate and the 
# increase of the number of residents financially unsecured. 

# 5) X1/X3 = 0.5952504, it is a moderate positive correlation.This means that 
# there is a not particular strong correlation between the increase of personal 
# income in the sate and the increase of the number of people in urban areas. 

# 6) X2/X3 = 0.2210149, t is a weak positive correlation. This means there is a 
# low correlation between the increase of the number of residents financially 
# unsecured and the increase of the number of people in urban areas.
  
############################
# Problem 2 - Economy
# Question 2
##########################

# I load my data : 
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt"
                          , header=T)
data <- expenditure[, c("Y", "X1", "X2", "X3", "Region")]

# I plot the relationship between Y and Region, using a boxplot : 

# With R basis:
boxplot(Y ~ Region, 
        data = expenditure)
points(mean(y))
means <- tapply(expenditure$Y, expenditure$Region, mean)
points(1:length(means), means, col = "red", pch = 19)

# With ggplot :
boxplot(Y ~ Region, data = data, 
        main = "Per Capita Expenditure by Region",
        xlab = "Region", ylab = "Per Capita Expenditure")
means <- tapply(data$Y, data$Region, mean)
points(1:length(means), means, col = "red", pch = 19)

# Interpretation : I can see which region (X) has the highest capita expenditure 
# on housing assistance (Y). To do so, I inspect the average red dotes 
# in the boxplot :  generally I can say that the highest one represents the 
# region with the highest per capita expenditure. Here are (in descending order): 
# Region 4 (West) ; Region 2 (Centre-Nord); Region 1 (Nord-Est); and Region 3 
# (Sud). In this case, the same applies to the median line. 


############################
# Problem 2 - Economy
# Question 3
##########################

# I load my data: 
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt"
                          , header=T)
data <- expenditure[, c("Y", "X1", "X2", "X3", "Region")]

colors <- as.factor(data$Region)

# I analyze the relationships (outcome/predictor one), plotting Y vs X1 :

# With R basis : 
plot(expenditure$X1, expenditure$Y,
     xlab = "per capita personal income in state", ylab = 
       "capita expenditure on shelters/housing assistance in state", 
     main = "Capita Expenditure/Personal Income")
model <- lm(Y ~ X1, data = expenditure)
abline(model, col = "blue", lwd = 2) 

# With ggplot: 
ggplot(data, aes(x = X1, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Capita Expenditure/Personal Income", x = "Per capita personal 
       income in state", y = "Per capita expenditure 
       on shelters/housing assistance in state")

# Interpretation : I can see a strong positive linear correlation between capital 
# personal income (X) and capital expenditure (Y). In other words, when there 
# is a increase of personal income in the sate, there is also an increase of 
# capital expenditure on shelters/housing assistance.

# I add the extra variable (Region) : 

# With R basis : 
plot(expenditure$X1, expenditure$Y, col = colors, pch = 19,
     xlab = "per capita personal income in state", ylab = 
       "capita expenditure on shelters/housing assistance in state", 
     main = "Capita Expenditure/Capita Personal Income/Region")
legend("topright", legend = levels(colors), 
       col = 1:length(levels(colors)), pch = 19)

# With ggplot: 
expenditure$Region <- as.factor(expenditure$Region)
ggplot(expenditure, aes(x = X1, y = Y, color = Region)) +
  geom_point(size = 3) +  # 'size' controls the size of the points
  labs(title = "Capita Expenditure/Capita Personal Income/Region",
       x = "Per capita personal income in state",
       y = "Per capita expenditure on shelters/housing assistance in state") +
  theme_minimal() +
  scale_color_discrete(name = "Region") + 
  theme(legend.position = "top")

# Interpretation :I can see which region (Z) has the highest capita expenditure 
# on housing assistance (Y), regarding its capita personal income (X). 
# Inspecting the graph, I can say that region 3 (South) is far below, 
# region 2 (North Central) is in the middle, and regions 4 (West) and 1 
# (Northeast) are above. 
  