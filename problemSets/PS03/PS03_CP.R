#Question One 
#1: 
url <- "https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv"
data <- read.csv(url)

model <- lm(voteshare ~ difflog, data = data)
summary(model)

#2: 
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
ggplot(data, aes(x = difflog, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Effect of Difference in Campaign Spending on Incumbent's Vote Share",
       x = "Difference in Campain Spending (difflog)",
       y = "Incumbent's Vote Share")

#3: 
residuals_model <- resid(model)

#4:
intercept <- coef(model)[1]
slope <- coef(model)[2]
cat("Prediction Equation: voteshare =", round(intercept, 3), "+", round(slope, 3), "* difflog\n")

#Question Two 

#Relationship Spending Difference/Presidential Candidate Vote Share. 

#1: 
url <- "https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv"
data <- read.csv(url)
model1 <- lm(presvote ~ difflog, data = data)
summary(model1)

#2:
library(ggplot2)
ggplot(data, aes(x = difflog, y = presvote)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Effect of Spending Difference in Campain Spending on Presidential Vote Share",
       x = "Difference in Campain Spending (difflog)",
       y = "Presidential Candidate's Vote Share (presvote)")

#3:
residuals_model1 <- resid(model1)

#4:
intercept1 <- coef(model1)[1]
slope1 <- coef(model1)[2]
cat("Prediction Equation for presvote:", round(intercept1, 3), "+", round(slope1, 3), "* difflog\n")

#Relationship Presidential Candidate Vote Share/Incumbent’s Vote Share.

#1:
model2 <- lm(voteshare ~ presvote, data = data)
summary(model2)

#2:
ggplot(data, aes(x = presvote, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", color = "yellow", se = FALSE) +
  labs(title = "Effect of Presidential Candidate Vote Share on Incumbent’s Vote Share",
       x = "Presidential Candidate Vote Share (presvote)",
       y = "Incumbent’s Vote Share (voteshare)")

#3:
intercept2 <- coef(model2)[1]
slope2 <- coef(model2)[2]
cat("Prediction Equation for voteshare:", round(intercept2, 3), "+", round(slope2, 3), "* presvote\n")

#Question Three 
residuals_voteshare_difflog <- resid(model) 
residuals_presvote_difflog <- resid(model1) 

#1: 
model_residuals <- lm(residuals_voteshare_difflog ~ residuals_presvote_difflog)

#2:
ggplot(data = NULL, aes(x = residuals_presvote_difflog, y = residuals_voteshare_difflog)) +
  geom_point() +
  geom_smooth(method = "lm", color = "pink", se = FALSE) +
  labs(title = "Relationship between Residuals of incumbent voteshare and residential condidate presvote (controlled for difflog)",
       x = "Residuals of residential condidate presvote ~ difflog",
       y = "Residuals of incumbent voteshare ~ difflog")

#3:
intercept_residuals <- coef(model_residuals)[1]
slope_residuals <- coef(model_residuals)[2]
cat("Prediction Equation for residuals of voteshare:", round(intercept_residuals, 3), "+", round(slope_residuals, 3), "* residuals of presvote\n")

#Question Four

#1: 
model_combined <- lm(voteshare ~ difflog + presvote, data = data)

#2:
intercept_combined <- coef(model_combined)[1]
slope_difflog <- coef(model_combined)[2]
slope_presvote <- coef(model_combined)[3]
cat("Prediction Equation for voteshare:", round(intercept_combined, 3), "+", round(slope_difflog, 3), "* difflog +", round(slope_presvote, 3), "* presvote\n")

#3:
summary(model_combined)

