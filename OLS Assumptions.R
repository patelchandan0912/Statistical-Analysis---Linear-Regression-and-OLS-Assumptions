#' VIOLATIONS OF OLS ASSUMPTIONS
#' 
#' Data: Advertising.csv
#' Sales (in thousands of units) for a particular product in 200 sales regions, against 
#' advertising budgets(in thousands of dollars) for TV, online, and print media ads.

rm(list=ls())
library(readr)
url <- "https://raw.githubusercontent.com/patelchandan0912/Statistical-Analysis---Linear-Regression-and-OLS-Assumptions/main/advertising.csv"
d <- read_csv(url)


str(d)
View(d)          
colSums(is.na(d))                             # Data is clean; no missing values

#' Descriptive analysis

hist(d$sales)
den <- density(d$sales)                       # Density function
plot(den, main="Kernel Density of Sales", col="red")
hist(d$sales, breaks=20, prob=T, main="Histogram of Sales")
lines(den, col="red")

plot(sales ~ tv, data=d)                      # Fanning and concave
plot(sales ~ online, data=d)                  # Fanning
plot(sales ~ print, data=d)                   # No pattern
pairs(d)                                      # Pair plots

round(cor(d),3)                               # Correlation matrix
# install.packages("Hmisc") 
library("Hmisc")
rcorr(as.matrix(d))                           # Correlation matrix with p-value

# install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(d)                          # One function that does it all

#' OLS Estimation

ols0 <- lm(sales ~ 1, data=d)                 # Intercept only model (no predictors) - baseline
summary(ols0)                                 
plot(ols0) 

ols1 <- lm(sales ~ tv + online + print, data=d) # Basic additive model
summary(ols1)                                 # Print has no effect
plot(ols1)                                    # Curvature on residual plot


ols2 <- lm(sales ~ tv*online*print, data=d)   # Interaction (multiplicative) model
summary(ols2)                                 # Significant interaction
plot(ols2)                                    # Still curvature on residual plot

ols2 <- lm(sales ~ tv*online + print, data=d) # Slightly modified interaction model

library(stargazer)
stargazer(ols0, ols1, ols2, type="text")


#' Test for assumptions

ols2$res
hist(ols2$res)
ols2$fit

plot(ols2$res ~ ols2$fit)                     # Residual plot
hist(ols2$res)

qqnorm(ols2$res)                              # Q-Q plot
qqline(ols2$res, col="red")

shapiro.test(ols2$res)                        # Shapiro-Wilk's test of multivariate normality
norm <- rnorm(200)
ks.test(norm, ols2$res)                       # Kolmogorov-Smirnov test

# install.packages("car")
library("car")
bartlett.test(list(ols2$res, ols2$fit))       # Bartlett's test of homoskedasticity
leveneTest(ols1$res, ols2$fit, center=mean)   # Levene's test of homoskedasticity
# install.packages("lmtest")
library(lmtest)
bptest(ols2)                                      # Breush-Pagan test

library("car")                                # Test of multicollinearity
vif(ols2)                                     # Variance inflation factor

dwtest(ols2)                                  # Durbin-Watson test of autocorrelation


#' Data transformation

plot(log(sales) ~ tv, data=d)                 # Still curvature, but less fanning
plot(log(sales) ~ online, data=d)             # No curvature or fanning

ols3 <- lm(log(sales) ~ tv*online + print, data=d) # Exponential model
summary(ols3)                                 # Significant interaction
plot(ols3)                                    # Residuals more homogeneous but still has curvature 


#' Quadratic (second-order) model
#' I() creates a new variable by combining specified variables

ols4 <- lm(log(sales) ~ tv + I(tv*tv) + online + I(online*online) + tv*online + print, data=d)   
summary(ols4)                                 # Final model; but this is not better than ols3
plot(ols4) 


#' Summarizing results using stargazer

library(stargazer)
stargazer(ols1, ols2, ols3, ols4, type="text", single.row=TRUE)

outfile = "AdvertisingResults.html"
stargazer(ols1, ols2, ols3, ols4, title="OLS Analysis of Advertising Data", out=outfile)



#' MLE Estimation
#' MLE is robust to non-normal and heteroskedastic populations (but not linearity)

mle <- glm(log(sales) ~ tv + I(tv*tv) + online + I(online*online) + tv*online + print, data=d, family=gaussian)
summary(mle)
plot(mle)

poisson <- glm(sales ~ tv + online + tv*online + print, data=d, family=poisson(link="log"))
summary(poisson)
plot(poisson)


#' Summarizing results using stargazer

library(stargazer)
stargazer(ols2, ols4, mle, poisson, title="Analysis of Advertising Data", type="text")
outfile = "AdvertisingResults.html"
stargazer(ols1, ols2, ols3, ols4, mle, poisson, title="OLS Analysis of Advertising Data", out=outfile)

