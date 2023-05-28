library(dplyr)
library(psych)
library(corrplot)
library(randomForest)
library(AICcmodavg)
library(xtable)

###############################
# MULTIVARIATE LINEAR REGRESSION
################################

# VARIABLES:
# Dependent - Velocity end (Continuous)
# Independent - SQ/NonSQ (Categorical)
# Confounder - Age (Continuous)
# Confounder - Commits
# Confounder - Developers
# Confounder - Issues
# Confounder - Velocity start (Continuous)

# Exploratory analysis
# cohortData <- read.csv('SQ_cohort_data_in_days(Final).csv')
# cohortDiffData <- read.csv('SQ_cohort_data_in_days(veloDiff).csv')
cohortData <- read.csv("SQ_cohort_data_in_days.csv")
View(cohortData)

# Get the absolute values for start and end values for velocity
cohortData$start_velocity_mean_days <- abs(cohortData$start_velocity_mean_days)
cohortData$end_velocity_mean_days <- abs(cohortData$end_velocity_mean_days)

str(cohortData)

velocityEnd <- cohortData$end_velocity_mean_days
exposed <- cohortData$exposed
age <- cohortData$age
commits <- cohortData$commits
devs <- cohortData$devs
issues <- cohortData$overall_issues
velocityStart <- cohortData$start_velocity_mean_days

analysisData <- data.frame(velocityEnd, age, commits, devs, issues, velocityStart, exposed)

summary(analysisData)

# Correcting the robust rounding on velocity measurements since by definition
# they are non-negative
analysisData$velocityEnd <- analysisData$velocityEnd + 0.01
analysisData$velocityStart <- analysisData$velocityStart + 0.01

#################################
# ANALYSIS WITH RAW DATA
#################################

# Explanatory analysis
boxplot(analysisData, main='nonNormalized')
multi.hist(analysisData)
cor(analysisData) # Unnoticeable correlation from vars towards the response var
pairs(cbind(analysisData)) # scatterplot of pairs of variables

# First initial model assuming linearity (normality)
fit <- lm(velocityEnd ~ exposed+age+commits+devs+issues+velocityStart, data = analysisData)
summary(fit)
# Multiple R-squared:  0.02257 & p-value: 0.2725 (Low-significance) 
# R-squared: the proportion of the variance in the response variable of a 
# regression model that can be explained by the predictor variables.

plot(fit)
# This model clearly denotes non-nonmality, just need to see the QQ plot to see how outliers 
# disturb the normality shape.
hist(rstandard(fit)) # Same here, there's no normality in the data

qqnorm(rstandard(fit)) 

plot(fitted(fit), rstandard(fit)) # It can be seen that the variance increases heavily proportionally to
# the mean. This is a clear hint to denote Gamma distribution and/or Inv Gaussian distribution

# Let's check first for interactions
fit2 <- lm(velocityEnd ~( exposed+age+commits+devs+issues+velocityStart)^2, data = analysisData)
summary(fit2)
# Multiple R-squared:  0.04887 & p-value: 0.7582 (BAD)
plot(fit2) # This setup presents the same.

fit3 <- lm(velocityEnd ~( exposed+age+commits+devs+issues+velocityStart)^3, data = analysisData)
summary(fit3) # Multiple R-squared:  0.134 & p-value: 0.3067 (Better values but still too weak)
plot(fit3)

# INTERPRETATION: Exploratory analysis as well as different combinations of variables clearly
# denote non-normality. I could check the exclusion of possible main effects, but still I don't think
# the model would be good enough since the lack of normality is quite evident.
# OPTIONS:
# 
#   - Perform a standard transformation and check the procedure again
#   - Adopt Gamma and/or Inv Gaussian distributions as assumptions

##########################################################################
# STANDARD NORMALIZATION OF DATA
##########################################################################

# Using Z-SCORE standardization
normalizedData <- analysisData %>% mutate_each_(list(~scale(.)%>% as.vector), 
                                                vars = c('velocityEnd', 'age', 'commits', 'devs', ' issues', 'velocityStart'))
# This normalization makes negative values for variables that by definition should be positive.

# Second manual approach
z_standardization <- function(x) {
  (x - mean(x)) / sd(x)
}
normalizedData <- as.data.frame(lapply(analysisData[1:6], z_standardization))

# Using MIN-MAX standardization
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
normalizedData <- as.data.frame(lapply(analysisData[1:6], min_max_norm))
# Min-max normalization offers non-negative values, we can try this way

# MANUAL MODIFICATION IN ORDER TO CONSIDER GAMMA AND INV GAUSSIAN DISTRIBUTIONS
normalizedData <- normalizedData + 0.01
normalizedData$exposure <- cohortData$exposed
head(normalizedData)



# Explanatory analysis
boxplot(normalizedData, main='Normalized')
multi.hist(normalizedData) # NOTE: Still there's too much skewness in the data
cor(normalizedData) # Unnoticeable correlation from vars towards the response var
pairs(cbind(normalizedData)) # scatterplot of pairs of variables

# First initial model assuming linearity (normality)
fit4 <- lm(velocityEnd ~ exposure+age+commits+devs+issues+velocityStart, data = normalizedData)
summary(fit4)
# Multiple R-squared:  0.02257 & p-value: 0.2725 (Low-significance) 
# R-squared: the proportion of the variance in the response variable of a 
# regression model that can be explained by the predictor variables.

plot(fit4)
# This model clearly denotes non-nonmality, just need to see the QQ plot to see how outliers 
# disturb the normality shape.
hist(rstandard(fit4)) # Same here, there's no normality in the data

qqnorm(rstandard(fit4)) 

plot(fitted(fit4), rstandard(fit4)) # It can be seen that the variance increases heavily proportionally to
# the mean. This is a clear hint to denote Gamma distribution and/or Inv Gaussian distribution

# Let's check first for interactions
fit5 <- lm(velocityEnd ~( exposure+age+commits+devs+issues+velocityStart)^2, data = normalizedData)
summary(fit5)
# Multiple R-squared:  0.04887 & p-value: 0.7582 (BAD)
plot(fit5) # This setup presents the same.

fit6 <- lm(velocityEnd ~( exposure+age+commits+devs+issues+velocityStart)^3, data = normalizedData)
summary(fit6) # Multiple R-squared:  0.134 & p-value: 0.3067 (Better values but still too weak)
plot(fit6)

# INTERPRETATION: Clearly there is no reasonable results to support the eligibility of
# normalized data in non of the tried cases. It's time for thre GLMs.

# In fact, after normalizing the data, at least the residuals should denote normality
# We check on that through the saphiro test
shapiro.test(residuals(fit4, type = 'pearson')) 
# p-value < 2.2e-16 (Should be more than 0.05 to be normal) HENCE, BAD SIGNAL

models <- list(fit, fit2, fit3, fit4, fit5, fit6)

mod.names <- c('raw', 'raw2int', 'raw3int','norm','norm2int','norm3int')

mvlmodels <- aictab(cand.set = models, modnames = mod.names)

#           K    AICc Delta_AICc AICcWt Cum.Wt       LL
# norm      8 -496.76       0.00      1      1   256.60
# norm2int 23 -472.82      23.93      0      1   261.18
# norm3int 43 -454.91      41.85      0      1   276.93
# raw       8 4517.11    5013.87      0      1 -2250.34
# raw2int  23 4541.05    5037.80      0      1 -2245.75
# raw3int  43 4558.96    5055.71      0      1 -2230.00

BIClmodels <- bictab(cand.set = models, modnames = mod.names)
#           K     BIC Delta_BIC BICWt Cum.Wt       LL
# norm      8 -466.66      0.00     1      1   256.60
# norm2int 23 -388.57     78.09     0      1   261.18
# norm3int 43 -303.73    162.93     0      1   276.93
# raw       8 4547.21   5013.87     0      1 -2250.34
# raw2int  23 4625.30   5091.96     0      1 -2245.75
# raw3int  43 4710.13   5176.79     0      1 -2230.00

### BEFORE TRYING DIFFERENT COMBINATIONS OF VARIABLES IN THE MLRs, LET'S TRY
### RANDOM FOREST LINEAR REGRESSION (Combination of linear regression trees)
##############

library(randomForest)
#install.packages('randomForest')

model1RF <- randomForest(velocityEnd ~ . , data = normalizedData, type = 'regression')
model1RF

estimates1RF <- predict(model1RF)

mseRF <- mean((normalizedData$velocityEnd - estimates1RF)^2) # 0.01403216

AIC(model1RF)

# Even though the MSE looks fine, there's an error if the % of Variance explaining ability of this method is negative
# Meanwhile in the MLR the same value Multiple R_sq was only 2.25%
summary(model1RF)

# Model validation

# RF - Mean of squared residuals: 0.01386118
# MLR - Mean of squared residuals: 0.01266318 (MLR is slightly better)

plot(model1RF) # At least the regression sees its error reduced by the increase of regression trees

VarImp <- randomForest::importance(model1RF)
VarImp <- as.matrix(VarImp[order(VarImp[,1], decreasing = TRUE),])

varImpPlot(model1RF, main = "Variable importance to explain velocityEnd")

##########################################################################
# BUILDING GLMs
##########################################################################

summary(analysisData)
# Need to manually change

#### GAUSSIAN DISTRIBUTION
gauss.id <- glm(velocityEnd ~ factor(exposed)+age+commits+devs+issues+velocityStart, family = gaussian(link = 'identity'), data = analysisData)
summary(gauss.id) # AIC: 4516.7

gauss.log <- glm(velocityEnd ~ factor(exposed)+age+commits+devs+issues+velocityStart, family = gaussian(link = 'log'), data = analysisData)
summary(gauss.log) # AIC: 4518.2

gauss.inv <- glm(velocityEnd ~ factor(exposed)+age+commits+devs+issues+velocityStart, family = gaussian(link = 'inverse'), data = analysisData)
summary(gauss.inv) # AIC: 4562

#### GAMMA DISTRIBUTION
gamma.id <- glm(velocityEnd ~ factor(exposed)+age+commits+devs+issues+velocityStart, family = Gamma(link = 'identity'), data = analysisData)
summary(gamma.id) # ERROR

gamma.log <- glm(velocityEnd ~ factor(exposed)+age+commits+devs+issues+velocityStart, family = Gamma(link = 'log'), data = analysisData)
summary(gamma.log) # AIC: 3667

gamma.inv <- glm(velocityEnd ~ factor(exposed)+age+commits+devs+issues+velocityStart, family = Gamma(link = 'inverse'), data = analysisData)
summary(gamma.inv) # AIC: 3670.9


#### INVERSE GAUSSIAN DISTRIBUTION
i.gauss.id <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart + exposed, family = inverse.gaussian(link = 'identity'), data = analysisData)
summary(i.gauss.id) # 

i.gauss.log <- glm(velocityEnd ~ factor(exposed)+age+commits+devs+issues+velocityStart, family = inverse.gaussian(link = 'log'), data = analysisData)
summary(i.gauss.log) # 

i.gauss.inv <- glm(velocityEnd ~ factor(exposed)+age+commits+devs+issues+velocityStart, family = inverse.gaussian(link = 'inverse'), data = analysisData)
summary(i.gauss.inv) # 

i.gauss.can <- glm(velocityEnd ~ factor(exposed)+age+commits+devs+issues+velocityStart, family = inverse.gaussian(link = '1/mu^2'), data = analysisData)
summary(i.gauss.can) # 

###############################################################
## APPROACHES THROUGH DIFFERENT SCALING METHODS
## (Data is so inbalanced that requires to be scaled)
###############################################################

dataX <- analysisData
dataX$sq <- cohortData$SQ.NonSQ
i.gauss.id <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart + factor(sq), family = inverse.gaussian(link = 'identity'), data = dataX)
gamma.id <- glm(velocityEnd ~ factor(sq)+age+commits+devs+issues+velocityStart, family = Gamma(link = 'identity'), data = dataX)
# Doesn't work with 0 values

#------------------------------------------------------------------
# Trying to normalize by min-max the response variable only
dataN <- analysisData
velocityEnd <- min_max_norm(analysisData[1]) 
dataN$velocityEnd <- velocityEnd[[1]] + 0.001

i.gauss.id <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart + exposed, family = inverse.gaussian(link = 'identity'), data = dataN)
i.gauss.log <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+ exposed, family = inverse.gaussian(link = 'log'), data = dataN)
i.gauss.inv <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+ exposed, family = inverse.gaussian(link = 'inverse'), data = dataN)
i.gauss.can <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+ exposed, family = inverse.gaussian(link = '1/mu^2'), data = dataN)

gamma.id <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+exposed, family = Gamma(link = 'identity'), data = dataN)
gamma.log <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+ exposed, family = Gamma(link = 'log'), data = dataN)
gamma.inv <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+ exposed, family = Gamma(link = 'inverse'), data = dataN)

gauss.id <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart + exposed, family = gaussian(link = 'identity'), data = dataN)
#------------------------------------------------------------------

# Normalizing only independent variables. (through z-score)
normData <- as.data.frame(lapply(analysisData[2:6], z_standardization))
normData$velocityEnd <- analysisData$velocityEnd
normData$exposed <- analysisData$exposed
summary(normData)
#------------------------------------------------------------------
# Normalizing only independent variables. (through min-max)
minmaxD <- as.data.frame(lapply(analysisData[2:6], min_max_norm))
minmaxD$velocityEnd <- analysisData$velocityEnd
minmaxD$exposed <- analysisData$exposed
summary(minmaxD)

## Need to manipulate to get into the conditions for Gamma and Gaussian
minmaxD <- minmaxD + 0.001
plot(minmaxD)
multi.hist(minmaxD)


i.gauss.id <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart + exposed, family = inverse.gaussian(link = 'identity'), data = minmaxD) # AIC: 5382.6
i.gauss.log <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+ exposed, family = inverse.gaussian(link = 'log'), data = minmaxD) # Cannot correct step size
i.gauss.inv <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+ exposed, family = inverse.gaussian(link = 'inverse'), data = minmaxD) # mu values close to 0 give error
i.gauss.can <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+ exposed, family = inverse.gaussian(link = '1/mu^2'), data = minmaxD) # not possible to offer starting vals

gamma.id <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+exposed, family = Gamma(link = 'identity'), data = minmaxD) # not possible to offer starting vals
gamma.log <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+ exposed, family = Gamma(link = 'log'), data = minmaxD) # AIC: 3648.4
gamma.inv <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+ exposed, family = Gamma(link = 'inverse'), data = minmaxD) # AIC: 3652.2

gauss.id <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart + exposed, family = gaussian(link = 'identity'), data = minmaxD) # AIC: 4516.7
gauss.log <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart + exposed, family = gaussian(link = 'log'), data = minmaxD) # AIC: 4518.2
gauss.inv <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart + exposed, family = gaussian(link = 'inverse'), data = minmaxD) # AIC: 4562.1

# Selection based on information criteria
models <- list(i.gauss.id, gamma.log, gamma.inv, gauss.id, gauss.log, gauss.inv)

mod.names <- c('inverse gaussian id', 'gamma log', 'gamma inv','gaussian id','gaussian log','gaussian inv')

mvlmodels <- aictab(cand.set = models, modnames = mod.names)

BIClmodels <- bictab(cand.set = models, modnames = mod.names)

# gamma log seems to be the model fitting the best the data.
summary(gamma.log) # AIC: 3648.4
# BIC: 3678.95

# Two way interaction gamma log
gamma.log2 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+ exposed)^2, family = Gamma(link = 'log'), data = minmaxD) # AIC: 3664.8
summary(gamma.log2)
BIC(gamma.log2) # [1] 3752.551
# seems that two-way interaction worsens the model

gamma.log3 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+ exposed)^3, family = Gamma(link = 'log'), data = minmaxD) # AIC: 3674.2
summary(gamma.log3)
BIC(gamma.log3) # 3838.289
# With three way interaction looks like specially velocityStart gets to be a common pattern in significant cases.

##################################################################
# BACKWARD SELECTION PROCESS WITH GAMMA LOG
##################################################################

# Still, let's start the backward selection process from the gamma.log model.
# The variable showing less statistical significance was# devs
gamma.log4 <- glm(velocityEnd ~ age+commits+issues+velocityStart+ exposed, family = Gamma(link = 'log'), data = minmaxD) # AIC:
summary(gamma.log4) # AIC: 3646.5
plot(gamma.log4)
BIC(gamma.log4) # 3673.2

est.gammalog4 <- predict(gamma.log4)
mse_gammalog4 <- mean((minmaxD$velocityEnd - est.gammalog4)^2)

anova(gamma.log,gamma.log4, test = 'F') # p-value: 0.8521
# Doesn't explain anything since both vals are the same no matter the order of models.
# Still, seems like AIC, BIC give better results.
# The MSE values are totally unreasonable, THE ONLY WAY TO GET REASONABLE
# ANSWERS MUST BE SCALING AL THE DATA

########################################################################
########################################################################
########################################################################

# SCALING ALL PARAMETERS
scaledData <- as.data.frame(lapply(analysisData[1:6], min_max_norm))
# MANUAL MODIFICATION IN ORDER TO CONSIDER GAMMA AND INV GAUSSIAN DISTRIBUTIONS
scaledData <- scaledData + 0.001
scaledData$exposure <- analysisData$exposed
head(scaledData)
summary(scaledData)

# EXPLORATORY ANALYSIS OF DATA
boxplot(scaledData[1:6], main='Boxplot of variable distributions', 
        names = c('Velocity end', 'Age', 'Commits', 'Developers', 'Issues', 'Velocity start'),
        ylab = 'Observed scaled values',
        cex.lab = 1.2)
multi.hist(scaledData[1:6], main = c('Velocity end', 'Age', 'Commits', 'Developers', 'Issues', 'Velocity start'),
           bcol = 'darkgrey', 
           density = T,
           dcol = c('darkblue', 'red'), 
           dlty = c('dotted', 'solid'), 
           lwd=2)
legend("bottomright", c("Normal fit", "Density distribution"), 
       fill = c("darkred", "red"))
cor(scaledData) # Unnoticeable correlation from vars towards the response var
pairs(cbind(scaledData), main = 'Correlation plot'
      , labels = c('Velocity end', 'Age', 'Commits', 'Developers', 'Issues', 'Velocity start', 'Exposure')
      , col=2
      , font.labels = 2
      , cex.labels = 1.6) # scatterplot of pairs of variables
# From the exploratory analysis we can denote low collinearity, that is,
# low linear dependence.
# Scatterplot shows indeed non-linearity specially when it comes to explain 
# the velocity at the end of the follow-up period.

#--------------------------------------------------------------------------
# Assuming linearity:
fit7 <- lm(velocityEnd ~ age+commits+devs+issues+velocityStart+exposed, data = scaledData)
summary(fit7) # # AIC(fit7)=-497.1978 || no significant variables
step(fit7) # BEST: lm(formula = velocityEnd ~ age + velocityStart, data = scaledData)
BIC(fit7)

estimates <- predict(fit7)
mse <- mean((scaledData$velocityEnd - estimates)^2) # 0.0127


fit8 <- lm(formula = velocityEnd ~ age + velocityStart, data = scaledData)
summary(fit8) # AIC=-504.1395 || Somewhat significance in one of the variables
BIC(fit8)
AIC(fit8)

estimates <- predict(fit8)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse


# Two way interaction
fit9 <- lm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^2, data = scaledData)
summary(fit9)
AIC(fit9)
step(fit9) # AIC=-1459.67 model: velocityEnd ~ age + velocityStart, already checked in fit8
BIC(fit9)

estimates <- predict(fit9)
mse <- mean((scaledData$velocityEnd - estimates)^2)

# Three way interaction
fit10 <- lm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^3, data = scaledData)
summary(fit10)
AIC(fit10)
step(fit10) # AIC=-1446.48
BIC(fit10) # BIC=-303.7341
# Fit offers good information criteria values, it also discovers significant interactions
# That need to be considered, and furthermore, IC values are not that far from model 8.

estimates <- predict(fit10)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse # 0.0112

# Based on AIC values step() function denotes that fit8 is the best model so far,
# AIC=-1459.67 || BIC=-388.568
# still, the explanatory variables aren't explanatory enough. Graphically it can be explained like this
plot(fit8)
hist(rstandard(fit8)) # Same here, there's no normality in the residuals either
qqnorm(rstandard(fit8))

# This indeed lead as that we can not assume linearity even after scaling the data.
# Thus, it implies that we should use GLM to test out different distributional assumptions.

# Printing the best model into Latex
print(xtable(summary(fit8, type='latex')))

#--------------------------------------------------------------------------
# Assuming non-linearity:

#---------------------------
# Gaussian Identity
gauss.id <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+exposed, family = gaussian(link = 'identity'), data = scaledData) 
summary(gauss.id) # AIC: -497.2 || no significant variables.
AIC(gauss.id)
step(gauss.id) # AIC: -504.1 || model: velocityEnd ~ age + velocityStart

estimates <- predict(gauss.id)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse

gauss.id2 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^2, family = gaussian(link = 'identity'), data = scaledData) 
summary(gauss.id2)
step(gauss.id2) # Offers the same best final model without interaction. # AIC: -504.1

estimates <- predict(gauss.id2)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse

gauss.id3 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^3, family = gaussian(link = 'identity'), data = scaledData) 
summary(gauss.id3) # AIC: -467.87 || Some interactions get enough significance
step(gauss.id3) # Offers the same best final result. # AIC: -492

estimates <- predict(gauss.id3)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse



# Best three way interaction model
gauss.id4 <- glm(formula = velocityEnd ~ age + commits + devs + issues + velocityStart + 
                   exposed + age:commits + age:devs + age:issues + age:velocityStart + 
                   age:exposed + commits:devs + commits:issues + commits:velocityStart + 
                   devs:issues + devs:velocityStart + issues:velocityStart + 
                   velocityStart:exposed + age:commits:devs + age:commits:issues + 
                   age:commits:velocityStart + age:devs:issues + age:issues:velocityStart + 
                   age:velocityStart:exposed + commits:devs:issues + commits:devs:velocityStart + 
                   commits:issues:velocityStart, family = gaussian(link = "identity"), 
                 data = scaledData)
summary(gauss.id4) 

estimates <- predict(gauss.id4)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse # 0.01139174

#---- R squared calculation
# The deviance-based R-squared 
rsq <- with(summary(gauss.id4), 1 - gauss.id4$deviance/gauss.id4$null.deviance) 
rsq # 0.1240467

#  the adjusted deviance-based R-squared
sampleSize <- dim(scaledData)[1]
modelParams <- length(gauss.id4$coefficients)
rsq_adj <- 1 - ((sampleSize-1)/(sampleSize-modelParams))*(1 - rsq)
rsq_adj # 0.04725856
#--------------

BIC(gauss.id4) # -381.3322, weaker than fit8, but comparing the difference of variables
# the penalization is relatively small.

plot(gauss.id4)
hist(rstandard(gauss.id4)) 
qqnorm(rstandard(gauss.id4))
# Residuals get a bit more normalized, but indeed this results depict what a linear model
# would be since we have only used identity link with Gaussian distribution.

#-------
# Gaussian Log
gauss.log1 <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+exposed, family = gaussian(link = 'log'), data = scaledData) 
summary(gauss.log1) # No significant main effects and a weaker AIC value AIC: -495.65
step(gauss.log1) # AIC=-502.75 || velocityEnd ~ age + velocityStart

estimates <- predict(gauss.log1)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse # 8.247362

gauss.log1.2 <- glm(formula = velocityEnd ~ age + velocityStart, family = gaussian(link = "log"), 
                    data = scaledData)
summary(gauss.log1.2)


gauss.log2 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^2, family = gaussian(link = 'log'), data = scaledData) 
summary(gauss.log2)# AIC: -500.43 || Some interactions are significant
step(gauss.log2) # AIC: -518.2 for model with some interactions

estimates <- predict(gauss.log2)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse

gauss.log2.1 <- glm(formula = velocityEnd ~ age + commits + devs + issues + velocityStart + 
                      exposed + age:issues + age:exposed + commits:devs + commits:exposed + 
                      issues:velocityStart + issues:exposed + velocityStart:exposed, 
                    family = gaussian(link = "log"), data = scaledData)
summary(gauss.log2.1) # Potential interaction + SIGNIFICANCY FOR EXPOSURE VARIABLE AIC: -518.17

estimates <- predict(gauss.log2.1)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse

gauss.log3 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^3, family = gaussian(link = 'log'), data = scaledData) 
summary(gauss.log3) # AIC: -569.97 || Potentially significant interaction BUT CRAZY ESTIMATES, DISCARDED
step(gauss.log3) # AIC: -580.4  

estimates <- predict(gauss.log3)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse

gauss.log3.1 <- glm(formula = velocityEnd ~ age + commits + devs + issues + velocityStart + 
                      exposed + age:commits + age:devs + age:issues + age:velocityStart + 
                      age:exposed + commits:devs + commits:issues + commits:velocityStart + 
                      commits:exposed + devs:issues + devs:velocityStart + devs:exposed + 
                      issues:velocityStart + issues:exposed + velocityStart:exposed + 
                      age:commits:devs + age:commits:issues + age:commits:velocityStart + 
                      age:commits:exposed + age:devs:issues + age:devs:velocityStart + 
                      age:devs:exposed + age:issues:velocityStart + age:velocityStart:exposed + 
                      commits:devs:issues + commits:devs:velocityStart + commits:issues:velocityStart + 
                      devs:issues:exposed, family = gaussian(link = "log"), data = scaledData)
summary(gauss.log3.1) # No significant values: DISCARDED

## OVERALL WITH GAUSSIAN LOG: gauss.log2.1 offered the best AIC while many interactions
# denoteed high significance. Howeveer, gauss.log1 offered a reduced version with good AIC
# despite its parameters' significance wasn't high enough.


#-------
# Gaussian Inverse
gauss.inv <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+exposed, family = gaussian(link = 'inverse'), data = scaledData)
summary(gauss.inv) # AIC: -449.61 || some initial significance levels.
step(gauss.inv) # AIC=-502.35 for velocityEnd ~ age model
gauss.inv.1 <- glm(velocityEnd ~ age, family = gaussian(link = 'inverse'), data = scaledData)
summary(gauss.inv.1)
BIC(glm(velocityEnd ~ age, family = gaussian(link = 'inverse'), data = scaledData))
BIC(gauss.inv)
# Indeed, BIC gives better values for the reduced model as it penalized the amount of variables.

estimates <- predict(gauss.inv)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse # 1073.62

# Two way interaction
gauss.inv2 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^2, family = gaussian(link = 'inverse'), data = scaledData)
summary(gauss.inv2) # AIC: 1044.1 || no significance, interaction clearly makes the inverse model worse. DISCARDED

estimates <- predict(gauss.inv2)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse 

gauss.inv3 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^3, family = gaussian(link = 'inverse'), data = scaledData)
summary(gauss.inv3) # AIC: -610.92 || significance in several main and interaction effect variables.

estimates <- predict(gauss.inv3)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse 

step(gauss.inv3)  
gauss.inv3.1 <- glm(formula = velocityEnd ~ age + commits + devs + issues + velocityStart + 
                      exposed + age:commits + age:devs + age:issues + age:velocityStart + 
                      age:exposed + commits:devs + commits:issues + commits:velocityStart + 
                      commits:exposed + devs:issues + devs:velocityStart + devs:exposed + 
                      issues:velocityStart + issues:exposed + velocityStart:exposed + 
                      age:commits:devs + age:commits:issues + age:commits:velocityStart + 
                      age:commits:exposed + age:devs:velocityStart + age:devs:exposed + 
                      age:issues:velocityStart + age:issues:exposed + age:velocityStart:exposed + 
                      commits:devs:velocityStart + commits:devs:exposed + commits:issues:velocityStart + 
                      commits:issues:exposed + commits:velocityStart:exposed + 
                      devs:issues:velocityStart + devs:issues:exposed + devs:velocityStart:exposed + 
                      issues:velocityStart:exposed, family = gaussian(link = "inverse"), 
                    data = scaledData)
summary(gauss.inv3.1) # AIC: -631.5 ||

# Testing the mse values:
estimates <- predict(gauss.inv3.1)
mse <- mean((scaledData$velocityEnd - estimates)^2)
# Unreasonable values... # 184158.7

#OVERALL GAUSSIAN INVERSE: Good AIC values but the MSE error values are extremely 
# unreasonable. At the global comparison stage this will be considered.


#--------------------------
# Gamma
gamma.id <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+exposed, family = Gamma(link = 'identity'), data = scaledData) 
summary(gamma.id) # AIC: -1262.3 Initially better AIC values. As expected small significance
step(gamma.id) # AIC: -1271 for model...
BIC(gamma.id) # -1231.716

estimates <- predict(gamma.id)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse # 0.0128

gamma.id.1 <- glm(formula = velocityEnd ~ age + velocityStart, family = Gamma(link = "identity"), data = scaledData)
summary(gamma.id.1) # AIC: -1271.2
BIC(gamma.id.1) # -1255.929 (Understandable, less variables)

estimates <- predict(gamma.id.1)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse

#---- R squared calculation
# The deviance-based R-squared 
rsq <- with(summary(gamma.id.1), 1 - gamma.id.1$deviance/gamma.id.1$null.deviance) 
rsq # 0.04992138

#  the adjusted deviance-based R-squared
sampleSize <- dim(scaledData)[1]
modelParams <- length(gamma.id.1$coefficients)
rsq_adj <- 1 - ((sampleSize-1)/(sampleSize-modelParams))*(1 - rsq)
rsq_adj # 0.0442152
#--------------

# NOTE: THERE'S A CLEAR PATTERN OF HIGHER SIGNIFICANCE ON age & velocityStart VARIABLES
plot(gamma.id.1) # Pearson residuals used
hist(rstandard(gamma.id.1)) # In fact!! Residuals get normalized despite small skewness
shapiro.test(residuals(gamma.id.1, type = 'pearson')) # Still, completely non-normal 
qqnorm(residuals(gamma.id.1, type = 'pearson')) # Non linearity at all
qqnorm(rstandard(gamma.id.1)) # Kind of normality

estimGamma.id.1 <- predict(gamma.id.1)
qqplot(scaledData$velocityEnd, estimGamma.id.1)
qqplot(gamma.id.1$fitted.values, gamma.id.1$residuals) # Slightly normal (?)

# Two-way interaction.
gamma.id.2 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^2, family = Gamma(link = 'identity'), data = scaledData) 
# No difference in the starting values, so it get's truncated

# Three-way interaction
gamma.id.3 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^3, family = Gamma(link = 'identity'), data = scaledData) 
# Same error

# OVERALL GAMMA ID: Main effect models offer better results, but the interaction effects are not able to converge.

#-------
gamma.log <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+exposed, family = Gamma(link = 'log'), data = scaledData) 
summary(gamma.log) # Slight significance but worse than ID regarding AIC: -1259.2
step(gamma.log) # AIC: -1266 for model...
gamma.log.1 <- glm(formula = velocityEnd ~ age + velocityStart, family = Gamma(link = "log"), data = scaledData)
summary(gamma.log.1)# Some significance is obtained from the variables.

estimates <- predict(gamma.log)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse # 8.4478

estimates <- predict(gamma.log.1)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse

# Two-way interaction
gamma.log2 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^2, family = Gamma(link = 'log'), data = scaledData) 
summary(gamma.log2) # AIC: -1245.9 || No significant interactions but just the one explaining issues:exposure
step(gamma.log2) # AIC: -1261 for model with mainly interactions with issues and exposed

estimates <- predict(gamma.log2)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse # 8.754885

gamma.log2.1 <- glm(formula = velocityEnd ~ age + commits + devs + issues + velocityStart + 
                      exposed + age:issues + commits:issues + devs:exposed + issues:exposed + 
                      velocityStart:exposed, family = Gamma(link = "log"), data = scaledData)
summary(gamma.log2.1) # AIC: -1260.9 the payoff is that there is not too much significance.

# Three-way interaction
gamma.log3 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^3, family = Gamma(link = 'log'), data = scaledData)
summary(gamma.log3) # AIC: -1242.9
step(gamma.log3) # AIC: -1260 for model

estimates <- predict(gamma.log3)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse

gamma.log3.1 <- glm(formula = velocityEnd ~ age + commits + devs + issues + velocityStart + 
                      exposed + age:commits + age:devs + age:issues + age:velocityStart + 
                      age:exposed + commits:devs + commits:issues + commits:velocityStart + 
                      devs:issues + devs:velocityStart + devs:exposed + issues:velocityStart + 
                      velocityStart:exposed + age:commits:devs + age:commits:velocityStart + 
                      age:devs:issues + age:devs:velocityStart + age:devs:exposed + 
                      age:velocityStart:exposed + commits:devs:issues + commits:issues:velocityStart + 
                      devs:issues:velocityStart, family = Gamma(link = "log"), 
                    data = scaledData)
summary(gamma.log3.1) # Good score plus significance in several interactions, estimated 
# paramters are too high though.

# OVERALL GAMMA LOG: Best model appears to be gamma.log.1 with lowest AIC.
#    - Main effects have significance
#    - Still need to try mse 
estim.gammalog.1 <- predict(gamma.log.1)
mse <- mean((scaledData$velocityEnd - estim.gammalog.1)^2)
# More or less reasonable values, still too high

#-------
gamma.inv <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+exposed, family = Gamma(link = 'inverse'), data = scaledData) 
summary(gamma.inv) # AIC: -1254.6 || No, significance
step(gamma.inv) # Does not admit possible combinations

estim.gammalog.1 <- predict(gamma.inv)
mse <- mean((scaledData$velocityEnd - estim.gammalog.1)^2)
mse

# Two way interaction
gamma.inv2 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^2, family = Gamma(link = 'inverse'), data = scaledData) 
# No difference in the starting values, so it get's truncated

# Three way interaction
gamma.inv3 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^3, family = Gamma(link = 'inverse'), data = scaledData) 
# No difference in the starting values, so it get's truncated

# OVERALL GAMMA: Gamma inverse only gives margin to the initial function, no other combination
# is allowed, what gives a bad sense. No significance, middle level AIC and this errors doesn't give
# too much eligibility.

#--------------------------
# Inverse Gaussian
i.gauss.id <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+exposed, family = inverse.gaussian(link = 'identity'), data = scaledData) 
summary(i.gauss.id) # AIC: -1276.2 || Significance in all variables despite exposed
step(i.gauss.id) # AIC: -1278 best model with devs out

estimates <- predict(i.gauss.id)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse # 0.021

i.gauss.id.1 <- glm(formula = velocityEnd ~ age + commits + issues + velocityStart + 
                      exposed, family = inverse.gaussian(link = "identity"), data = scaledData)
summary(i.gauss.id.1) # AIC: -1278 ||  Significance in all variables despite exposed and devs removed

# Printing to latex
print(xtable(summary(i.gauss.id.1, type='latex')))

#---- R squared calculation
# The deviance-based R-squared 
rsq <- with(summary(i.gauss.id.1), 1 - i.gauss.id.1$deviance/i.gauss.id.1$null.deviance) 
rsq # 0.04631214

#  the adjusted deviance-based R-squared
sampleSize <- dim(scaledData)[1]
modelParams <- length(i.gauss.id.1$coefficients)
rsq_adj <- 1 - ((sampleSize-1)/(sampleSize-modelParams))*(1 - rsq)
rsq_adj # 0.03186233
#--------------

estim.i.gauss.id.1 <- predict(i.gauss.id.1)
mse <- mean((scaledData$velocityEnd - estim.i.gauss.id.1)^2)
mse
# 0.01729449 really good mse value!!!

i.gauss.id2 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^3, family = inverse.gaussian(link = 'identity'), data = scaledData) 

# OVERALL INV GAUSSIAN ID: Step gives a model with realistic parameter estimates and the best AIC value
# seen so far. Furthermore, the mse are quite accurate what gives a high predictive value to this model.

#-------
i.gauss.log <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+exposed, family = inverse.gaussian(link = 'log'), data = scaledData) 
summary(i.gauss.log) # AIC: -1265.1 || Some significance...

estimates <- predict(i.gauss.log)
mse <- mean((scaledData$velocityEnd - estimates)^2)
mse

step(i.gauss.log) # Error... cannot correct step size

i.gauss.log2 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^2, family = inverse.gaussian(link = 'log'), data = scaledData)
# cannot correct step size

i.gauss.log3 <- glm(velocityEnd ~ (age+commits+devs+issues+velocityStart+exposed)^3, family = inverse.gaussian(link = 'log'), data = scaledData)
# cannot correct step size

# INVERSE GAUSSIAN LOG: Similar pattern as gamma inverse, errors while estimating
# there's no option to check the best combination.

#-------
i.gauss.inv <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+exposed, family = inverse.gaussian(link = 'inverse'), data = scaledData) 
# Error on doing inverse gaussian inverse


#-------
i.gauss.can <- glm(velocityEnd ~ age+commits+devs+issues+velocityStart+exposed, family = inverse.gaussian(link = '1/mu^2'), data = scaledData) 
# Error doing the inverse gaussian canonical

# OVERALL INVERSE GAUSSIAN: Only the inverse gaussian id gives good values since¨
# the other models cannot converge, given this we start the next stage of model comparison to get the best one.


# MODEL COMPARISON:

models <- list(fit8, gauss.id4, gamma.id.1, i.gauss.id.1)
mod.names <- c('Simplified LM', 'Interaction Gaussian GLM', 'Simplified Gamma GLM'
               , 'Main effect I. Gaussian GLM')
aicmodels <- aictab(cand.set = models, modnames = mod.names)

par(mfrow=c(1,2))
plot(fit8)
plot(gauss.id4)
plot(gamma.id.1)
plot(i.gauss.id.1)

par(mfrow=c(1,1))

# Residuals for Gamma
p.resid.gamma <- residuals(gamma.id.1, type = 'pearson')
pred.gamma <- predict(gamma.id.1)
plot(p.resid.gamma, pred.gamma)

#Residuals for Inverse Gaussian

#--------------------------------------------------------------------------

### BEFORE TRYING DIFFERENT COMBINATIONS OF VARIABLES IN THE MLRs, LET'S TRY
### RANDOM FOREST LINEAR REGRESSION (Combination of linear regression trees)
##############

library(randomForest)
#install.packages('randomForest')

lmRF100 <- randomForest(velocityEnd ~ age + velocityStart, data = scaledData, type = 'regression', ntree = 100)
lmRF200 <- randomForest(velocityEnd ~ age + velocityStart, data = scaledData, type = 'regression', ntree = 200)
lmRF500 <- randomForest(velocityEnd ~ age + velocityStart, data = scaledData, type = 'regression', ntree = 500)
lmRF1000 <- randomForest(velocityEnd ~ age + velocityStart, data = scaledData, type = 'regression', ntree = 1000)
estimates <- predict(lmRF1000)
mseRF <- mean((scaledData$velocityEnd - estimates)^2) 
mseRF

gaussRF100 <- randomForest(velocityEnd ~ age + commits + devs + issues + velocityStart + 
                             exposed + age:commits + age:devs + age:issues + age:velocityStart + 
                             age:exposed + commits:devs + commits:issues + commits:velocityStart + 
                             devs:issues + devs:velocityStart + issues:velocityStart + 
                             velocityStart:exposed + age:commits:devs + age:commits:issues + 
                             age:commits:velocityStart + age:devs:issues + age:issues:velocityStart + 
                             age:velocityStart:exposed + commits:devs:issues + commits:devs:velocityStart + 
                             commits:issues:velocityStart, data = scaledData, type = 'regression', ntree = 100)
gaussRF200 <- randomForest(velocityEnd ~ age + commits + devs + issues + velocityStart + 
                             exposed + age:commits + age:devs + age:issues + age:velocityStart + 
                             age:exposed + commits:devs + commits:issues + commits:velocityStart + 
                             devs:issues + devs:velocityStart + issues:velocityStart + 
                             velocityStart:exposed + age:commits:devs + age:commits:issues + 
                             age:commits:velocityStart + age:devs:issues + age:issues:velocityStart + 
                             age:velocityStart:exposed + commits:devs:issues + commits:devs:velocityStart + 
                             commits:issues:velocityStart, data = scaledData, type = 'regression', ntree = 200)
gaussRF500 <- randomForest(velocityEnd ~ age + commits + devs + issues + velocityStart + 
                             exposed + age:commits + age:devs + age:issues + age:velocityStart + 
                             age:exposed + commits:devs + commits:issues + commits:velocityStart + 
                             devs:issues + devs:velocityStart + issues:velocityStart + 
                             velocityStart:exposed + age:commits:devs + age:commits:issues + 
                             age:commits:velocityStart + age:devs:issues + age:issues:velocityStart + 
                             age:velocityStart:exposed + commits:devs:issues + commits:devs:velocityStart + 
                             commits:issues:velocityStart, data = scaledData, type = 'regression', ntree = 500)
gaussRF1000 <- randomForest(velocityEnd ~ age + commits + devs + issues + velocityStart + 
                             exposed + age:commits + age:devs + age:issues + age:velocityStart + 
                             age:exposed + commits:devs + commits:issues + commits:velocityStart + 
                             devs:issues + devs:velocityStart + issues:velocityStart + 
                             velocityStart:exposed + age:commits:devs + age:commits:issues + 
                             age:commits:velocityStart + age:devs:issues + age:issues:velocityStart + 
                             age:velocityStart:exposed + commits:devs:issues + commits:devs:velocityStart + 
                             commits:issues:velocityStart, data = scaledData, type = 'regression', ntree = 1000)
estimates <- predict(gaussRF1000)
mseRF <- mean((scaledData$velocityEnd - estimates)^2) 
mseRF

# Gamma has the same structure as LM
i.gauss.idRF100 <- randomForest(velocityEnd ~ age + commits + issues + velocityStart + 
                               exposed, data = scaledData, type = 'regression', ntree = 100)
i.gauss.idRF200 <- randomForest(velocityEnd ~ age + commits + issues + velocityStart + 
                                  exposed, data = scaledData, type = 'regression', ntree = 200)
i.gauss.idRF500 <- randomForest(velocityEnd ~ age + commits + issues + velocityStart + 
                                  exposed, data = scaledData, type = 'regression', ntree = 500)
i.gauss.idRF1000 <- randomForest(velocityEnd ~ age + commits + issues + velocityStart + 
                                  exposed, data = scaledData, type = 'regression', ntree = 1000)
estimates <- predict(i.gauss.idRF1000)
mseRF <- mean((scaledData$velocityEnd - estimates)^2) 
mseRF

simple_me <- c(0.01519937, 0.01502404, 0.01511484, 0.01500752)
interaction <- c(0.01418135, 0.01407198, 0.01407432, 0.01390119)
noDevs <- c(0.01370274, 0.01330181, 0.013265, 0.01313228)
mseVals <- cbind(simple_me, interaction, noDevs)
numTrees <- c(100, 200, 500, 1000)

plot(numTrees, simple_me, xlab = 'Number of trees', ylab = 'Models',type = 'b', 
     ylim = c(0.012, 0.016), pch = 19, main = 'MSE values for Random Forest',
     lwd = 4)
text(numTrees, simple_me, round(simple_me, 4), pos=3)
lines(numTrees,interaction, col='red', type = 'b', pch = 19, lwd = 4)
text(numTrees, interaction, round(interaction, 4), pos=3)
lines(numTrees, noDevs, col='darkgreen', type = 'b', pch = 19, lwd = 4)
text(numTrees, noDevs, round(noDevs, 4), pos=3)
#legend(0, 0.016, legend = c('Simple ME model', 'Interaction model', 'No developers ME models'),
#       col=c('black', 'red', 'darkgreen'))
legend(locator(), labels = c('Simple ME model', 'Interaction model', 'No developers ME models'))

# Even though the MSE looks fine, there's an error if the % of Variance explaining ability of this method is negative
# Meanwhile in the MLR the same value Multiple R_sq was only 2.25%
summary(model1RF)

# Model validation

# RF - Mean of squared residuals: 0.01386118
# MLR - Mean of squared residuals: 0.01266318 (MLR is slightly better)

plot(i.gauss.idRF1000) # At least the regression sees its error reduced by the increase of regression trees

VarImp <- randomForest::importance(i.gauss.idRF1000)
VarImp <- as.matrix(VarImp[order(VarImp[,1], decreasing = TRUE),])

varImpPlot(i.gauss.idRF1000, main = "Variable importance explaining Velocity end",
           type = 2, col='red', pch=19, cex= 1.5)
