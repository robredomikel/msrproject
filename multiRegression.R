library(plyr)
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
sq_nonsq <- cohortData$SQ.NonSQ
age <- cohortData$age
commits <- cohortData$commits
devs <- cohortData$devs
issues <- cohortData$overall_issues
velocityStart <- cohortData$start_velocity_mean_days

analysisData <- data.frame(velocityEnd, age, commits, devs, issues, velocityStart)

summary(analysisData)


#################################
# ANALYSIS WITH RAW DATA
#################################
# Normalizing the data (If we wanrt to performed the scaled analysis)


boxplot(analysisData, main='nonNormalized')
multi.hist(analysisData)

# If we want to check for correlation
correlations <- cor(analysisData)
corrplot(correlations, method = 'pie', type = 'lower')
# Low correlation, but still the nost related ones could be "age", "velocityStart" and perhaps "commits"


### Model fitting (FIRST WITH ALL THE ASSUMED VARIABLES)

model1 <- lm(velocityEnd ~ factor(sq_nonsq)+age+commits+devs+issues+velocityStart, data = analysisData)
model1

summary(model1)
print(xtable(model1, type='latex'))
# Not a single explanatory variable seems to be statistically significant.

# Model validity
AIC(model1) # Later going to compare different models

estimates1 <- predict(model1)
mse1 <- mean((analysisData$velocityEnd - estimates1)^2)

# I wouldn't consider doing a regression with the raw data

plot(analysisData$velocityEnd, estimates1, xlab = "values", ylab = "estimates")
abline(a=0,b=1)

par(mfrow=c(2,2))
plot(model1)

par(mfrow=c(1,1))
plot(model1$residuals)

#######################################
# ANALYSIS WITH NORMALIZED DATA
#######################################
# (From here onwards all the analysis is done with the data standardized)

# Normalizing the data (If we wanrt to performed the scaled analysis)
max_data <- apply(analysisData, 2, max)
min_data <- apply(analysisData, 2, min)

normalizedData <- as.data.frame(scale(analysisData, center = min_data, scale = max_data - min_data))

# Testing normality on the dependent variable
shapiro.test(normalizedData$velocityEnd) # Non normally distributed
# It can be clearly seen from the plots that even with the data normalized the distributions
# present clear skewness

summary(normalizedData)
par(mfrow=c(1,1))
boxplot(normalizedData, main='normalized')
boxplot(analysisData, main='nonNormalized')

# Multihistogram
multi.hist(normalizedData) 
# The response variable behaves in a skewed way, maybe we could try Gamma distribution

# If we want to check for correlation
correlations <- cor(normalizedData)
corrplot(correlations, method = 'pie', type = 'lower')
# Low correlation, but still the nost related ones could be "age", "velocityStart" and perhaps "commits"

normalizedData$sqNonsq <- factor(sq_nonsq)

### Model fitting (FIRST WITH ALL THE ASSUMED VARIABLES)

model1N <- lm(velocityEnd ~ . , data = normalizedData)
model1N

summary(model1N)
print(xtable(summary(model1N, type='latex')))
# Not a single explanatory variable seems to be statistically significant.

# Testing normality of the model
shapiro.test(residuals(model1N, type = 'pearson')) # Still, residuals aren't normally distributed

# Model validity
AIC(model1N) # Later going to compare different models

estimates1N <- predict(model1N)
mse1N <- mean((normalizedData$velocityEnd - estimates1N)^2)

plot(normalizedData$velocityEnd, estimates1N, xlab = "values", ylab = "estimates")
abline(a=0,b=1)

par(mfrow=c(2,2))
plot(model1N)

# From the residuals vs fitted plot we could get to see a linear pattern even though most of
# the residuals are negative and tend to be negative when the fitted values increase.
# Hence may say that the a linear model fits well the data.

# The qq plot shows the model is able to predict the system with good approximation,
# but at the same time it's not the best.

par(mfrow=c(1,1))
plot(model1N$residuals)



### BEFORE TRYING DIFFERENT COMBINATIONS OF VARIABLES IN THE MLRs, LET'S TRY
### RANDOM FOREST LINEAR REGRESSION (Combination of linear regression trees)
##############


#install.packages('randomForest')

model1RF <- randomForest(velocityEnd ~ . , data = normalizedData)
model1RF

estimates1RF <- predict(model1RF)

mseRF <- mean((normalizedData$velocityEnd - estimates1RF)^2)

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

########################
# So far we have seen that the best regression comes from the linear model
# performed with the data normalized. I'll check now how different glm models
# behave

#######################################################

# Special section to manually modlify data values by 0.01 (ONLY FOR GAMMA AND GLM MODELS)
velocityEndMOD <- normalizedData$velocityEnd + 0.01
ageMOD <- normalizedData$age + 0.01
commitsMOD <- normalizedData$commits + 0.01
devsMOD <- normalizedData$devs + 0.01
issuesMOD <- normalizedData$issues + 0.01
velocityStartMOD <- normalizedData$velocityStart + 0.01

normalizedDataMOD <- data.frame(velocityEndMOD, sq_nonsq, ageMOD, commitsMOD, devsMOD, issuesMOD, velocityStartMOD)

summary(normalizedDataMOD)

#######################################################


# We can see that the glm with the Gaussian family (dependent vairiable normally distributed performs the same regression)

# Identity
glmNormal <- glm(normalizedDataMOD$velocityEnd ~ factor(normalizedDataMOD$sq_nonsq) 
                 + normalizedDataMOD$age + normalizedDataMOD$commits + normalizedDataMOD$devs 
                 + normalizedDataMOD$issues + normalizedDataMOD$velocityStart, family = gaussian(link = 'identity'))
glmNormal


estimatesIden <- predict(glmNormal)
mseGlmIden <- mean((normalizedDataMOD$velocityEndMOD - estimatesIden)^2)

# Logarithmic
glmNormalLog <- glm(normalizedDataMOD$velocityEnd ~ factor(cohortData$SQ.NonSQ) 
                 + normalizedDataMOD$age + normalizedDataMOD$commits + normalizedDataMOD$devs 
                 + normalizedDataMOD$issues + normalizedDataMOD$velocityStart, family = gaussian(link = 'log'))

estimatesLog <- predict(glmNormalLog)
mseGlmLog <- mean((normalizedDataMOD$velocityEndMOD - estimatesLog)^2)

# Inverse Gaussian
glmNormalInv <- glm(normalizedDataMOD$velocityEnd ~ factor(cohortData$SQ.NonSQ) 
                    + normalizedDataMOD$age + normalizedDataMOD$commits + normalizedDataMOD$devs 
                    + normalizedDataMOD$issues + normalizedDataMOD$velocityStart, family = gaussian(link = 'inverse')) # The algorithm doesn't converge, but the regression was completed

estimatesInv <- predict(glmNormalInv)
mseGlmInv <- mean((normalizedDataMOD$velocityEndMOD - estimatesInv)^2)

# Exponential
glmNormalExp <- glm(normalizedDataMOD$velocityEnd ~ factor(cohortData$SQ.NonSQ) 
                    + log(normalizedDataMOD$age) + log(normalizedDataMOD$commits) + log(normalizedDataMOD$devs) 
                    + log(normalizedDataMOD$issues) + log(normalizedDataMOD$velocityStart), family = gaussian(link = 'inverse'))

estimatesExp <- predict(glmNormalExp)
mseGlmExp <- mean((normalizedDataMOD$velocityEndMOD - estimatesExp)^2)


########################
# Trying Gamma distribution

glmGammaIden <- glm(normalizedDataMOD$velocityEnd ~ factor(cohortData$SQ.NonSQ) 
                    + normalizedDataMOD$age + normalizedDataMOD$commits + normalizedDataMOD$devs 
                    + normalizedDataMOD$issues + normalizedDataMOD$velocityStart, family = Gamma(link = 'identity'), data = normalizedDataMOD)

estimGammaI <- predict(glmGammaIden)
mseGammaI <- mean((normalizedDataMOD$velocityEndMOD - estimGammaI)^2)

glmGammaLog <- glm(normalizedDataMOD$velocityEnd ~ factor(cohortData$SQ.NonSQ) 
                    + normalizedDataMOD$age + normalizedDataMOD$commits + normalizedDataMOD$devs 
                    + normalizedDataMOD$issues + normalizedDataMOD$velocityStart, family = Gamma(link = 'log'))

estimGammaLog <- predict(glmGammaLog)
mseGammaLog <- mean((normalizedDataMOD$velocityEndMOD - estimGammaLog)^2)

glmGammaInv <- glm(normalizedDataMOD$velocityEnd ~ factor(cohortData$SQ.NonSQ) 
                    + normalizedDataMOD$age + normalizedDataMOD$commits + normalizedDataMOD$devs 
                    + normalizedDataMOD$issues + normalizedDataMOD$velocityStart, family = Gamma(link = 'inverse'))

estimGammaInv <- predict(glmGammaInv)
mseGammaInv <- mean((normalizedDataMOD$velocityEndMOD - estimGammaInv)^2)


## TABLE OF MODEL VALIDATION THROUGH AKAIKE CRITERIA

# Multivariate Linear Model
models <- list(model1, model1N)

mod.names <- c('raw data LM', 'normalized LM')

mvlmodels <- aictab(cand.set = models, modnames = mod.names)

# Multivariate Generalized Linear Models
modelsGLM <- list(glmNormal, glmNormalLog, glmNormalInv, glmNormalExp, glmGammaIden, glmGammaLog, glmGammaInv)

mod.namesGLM <- c('glm-NormId', 'glm-NormLog', 'glm-NormInv', 'glm-NormalExp', 'glm-GammaId', 'glm-GammaLog', 'glm-GammaInv')

glmmodels <- aictab(cand.set = modelsGLM, modnames = mod.namesGLM)

print(xtable(mvlmodels, type='latex'))

modelsAIC <- rbind(mvlmodels, glmmodels) # List type

# AIC and mse vals combined
mseVals <- c(mse1N, mse1, mseGammaI, mseGammaLog, mseGammaInv, mseGlmExp, mseGlmIden, mseGlmLog, mseGlmInv)

modelValidation <- data.frame(modelsAIC$Modnames, modelsAIC$AICc, 
                              mseVals)

rfVals <- c('model1RF', NA, mseRF)

modelValidation <- rbind(modelValidation, rfVals)

print(xtable(modelValidation, type='latex'))


####################################################
# REPORTING THE BEST REGRESSION (Gamma distribution, identity link)
####################################################

summary(glmGammaIden)
print(xtable(summary(glmGammaIden, type='latex')))
plot(normalizedDataMOD$velocityEnd, estimGammaI, xlab = "values", ylab = "estimates")
abline(a=0,b=1)

par(mfrow=c(2,2))
plot(glmGammaIden)

par(mfrow=c(1,1))
plot(glmGammaIden$residuals)


####################################################
# CHECKING THE EFFECT SIZE OF THE REGRESSION (Gamma distribution, identity link)
####################################################

#install.packages('effectsize')
library(effectsize)

normalizedDataMOD$sq_nonsq <- cohortData$SQ.NonSQ

glmGammaIden <- glm(normalizedDataMOD$velocityEnd ~ factor(normalizedDataMOD$sq_nonsq) 
                    + normalizedDataMOD$age + normalizedDataMOD$commits + normalizedDataMOD$devs 
                    + normalizedDataMOD$issues + normalizedDataMOD$velocityStart, family = Gamma(link = 'identity'), data = normalizedDataMOD)

# effect-size measure for the provided input model
effectsize(glmGammaIden)


# There are no clear statistics defined for GLM in particular, since they since Rsq is
# normally calculated through OLS. But based on the general use, Rsq can be also calcualted
# through the deviance of the implemented model and thus explain how the model
# can explain the variance of the dependent variable in this model.

# The deviance-based R-squared 
rsq <- with(summary(glmGammaIden), 1 - glmGammaIden$deviance/glmGammaIden$null.deviance) 
# 0.06160499

#  the adjusted deviance-based R-squared
sampleSize <- dim(normalizedDataMOD)[1]
modelParams <- length(glmGammaIden$coefficients)
rsq_adj <- 1 - ((sampleSize-1)/(sampleSize-modelParams))*(1 - rsq)
# 0.0444914


