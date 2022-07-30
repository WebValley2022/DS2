# Causal Data Science methods for Assessing the Effects of Environmental Policies on Air Pollution Levels
# Hands-On Laboratory

rm(list=ls())
library(cobalt)
library(ggplot2)
library(MatchIt)
library(randomForest)

setwd("/Users/falco/Desktop/FBK - Causal Data Science/Data")
load("simplified_data_zigler.RData")

colnames(data_zigler)

confounder_names = c("PctUrban", "PctBlack", "PctHisp",
                  "PctHighSchool", "MedianHHInc", "PctPoor", "PctFemale",
                  "PctOccupied", "PctMovedIn5", "MedianHValue", "logpop",
                  "smokerate2000", "avgdewpt", "avgtemp", "avgrelhum")

N <- nrow(data_zigler)
N

Nc <- sum(data_zigler$a == 0)
Nt <- sum(data_zigler$a == 1)

## Potential Outcomes Imputation

# Imputing from the Empirical Distributions
data_zigler$impute = rep(NA,N)
data_zigler$impute[data_zigler$a == 0] <- sample(data_zigler$pm2.5[data_zigler$a == 1], Nc, replace=T) 
data_zigler$impute[data_zigler$a == 1] <- sample(data_zigler$pm2.5[data_zigler$a == 0], Nt, replace=T)

Y1 = c(data_zigler$pm2.5[data_zigler$a == 1], data_zigler$impute[data_zigler$a == 0])
Y0 = c(data_zigler$pm2.5[data_zigler$a == 0], data_zigler$impute[data_zigler$a == 1])

mean_diff_emp = mean(Y1)-mean(Y0)
median_diff_emp = median(Y1)-median(Y0)
mean_diff_emp
median_diff_emp

mean_tau_emp=c() 
median_tau_emp=c()

for(i in 1:10000){
  data_zigler$impute = rep(NA,N)
  data_zigler$impute[data_zigler$a == 0] <- sample(data_zigler$pm2.5[data_zigler$a == 1], Nc, replace=T)
  data_zigler$impute[data_zigler$a == 1] <- sample(data_zigler$pm2.5[data_zigler$a == 0], Nt, replace=T)
  
  Y1 = c(data_zigler$pm2.5[data_zigler$a == 1], data_zigler$impute[data_zigler$a == 0])
  Y0 = c(data_zigler$pm2.5[data_zigler$a == 0], data_zigler$impute[data_zigler$a == 1])
  
  mean_tau_emp = c(mean_tau_emp, mean(Y1)-mean(Y0))
  median_tau_emp=c(median_tau_emp, median(Y1)-median(Y0))
}

mean(mean_tau_emp)
quantile(mean_tau_emp, probs = c(0.025,0.5, 0.975))

mean(median_tau_emp)
quantile(median_tau_emp, probs = c(0.025,0.5, 0.975))


# Imputing from Normal Distributions
data_zigler$impute = rep(NA,N)
data_zigler$impute[data_zigler$a == 0] <- rnorm(Nc, mean(data_zigler$pm2.5[data_zigler$a == 1]), sd(data_zigler$pm2.5[data_zigler$a == 1]))
data_zigler$impute[data_zigler$a == 1] <- rnorm(Nt, mean(data_zigler$pm2.5[data_zigler$a == 0]), sd(data_zigler$pm2.5[data_zigler$a == 0]))

Y1 = c(data_zigler$pm2.5[data_zigler$a == 1], data_zigler$impute[data_zigler$a == 0])
Y0 = c(data_zigler$pm2.5[data_zigler$a == 0], data_zigler$impute[data_zigler$a == 1])

mean_diff_normal = mean(Y1)-mean(Y0)
median_diff_normal = median(Y1)-median(Y0)
mean_diff_normal
median_diff_normal

mean_tau_normal=c()
median_tau_normal=c()

for(i in 1:10000){
  data_zigler$impute = rep(NA,N)
  data_zigler$impute[data_zigler$a == 0] <- rnorm(Nc, mean(data_zigler$pm2.5[data_zigler$a == 1]), sd(data_zigler$pm2.5[data_zigler$a == 1]))
  data_zigler$impute[data_zigler$a == 1] <- rnorm(Nt, mean(data_zigler$pm2.5[data_zigler$a == 0]), sd(data_zigler$pm2.5[data_zigler$a == 0]))
  
  Y1 = c(data_zigler$pm2.5[data_zigler$a==1],data_zigler$impute[data_zigler$a==0])
  Y0 = c(data_zigler$pm2.5[data_zigler$a==0],data_zigler$impute[data_zigler$a==1])
  
  mean_tau_normal = c(mean_tau_normal, mean(Y1)-mean(Y0))
  median_tau_normal = c(median_tau_normal, median(Y1)-median(Y0))
}

mean(mean_tau_normal)
quantile(mean_tau_normal, probs = c(0.025,0.5, 0.975))

mean(median_tau_normal)
quantile(median_tau_normal, probs = c(0.025,0.5, 0.975))

# Model Statement 
model_fit <- as.formula(paste("pm2.5 ~ a +", paste(confounder_names, collapse = "+", sep = "")))

# Single Linear Regression
fit = lm(model_fit, data = data_zigler) 
summary(fit)
average_effect = summary(fit)$coef['a',1] 
average_effect


# Missing Potential Outcome Imputation Via Regression
data_zigler1 = data_zigler
data_zigler1[,'a'] = 1 - data_zigler1[,'a']

predictY<-predict(fit, newdata=data_zigler1) 
sigmaY <- summary(fit)$sigma

mean_tau_cov=c()
median_tau_cov=c()

for(i in 1:10000){
  data_zigler$impute = rep(NA, N)
  data_zigler$impute = rnorm(N, predictY, sigmaY)
  
  Y1 = c(data_zigler$pm2.5[data_zigler$a == 1], data_zigler$impute[data_zigler$a == 0])
  Y0 = c(data_zigler$pm2.5[data_zigler$a == 0], data_zigler$impute[data_zigler$a == 1])
  
  mean_tau_cov = c(mean_tau_cov, mean(Y1) - mean(Y0))
  median_tau_cov = c(median_tau_cov, median(Y1) - median(Y0))
}

mean(mean_tau_cov)
quantile(mean_tau_cov, probs = c(0.025,0.5, 0.975))

mean(median_tau_cov)
quantile(median_tau_cov, probs = c(0.025,0.5, 0.975))

# Two Models Imputation of the Potential Outcomes
model_fit1 <- as.formula(paste("pm2.5 ~", paste(confounder_names, collapse = "+", sep = "")))
fit1 <- lm(model_fit1, data=data_zigler[data_zigler$a == 1, ])
fit0 <- lm(model_fit1, data=data_zigler[data_zigler$a == 0, ])

predictY1 <- predict(fit1, newdata=data_zigler[data_zigler$a == 0, ])
predictY0 <- predict(fit0, newdata=data_zigler[data_zigler$a == 1, ])

sigmaY1 <- summary(fit1)$sigma
sigmaY0 <- summary(fit0)$sigma

mean_tau_cov=c()
median_tau_cov=c()

for(i in 1:10000){
  data_zigler$impute = rep(NA,N)
  data_zigler$impute[data_zigler$a == 0] = rnorm(Nc, predictY1, sigmaY1)
  data_zigler$impute[data_zigler$a == 1] = rnorm(Nt, predictY0, sigmaY0)
  
  Y1 = c(data_zigler$pm2.5[data_zigler$a == 1], data_zigler$impute[data_zigler$a == 0])
  Y0 = c(data_zigler$pm2.5[data_zigler$a == 0], data_zigler$impute[data_zigler$a == 1])
  
  mean_tau_cov = c(mean_tau_cov, mean(Y1)-mean(Y0))
  median_tau_cov = c(median_tau_cov, median(Y1)-median(Y0))
}

mean(mean_tau_cov)
quantile(mean_tau_cov, probs = c(0.025,0.5, 0.975))

mean(median_tau_cov)
quantile(median_tau_cov, probs = c(0.025,0.5, 0.975))


# Random Forest
rf <- randomForest(model_fit1, data = data_zigler)
fitted_rf1 <- predict(rf, data_zigler[data_zigler$a == 1, ],  type="response")
fitted_rf0 <- predict(rf, data_zigler[data_zigler$a == 0, ],  type="response")

mean_tau_rf=c()
median_tau_rf=c()

for(i in 1:10000){
  data_zigler$impute = rep(NA,N)
  data_zigler$impute[data_zigler$a == 0] = sample(fitted_rf1, Nt, replace=T)
  data_zigler$impute[data_zigler$a == 1] = sample(fitted_rf0, Nc, replace=T)
  
  Y1 = c(data_zigler$pm2.5[data_zigler$a == 1], data_zigler$impute[data_zigler$a == 0])
  Y0 = c(data_zigler$pm2.5[data_zigler$a == 0], data_zigler$impute[data_zigler$a == 1])
  
  mean_tau_rf = c(mean_tau_cov, mean(Y1)-mean(Y0))
  median_tau_rf= c(median_tau_cov, median(Y1)-median(Y0))
}

mean(mean_tau_rf)
quantile(mean_tau_rf, probs = c(0.025,0.5, 0.975))

mean(median_tau_rf)
quantile(median_tau_rf, probs = c(0.025,0.5, 0.975))


# 1 to 1 Matching
formula_matching <- as.formula(paste("a ~", paste(confounder_names, collapse = "+", sep = "")))
mnrp <- matchit(formula_matching, data = data_zigler, method = "nearest",  replace = FALSE, caliper = 0.1)
summary(mnrp)

cov_bal_plots <- love.plot(mnrp, drop.distance = TRUE, 
                           var.order = "unadjusted",
                           abs = TRUE,
                           line = TRUE, 
                           thresholds = c(m = .1))  +
  ggtitle("Covariate Balance")
cov_bal_plots
data_zigler$match.weights <- mnrp$weights

matched.data.nrp <- match.data(mnrp)

#Estimate treatment effects
model_summary_1.1 <- summary(glm(model_fit,
                             data = matched.data.nrp,
                             weights = weights), robust = "HC1", digits = 5)
model_summary_1.1


# Propensity Score Matching with Replacement
m <- matchit(formula_matching, data = data_zigler, method = "nearest",  replace = TRUE, caliper = 0.1)
    
cov_bal_plots <- love.plot(m, drop.distance = TRUE, 
                           var.order = "unadjusted",
                           abs = TRUE,
                           line = TRUE, 
                            thresholds = c(m = .1))  +
ggtitle("Covariate Balance")
cov_bal_plots
  
data_zigler$match.weights <- m$weights
  
#Estimate treatment effects
model_summary <- summary(glm(pm2.5 ~ a + x,
                            data = data_zigler[data_zigler$match.weights > 0,],
                            weights = match.weights), robust = "HC1", digits = 5)
model_summary


