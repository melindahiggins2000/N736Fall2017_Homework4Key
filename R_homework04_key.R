# ===================================
# Homework 4 Answer Key
#
# by Melinda Higgins, PhD
# dated 10/30/2018
# ===================================

library(tidyverse)
library(haven)

helpdat <- haven::read_spss("helpmkh.sav")

# create subset
# select indtot, cesd and racegrp

h1 <- helpdat %>%
  select(indtot, cesd, racegrp)

# create a "factor" class
# variable for racegrp

h1$racegrp.f <- as.factor(h1$racegrp)

table(h1$racegrp.f)

# run simple linear regression
# using the lm
# save the results in the fit1 object
fit1 <- lm(cesd ~ indtot, data=h1)

# look at a summary() of the model
summary(fit1)

# get the ANOVA for this model
# this gives you TYPE I Sums of Squares
anova(fit1)

# get diagnostic plots
par(mfrow=c(2,2))
plot(fit1)

# reset par
par(mfrow=c(1,1))

# histogram of the residuals
hist(fit1$residuals)

# see the coefficients
coefficients(fit1)

# use the car package to get additional diagnostics
# and plotting options
library(car)

# Durbin-Watson Test for Autocorrelated Errors
car::durbinWatsonTest(fit1)

# component plus residual plots
# these are also called partial residual plots
car::crPlots(fit1)

# homoscedasticity
# look for non-constant error variance
car::ncvTest(fit1)

# also look at the spreadLevelPlot
# this also provides a suggestion of 
# possible power transformation
car::spreadLevelPlot(fit1)

# this suggests a power transformation
# of 1.639, almost 2, Ynew = Y^2
plot(h1$indtot, h1$cesd)
abline(lm(h1$cesd ~ h1$indtot))

plot(h1$indtot, h1$cesd^2)
abline(lm(h1$cesd^2 ~ h1$indtot))

# scatterplot of fitted model
# using the car package, add the smooth option
car::scatterplot(cesd ~ indtot, data=h1, 
                 smooth=TRUE)

# you can also use a ggplot2 approach
ggplot(h1, aes(indtot, cesd)) +
  geom_point() +
  stat_smooth(method = lm)

# global test of linear model assumptions
# install gvlma package
library(gvlma)
gvmodel <- gvlma::gvlma(fit1)
summary(gvmodel)

# try with the power transformation
# less assumptions acceptable - don't
# do the transformation
fit1t <- lm(h1$cesd^2 ~ h1$indtot, data=h1)
gvlma::gvlma(fit1t)
par(mfrow=c(2,2))
plot(fit1t)
par(mfrow=c(1,1))

# one-way ANOVA
# we can use the lm() function
# it does "dummy" coding on the fly
# run racegrp as either the character
# type or as a factor - either will work
fit2.lm <- lm(cesd ~ racegrp, data=h1)
summary(fit2.lm)

fit2.lm <- lm(cesd ~ racegrp.f, data=h1)
summary(fit2.lm)

# the aov() function
# gives the global test for the "group" effect
fit2.aov <- aov(cesd ~ racegrp, data=h1)
summary(fit2.aov)

# get a means plot using
# plotmeans() from gplots package
library(gplots)
gplots::plotmeans(cesd ~ racegrp, 
                  data=h1)

# post hoc tests
# Tukey HSD
TukeyHSD(fit2.aov)

# another plot of the 95% CI
# for the pairwise comparisons
#rotate labels
par(las=2) 
# modify margins to get labels inside margins
#par(mar=c(5,12,4,2))
plot(TukeyHSD(fit2.aov))

# alternate post hoc tests in r
# see https://stats.idre.ucla.edu/r/faq/how-can-i-do-post-hoc-pairwise-comparisons-in-r/ 

# no error rate adjustment for 
# multiple pairwise comparisons
# no error-rate correction
pairwise.t.test(h1$cesd, h1$racegrp, p.adj = "none")

# using Bonferroni error-rate correction
pairwise.t.test(h1$cesd, h1$racegrp, p.adj = "bonf")

# using the Holm error-rate correction
pairwise.t.test(h1$cesd, h1$racegrp, p.adj = "holm")

# assess ANOVA test assumptions
# the simulate options
# if TRUE will calculate a 
# confidence interval by parametric bootstrap
car::qqPlot(fit2.aov,
            simulate=TRUE)

# barlett's test for homogenity of variances
# note: put the formula back in
bartlett.test(cesd ~ racegrp, data=h1)

# outlier test from the car package
car::outlierTest(fit2.aov)
