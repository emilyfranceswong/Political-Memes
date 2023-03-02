rm(list = ls(all.names = TRUE))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
# Loading in necessary packages:
library(readxl)
library(nlme)
library(lme4)
library(mediation)
library(lavaan) # results reported in manuscript used lavaan_0.6-5
meme <- read_excel("political_memes.xlsx")
View(meme)

# Organizing variables:
political4 <- as.factor(meme$political_self)
meme$political2 <- as.factor(ifelse(political4==1,'conservative',ifelse(political4==2,'conservative',ifelse(political4==3,'liberal',ifelse(political4==4,'liberal',NA)))))
meme$meme <- as.factor(meme$meme)
meme$congruity <- ifelse(meme$political2==meme$meme,1,0)
congruity <- meme$congruity
meme$share <- ifelse(meme$share==1,1,0)
meme$share <- ordered(meme$share)

# Mean centering  variables for interpretability:
meme$age <- meme$age - mean(meme$age)
meme$funny_scale <- meme$funny_scale - mean(meme$funny_scale)
meme$understand <- meme$understand - mean(meme$understand)
meme$aptness <- meme$aptness - mean(meme$aptness)
meme$surprise <- meme$surprise - mean(meme$surprise)
meme$familiar <- meme$familiar - mean(meme$familiar)
meme$relatable <- meme$relatable - mean(meme$relatable)
meme$interact <- meme$aptness*meme$surprise

SEM <- '
agree ~ d1*congruity
relatable ~ d2*agree + d11* congruity
aptness ~ d3*relatable + d21*agree + d12* congruity
funny_scale ~ a1 * aptness + aw*interact + d31*relatable + d22*agree + d13* congruity
understand ~ a2 * aptness + d32*relatable + d23*agree + d14* congruity
share ~ d15*congruity + d24*agree + d33*relatable + a*aptness + w*interact + b1*funny_scale + b2*understand

indirect1 := d1*d2*d3*(a1+0*aw)*b1
indirect2 := d1*d2*d3*a2*b2
total_ind := indirect1 + indirect2
direct := d15
'
fit <- lavaan::sem(model = SEM, data = meme, se = "robust.cluster", bootstrap = 5000,cluster="subject")
summary(fit,fit.measures=TRUE, rsq=TRUE)
parameters <- lavaan::parameterEstimates(fit, boot.ci.type = "percentile")


