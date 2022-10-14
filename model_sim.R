# Library ----
library(tidyverse)
library(mlr3verse)
library(plm)
library(pglm)
library(readxl)
library(dplyr)
library('DataCombine')
library('stargazer')
library('simpleboot')
library('Rpdb')
library('data.table')
library(ggplot2)
library('zoo')
library('fixest')
library('timeDate')
library('DescTools')
library('summarytools')
library(gtools)
library(lubridate)
library('fuzzyjoin')
library(readr)
library('ggpubr')
library(marginaleffects)
library('timeperiodsR')
library('synthdid')
library('scales')
library('fitdistrplus')
library('extraDistr')
library('evd')

t <- data.frame(v1 = abs(t1[[4]]$residuals), v2 = sf3[t1[[4]]$obs_selection$obsRemoved,]$size)
feols(v1 ~ v2, aggregate(v1 ~ v2, t, mean))

# Plotting the distribution
ggplot(data = data.frame(x=c(0,20)), aes(x = x)) + 
  stat_function(fun = dfrechet, args = list(lambda = .53, sigma = .358))


# initialize model parameters
  # the standard deviation of the random variable
  r1 <- feols(log(PPP) ~ size, sf3)
  plotdist(exp(r1$residuals), histo=TRUE)
  t <- fitdist(exp(r1$residuals), "frechet", start = list(lambda = 1, mu = 0, sigma = 1)); plot(t); t$loglik # sd of e = 2.286
  t <- fitdist(r1$residuals, "gumbel", start=list(mu = 0, sigma = 1)); plot(t); t$loglik # sd of e = 2.286
  
  #the mean and spread of size
  r2 <-na.omit(unique(sf3[,c('RSSD','size')])$size)
  plotdist(as.numeric(r2))
  t <- fitdist(as.numeric(r2), 'gamma')
  
  #the mean and spread of ra ratio
  r3 <-na.omit(unique(sf3[,c('RSSD','reserve_asset_ratio')])$reserve_asset_ratio)
  plotdist(as.numeric(r3))
  t <- fitdist(as.numeric(r2), 'weibull'); plot(t); t