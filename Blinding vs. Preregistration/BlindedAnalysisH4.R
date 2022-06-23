# Analysis conducted on the blinded version of the data
# 
# Hypothesis 4: Analysis teams in the preregistration condition should 
# (1) deviate more often from their planned analysis than analysis teams 
# in the blinding condition and (2) when they deviate from their analysis plan, 
# analysis teams in the preregistration condition should deviate on more items 
# than analysis teams in the blinding condition. We will test this hypothesis 
# against the null hypothesis that both groups (1) deviate the same number of 
# times from their analysis plan and (2) when they deviate from their analysis 
# plan they do so for the same number of items.

#setwd('...') set working directory
rm(list=ls())
library(rethinking)
library(rstan)

fixed_seed <- 2021 # set seed for reproducibility purposes

datafile <- '../../data/data_prereg_blinding_blinded.csv' # specify correct path
dat <- read.csv(file = datafile, header=T)

## Stan Model
  dat$cond <- as.numeric(as.factor(dat$condition)) - 1
  datalist <- list(N = nrow(dat), 
                   y = dat$deviation_count)
  
  m0 <- stan(file ="h4_m0.stan",
             data = datalist,
             seed = fixed_seed,
             iter = 10000,
             warmup = 1000,
             cores = 4)
  
  datalist <- list(N = nrow(dat),
                   y = dat$deviation_count,
                   x = dat$cond)
  m1 <- stan(file ="h4_m1.stan",
             data = datalist,
             seed = fixed_seed,
             iter = 10000,
             warmup = 1000,
             cores = 4)
  
  set.seed(fixed_seed)
  m0_bridge <- bridgesampling::bridge_sampler(m0, seed = fixed_seed)
  set.seed(fixed_seed)
  m1_bridge <- bridgesampling::bridge_sampler(m1, seed = fixed_seed)
  
  bf_10 <- bridgesampling::bf(m1_bridge, m0_bridge)
  
  samp <- rstan::extract(m1)
  post <- mean(samp$bp<0 & samp$bl>0)
  prior <- 0.5^2
  
  bf_re <- post/prior 
  bf_r0 <- bf_10$bf * bf_re

## Results 
  bf_r0
  mean(dat$deviation_count[dat$condition=='blinding' & dat$deviation_count > 0])
  mean(dat$deviation_count[dat$condition=='preregistration' & dat$deviation_count > 0])
  sum(dat$deviation_count[dat$condition == 'blinding'] == 0)
  sum(dat$deviation_count[dat$condition == 'preregistration'] == 0)
  
  p_nodeviation_prereg <- logistic(median(samp[['ap']]) + median(samp[['bp']])) # probability of deviation in preregistration condition
  p_nodeviation_blinding <- logistic(median(samp[['ap']])) # probability of deviation in blinding condition
  m_deviations_prereg <- exp(median(samp[['al']]) + median(samp[['bl']])) # mean number of deviations when deviated in preregistration condition
  m_deviations_blinding <- exp(median(samp[['al']])) # mean number of deviations when deviated in blinding condition
  
  p_nodeviation_prereg
  p_nodeviation_blinding
  m_deviations_prereg
  m_deviations_blinding
  