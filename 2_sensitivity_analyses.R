# Study: Border biosecurity interceptions for air passengers – assessing intervention methods and analytic tools
#
# Code authored by: Nicholas Moran, CEBRA, University of Melbourne
#
# Date: May 2024



#Loading required packages:
# - data processing
library(tidyverse); library(data.table); library(lubridate); library(xtable)
# - modelling
library(lme4); library(lmerTest);  library(car); library(performance); library(emmeans)
#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(brms); library(cmdstanr); library(MASS)



#### 1. Main analysis models (glmer) ####

#Preparing data set - 
Pass_BAS_dat_processed <- read.csv('~/CEBRA_AirInterventions/Pass_BAS_dat_processed_danon.csv')

# - Removing flights with extremely extremely high passenger counts (n=26)
#nrow(subset(Pass_BAS_dat_processed, PassengerCount >= 500))
Pass_BAS_dat_processed.mod <- subset(Pass_BAS_dat_processed, PassengerCount <= 500)

# - Removing one entry with the bag search count missing and one over 1000. 
Pass_BAS_dat_processed.mod <- subset(Pass_BAS_dat_processed.mod, BagSearchCount != 'NA')
Pass_BAS_dat_processed.mod <- subset(Pass_BAS_dat_processed.mod, BagSearchCount <= 1000)

# - Excluding Bridport, as low numbers caused issues estimating marginal means. 
Pass_BAS_dat_processed.mod <- subset(Pass_BAS_dat_processed.mod, Location != "Airport_G")

# - Sqrt-transforming and Z-scaling count variables.
Pass_BAS_dat_processed.mod$sqrt.BagSearchCount <- sqrt(Pass_BAS_dat_processed.mod$BagSearchCount)
Pass_BAS_dat_processed.mod$sqrt.PassengerCount <- sqrt(Pass_BAS_dat_processed.mod$PassengerCount)
Pass_BAS_dat_processed.mod$sqrt.BagSearchCount.Z <- scale(Pass_BAS_dat_processed.mod$sqrt.BagSearchCount)
Pass_BAS_dat_processed.mod$sqrt.PassengerCount.Z <- scale(Pass_BAS_dat_processed.mod$sqrt.PassengerCount)


# - Final counts for summary data
nrow(Pass_BAS_dat_processed) #59917
sum(Pass_BAS_dat_processed$N_Total_FF) #43803 (corrected to 43697)
sum(Pass_BAS_dat_processed$N_Total) #66675

# - Final counts for modelling
nrow(Pass_BAS_dat_processed.mod) #59864
sum(Pass_BAS_dat_processed.mod$N_Total_FF) #43761 (corrected to 43655)
sum(Pass_BAS_dat_processed.mod$N_Total) #66617



#Brms implementation - 
adapt_delta_value <- 0.99
max_treedepth_value <- 20
iterations <- 3000
burnin <- 1000
thinning <- 2


##Model 1
#Pass_BAS_full_DD_total_brm <- brm(N_Total ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z +  
#                                    (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = poisson(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#summary(Pass_BAS_full_DD_total_brm)
#save(Pass_BAS_full_DD_total_brm, file = "~/21O_Biosecurity_Tasmania/NPM/Models/Sensitivity/Pass_BAS_full_DD_total_brm.RData")
##plot(Pass_BAS_full_DD_total_brm)
#
##Model 2
#Pass_BAS_full_Declarin_brm <- brm(N_Declarations ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + 
#                                    (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = poisson(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#summary(Pass_BAS_full_Declarin_brm)
#save(Pass_BAS_full_Declarin_brm, file = "~/21O_Biosecurity_Tasmania/NPM/Models/Sensitivity/Pass_BAS_full_Declarin_brm.RData")
##plot(Pass_BAS_full_Declarin_brm)
#
##Model 3
#Pass_BAS_full_Detectin_brm <- brm(N_Detections ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + 
#                                    (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = poisson(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#summary(Pass_BAS_full_Detectin_brm)
#save(Pass_BAS_full_Detectin_brm, file = "~/21O_Biosecurity_Tasmania/NPM/Models/Sensitivity/Pass_BAS_full_Detectin_brm.RData")
##plot(Pass_BAS_full_Detectin_brm)
#
##Model 4
#Pass_BAS_full_DD_total_FF_brm <- brm(N_Total_FF ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z +  
#                                       (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = poisson(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#summary(Pass_BAS_full_DD_total_FF_brm)
#save(Pass_BAS_full_DD_total_FF_brm, file = "~/21O_Biosecurity_Tasmania/NPM/Models/Sensitivity/Pass_BAS_full_DD_total_FF_brm.RData")
##plot(Pass_BAS_full_DD_total_FF_brm)
#
##Model 5
#Pass_BAS_full_Declarin_FF_brm <- brm(N_Declarations_FF ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + 
#                                       (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = poisson(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#summary(Pass_BAS_full_Declarin_FF_brm)
#save(Pass_BAS_full_Declarin_FF_brm, file = "~/21O_Biosecurity_Tasmania/NPM/Models/Sensitivity/Pass_BAS_full_Declarin_FF_brm.RData")
##plot(Pass_BAS_full_Declarin_FF_brm)
#
##Model 6
#start_time <- Sys.time()
#Pass_BAS_full_Detectin_FF_brm <- brm(N_Detections_FF ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + 
#                                       (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = poisson(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#end_time <- Sys.time()
##end_time - start_time #Time difference of 2.260156 hours
#summary(Pass_BAS_full_Detectin_FF_brm)
#save(Pass_BAS_full_Detectin_FF_brm, file = "~/21O_Biosecurity_Tasmania/NPM/Models/Sensitivity/Pass_BAS_full_Detectin_FF_brm.RData")
##plot(Pass_BAS_full_Detectin_FF_brm)
#
#
#
##Brms implementation (zero inflation sensitivity) - 
##Model 7
#Pass_BAS_full_DD_total_brm_zif <- brm(N_Total ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + 
#                                        (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = zero_inflated_poisson(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#summary(Pass_BAS_full_DD_total_brm_zif)
#save(Pass_BAS_full_DD_total_brm_zif, file = "~/21O_Biosecurity_Tasmania/NPM/Models/Sensitivity/Pass_BAS_full_DD_total_brm_zif.RData")
##plot(Pass_BAS_full_DD_total_brm_zif)
#
##Model 8
#Pass_BAS_full_Declarin_FF_brm_zif <- brm(N_Declarations_FF ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + 
#                                          (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = zero_inflated_poisson(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#summary(Pass_BAS_full_Declarin_FF_brm_zif)
#save(Pass_BAS_full_Declarin_FF_brm_zif, file = "~/21O_Biosecurity_Tasmania/NPM/Models/Sensitivity/Pass_BAS_full_Declarin_FF_brm_zif.RData")
##plot(Pass_BAS_full_Declarin_FF_brm_zif)
#
#
#
##Negative binomial implementation (overdispersion sensitivity) - 
##Model 9
#start_time2 <- Sys.time()
#Pass_BAS_full_DD_total_brm_ngb <- brm(N_Total ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + 
#                                        (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = negbinomial(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#end_time2 <- Sys.time()
##end_time2 - start_time2 #Time difference of 1.137333 hours
#summary(Pass_BAS_full_DD_total_brm_ngb)
#save(Pass_BAS_full_DD_total_brm_ngb, file = "~/21O_Biosecurity_Tasmania/NPM/Models/Sensitivity/Pass_BAS_full_DD_total_brm_ngb.RData")
##plot(Pass_BAS_full_DD_total_brm_ngb)
#
##Model 10
#Pass_BAS_full_Declarin_FF_brm_ngb <- brm(N_Declarations_FF ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + 
#                                        (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = negbinomial(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#summary(Pass_BAS_full_Declarin_FF_brm_ngb)
#save(Pass_BAS_full_Declarin_FF_brm_ngb, file = "~/21O_Biosecurity_Tasmania/NPM/Models/Sensitivity/Pass_BAS_full_Declarin_FF_brm_ngb.RData")
##plot(Pass_BAS_full_Declarin_FF_brm_ngb)
#
#
#
##Zero inflated negative binomial implementation (porque no los dos?) - 
##Model 11
#start_time3 <- Sys.time()
#Pass_BAS_full_DD_total_brm_ngbzif <- brm(N_Total ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + 
#                                        (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = zero_inflated_negbinomial(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#end_time3 <- Sys.time()
##end_time3 - start_time3 #Time difference of 4.463617 hours
#summary(Pass_BAS_full_DD_total_brm_ngbzif)
#save(Pass_BAS_full_DD_total_brm_ngbzif, file = "~/21O_Biosecurity_Tasmania/NPM/Models/Sensitivity/Pass_BAS_full_DD_total_brm_ngbzif.RData")
##plot(Pass_BAS_full_DD_total_brm_ngbzif)
#
##Model 12
#Pass_BAS_full_Declarin_FF_brm_ngbzif <- brm(N_Declarations_FF ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + 
#                                        (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = zero_inflated_negbinomial(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#summary(Pass_BAS_full_Declarin_FF_brm_ngbzif)
#save(Pass_BAS_full_Declarin_FF_brm_ngbzif, file = "~/21O_Biosecurity_Tasmania/NPM/Models/Sensitivity/Pass_BAS_full_Declarin_FF_brm_ngbzif.RData")
##plot(Pass_BAS_full_Declarin_FF_brm_ngbzif)





