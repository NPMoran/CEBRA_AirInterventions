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


###Model 1
#start_time <- Sys.time()
#Pass_BAS_full_DD_total_brm <- brm(N_Total ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z +  
#                                    (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = poisson(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#end_time <- Sys.time()
##end_time - start_time #Time difference 59.39213 of mins
#summary(Pass_BAS_full_DD_total_brm)
#save(Pass_BAS_full_DD_total_brm, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_brm.RData")
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
#save(Pass_BAS_full_Declarin_brm, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_brm.RData")
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
#save(Pass_BAS_full_Detectin_brm, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Detectin_brm.RData")
##plot(Pass_BAS_full_Detectin_brm)
#
#Model 4
#Pass_BAS_full_DD_total_FF_brm <- brm(N_Total_FF ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z +  
#                                       (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = poisson(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#summary(Pass_BAS_full_DD_total_FF_brm)
#save(Pass_BAS_full_DD_total_FF_brm, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_FF_brm.RData")
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
#save(Pass_BAS_full_Declarin_FF_brm, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_brm.RData")
##plot(Pass_BAS_full_Declarin_FF_brm)
#
##Model 6
#Pass_BAS_full_Detectin_FF_brm <- brm(N_Detections_FF ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + 
#                                       (1|FlightOrigin/FlightNumber),
#                        data   = Pass_BAS_dat_processed.mod, 
#                        family = poisson(link = "log"),
#                        control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
#                        chains = 3, cores = 3, iter = iterations, warmup = burnin, thin = thinning,
#                        backend = "cmdstanr", threads = threading(4))
#summary(Pass_BAS_full_Detectin_FF_brm)
#save(Pass_BAS_full_Detectin_FF_brm, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Detectin_FF_brm.RData")
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
#save(Pass_BAS_full_DD_total_brm_zif, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_brm_zif.RData")
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
#save(Pass_BAS_full_Declarin_FF_brm_zif, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_brm_zif.RData")
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
##end_time2 - start_time2 #Time difference of 48.24464 mins
#summary(Pass_BAS_full_DD_total_brm_ngb)
#save(Pass_BAS_full_DD_total_brm_ngb, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_brm_ngb.RData")
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
#save(Pass_BAS_full_Declarin_FF_brm_ngb, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_brm_ngb.RData")
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
##end_time3 - start_time3 #Time difference of 1.362396 hours
#summary(Pass_BAS_full_DD_total_brm_ngbzif)
#save(Pass_BAS_full_DD_total_brm_ngbzif, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_brm_ngbzif.RData")
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
#save(Pass_BAS_full_Declarin_FF_brm_ngbzif, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_brm_ngbzif.RData")
##plot(Pass_BAS_full_Declarin_FF_brm_ngbzif)



#Original glm versions
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_glm.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_glm.RData")

#Bayesian brms versions
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_brm.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_brm.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Detectin_brm.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_FF_brm.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_brm.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Detectin_FF_brm.RData") 

#Zero inflated versions
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_brm_zif.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_brm_zif.RData")

#Negative binomial versions
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_brm_ngb.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_brm_ngb.RData")

#Zero inflated negative binomial versions
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_brm_ngbzif.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_brm_ngbzif.RData")



#### 2A. N_Total model comparison: Regime ####
Regi_ems1 <- emmeans(Pass_BAS_full_DD_total_glm, ~Regime)
Regi_ems2 <- emmeans(Pass_BAS_full_DD_total_brm, ~Regime)
Regi_ems3 <- emmeans(Pass_BAS_full_DD_total_brm_zif, ~Regime)
Regi_ems4 <- emmeans(Pass_BAS_full_DD_total_brm_ngb, ~Regime)
Regi_ems5 <- emmeans(Pass_BAS_full_DD_total_brm_ngbzif, ~Regime)

Regi_pairs1 <- as.data.frame(summary(pairs(Regi_ems1), point.est = mean))
Regi_pairs2 <- as.data.frame(summary(pairs(Regi_ems2), point.est = mean))
Regi_pairs3 <- as.data.frame(summary(pairs(Regi_ems3), point.est = mean))
Regi_pairs4 <- as.data.frame(summary(pairs(Regi_ems4), point.est = mean))
Regi_pairs5 <- as.data.frame(summary(pairs(Regi_ems5), point.est = mean))

Regi_ems1 <- as.data.frame(Regi_ems1) 
Regi_ems2 <- as.data.frame(Regi_ems2) 
Regi_ems3 <- as.data.frame(Regi_ems3) 
Regi_ems4 <- as.data.frame(Regi_ems4) 
Regi_ems5 <- as.data.frame(Regi_ems5) 

Regi_ems1$Regime <- c("One BI:", "Two BIs:", "One DDT:", "One DDT & one BI:", "Two DDTs:")
Regi_ems2$Regime <- c("One BI:", "Two BIs:", "One DDT:", "One DDT & one BI:", "Two DDTs:")
Regi_ems3$Regime <- c("One BI:", "Two BIs:", "One DDT:", "One DDT & one BI:", "Two DDTs:")
Regi_ems4$Regime <- c("One BI:", "Two BIs:", "One DDT:", "One DDT & one BI:", "Two DDTs:")
Regi_ems5$Regime <- c("One BI:", "Two BIs:", "One DDT:", "One DDT & one BI:", "Two DDTs:")

Regi_ems1$Model <- "Poisson_glm" 
Regi_ems2$Model <- "Poisson_brm" 
Regi_ems3$Model <- "Poisson_Zinf_brm" 
Regi_ems4$Model <- "Negbin_brm" 
Regi_ems5$Model <- "Negbin_Zinf_brm" 

#Regi_ems1$Position <- c(1:5)
#Regi_ems2$Position <- Regi_ems1$Position - 0.17
#Regi_ems3$Position <- Regi_ems1$Position - 0.34
#Regi_ems4$Position <- Regi_ems1$Position - 0.51
#Regi_ems5$Position <- Regi_ems1$Position - 0.68

Regi_ems1$Position <- c(23:27)
Regi_ems2$Position <- c(17.5:21.5)
Regi_ems3$Position <- c(12:16)
Regi_ems4$Position <- c(6.5:10.5)
Regi_ems5$Position <- c(1:5)

Regi_ems1 <- Regi_ems1[,-c(3,4)]
colnames(Regi_ems1) <- c("Regime", "emmean", "LCI", "UCI", "Model", "Position")
colnames(Regi_ems2) <- c("Regime", "emmean", "LCI", "UCI", "Model", "Position")
colnames(Regi_ems3) <- c("Regime", "emmean", "LCI", "UCI", "Model", "Position")
colnames(Regi_ems4) <- c("Regime", "emmean", "LCI", "UCI", "Model", "Position")
colnames(Regi_ems5) <- c("Regime", "emmean", "LCI", "UCI", "Model", "Position")

Regi_emsA <- rbind(Regi_ems1,Regi_ems2,Regi_ems3,Regi_ems4,Regi_ems5)
Regi_emsA$Model <- ordered(Regi_emsA$Model, levels = c("Poisson_glm", "Poisson_brm", "Poisson_Zinf_brm", "Negbin_brm", "Negbin_Zinf_brm"))

Regi_emsA$N_mean <- exp(Regi_emsA$emmean)
Regi_emsA$N_lci <- exp(Regi_emsA$LCI)
Regi_emsA$N_uci <- exp(Regi_emsA$UCI)

Regi_emsA$text <- paste(Regi_emsA$Regime, format(round(exp(Regi_emsA$emmean), digits = 2), nsmall = 2), sep = ' ')
Regi_emsA$text <- paste(Regi_emsA$text, format(round(exp(Regi_emsA$LCI), digits = 2), nsmall = 2), sep = ' [')
Regi_emsA$text <- paste(Regi_emsA$text, format(round(exp(Regi_emsA$UCI), digits = 2), nsmall = 2), sep = ', ')
Regi_emsA$text <- paste(Regi_emsA$text, '', sep = ']')

Fig_Sens_A <- ggplot(Regi_emsA, aes(x = N_mean, y = Position)) +
  scale_x_continuous(limits = c(-0.6, 1.7), expand = c(0, 0), breaks=c(0.0, 0.5, 1.0, 1.5)) +
  scale_y_continuous(limits = c(0.1, 28), expand = c(0, 0), breaks=NULL) +
  theme(legend.position = c(0.875,0.20),
        legend.title = element_blank(),
        axis.text.y = element_blank(), 
        legend.text = element_text(size = 7.5, colour = "black", face = 'italic'), 
        legend.key.size = unit(1.2,"line"),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(aes(color = Model), x = Regi_emsA$N_lci, y = Regi_emsA$Position, xend = Regi_emsA$N_uci, yend = Regi_emsA$Position, size = 0.7) + 
  geom_point(aes(color = Model), shape = 19, size = 2) +
  scale_colour_manual(values = c("#1BAB70","#4C5083" , "#1F85B9", "#C1D174", "#AA9335")) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  geom_text(aes(label=text, fontface = 1), hjust = "left", x =-0.57, vjust=0.25, size = 2.3) +
  #geom_text(aes(label=BeforeAfter, fontface = 1), hjust = "left", x =-10.5, vjust=0.5, size = 1.8) +
  labs(x = "Estimated total BRM interceptions/flight by regime",
       y = "") 
Fig_Sens_A

ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Sens_A.png", width = 18, height = 12, units = "cm", Fig_Sens_A, dpi = 600)




#### 2B. N_Total model comparison: Bag Searches ####
Pass_BAS_full_DD_total_glm.fixef <- read.csv("~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_glm.fixef.csv")
Pass_BAS_full_DD_total_brm.fixef <- as.data.frame(fixef(Pass_BAS_full_DD_total_brm))[,-2]
Pass_BAS_full_DD_total_brm_zif.fixef <- as.data.frame(fixef(Pass_BAS_full_DD_total_brm_zif))[,-2]
Pass_BAS_full_DD_total_brm_ngb.fixef <- as.data.frame(fixef(Pass_BAS_full_DD_total_brm_ngb))[,-2]
Pass_BAS_full_DD_total_brm_ngbzif.fixef <- as.data.frame(fixef(Pass_BAS_full_DD_total_brm_ngbzif))[,-2]

Eff_Bag1 <- as.data.frame(Pass_BAS_full_DD_total_glm.fixef[11,-1])
Eff_Bag2 <- as.data.frame(Pass_BAS_full_DD_total_brm.fixef[11,])
Eff_Bag3 <- as.data.frame(Pass_BAS_full_DD_total_brm_zif.fixef[11,])
Eff_Bag4 <- as.data.frame(Pass_BAS_full_DD_total_brm_ngb.fixef[11,])
Eff_Bag5 <- as.data.frame(Pass_BAS_full_DD_total_brm_ngbzif.fixef[11,])

colnames(Eff_Bag1) <- c("Total", "Total_lci", "Total_uci")
colnames(Eff_Bag2) <- c("Total", "Total_lci", "Total_uci")
colnames(Eff_Bag3) <- c("Total", "Total_lci", "Total_uci")
colnames(Eff_Bag4) <- c("Total", "Total_lci", "Total_uci")
colnames(Eff_Bag5) <- c("Total", "Total_lci", "Total_uci")

Eff_Bag <- rbind(Eff_Bag1,Eff_Bag2,Eff_Bag3,Eff_Bag4,Eff_Bag5)
rownames(Eff_Bag) <- NULL
Eff_Bag$Model <- c("Poisson_glm", "Poisson_brm", "Poisson_Zinf_brm", "Negbin_brm", "Negbin_Zinf_brm")

Eff_Bag$Exp_est <- exp(Eff_Bag$Total)^(1/(sd(Pass_BAS_dat_processed.mod$sqrt.BagSearchCount)))
Eff_Bag$LCI <- exp(Eff_Bag$Total_lci)^(1/(sd(Pass_BAS_dat_processed.mod$sqrt.BagSearchCount)))
Eff_Bag$UCI <- exp(Eff_Bag$Total_uci)^(1/(sd(Pass_BAS_dat_processed.mod$sqrt.BagSearchCount)))

Eff_Bag$Effect_percent <- paste(round(((Eff_Bag$Exp_est-1)*100), digits = 2), round(((Eff_Bag$LCI-1)*100), digits = 2), sep = "% [")
Eff_Bag$Effect_percent <- paste(Eff_Bag$Effect_percent, round(((Eff_Bag$UCI-1)*100), digits = 2), sep = "%, ")
Eff_Bag$Effect_percent <- paste(Eff_Bag$Effect_percent, "%]", sep = "")

Eff_Bag <- Eff_Bag[,c(4,8)]

#knitr::kable(Eff_Bag, "simple", align = "lc", row.names = FALSE)




#### 2C. N_Total model comparison: Flight Origins ####
Pass_BAS_full_DD_total_glm.ranef <- read.csv("~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_glm.ranef.csv")
Pass_BAS_full_DD_total_glm.FO <- subset(Pass_BAS_full_DD_total_glm.ranef, grpvar == "FlightOrigin")
Pass_BAS_full_DD_total_brm.ranef_FO <- as.data.frame(ranef(Pass_BAS_full_DD_total_brm)$FlightOrigin)
Pass_BAS_full_DD_total_brm.ranef_zif_FO <- as.data.frame(ranef(Pass_BAS_full_DD_total_brm_zif)$FlightOrigin)
Pass_BAS_full_DD_total_brm.ranef_ngb_FO <- as.data.frame(ranef(Pass_BAS_full_DD_total_brm_ngb)$FlightOrigin)
Pass_BAS_full_DD_total_brm.ranef_ngbzif_FO <- as.data.frame(ranef(Pass_BAS_full_DD_total_brm_ngbzif)$FlightOrigin)

Pass_BAS_full_DD_total_glm.FO$lci <- Pass_BAS_full_DD_total_glm.FO$condval - 1.96*(Pass_BAS_full_DD_total_glm.FO$condsd)
Pass_BAS_full_DD_total_glm.FO$uci <- Pass_BAS_full_DD_total_glm.FO$condval + 1.96*(Pass_BAS_full_DD_total_glm.FO$condsd)
Pass_BAS_full_DD_total_glm.FO <- Pass_BAS_full_DD_total_glm.FO[order(Pass_BAS_full_DD_total_glm.FO$condval,decreasing=TRUE),]
Pass_BAS_full_DD_total_glm.FO$Position <- c(7:1)

#reordering dataframes to match
Pass_BAS_full_DD_total_brm.ranef_FO$Group <- rownames(Pass_BAS_full_DD_total_brm.ranef_FO)
Pass_BAS_full_DD_total_brm.ranef_zif_FO$Group <- rownames(Pass_BAS_full_DD_total_brm.ranef_zif_FO)
Pass_BAS_full_DD_total_brm.ranef_ngb_FO$Group <- rownames(Pass_BAS_full_DD_total_brm.ranef_ngb_FO)
Pass_BAS_full_DD_total_brm.ranef_ngbzif_FO$Group <- rownames(Pass_BAS_full_DD_total_brm.ranef_ngbzif_FO)

target <- Pass_BAS_full_DD_total_glm.FO$grp
Pass_BAS_full_DD_total_brm.ranef_FO <- Pass_BAS_full_DD_total_brm.ranef_FO[match(target, Pass_BAS_full_DD_total_brm.ranef_FO$Group),]
Pass_BAS_full_DD_total_brm.ranef_zif_FO <- Pass_BAS_full_DD_total_brm.ranef_zif_FO[match(target, Pass_BAS_full_DD_total_brm.ranef_zif_FO$Group),]
Pass_BAS_full_DD_total_brm.ranef_ngb_FO <- Pass_BAS_full_DD_total_brm.ranef_ngb_FO[match(target, Pass_BAS_full_DD_total_brm.ranef_ngb_FO$Group),]
Pass_BAS_full_DD_total_brm.ranef_ngbzif_FO <- Pass_BAS_full_DD_total_brm.ranef_ngbzif_FO[match(target, Pass_BAS_full_DD_total_brm.ranef_ngbzif_FO$Group),]

Pass_BAS_full_DD_total_brm.ranef_FO$Position <- Pass_BAS_full_DD_total_glm.FO$Position - 0.1
Pass_BAS_full_DD_total_brm.ranef_zif_FO$Position <- Pass_BAS_full_DD_total_glm.FO$Position - 0.2
Pass_BAS_full_DD_total_brm.ranef_ngb_FO$Position <- Pass_BAS_full_DD_total_glm.FO$Position - 0.3
Pass_BAS_full_DD_total_brm.ranef_ngbzif_FO$Position <- Pass_BAS_full_DD_total_glm.FO$Position - 0.4

Pass_BAS_full_DD_total_glm.FO <- Pass_BAS_full_DD_total_glm.FO[,c(4,5,7,8,9)]
Pass_BAS_full_DD_total_brm.ranef_FO <- Pass_BAS_full_DD_total_brm.ranef_FO[,c(5,1,3,4,6)]
Pass_BAS_full_DD_total_brm.ranef_zif_FO <- Pass_BAS_full_DD_total_brm.ranef_zif_FO[,c(5,1,3,4,6)]
Pass_BAS_full_DD_total_brm.ranef_ngb_FO <- Pass_BAS_full_DD_total_brm.ranef_ngb_FO[,c(5,1,3,4,6)]
Pass_BAS_full_DD_total_brm.ranef_ngbzif_FO <- Pass_BAS_full_DD_total_brm.ranef_ngbzif_FO[,c(5,1,3,4,6)]

colnames(Pass_BAS_full_DD_total_glm.FO) <- c("Group", "Intercept", "LCI", "UCI", "Position")
colnames(Pass_BAS_full_DD_total_brm.ranef_FO) <- c("Group", "Intercept", "LCI", "UCI", "Position")
colnames(Pass_BAS_full_DD_total_brm.ranef_zif_FO) <- c("Group", "Intercept", "LCI", "UCI", "Position")
colnames(Pass_BAS_full_DD_total_brm.ranef_ngb_FO) <- c("Group", "Intercept", "LCI", "UCI", "Position")
colnames(Pass_BAS_full_DD_total_brm.ranef_ngbzif_FO) <- c("Group", "Intercept", "LCI", "UCI", "Position")

Pass_BAS_full_DD_total_glm.FO$Model <- "Poisson_glm"
Pass_BAS_full_DD_total_brm.ranef_FO$Model <- "Poisson_brm"
Pass_BAS_full_DD_total_brm.ranef_zif_FO$Model <- "Poisson_Zinf_brm"
Pass_BAS_full_DD_total_brm.ranef_ngb_FO$Model <- "Negbin_brm"
Pass_BAS_full_DD_total_brm.ranef_ngbzif_FO$Model <- "Negbin_Zinf_brm"

Pass_BAS_full_DD_total_brm.ranef_FO$Group <- ""
Pass_BAS_full_DD_total_brm.ranef_zif_FO$Group <- ""
Pass_BAS_full_DD_total_brm.ranef_ngb_FO$Group <- ""
Pass_BAS_full_DD_total_brm.ranef_ngbzif_FO$Group <- ""

Orig_Sens_A <- rbind(Pass_BAS_full_DD_total_glm.FO, Pass_BAS_full_DD_total_brm.ranef_FO, 
                     Pass_BAS_full_DD_total_brm.ranef_zif_FO, Pass_BAS_full_DD_total_brm.ranef_ngb_FO, 
                     Pass_BAS_full_DD_total_brm.ranef_ngbzif_FO)
Orig_Sens_A$Model <- ordered(Orig_Sens_A$Model, levels = c("Poisson_glm", "Poisson_brm", "Poisson_Zinf_brm", "Negbin_brm", "Negbin_Zinf_brm"))


Fig_Sens_B <- ggplot(Orig_Sens_A, aes(x = Intercept, y = Position)) +
  scale_x_continuous(limits = c(-0.45, 0.45), expand = c(0, 0), breaks=c(-0.3, 0.0, 0.3)) +
  scale_y_continuous(limits = c(0.2, 7.4), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = c(0.825,0.125),
        legend.title = element_blank(),
        axis.text.y = element_blank(), 
        legend.text = element_text(size = 7, colour = "black", face = 'italic'), 
        legend.key.size = unit(0.8,"line"),
        legend.background = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(aes(color = Model), x = Orig_Sens_A$LCI, y = Orig_Sens_A$Position, xend = Orig_Sens_A$UCI, yend = Orig_Sens_A$Position, size = 0.4) + 
  scale_colour_manual(values = c("#1BAB70","#4C5083" , "#1F85B9", "#C1D174", "#AA9335")) +
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=Group, fontface = 1), hjust = "left", x =-0.435, vjust=0, size = 2.5) +
  labs(x = "Est. total BRM interceptions/flight by flight origin",
       y = "")
Fig_Sens_B

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Sens_B.png", width = 9, height = 11, units = "cm", Fig_Sens_B, dpi = 600)




#### 3A. N_Declarations_FF model comparison: Regime ####
Regi_ems1 <- emmeans(Pass_BAS_full_Declarin_FF_glm, ~Regime)
Regi_ems2 <- emmeans(Pass_BAS_full_Declarin_FF_brm, ~Regime)
Regi_ems3 <- emmeans(Pass_BAS_full_Declarin_FF_brm_zif, ~Regime)
Regi_ems4 <- emmeans(Pass_BAS_full_Declarin_FF_brm_ngb, ~Regime)
Regi_ems5 <- emmeans(Pass_BAS_full_Declarin_FF_brm_ngbzif, ~Regime)

Regi_pairs1 <- as.data.frame(summary(pairs(Regi_ems1), point.est = mean))
Regi_pairs2 <- as.data.frame(summary(pairs(Regi_ems2), point.est = mean))
Regi_pairs3 <- as.data.frame(summary(pairs(Regi_ems3), point.est = mean))
Regi_pairs4 <- as.data.frame(summary(pairs(Regi_ems4), point.est = mean))
Regi_pairs5 <- as.data.frame(summary(pairs(Regi_ems5), point.est = mean))

Regi_ems1 <- as.data.frame(Regi_ems1) 
Regi_ems2 <- as.data.frame(Regi_ems2) 
Regi_ems3 <- as.data.frame(Regi_ems3) 
Regi_ems4 <- as.data.frame(Regi_ems4) 
Regi_ems5 <- as.data.frame(Regi_ems5) 

Regi_ems1$Regime <- c("One BI:", "Two BIs:", "One DDT:", "One DDT & one BI:", "Two DDTs:")
Regi_ems2$Regime <- c("One BI:", "Two BIs:", "One DDT:", "One DDT & one BI:", "Two DDTs:")
Regi_ems3$Regime <- c("One BI:", "Two BIs:", "One DDT:", "One DDT & one BI:", "Two DDTs:")
Regi_ems4$Regime <- c("One BI:", "Two BIs:", "One DDT:", "One DDT & one BI:", "Two DDTs:")
Regi_ems5$Regime <- c("One BI:", "Two BIs:", "One DDT:", "One DDT & one BI:", "Two DDTs:")

Regi_ems1$Model <- "Poisson_glm" 
Regi_ems2$Model <- "Poisson_brm" 
Regi_ems3$Model <- "Poisson_Zinf_brm" 
Regi_ems4$Model <- "Negbin_brm" 
Regi_ems5$Model <- "Negbin_Zinf_brm" 

#Regi_ems1$Position <- c(1:5)
#Regi_ems2$Position <- Regi_ems1$Position - 0.17
#Regi_ems3$Position <- Regi_ems1$Position - 0.34
#Regi_ems4$Position <- Regi_ems1$Position - 0.51
#Regi_ems5$Position <- Regi_ems1$Position - 0.68

Regi_ems1$Position <- c(23:27)
Regi_ems2$Position <- c(17.5:21.5)
Regi_ems3$Position <- c(12:16)
Regi_ems4$Position <- c(6.5:10.5)
Regi_ems5$Position <- c(1:5)

Regi_ems1 <- Regi_ems1[,-c(3,4)]
colnames(Regi_ems1) <- c("Regime", "emmean", "LCI", "UCI", "Model", "Position")
colnames(Regi_ems2) <- c("Regime", "emmean", "LCI", "UCI", "Model", "Position")
colnames(Regi_ems3) <- c("Regime", "emmean", "LCI", "UCI", "Model", "Position")
colnames(Regi_ems4) <- c("Regime", "emmean", "LCI", "UCI", "Model", "Position")
colnames(Regi_ems5) <- c("Regime", "emmean", "LCI", "UCI", "Model", "Position")

Regi_emsA <- rbind(Regi_ems1,Regi_ems2,Regi_ems3,Regi_ems4,Regi_ems5)
Regi_emsA$Model <- ordered(Regi_emsA$Model, levels = c("Poisson_glm", "Poisson_brm", "Poisson_Zinf_brm", "Negbin_brm", "Negbin_Zinf_brm"))

Regi_emsA$N_mean <- exp(Regi_emsA$emmean)
Regi_emsA$N_lci <- exp(Regi_emsA$LCI)
Regi_emsA$N_uci <- exp(Regi_emsA$UCI)

Regi_emsA$text <- paste(Regi_emsA$Regime, format(round(exp(Regi_emsA$emmean), digits = 2), nsmall = 2), sep = ' ')
Regi_emsA$text <- paste(Regi_emsA$text, format(round(exp(Regi_emsA$LCI), digits = 2), nsmall = 2), sep = ' [')
Regi_emsA$text <- paste(Regi_emsA$text, format(round(exp(Regi_emsA$UCI), digits = 2), nsmall = 2), sep = ', ')
Regi_emsA$text <- paste(Regi_emsA$text, '', sep = ']')

Fig_Sens_C <- ggplot(Regi_emsA, aes(x = N_mean, y = Position)) +
  scale_x_continuous(limits = c(-0.6, 1.7), expand = c(0, 0), breaks=c(0.0, 0.5, 1.0, 1.5)) +
  scale_y_continuous(limits = c(0.1, 28), expand = c(0, 0), breaks=NULL) +
  theme(legend.position = c(0.875,0.20),
        legend.title = element_blank(),
        axis.text.y = element_blank(), 
        legend.text = element_text(size = 7.5, colour = "black", face = 'italic'), 
        legend.key.size = unit(1.2,"line"),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(aes(color = Model), x = Regi_emsA$N_lci, y = Regi_emsA$Position, xend = Regi_emsA$N_uci, yend = Regi_emsA$Position, size = 0.7) + 
  geom_point(aes(color = Model), shape = 19, size = 2) +
  scale_colour_manual(values = c("#1BAB70","#4C5083" , "#1F85B9", "#C1D174", "#AA9335")) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  geom_text(aes(label=text, fontface = 1), hjust = "left", x =-0.57, vjust=0.25, size = 2.3) +
  #geom_text(aes(label=BeforeAfter, fontface = 1), hjust = "left", x =-10.5, vjust=0.5, size = 1.8) +
  labs(x = "Estimated total FF host declarations/flight by regime",
       y = "") 
Fig_Sens_C

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Sens_C.png", width = 18, height = 12, units = "cm", Fig_Sens_C, dpi = 600)





#### 3B. N_Declarations_FF model comparison: Bag Searches ####
Pass_BAS_full_Declarin_FF_glm.fixef <- read.csv("~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_glm.fixef.csv")
Pass_BAS_full_Declarin_FF_brm.fixef <- as.data.frame(fixef(Pass_BAS_full_Declarin_FF_brm))[,-2]
Pass_BAS_full_Declarin_FF_brm_zif.fixef <- as.data.frame(fixef(Pass_BAS_full_Declarin_FF_brm_zif))[,-2]
Pass_BAS_full_Declarin_FF_brm_ngb.fixef <- as.data.frame(fixef(Pass_BAS_full_Declarin_FF_brm_ngb))[,-2]
Pass_BAS_full_Declarin_FF_brm_ngbzif.fixef <- as.data.frame(fixef(Pass_BAS_full_Declarin_FF_brm_ngbzif))[,-2]

Eff_Bag1 <- as.data.frame(Pass_BAS_full_Declarin_FF_glm.fixef[11,-1])
Eff_Bag2 <- as.data.frame(Pass_BAS_full_Declarin_FF_brm.fixef[11,])
Eff_Bag3 <- as.data.frame(Pass_BAS_full_Declarin_FF_brm_zif.fixef[11,])
Eff_Bag4 <- as.data.frame(Pass_BAS_full_Declarin_FF_brm_ngb.fixef[11,])
Eff_Bag5 <- as.data.frame(Pass_BAS_full_Declarin_FF_brm_ngbzif.fixef[11,])

colnames(Eff_Bag1) <- c("Total", "Total_lci", "Total_uci")
colnames(Eff_Bag2) <- c("Total", "Total_lci", "Total_uci")
colnames(Eff_Bag3) <- c("Total", "Total_lci", "Total_uci")
colnames(Eff_Bag4) <- c("Total", "Total_lci", "Total_uci")
colnames(Eff_Bag5) <- c("Total", "Total_lci", "Total_uci")

Eff_Bag <- rbind(Eff_Bag1,Eff_Bag2,Eff_Bag3,Eff_Bag4,Eff_Bag5)
rownames(Eff_Bag) <- NULL
Eff_Bag$Model <- c("Poisson_glm", "Poisson_brm", "Poisson_Zinf_brm", "Negbin_brm", "Negbin_Zinf_brm")

Eff_Bag$Exp_est <- exp(Eff_Bag$Total)^(1/(sd(Pass_BAS_dat_processed.mod$sqrt.BagSearchCount)))
Eff_Bag$LCI <- exp(Eff_Bag$Total_lci)^(1/(sd(Pass_BAS_dat_processed.mod$sqrt.BagSearchCount)))
Eff_Bag$UCI <- exp(Eff_Bag$Total_uci)^(1/(sd(Pass_BAS_dat_processed.mod$sqrt.BagSearchCount)))

Eff_Bag$Effect_percent <- paste(round(((Eff_Bag$Exp_est-1)*100), digits = 2), round(((Eff_Bag$LCI-1)*100), digits = 2), sep = "% [")
Eff_Bag$Effect_percent <- paste(Eff_Bag$Effect_percent, round(((Eff_Bag$UCI-1)*100), digits = 2), sep = "%, ")
Eff_Bag$Effect_percent <- paste(Eff_Bag$Effect_percent, "%]", sep = "")

Eff_Bag <- Eff_Bag[,c(4,8)]

#knitr::kable(Eff_Bag, "simple", align = "lc", row.names = FALSE)




#### 3B. N_Declarations_FF model comparison: Flight Origins ####
Pass_BAS_full_Declarin_FF_glm.ranef <- read.csv("~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_glm.ranef.csv")
Pass_BAS_full_Declarin_FF_glm.FO <- subset(Pass_BAS_full_Declarin_FF_glm.ranef, grpvar == "FlightOrigin")
Pass_BAS_full_Declarin_FF_brm.ranef_FO <- as.data.frame(ranef(Pass_BAS_full_Declarin_FF_brm)$FlightOrigin)
Pass_BAS_full_Declarin_FF_brm.ranef_zif_FO <- as.data.frame(ranef(Pass_BAS_full_Declarin_FF_brm_zif)$FlightOrigin)
Pass_BAS_full_Declarin_FF_brm.ranef_ngb_FO <- as.data.frame(ranef(Pass_BAS_full_Declarin_FF_brm_ngb)$FlightOrigin)
Pass_BAS_full_Declarin_FF_brm.ranef_ngbzif_FO <- as.data.frame(ranef(Pass_BAS_full_Declarin_FF_brm_ngbzif)$FlightOrigin)

Pass_BAS_full_Declarin_FF_glm.FO$lci <- Pass_BAS_full_Declarin_FF_glm.FO$condval - 1.96*(Pass_BAS_full_Declarin_FF_glm.FO$condsd)
Pass_BAS_full_Declarin_FF_glm.FO$uci <- Pass_BAS_full_Declarin_FF_glm.FO$condval + 1.96*(Pass_BAS_full_Declarin_FF_glm.FO$condsd)
Pass_BAS_full_Declarin_FF_glm.FO <- Pass_BAS_full_Declarin_FF_glm.FO[order(Pass_BAS_full_Declarin_FF_glm.FO$condval,decreasing=TRUE),]
Pass_BAS_full_Declarin_FF_glm.FO$Position <- c(7:1)

#reordering dataframes to match
Pass_BAS_full_Declarin_FF_brm.ranef_FO$Group <- rownames(Pass_BAS_full_Declarin_FF_brm.ranef_FO)
Pass_BAS_full_Declarin_FF_brm.ranef_zif_FO$Group <- rownames(Pass_BAS_full_Declarin_FF_brm.ranef_zif_FO)
Pass_BAS_full_Declarin_FF_brm.ranef_ngb_FO$Group <- rownames(Pass_BAS_full_Declarin_FF_brm.ranef_ngb_FO)
Pass_BAS_full_Declarin_FF_brm.ranef_ngbzif_FO$Group <- rownames(Pass_BAS_full_Declarin_FF_brm.ranef_ngbzif_FO)

target <- Pass_BAS_full_Declarin_FF_glm.FO$grp
Pass_BAS_full_Declarin_FF_brm.ranef_FO <- Pass_BAS_full_Declarin_FF_brm.ranef_FO[match(target, Pass_BAS_full_Declarin_FF_brm.ranef_FO$Group),]
Pass_BAS_full_Declarin_FF_brm.ranef_zif_FO <- Pass_BAS_full_Declarin_FF_brm.ranef_zif_FO[match(target, Pass_BAS_full_Declarin_FF_brm.ranef_zif_FO$Group),]
Pass_BAS_full_Declarin_FF_brm.ranef_ngb_FO <- Pass_BAS_full_Declarin_FF_brm.ranef_ngb_FO[match(target, Pass_BAS_full_Declarin_FF_brm.ranef_ngb_FO$Group),]
Pass_BAS_full_Declarin_FF_brm.ranef_ngbzif_FO <- Pass_BAS_full_Declarin_FF_brm.ranef_ngbzif_FO[match(target, Pass_BAS_full_Declarin_FF_brm.ranef_ngbzif_FO$Group),]

Pass_BAS_full_Declarin_FF_brm.ranef_FO$Position <- Pass_BAS_full_Declarin_FF_glm.FO$Position - 0.1
Pass_BAS_full_Declarin_FF_brm.ranef_zif_FO$Position <- Pass_BAS_full_Declarin_FF_glm.FO$Position - 0.2
Pass_BAS_full_Declarin_FF_brm.ranef_ngb_FO$Position <- Pass_BAS_full_Declarin_FF_glm.FO$Position - 0.3
Pass_BAS_full_Declarin_FF_brm.ranef_ngbzif_FO$Position <- Pass_BAS_full_Declarin_FF_glm.FO$Position - 0.4

Pass_BAS_full_Declarin_FF_glm.FO <- Pass_BAS_full_Declarin_FF_glm.FO[,c(4,5,7,8,9)]
Pass_BAS_full_Declarin_FF_brm.ranef_FO <- Pass_BAS_full_Declarin_FF_brm.ranef_FO[,c(5,1,3,4,6)]
Pass_BAS_full_Declarin_FF_brm.ranef_zif_FO <- Pass_BAS_full_Declarin_FF_brm.ranef_zif_FO[,c(5,1,3,4,6)]
Pass_BAS_full_Declarin_FF_brm.ranef_ngb_FO <- Pass_BAS_full_Declarin_FF_brm.ranef_ngb_FO[,c(5,1,3,4,6)]
Pass_BAS_full_Declarin_FF_brm.ranef_ngbzif_FO <- Pass_BAS_full_Declarin_FF_brm.ranef_ngbzif_FO[,c(5,1,3,4,6)]

colnames(Pass_BAS_full_Declarin_FF_glm.FO) <- c("Group", "Intercept", "LCI", "UCI", "Position")
colnames(Pass_BAS_full_Declarin_FF_brm.ranef_FO) <- c("Group", "Intercept", "LCI", "UCI", "Position")
colnames(Pass_BAS_full_Declarin_FF_brm.ranef_zif_FO) <- c("Group", "Intercept", "LCI", "UCI", "Position")
colnames(Pass_BAS_full_Declarin_FF_brm.ranef_ngb_FO) <- c("Group", "Intercept", "LCI", "UCI", "Position")
colnames(Pass_BAS_full_Declarin_FF_brm.ranef_ngbzif_FO) <- c("Group", "Intercept", "LCI", "UCI", "Position")

Pass_BAS_full_Declarin_FF_glm.FO$Model <- "Poisson_glm"
Pass_BAS_full_Declarin_FF_brm.ranef_FO$Model <- "Poisson_brm"
Pass_BAS_full_Declarin_FF_brm.ranef_zif_FO$Model <- "Poisson_Zinf_brm"
Pass_BAS_full_Declarin_FF_brm.ranef_ngb_FO$Model <- "Negbin_brm"
Pass_BAS_full_Declarin_FF_brm.ranef_ngbzif_FO$Model <- "Negbin_Zinf_brm"

Pass_BAS_full_Declarin_FF_brm.ranef_FO$Group <- ""
Pass_BAS_full_Declarin_FF_brm.ranef_zif_FO$Group <- ""
Pass_BAS_full_Declarin_FF_brm.ranef_ngb_FO$Group <- ""
Pass_BAS_full_Declarin_FF_brm.ranef_ngbzif_FO$Group <- ""

Orig_Sens_A <- rbind(Pass_BAS_full_Declarin_FF_glm.FO, Pass_BAS_full_Declarin_FF_brm.ranef_FO, 
                     Pass_BAS_full_Declarin_FF_brm.ranef_zif_FO, Pass_BAS_full_Declarin_FF_brm.ranef_ngb_FO, 
                     Pass_BAS_full_Declarin_FF_brm.ranef_ngbzif_FO)
Orig_Sens_A$Model <- ordered(Orig_Sens_A$Model, levels = c("Poisson_glm", "Poisson_brm", "Poisson_Zinf_brm", "Negbin_brm", "Negbin_Zinf_brm"))


Fig_Sens_D <- ggplot(Orig_Sens_A, aes(x = Intercept, y = Position)) +
  scale_x_continuous(limits = c(-1, 1), expand = c(0, 0), breaks=c(-0.5, 0.0, 0.5)) +
  scale_y_continuous(limits = c(0.2, 7.4), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = c(0.825,0.125),
        legend.title = element_blank(),
        axis.text.y = element_blank(), 
        legend.text = element_text(size = 7, colour = "black", face = 'italic'), 
        legend.key.size = unit(0.8,"line"),
        legend.background = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 8, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(aes(color = Model), x = Orig_Sens_A$LCI, y = Orig_Sens_A$Position, xend = Orig_Sens_A$UCI, yend = Orig_Sens_A$Position, size = 0.4) + 
  scale_colour_manual(values = c("#1BAB70","#4C5083" , "#1F85B9", "#C1D174", "#AA9335")) +
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=Group, fontface = 1), hjust = "left", x =-0.975, vjust=0, size = 2.5) +
  labs(x = "Est. total FF host declarations/flight by flight origin",
       y = "")
Fig_Sens_D

ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Sens_D.png", width = 9, height = 11, units = "cm", Fig_Sens_D, dpi = 600)




#### 4. Overdispersion/zinf checks ####

check_zeroinflation(Pass_BAS_full_DD_total_glm)
check_overdispersion(Pass_BAS_full_DD_total_glm)

check_zeroinflation(Pass_BAS_full_Declarin_FF_glm)
check_overdispersion(Pass_BAS_full_Declarin_FF_glm)



#### 5. Model fitting checks ####
#https://easystats.github.io/performance/
  
#AIC(Pass_BAS_full_DD_total_glm)
#WAIC(Pass_BAS_full_DD_total_brm)
#WAIC(Pass_BAS_full_DD_total_brm_zif)



#r2_nakagawa(Pass_BAS_full_DD_total_glm) # Conditional R2: 0.570, Marginal R2: 0.543
#r2_bayes(Pass_BAS_full_DD_total_brm) # Conditional R2: 0.312 (95% CI [0.306, 0.318]) Marginal R2: 0.266 (95% CI [0.222, 0.307])
#r2_bayes(Pass_BAS_full_DD_total_brm_zif) # Conditional R2: 0.281 (95% CI [0.274, 0.288]) Marginal R2: 0.243 (95% CI [0.205, 0.282])
#r2_bayes(Pass_BAS_full_DD_total_brm_ngb) # Conditional R2: 0.500 (95% CI [0.500, 0.500]) Marginal R2: 0.500 (95% CI [0.499, 0.500])
#r2_bayes(Pass_BAS_full_DD_total_brm_ngbzif) # Conditional R2: 0.500 (95% CI [0.500, 0.500]) Marginal R2: 0.500 (95% CI [0.500, 0.500])






