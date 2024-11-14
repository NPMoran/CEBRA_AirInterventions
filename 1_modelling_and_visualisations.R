# Study: Border biosecurity interceptions for air passengers – assessing intervention methods and analytic tools
#
# Code authored by: [redacted]
#
# Date: May 2024



#Loading required packages:
# - data processing
library(tidyverse); library(data.table); library(lubridate); library(xtable)
# - modelling
library(lme4); library(lmerTest);  library(car); library(performance); library(emmeans)
#library(brms)



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



##Model 1, glmer implementation
#Pass_BAS_full_DD_total_glm <- glmer(N_Total ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + (1|FlightOrigin/FlightNumber), family = poisson, data=Pass_BAS_dat_processed.mod, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
#
###Model 2, glmer implementation
#Pass_BAS_full_Declarin_glm <- glmer(N_Declarations ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + (1|FlightOrigin/FlightNumber), family = poisson, data=Pass_BAS_dat_processed.mod , control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
#
###Model 3, glmer implementation
#Pass_BAS_full_Detectin_glm <- glmer(N_Detections ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + (1|FlightOrigin/FlightNumber), family = poisson, data=Pass_BAS_dat_processed.mod, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
#
###Model 4, glmer implementation
#Pass_BAS_full_DD_total_FF_glm <- glmer(N_Total_FF ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + (1|FlightOrigin/FlightNumber), family = poisson, data=Pass_BAS_dat_processed.mod, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
#
###Model 5, glmer implementation
#Pass_BAS_full_Declarin_FF_glm <- glmer(N_Declarations_FF ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + (1|FlightOrigin/FlightNumber), family = poisson, data=Pass_BAS_dat_processed.mod, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
#
###Model 6, glmer implementation
#Pass_BAS_full_Detectin_FF_glm <- glmer(N_Detections_FF ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + (1|FlightOrigin/FlightNumber), family = poisson, data=Pass_BAS_dat_processed.mod, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
#
#
#save(Pass_BAS_full_DD_total_glm, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_glm.RData")
#save(Pass_BAS_full_Declarin_glm, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_glm.RData")
#save(Pass_BAS_full_Detectin_glm, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Detectin_glm.RData")
#save(Pass_BAS_full_DD_total_FF_glm, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_FF_glm.RData")
#save(Pass_BAS_full_Declarin_FF_glm, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_glm.RData")
#save(Pass_BAS_full_Detectin_FF_glm, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Detectin_FF_glm.RData")

load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_glm.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_glm.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Detectin_glm.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_FF_glm.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_glm.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_Detectin_FF_glm.RData")

#summary(Pass_BAS_full_DD_total_glm)
#summary(Pass_BAS_full_Declarin_glm)
#summary(Pass_BAS_full_Detectin_glm)
#summary(Pass_BAS_full_DD_total_FF_glm)
#summary(Pass_BAS_full_Declarin_FF_glm)
#summary(Pass_BAS_full_Detectin_FF_glm)

#r2_nakagawa(Pass_BAS_full_DD_total_glm)
#r2_nakagawa(Pass_BAS_full_Declarin_glm)
#r2_nakagawa(Pass_BAS_full_Detectin_glm)
#r2_nakagawa(Pass_BAS_full_DD_total_FF_glm)
#r2_nakagawa(Pass_BAS_full_Declarin_FF_glm)
#r2_nakagawa(Pass_BAS_full_Detectin_FF_glm)



Pass_BAS_full_DD_total_glm.fixef <- as.data.frame(fixef(Pass_BAS_full_DD_total_glm))
Pass_BAS_full_Declarin_glm.fixef <- as.data.frame(fixef(Pass_BAS_full_Declarin_glm))
Pass_BAS_full_Detectin_glm.fixef <- as.data.frame(fixef(Pass_BAS_full_Detectin_glm))
Pass_BAS_full_DD_total_glm_confint <- confint(Pass_BAS_full_DD_total_glm, method="Wald")[3:14,]
Pass_BAS_full_Declarin_glm_confint <- confint(Pass_BAS_full_Declarin_glm, method="Wald")[3:14,]
Pass_BAS_full_Detectin_glm_confint <- confint(Pass_BAS_full_Detectin_glm, method="Wald")[3:14,]
Pass_BAS_full_DD_total_glm.fixef <- cbind(Pass_BAS_full_DD_total_glm.fixef,Pass_BAS_full_DD_total_glm_confint)
Pass_BAS_full_Declarin_glm.fixef <- cbind(Pass_BAS_full_Declarin_glm.fixef,Pass_BAS_full_Declarin_glm_confint)
Pass_BAS_full_Detectin_glm.fixef <- cbind(Pass_BAS_full_Detectin_glm.fixef,Pass_BAS_full_Detectin_glm_confint)
colnames(Pass_BAS_full_DD_total_glm.fixef) <- c("Total", "Total_lci", "Total_uci")
colnames(Pass_BAS_full_Declarin_glm.fixef) <- c("Decle", "Decle_lci", "Decle_uci")
colnames(Pass_BAS_full_Detectin_glm.fixef) <- c("Detec", "Detec_lci", "Detec_uci")

Pass_BAS_full_DD_total_glm.ranef <- as.data.frame(ranef(Pass_BAS_full_DD_total_glm))
Pass_BAS_full_Declarin_glm.ranef <- as.data.frame(ranef(Pass_BAS_full_Declarin_glm))
Pass_BAS_full_Detectin_glm.ranef <- as.data.frame(ranef(Pass_BAS_full_Detectin_glm))

#write.csv(Pass_BAS_full_DD_total_glm.fixef, "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_glm.fixef.csv")
#write.csv(Pass_BAS_full_Declarin_glm.fixef, "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_glm.fixef.csv")
#write.csv(Pass_BAS_full_Detectin_glm.fixef, "~/CEBRA_AirInterventions/models/Pass_BAS_full_Detectin_glm.fixef.csv")
#write.csv(Pass_BAS_full_DD_total_glm.ranef, "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_glm.ranef.csv")
#write.csv(Pass_BAS_full_Declarin_glm.ranef, "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_glm.ranef.csv")
#write.csv(Pass_BAS_full_Detectin_glm.ranef, "~/CEBRA_AirInterventions/models/Pass_BAS_full_Detectin_glm.ranef.csv")

Pass_BAS_full_DD_total_FF_glm.fixef <- as.data.frame(fixef(Pass_BAS_full_DD_total_FF_glm))
Pass_BAS_full_Declarin_FF_glm.fixef <- as.data.frame(fixef(Pass_BAS_full_Declarin_FF_glm))
Pass_BAS_full_Detectin_FF_glm.fixef <- as.data.frame(fixef(Pass_BAS_full_Detectin_FF_glm))
Pass_BAS_full_DD_total_FF_glm_confint <- confint(Pass_BAS_full_DD_total_FF_glm, method="Wald")[3:14,]
Pass_BAS_full_Declarin_FF_glm_confint <- confint(Pass_BAS_full_Declarin_FF_glm, method="Wald")[3:14,]
Pass_BAS_full_Detectin_FF_glm_confint <- confint(Pass_BAS_full_Detectin_FF_glm, method="Wald")[3:14,]
Pass_BAS_full_DD_total_FF_glm.fixef <- cbind(Pass_BAS_full_DD_total_FF_glm.fixef,Pass_BAS_full_DD_total_FF_glm_confint)
Pass_BAS_full_Declarin_FF_glm.fixef <- cbind(Pass_BAS_full_Declarin_FF_glm.fixef,Pass_BAS_full_Declarin_FF_glm_confint)
Pass_BAS_full_Detectin_FF_glm.fixef <- cbind(Pass_BAS_full_Detectin_FF_glm.fixef,Pass_BAS_full_Detectin_FF_glm_confint)
colnames(Pass_BAS_full_DD_total_FF_glm.fixef) <- c("Total", "Total_lci", "Total_uci")
colnames(Pass_BAS_full_Declarin_FF_glm.fixef) <- c("Decle", "Decle_lci", "Decle_uci")
colnames(Pass_BAS_full_Detectin_FF_glm.fixef) <- c("Detec", "Detec_lci", "Detec_uci")

Pass_BAS_full_DD_total_FF_glm.ranef <- as.data.frame(ranef(Pass_BAS_full_DD_total_FF_glm))
Pass_BAS_full_Declarin_FF_glm.ranef <- as.data.frame(ranef(Pass_BAS_full_Declarin_FF_glm))
Pass_BAS_full_Detectin_FF_glm.ranef <- as.data.frame(ranef(Pass_BAS_full_Detectin_FF_glm))

#write.csv(Pass_BAS_full_DD_total_FF_glm.fixef, "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_FF_glm.fixef.csv")
#write.csv(Pass_BAS_full_Declarin_FF_glm.fixef, "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_glm.fixef.csv")
#write.csv(Pass_BAS_full_Detectin_FF_glm.fixef, "~/CEBRA_AirInterventions/models/Pass_BAS_full_Detectin_FF_glm.fixef.csv")
#write.csv(Pass_BAS_full_DD_total_FF_glm.ranef, "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_FF_glm.ranef.csv")
#write.csv(Pass_BAS_full_Declarin_FF_glm.ranef, "~/CEBRA_AirInterventions/models/Pass_BAS_full_Declarin_FF_glm.ranef.csv")
#write.csv(Pass_BAS_full_Detectin_FF_glm.ranef, "~/CEBRA_AirInterventions/models/Pass_BAS_full_Detectin_FF_glm.ranef.csv")




#### 2A. Outputs: Location ####

Means_Location<-setDT(Pass_BAS_dat_processed)[ , list(mean_location = mean(N_Total),
                                                      mean_FF_location = mean(N_Total_FF)), 
                                               by = .(Location)]
Means_Location


##Plot: BRM Interceptions
Loca_ems1 <- emmeans(Pass_BAS_full_DD_total_glm, ~Location)
Loca_ems2 <- emmeans(Pass_BAS_full_Declarin_glm, ~Location)
Loca_ems3 <- emmeans(Pass_BAS_full_Detectin_glm, ~Location)

Loca_pairs1 <- as.data.frame(summary(pairs(Loca_ems1), point.est = mean))
Loca_pairs2 <- as.data.frame(summary(pairs(Loca_ems2), point.est = mean))
Loca_pairs3 <- as.data.frame(summary(pairs(Loca_ems3), point.est = mean))

Loca_ems1 <- as.data.frame(Loca_ems1) 
Loca_ems2 <- as.data.frame(Loca_ems2) 
Loca_ems3 <- as.data.frame(Loca_ems3) 

Loca_ems1$Variable <- "N_Total" 
Loca_ems2$Variable <- "N_Declarations" 
Loca_ems3$Variable <- "N_Detections" 

Loca_ems1$Position <- c(20:15)
Loca_ems2$Position <- c(13:8)
Loca_ems3$Position <- c(6:1)

Loca_emsA <- rbind(Loca_ems1,Loca_ems2,Loca_ems3)
Loca_emsA$Variable <- ordered(Loca_emsA$Variable, levels = c("N_Total", "N_Declarations", "N_Detections"))

Loca_emsA$N_mean <- exp(Loca_emsA$emmean)
Loca_emsA$N_lci <- exp(Loca_emsA$asymp.LCL)
Loca_emsA$N_uci <- exp(Loca_emsA$asymp.UCL)

#Loca_emsA$Location <- gsub(".*\\_", "", Loca_emsA$Location) 

Loca_emsA$text <- paste(Loca_emsA$Location, round(exp(Loca_emsA$emmean), digits = 2), sep = ': ')
Loca_emsA$text <- paste(Loca_emsA$text, round(exp(Loca_emsA$asymp.LCL), digits = 2), sep = ' [')
Loca_emsA$text <- paste(Loca_emsA$text, round(exp(Loca_emsA$asymp.UCL), digits = 2), sep = ', ')
Loca_emsA$text <- paste(Loca_emsA$text, '', sep = ']')
Loca_emsA$text

Fig_Air_Loca_A <- ggplot(Loca_emsA, aes(x = N_mean, y = Position)) +
  scale_x_continuous(limits = c(-0.8, 2.2), expand = c(0, 0), breaks=c(0.0, 0.5, 1.0, 1.5, 2.0)) +
  scale_y_continuous(limits = c(0, 21), expand = c(0, 0), breaks=NULL) +
  theme(legend.position = c(0.85,0.175),
        legend.title = element_blank(),
        axis.text.y = element_blank(), 
        legend.text = element_text(size = 9, colour = "black", face = 'italic'), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", linewidth = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1)) +
  geom_segment(aes(color = Variable), x = Loca_emsA$N_lci, y = Loca_emsA$Position, xend = Loca_emsA$N_uci, yend = Loca_emsA$Position, size = 0.7) + 
  geom_point(aes(color = Variable), shape = 19, size = 2) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(7, 14), linetype = 2, colour = "black", linewidth = 0.2) +
  geom_text(aes(label=text, fontface = 1), hjust = "left", x =-0.77, vjust=0.25, size = 2.3) +
  #geom_text(aes(label=BeforeAfter, fontface = 1), hjust = "left", x =-10.5, vjust=0.5, size = 1.8) +
  labs(x = "Estimated BRM interceptions per flight",
       y = "") 
Fig_Air_Loca_A

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Loca_A.png", width = 18, height = 10, units = "cm", Fig_Air_Loca_A, dpi = 600)




##Plot: FF Interceptions
Loca_ems4 <- emmeans(Pass_BAS_full_DD_total_FF_glm, ~Location)
Loca_ems5 <- emmeans(Pass_BAS_full_Declarin_FF_glm, ~Location)
Loca_ems6 <- emmeans(Pass_BAS_full_Detectin_FF_glm, ~Location)

#pairwise
Loca_pairs4 <- as.data.frame(summary(pairs(Loca_ems4), point.est = mean))
Loca_pairs5 <- as.data.frame(summary(pairs(Loca_ems5), point.est = mean))
Loca_pairs6 <- as.data.frame(summary(pairs(Loca_ems6), point.est = mean))

#text
Loca_ems4 <- as.data.frame(Loca_ems4) 
Loca_ems5 <- as.data.frame(Loca_ems5) 
Loca_ems6 <- as.data.frame(Loca_ems6) 

Loca_ems4$Variable <- "N_Total_FF" 
Loca_ems5$Variable <- "N_Declarations_FF" 
Loca_ems6$Variable <- "N_Detections_FF" 

Loca_ems4$Position <- c(20:15)
Loca_ems5$Position <- c(13:8)
Loca_ems6$Position <- c(6:1)

Loca_emsB <- rbind(Loca_ems4,Loca_ems5,Loca_ems6)
Loca_emsB$Variable <- ordered(Loca_emsB$Variable, levels = c("N_Total_FF", "N_Declarations_FF", "N_Detections_FF"))

Loca_emsB$N_mean <- exp(Loca_emsB$emmean)
Loca_emsB$N_lci <- exp(Loca_emsB$asymp.LCL)
Loca_emsB$N_uci <- exp(Loca_emsB$asymp.UCL)

#Loca_emsB$Location <- gsub(".*\\_", "", Loca_emsB$Location) 

Loca_emsB$text <- paste(Loca_emsB$Location, round(exp(Loca_emsB$emmean), digits = 2), sep = ': ')
Loca_emsB$text <- paste(Loca_emsB$text, round(exp(Loca_emsB$asymp.LCL), digits = 2), sep = ' [')
Loca_emsB$text <- paste(Loca_emsB$text, round(exp(Loca_emsB$asymp.UCL), digits = 2), sep = ', ')
Loca_emsB$text <- paste(Loca_emsB$text, '', sep = ']')
#Loca_emsB$text

Fig_Air_Loca_B <- ggplot(Loca_emsB, aes(x = N_mean, y = Position)) +
  scale_x_continuous(limits = c(-0.8, 2.2), expand = c(0, 0), breaks=c(0.0, 0.5, 1.0, 1.5, 2.0)) +
  scale_y_continuous(limits = c(0, 21), expand = c(0, 0), breaks=NULL) +
  theme(legend.position = c(0.85,0.175),
        legend.title = element_blank(),
        axis.text.y = element_blank(), 
        legend.text = element_text(size = 9, colour = "black", face = 'italic'), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", linewidth = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1)) +
  geom_segment(aes(color = Variable), x = Loca_emsB$N_lci, y = Loca_emsB$Position, xend = Loca_emsB$N_uci, yend = Loca_emsB$Position, size = 0.7) + 
  geom_point(aes(color = Variable), shape = 19, size = 2) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(7, 14), linetype = 2, colour = "black", linewidth = 0.2) +
  geom_text(aes(label=text, fontface = 1), hjust = "left", x =-0.77, vjust=0.25, size = 2.3) +
  #geom_text(aes(label=BeforeAfter, fontface = 1), hjust = "left", x =-10.5, vjust=0.5, size = 1.8) +
  labs(x = "Estimated FF host interceptions per flight",
       y = "") 
Fig_Air_Loca_B

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Loca_B.png", width = 18, height = 10, units = "cm", Fig_Air_Loca_B, dpi = 600)




#### 2B. Outputs: Regime ####

Means_Regime <-setDT(Pass_BAS_dat_processed)[ , list(mean_regime = mean(N_Total), 
                                                     mean_FF_regime = mean(N_Total_FF)), 
                                              by = .(Regime)]
Means_Regime

##Plot: FF Interceptions by Location X Regime
Pass_BAS_dat_processed_1 <- subset(Pass_BAS_dat_processed, Location == "Airport_A")
Pass_BAS_dat_processed_2 <- subset(Pass_BAS_dat_processed, Location == "Airport_B")
Pass_BAS_dat_processed_3 <- subset(Pass_BAS_dat_processed, Location == "Airport_C")
Pass_BAS_dat_processed_4 <- subset(Pass_BAS_dat_processed, Location == "Airport_D")
Pass_BAS_dat_processed_5 <- subset(Pass_BAS_dat_processed, Location == "Airport_E")
Pass_BAS_dat_processed_6 <- subset(Pass_BAS_dat_processed, Location == "Airport_F")

LocReg <- setDT(Pass_BAS_dat_processed)[ , list(N_Total_mean = mean(N_Total), 
                                                N_Declarations_mean = mean(N_Declarations),
                                                N_Detections_mean = mean(N_Detections)), 
                                         by = .(Regime)]
LocReg$Location <- "All"

LocReg_1 <- setDT(Pass_BAS_dat_processed_1)[ , list(N_Total_mean = mean(N_Total), 
                                                    N_Declarations_mean = mean(N_Declarations),
                                                    N_Detections_mean = mean(N_Detections)), 
                                             by = .(Regime)]
LocReg_1$Location <- "Airport_A"

LocReg_2 <- setDT(Pass_BAS_dat_processed_2)[ , list(N_Total_mean = mean(N_Total), 
                                                    N_Declarations_mean = mean(N_Declarations),
                                                    N_Detections_mean = mean(N_Detections)), 
                                             by = .(Regime)]
LocReg_2$Location <- "Airport_B"

LocReg_3 <- setDT(Pass_BAS_dat_processed_3)[ , list(N_Total_mean = mean(N_Total), 
                                                    N_Declarations_mean = mean(N_Declarations),
                                                    N_Detections_mean = mean(N_Detections)), 
                                             by = .(Regime)]
LocReg_3$Location <- "Airport_C"

LocReg_4 <- setDT(Pass_BAS_dat_processed_4)[ , list(N_Total_mean = mean(N_Total), 
                                                    N_Declarations_mean = mean(N_Declarations),
                                                    N_Detections_mean = mean(N_Detections)), 
                                             by = .(Regime)]
LocReg_4$Location <- "Airport_D"

LocReg_5 <- setDT(Pass_BAS_dat_processed_5)[ , list(N_Total_mean = mean(N_Total), 
                                                    N_Declarations_mean = mean(N_Declarations),
                                                    N_Detections_mean = mean(N_Detections)), 
                                             by = .(Regime)]
LocReg_5$Location <- "Airport_E"

LocReg_6 <- setDT(Pass_BAS_dat_processed_6)[ , list(N_Total_mean = mean(N_Total), 
                                                    N_Declarations_mean = mean(N_Declarations),
                                                    N_Detections_mean = mean(N_Detections)), 
                                             by = .(Regime)]
LocReg_6$Location <- "Airport_F"


LocReg_all <- rbind(LocReg,LocReg_1,LocReg_2,LocReg_3,LocReg_4,LocReg_5,LocReg_6)

LocReg_all$Regime <- case_when(
  LocReg_all$Regime %in% c("BI") ~ "one BI",
  LocReg_all$Regime %in% c("BI x 2") ~ "two BIs",
  LocReg_all$Regime %in% c("DDT") ~ "one DDT",
  LocReg_all$Regime %in% c("DDT + BI") ~ "one DDT & BI",
  LocReg_all$Regime %in% c("DDT x2") ~ "two DDTs",
  .default = LocReg_all$Regime
)

LocReg_all$Regime <- ordered(LocReg_all$Regime, levels = c("one BI", "two BIs", "one DDT", "one DDT & BI", "two DDTs"))
LocReg_all$Location <- ordered(LocReg_all$Location, levels = c("All","Airport_A","Airport_B","Airport_C", "Airport_D", "Airport_E", "Airport_F"))

Fig_Air_Regi_Obvs <- ggplot(LocReg_all, aes(x = N_Detections_mean, y = N_Declarations_mean)) +
  scale_y_continuous(limits = c(0, 1.2), expand = c(0.05,0), breaks=c(0.0, 0.5, 1.0)) +
  scale_x_continuous(limits = c(0, 2), expand = c(0.05,0), breaks=c(0.0, 0.5, 1.0, 1.5, 2.0)) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 8, colour = "black"), 
        axis.text.x = element_text(size = 8, colour = "black"), 
        axis.text.y = element_text(size = 8, colour = "black"), 
        axis.title.x  = element_text(size=10, vjust = 0.1),
        axis.title.y  = element_text(size=10, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.7) +
  geom_hline(yintercept = 0, linetype = 2, colour = "black", size = 0.7) +
  geom_segment(color = 'grey', x =0, y = 0.5, xend = 0.5, yend = 0, size = 0.45, linetype=5) + 
  geom_segment(color = 'grey', x =0, y = 1.0, xend = 1.0, yend = 0, size = 0.45, linetype=5) + 
  geom_segment(color = 'grey', x =0, y = 1.5, xend = 1.5, yend = 0, size = 0.45, linetype=5) + 
  geom_segment(color = 'grey', x =0, y = 2.0, xend = 2.0, yend = 0, size = 0.45, linetype=5) + 
  geom_segment(color = 'grey', x =0, y = 2.5, xend = 2.5, yend = 0, size = 0.45, linetype=5) + 
  geom_segment(color = 'grey', x =0, y = 3.0, xend = 3.0, yend = 0, size = 0.45, linetype=5) + 
  geom_point(aes(shape = Regime, color = Location), size = 5.5, alpha = 0.85) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Average BRM detections per flight",
       y = "Average BRM declarations per flight") 
Fig_Air_Regi_Obvs

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Regi_Obvs.png", width = 18, height = 9, units = "cm", Fig_Air_Regi_Obvs, dpi = 600)


##Plot: BRM Interceptions
Regi_ems1 <- emmeans(Pass_BAS_full_DD_total_glm, ~Regime)
Regi_ems2 <- emmeans(Pass_BAS_full_Declarin_glm, ~Regime)
Regi_ems6 <- emmeans(Pass_BAS_full_Detectin_glm, ~Regime)

Regi_pairs1 <- as.data.frame(summary(pairs(Regi_ems1), point.est = mean))
Regi_pairs2 <- as.data.frame(summary(pairs(Regi_ems2), point.est = mean))
Regi_pairs3 <- as.data.frame(summary(pairs(Regi_ems6), point.est = mean))

Regi_ems1 <- as.data.frame(Regi_ems1) 
Regi_ems2 <- as.data.frame(Regi_ems2) 
Regi_ems6 <- as.data.frame(Regi_ems6) 

Regi_ems1$Regime <- c("One BI", "Two BIs", "One DDT", "One DDT & one BI", "Two DDTs")
Regi_ems2$Regime <- c("One BI", "Two BIs", "One DDT", "One DDT & one BI", "Two DDTs")
Regi_ems6$Regime <- c("One BI", "Two BIs", "One DDT", "One DDT & one BI", "Two DDTs")

Regi_ems1$Variable <- "N_Total" 
Regi_ems2$Variable <- "N_Declarations" 
Regi_ems6$Variable <- "N_Detections" 

Regi_ems1$Position <- c(13:17)
Regi_ems2$Position <- c(7:11)
Regi_ems6$Position <- c(1:5)

Regi_emsA <- rbind(Regi_ems1,Regi_ems2,Regi_ems6)
Regi_emsA$Variable <- ordered(Regi_emsA$Variable, levels = c("N_Total", "N_Declarations", "N_Detections"))

Regi_emsA$N_mean <- exp(Regi_emsA$emmean)
Regi_emsA$N_lci <- exp(Regi_emsA$asymp.LCL)
Regi_emsA$N_uci <- exp(Regi_emsA$asymp.UCL)

Regi_emsA$text <- paste(Regi_emsA$Regime, round(exp(Regi_emsA$emmean), digits = 2), sep = ': ')
Regi_emsA$text <- paste(Regi_emsA$text, round(exp(Regi_emsA$asymp.LCL), digits = 2), sep = ' [')
Regi_emsA$text <- paste(Regi_emsA$text, round(exp(Regi_emsA$asymp.UCL), digits = 2), sep = ', ')
Regi_emsA$text <- paste(Regi_emsA$text, '', sep = ']')
#Regi_emsA$text

Fig_Air_Regi_A <- ggplot(Regi_emsA, aes(x = N_mean, y = Position)) +
  scale_x_continuous(limits = c(-0.6, 1.7), expand = c(0, 0), breaks=c(0.0, 0.5, 1.0, 1.5)) +
  scale_y_continuous(limits = c(0, 18), expand = c(0, 0), breaks=NULL) +
  theme(legend.position = c(0.875,0.175),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 9, colour = "black", face = 'italic'), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(aes(color = Variable), x = Regi_emsA$N_lci, y = Regi_emsA$Position, xend = Regi_emsA$N_uci, yend = Regi_emsA$Position, size = 0.7) + 
  geom_point(aes(color = Variable), shape = 19, size = 2) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  geom_hline(yintercept = c(6, 12), linetype = 2, colour = "black", size = 0.2) +
  geom_text(aes(label=text, fontface = 1), hjust = "left", x =-0.57, vjust=0.25, size = 2.3) +
  #geom_text(aes(label=BeforeAfter, fontface = 1), hjust = "left", x =-10.5, vjust=0.5, size = 1.8) +
  labs(x = "Estimated BRM interceptions per flight",
       y = "") 
Fig_Air_Regi_A

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Regi_A.png", width = 18, height = 8, units = "cm", Fig_Air_Regi_A, dpi = 600)




##Plot: FF Interceptions
Regi_ems4 <- emmeans(Pass_BAS_full_DD_total_FF_glm, ~Regime)
Regi_ems5 <- emmeans(Pass_BAS_full_Declarin_FF_glm, ~Regime)
Regi_ems3 <- emmeans(Pass_BAS_full_Detectin_FF_glm, ~Regime)

Regi_pairs4 <- as.data.frame(summary(pairs(Regi_ems4), point.est = mean))
Regi_pairs5 <- as.data.frame(summary(pairs(Regi_ems5), point.est = mean))
Regi_pairs6 <- as.data.frame(summary(pairs(Regi_ems3), point.est = mean))

Regi_ems4 <- as.data.frame(Regi_ems4) 
Regi_ems5 <- as.data.frame(Regi_ems5) 
Regi_ems3 <- as.data.frame(Regi_ems3) 

Regi_ems4$Regime <- c("One BI", "Two BIs", "One DDT", "One DDT & one BI", "Two DDTs")
Regi_ems5$Regime <- c("One BI", "Two BIs", "One DDT", "One DDT & one BI", "Two DDTs")
Regi_ems3$Regime <- c("One BI", "Two BIs", "One DDT", "One DDT & one BI", "Two DDTs")

Regi_ems4$Variable <- "N_Total_FF" 
Regi_ems5$Variable <- "N_Declarations_FF" 
Regi_ems3$Variable <- "N_Detections_FF" 

Regi_ems4$Position <- c(13:17)
Regi_ems5$Position <- c(7:11)
Regi_ems3$Position <- c(1:5)

Regi_emsB <- rbind(Regi_ems4,Regi_ems5,Regi_ems3)
Regi_emsB$Variable <- ordered(Regi_emsB$Variable, levels = c("N_Total_FF", "N_Declarations_FF", "N_Detections_FF"))

Regi_emsB$N_mean <- exp(Regi_emsB$emmean)
Regi_emsB$N_lci <- exp(Regi_emsB$asymp.LCL)
Regi_emsB$N_uci <- exp(Regi_emsB$asymp.UCL)

Regi_emsB$text <- paste(Regi_emsB$Regime, round(exp(Regi_emsB$emmean), digits = 2), sep = ': ')
Regi_emsB$text <- paste(Regi_emsB$text, round(exp(Regi_emsB$asymp.LCL), digits = 2), sep = ' [')
Regi_emsB$text <- paste(Regi_emsB$text, round(exp(Regi_emsB$asymp.UCL), digits = 2), sep = ', ')
Regi_emsB$text <- paste(Regi_emsB$text, '', sep = ']')
#Regi_emsB$text

Fig_Air_Regi_B <- ggplot(Regi_emsB, aes(x = N_mean, y = Position)) +
  scale_x_continuous(limits = c(-0.6, 1.7), expand = c(0, 0), breaks=c(0.0, 0.5, 1.0, 1.5)) +
  scale_y_continuous(limits = c(0, 18), expand = c(0, 0), breaks=NULL) +
  theme(legend.position = c(0.875,0.175),
        legend.title = element_blank(),
        axis.text.y = element_blank(), 
        legend.background = element_blank(), 
        legend.text = element_text(size = 9, colour = "black", face = 'italic'), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(aes(color = Variable), x = Regi_emsB$N_lci, y = Regi_emsB$Position, xend = Regi_emsB$N_uci, yend = Regi_emsB$Position, size = 0.7) + 
  geom_point(aes(color = Variable), shape = 19, size = 2) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  geom_hline(yintercept = c(6, 12), linetype = 2, colour = "black", size = 0.2) +
  geom_text(aes(label=text, fontface = 1), hjust = "left", x =-0.57, vjust=0.25, size = 2.3) +
  #geom_text(aes(label=BeforeAfter, fontface = 1), hjust = "left", x =-10.5, vjust=0.5, size = 1.8) +
  labs(x = "Estimated FF host interceptions per flight",
       y = "") 
Fig_Air_Regi_B

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Regi_B.png", width = 18, height = 8, units = "cm", Fig_Air_Regi_B, dpi = 600)




#### 2C. Outputs: Bag searches ####
## -  extracting effect estimates as per-(sqrt)unit percentage effects
Eff_Bag1 <- as.data.frame(Pass_BAS_full_DD_total_glm.fixef[11,])
Eff_Bag2 <- as.data.frame(Pass_BAS_full_Declarin_glm.fixef[11,])
Eff_Bag3 <- as.data.frame(Pass_BAS_full_Detectin_glm.fixef[11,])
Eff_Bag4 <- as.data.frame(Pass_BAS_full_DD_total_FF_glm.fixef[11,])
Eff_Bag5 <- as.data.frame(Pass_BAS_full_Declarin_FF_glm.fixef[11,])
Eff_Bag6 <- as.data.frame(Pass_BAS_full_Detectin_FF_glm.fixef[11,])
colnames(Eff_Bag1) <- c("A","B","C")
colnames(Eff_Bag2) <- c("A","B","C")
colnames(Eff_Bag3) <- c("A","B","C")
colnames(Eff_Bag4) <- c("A","B","C")
colnames(Eff_Bag5) <- c("A","B","C")
colnames(Eff_Bag6) <- c("A","B","C")
Eff_Bag <- rbind(Eff_Bag1,Eff_Bag2,Eff_Bag3,Eff_Bag4,Eff_Bag5,Eff_Bag6)

#converting the effect to a proportional change in root-bag searches (i.e. reverse z-scaling)
Eff_Bag$Exp_est <- exp(Eff_Bag$A)^(1/(sd(subset(Pass_BAS_dat_processed.mod, BagSearchCount != 'NA')$sqrt.BagSearchCount)))
Eff_Bag$LCI <- exp(Eff_Bag$B)^(1/(sd(subset(Pass_BAS_dat_processed.mod, BagSearchCount != 'NA')$sqrt.BagSearchCount)))
Eff_Bag$UCI <- exp(Eff_Bag$C)^(1/(sd(subset(Pass_BAS_dat_processed.mod, BagSearchCount != 'NA')$sqrt.BagSearchCount)))

Eff_Bag$Effect_percent <- paste(round(((Eff_Bag$Exp_est-1)*100), digits = 1), round(((Eff_Bag$LCI-1)*100), digits = 1), sep = "% [")
Eff_Bag$Effect_percent <- paste(Eff_Bag$Effect_percent, round(((Eff_Bag$UCI-1)*100), digits = 1), sep = "%, ")
Eff_Bag$Effect_percent <- paste(Eff_Bag$Effect_percent, "%]", sep = "")
Eff_Bag$Variable <- c("N_Total","N_Declarations","N_Detections","N_Total_FF","N_Declarations_FF","N_Detections_FF")
Eff_Bag <- Eff_Bag[,-1:-6]
Eff_Bag <- Eff_Bag[,c(2,1)]
knitr::kable(Eff_Bag, "simple", align = "lc", row.names = FALSE)



#### 2D. Outputs: Passenger Counts ####
## -  extracting effect estimates as per-(sqrt)unit percentage effects
Eff_Pas1 <- as.data.frame(Pass_BAS_full_DD_total_glm.fixef[12,])
Eff_Pas2 <- as.data.frame(Pass_BAS_full_Declarin_glm.fixef[12,])
Eff_Pas3 <- as.data.frame(Pass_BAS_full_Detectin_glm.fixef[12,])
Eff_Pas4 <- as.data.frame(Pass_BAS_full_DD_total_FF_glm.fixef[12,])
Eff_Pas5 <- as.data.frame(Pass_BAS_full_Declarin_FF_glm.fixef[12,])
Eff_Pas6 <- as.data.frame(Pass_BAS_full_Detectin_FF_glm.fixef[12,])
colnames(Eff_Pas1) <- c("A","B","C")
colnames(Eff_Pas2) <- c("A","B","C")
colnames(Eff_Pas3) <- c("A","B","C")
colnames(Eff_Pas4) <- c("A","B","C")
colnames(Eff_Pas5) <- c("A","B","C")
colnames(Eff_Pas6) <- c("A","B","C")

Eff_Pas <- rbind(Eff_Pas1,Eff_Pas2,Eff_Pas3,
                 Eff_Pas4,Eff_Pas5,Eff_Pas6)

#converting the effect to a proportional change in root-bag searches (i.e. reverse z-scaling)
Eff_Pas$Exp_est <- exp(Eff_Pas$A)^(1/(sd(subset(Pass_BAS_dat_processed.mod, PassengerCount != 'NA')$sqrt.PassengerCount)))
Eff_Pas$LCI <- exp(Eff_Pas$B)^(1/(sd(subset(Pass_BAS_dat_processed.mod, PassengerCount != 'NA')$sqrt.PassengerCount)))
Eff_Pas$UCI <- exp(Eff_Pas$C)^(1/(sd(subset(Pass_BAS_dat_processed.mod, PassengerCount != 'NA')$sqrt.PassengerCount)))

Eff_Pas$Effect_percent <- paste(round(((Eff_Pas$Exp_est-1)*100), digits = 1), round(((Eff_Pas$LCI-1)*100), digits = 1), sep = "% [")
Eff_Pas$Effect_percent <- paste(Eff_Pas$Effect_percent, round(((Eff_Pas$UCI-1)*100), digits = 1), sep = "%, ")
Eff_Pas$Effect_percent <- paste(Eff_Pas$Effect_percent, "%]", sep = "")
Eff_Pas$Variable <- c("N_Total","N_Declarations","N_Detections","N_Total_FF","N_Declarations_FF","N_Detections_FF")
Eff_Pas <- Eff_Pas[,-1:-6]
Eff_Pas <- Eff_Pas[,c(2,1)]
knitr::kable(Eff_Pas, "simple", align = "lc", row.names = FALSE)




#### 2E. Outputs: Flight Origin/ Flight Number ####

#Plots: BRM Interceptions X Origin
Pass_BAS_full_DD_total_glm.FO <- subset(Pass_BAS_full_DD_total_glm.ranef, grpvar == "FlightOrigin")
Pass_BAS_full_DD_total_glm.FO$lci <- Pass_BAS_full_DD_total_glm.FO$condval - 1.96*(Pass_BAS_full_DD_total_glm.FO$condsd)
Pass_BAS_full_DD_total_glm.FO$uci <- Pass_BAS_full_DD_total_glm.FO$condval + 1.96*(Pass_BAS_full_DD_total_glm.FO$condsd)
Pass_BAS_full_DD_total_glm.FO <- Pass_BAS_full_DD_total_glm.FO[order(Pass_BAS_full_DD_total_glm.FO$condval,decreasing=TRUE),]
Pass_BAS_full_DD_total_glm.FO$Position <- c(7:1)

Fig_Air_Orig_A1 <- ggplot(Pass_BAS_full_DD_total_glm.FO, aes(x = condval, y = Position)) +
  scale_x_continuous(limits = c(-0.3, 0.3), expand = c(0, 0), breaks=c(-0.3, 0.0, 0.3)) +
  scale_y_continuous(limits = c(0.25, 7.75), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(colour = '#1B9E77', x = Pass_BAS_full_DD_total_glm.FO$lci, y = Pass_BAS_full_DD_total_glm.FO$Position, xend = Pass_BAS_full_DD_total_glm.FO$uci, yend = Pass_BAS_full_DD_total_glm.FO$Position, size = 0.4) + 
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=grp, fontface = 1), hjust = "left", x =-0.29, vjust=-0.4, size = 2.3) +
  labs(x = "Intercept estimates by flight origin",
       y = "") +
  annotate("text", x = 0.15, y = 0.6, size = 2.4, label = "italic(N_Total)", parse = TRUE)
Fig_Air_Orig_A1

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Orig_A1.png", width = 6, height = 5, units = "cm", Fig_Air_Orig_A1, dpi = 600)


Pass_BAS_full_Declarin_glm.FO <- subset(Pass_BAS_full_Declarin_glm.ranef, grpvar == "FlightOrigin")
Pass_BAS_full_Declarin_glm.FO$lci <- Pass_BAS_full_Declarin_glm.FO$condval - 1.96*(Pass_BAS_full_Declarin_glm.FO$condsd)
Pass_BAS_full_Declarin_glm.FO$uci <- Pass_BAS_full_Declarin_glm.FO$condval + 1.96*(Pass_BAS_full_Declarin_glm.FO$condsd)
Pass_BAS_full_Declarin_glm.FO <- Pass_BAS_full_Declarin_glm.FO[order(Pass_BAS_full_Declarin_glm.FO$condval,decreasing=TRUE),]
Pass_BAS_full_Declarin_glm.FO$Position <- c(7:1)

Fig_Air_Orig_A2 <- ggplot(Pass_BAS_full_Declarin_glm.FO, aes(x = condval, y = Position)) +
  scale_x_continuous(limits = c(-0.6, 0.6), expand = c(0, 0), breaks=c(-0.6, 0.0, 0.6)) +
  scale_y_continuous(limits = c(0.25, 7.75), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(colour = '#D95F02', x = Pass_BAS_full_Declarin_glm.FO$lci, y = Pass_BAS_full_Declarin_glm.FO$Position, xend = Pass_BAS_full_Declarin_glm.FO$uci, yend = Pass_BAS_full_Declarin_glm.FO$Position, size = 0.4) + 
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=grp, fontface = 1), hjust = "left", x =-0.58, vjust=-0.4, size = 2.3) +
  labs(x = "Intercept estimates by flight origin",
       y = "") +
  annotate("text", x = 0.3, y = 0.6, size = 2.4, label = "italic(N_Declarations)", parse = TRUE)
Fig_Air_Orig_A2

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Orig_A2.png", width = 6, height = 5, units = "cm", Fig_Air_Orig_A2, dpi = 600)



Pass_BAS_full_Detectin_glm.FO <- subset(Pass_BAS_full_Detectin_glm.ranef, grpvar == "FlightOrigin")
Pass_BAS_full_Detectin_glm.FO$lci <- Pass_BAS_full_Detectin_glm.FO$condval - 1.96*(Pass_BAS_full_Detectin_glm.FO$condsd)
Pass_BAS_full_Detectin_glm.FO$uci <- Pass_BAS_full_Detectin_glm.FO$condval + 1.96*(Pass_BAS_full_Detectin_glm.FO$condsd)
Pass_BAS_full_Detectin_glm.FO <- Pass_BAS_full_Detectin_glm.FO[order(Pass_BAS_full_Detectin_glm.FO$condval,decreasing=TRUE),]
Pass_BAS_full_Detectin_glm.FO$Position <- c(7:1)

Fig_Air_Orig_A3 <- ggplot(Pass_BAS_full_Detectin_glm.FO, aes(x = condval, y = Position)) +
  scale_x_continuous(limits = c(-0.3, 0.3), expand = c(0, 0), breaks=c(-0.3, 0.0, 0.3)) +
  scale_y_continuous(limits = c(0.25, 7.75), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(colour = '#7570B3', x = Pass_BAS_full_Detectin_glm.FO$lci, y = Pass_BAS_full_Detectin_glm.FO$Position, xend = Pass_BAS_full_Detectin_glm.FO$uci, yend = Pass_BAS_full_Detectin_glm.FO$Position, size = 0.4) + 
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=grp, fontface = 1), hjust = "left", x =-0.29, vjust=-0.4, size = 2.3) +
  labs(x = "Intercept estimates by flight origin",
       y = "") +
  annotate("text", x = 0.15, y = 0.6, size = 2.4, label = "italic(N_Detections)", parse = TRUE)
Fig_Air_Orig_A3

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Orig_A3.png", width = 6, height = 5, units = "cm", Fig_Air_Orig_A3, dpi = 600)



#Plots: FF Interceptions X Origin

Pass_BAS_full_DD_total_FF_glm.FO <- subset(Pass_BAS_full_DD_total_FF_glm.ranef, grpvar == "FlightOrigin")
Pass_BAS_full_DD_total_FF_glm.FO$lci <- Pass_BAS_full_DD_total_FF_glm.FO$condval - 1.96*(Pass_BAS_full_DD_total_FF_glm.FO$condsd)
Pass_BAS_full_DD_total_FF_glm.FO$uci <- Pass_BAS_full_DD_total_FF_glm.FO$condval + 1.96*(Pass_BAS_full_DD_total_FF_glm.FO$condsd)
Pass_BAS_full_DD_total_FF_glm.FO <- Pass_BAS_full_DD_total_FF_glm.FO[order(Pass_BAS_full_DD_total_FF_glm.FO$condval,decreasing=TRUE),]
Pass_BAS_full_DD_total_FF_glm.FO$Position <- c(7:1)

Fig_Bir_Orig_B1 <- ggplot(Pass_BAS_full_DD_total_FF_glm.FO, aes(x = condval, y = Position)) +
  scale_x_continuous(limits = c(-0.3, 0.3), expand = c(0, 0), breaks=c(-0.3, 0.0, 0.3)) +
  scale_y_continuous(limits = c(0.25, 7.75), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(colour = '#1B9E77', x = Pass_BAS_full_DD_total_FF_glm.FO$lci, y = Pass_BAS_full_DD_total_FF_glm.FO$Position, xend = Pass_BAS_full_DD_total_FF_glm.FO$uci, yend = Pass_BAS_full_DD_total_FF_glm.FO$Position, size = 0.4) + 
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=grp, fontface = 1), hjust = "left", x =-0.29, vjust=-0.4, size = 2.3) +
  labs(x = "Intercept estimates by flight origin",
       y = "") +
  annotate("text", x = 0.15, y = 0.6, size = 2.4, label = "italic(N_Total_FF)", parse = TRUE)
Fig_Bir_Orig_B1

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Orig_B1.png", width = 6, height = 5, units = "cm", Fig_Bir_Orig_B1, dpi = 600)


Pass_BAS_full_Declarin_FF_glm.FO <- subset(Pass_BAS_full_Declarin_FF_glm.ranef, grpvar == "FlightOrigin")
Pass_BAS_full_Declarin_FF_glm.FO$lci <- Pass_BAS_full_Declarin_FF_glm.FO$condval - 1.96*(Pass_BAS_full_Declarin_FF_glm.FO$condsd)
Pass_BAS_full_Declarin_FF_glm.FO$uci <- Pass_BAS_full_Declarin_FF_glm.FO$condval + 1.96*(Pass_BAS_full_Declarin_FF_glm.FO$condsd)
Pass_BAS_full_Declarin_FF_glm.FO <- Pass_BAS_full_Declarin_FF_glm.FO[order(Pass_BAS_full_Declarin_FF_glm.FO$condval,decreasing=TRUE),]
Pass_BAS_full_Declarin_FF_glm.FO$Position <- c(7:1)

Fig_Bir_Orig_B2 <- ggplot(Pass_BAS_full_Declarin_FF_glm.FO, aes(x = condval, y = Position)) +
  scale_x_continuous(limits = c(-0.6, 0.6), expand = c(0, 0), breaks=c(-0.6, 0.0, 0.6)) +
  scale_y_continuous(limits = c(0.25, 7.75), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(colour = '#D95F02', x = Pass_BAS_full_Declarin_FF_glm.FO$lci, y = Pass_BAS_full_Declarin_FF_glm.FO$Position, xend = Pass_BAS_full_Declarin_FF_glm.FO$uci, yend = Pass_BAS_full_Declarin_FF_glm.FO$Position, size = 0.4) + 
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=grp, fontface = 1), hjust = "left", x =-0.58, vjust=-0.4, size = 2.3) +
  labs(x = "Intercept estimates by flight origin",
       y = "") +
  annotate("text", x = 0.3, y = 0.6, size = 2.4, label = "italic(N_Declarations_FF)", parse = TRUE)
Fig_Bir_Orig_B2

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Orig_B2.png", width = 6, height = 5, units = "cm", Fig_Bir_Orig_B2, dpi = 600)



Pass_BAS_full_Detectin_FF_glm.FO <- subset(Pass_BAS_full_Detectin_FF_glm.ranef, grpvar == "FlightOrigin")
Pass_BAS_full_Detectin_FF_glm.FO$lci <- Pass_BAS_full_Detectin_FF_glm.FO$condval - 1.96*(Pass_BAS_full_Detectin_FF_glm.FO$condsd)
Pass_BAS_full_Detectin_FF_glm.FO$uci <- Pass_BAS_full_Detectin_FF_glm.FO$condval + 1.96*(Pass_BAS_full_Detectin_FF_glm.FO$condsd)
Pass_BAS_full_Detectin_FF_glm.FO <- Pass_BAS_full_Detectin_FF_glm.FO[order(Pass_BAS_full_Detectin_FF_glm.FO$condval,decreasing=TRUE),]
Pass_BAS_full_Detectin_FF_glm.FO$Position <- c(7:1)

Fig_Bir_Orig_B3 <- ggplot(Pass_BAS_full_Detectin_FF_glm.FO, aes(x = condval, y = Position)) +
  scale_x_continuous(limits = c(-0.4, 0.4), expand = c(0, 0), breaks=c(-0.4, 0.0, 0.4)) +
  scale_y_continuous(limits = c(0.25, 7.75), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(colour = '#7570B3', x = Pass_BAS_full_Detectin_FF_glm.FO$lci, y = Pass_BAS_full_Detectin_FF_glm.FO$Position, xend = Pass_BAS_full_Detectin_FF_glm.FO$uci, yend = Pass_BAS_full_Detectin_FF_glm.FO$Position, size = 0.4) + 
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=grp, fontface = 1), hjust = "left", x =-0.385, vjust=-0.4, size = 2.3) +
  labs(x = "Intercept estimates by flight origin",
       y = "") +
  annotate("text", x = 0.2, y = 0.6, size = 2.4, label = "italic(N_Detections_FF)", parse = TRUE)
Fig_Bir_Orig_B3

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Orig_B3.png", width = 6, height = 5, units = "cm", Fig_Bir_Orig_B3, dpi = 600)




#Plots: BRM Interceptions X Number
Pass_BAS_full_DD_total_glm.FN <- subset(Pass_BAS_full_DD_total_glm.ranef, grpvar == "FlightNumber:FlightOrigin")
Pass_BAS_full_DD_total_glm.FN$lci <- Pass_BAS_full_DD_total_glm.FN$condval - 1.96*(Pass_BAS_full_DD_total_glm.FN$condsd)
Pass_BAS_full_DD_total_glm.FN$uci <- Pass_BAS_full_DD_total_glm.FN$condval + 1.96*(Pass_BAS_full_DD_total_glm.FN$condsd)
Pass_BAS_full_DD_total_glm.FN <- Pass_BAS_full_DD_total_glm.FN[order(Pass_BAS_full_DD_total_glm.FN$condval,decreasing=TRUE),]
Pass_BAS_full_DD_total_glm.FN$Position <- c(217:1)

Fig_Air_Numb_A1 <- ggplot(Pass_BAS_full_DD_total_glm.FN, aes(x = condval, y = Position)) +
  scale_x_continuous(limits = c(-1, 0.8), expand = c(0, 0), breaks=c(-0.8, 0.0, 0.8)) +
  scale_y_continuous(limits = c(-1, 219), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(colour = '#1B9E77', x = Pass_BAS_full_DD_total_glm.FN$lci, y = Pass_BAS_full_DD_total_glm.FN$Position, xend = Pass_BAS_full_DD_total_glm.FN$uci, yend = Pass_BAS_full_DD_total_glm.FN$Position, size = 0.4) + 
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=grp, fontface = 1), hjust = "left", x =-0.97, vjust=0.5, size = 1.2) +
  labs(x = "Intercept estimates by flight number",
       y = "") +
  annotate("text", x = 0.4, y = 0.65, size = 2.4, label = "italic(N_Total)", parse = TRUE)
Fig_Air_Numb_A1

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Numb_A1.png", width = 6, height = 24, units = "cm", Fig_Air_Numb_A1, dpi = 600)



Pass_BAS_full_Declarin_glm.FN <- subset(Pass_BAS_full_Declarin_glm.ranef, grpvar == "FlightNumber:FlightOrigin")
Pass_BAS_full_Declarin_glm.FN$lci <- Pass_BAS_full_Declarin_glm.FN$condval - 1.96*(Pass_BAS_full_Declarin_glm.FN$condsd)
Pass_BAS_full_Declarin_glm.FN$uci <- Pass_BAS_full_Declarin_glm.FN$condval + 1.96*(Pass_BAS_full_Declarin_glm.FN$condsd)
Pass_BAS_full_Declarin_glm.FN <- Pass_BAS_full_Declarin_glm.FN[order(Pass_BAS_full_Declarin_glm.FN$condval,decreasing=TRUE),]
Pass_BAS_full_Declarin_glm.FN$Position <- c(217:1)
#summary(Pass_BAS_full_Declarin_glm.FN)

Fig_Air_Numb_A2 <- ggplot(Pass_BAS_full_Declarin_glm.FN, aes(x = condval, y = Position)) +
  scale_x_continuous(limits = c(-2.0, 1.5), expand = c(0, 0), breaks=c(-1.5, 0.0, 1.5)) +
  scale_y_continuous(limits = c(-1, 219), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(colour = '#D95F02', x = Pass_BAS_full_Declarin_glm.FN$lci, y = Pass_BAS_full_Declarin_glm.FN$Position, xend = Pass_BAS_full_Declarin_glm.FN$uci, yend = Pass_BAS_full_Declarin_glm.FN$Position, size = 0.4) + 
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=grp, fontface = 1), hjust = "left", x =-1.95, vjust=0.5, size = 1.2) +
  labs(x = "Intercept estimates by flight number",
       y = "") +
  annotate("text", x = 0.75, y = 0.65, size = 2.4, label = "italic(N_Declared)", parse = TRUE)
Fig_Air_Numb_A2

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Numb_A2.png", width = 6, height = 24, units = "cm", Fig_Air_Numb_A2, dpi = 600)



Pass_BAS_full_Detectin_glm.FN <- subset(Pass_BAS_full_Detectin_glm.ranef, grpvar == "FlightNumber:FlightOrigin")
Pass_BAS_full_Detectin_glm.FN$lci <- Pass_BAS_full_Detectin_glm.FN$condval - 1.96*(Pass_BAS_full_Detectin_glm.FN$condsd)
Pass_BAS_full_Detectin_glm.FN$uci <- Pass_BAS_full_Detectin_glm.FN$condval + 1.96*(Pass_BAS_full_Detectin_glm.FN$condsd)
Pass_BAS_full_Detectin_glm.FN <- Pass_BAS_full_Detectin_glm.FN[order(Pass_BAS_full_Detectin_glm.FN$condval,decreasing=TRUE),]
Pass_BAS_full_Detectin_glm.FN$Position <- c(217:1)
#summary(Pass_BAS_full_Detectin_glm.FN)

Fig_Air_Numb_A3 <- ggplot(Pass_BAS_full_Detectin_glm.FN, aes(x = condval, y = Position)) +
  scale_x_continuous(limits = c(-1.2, 1.0), expand = c(0, 0), breaks=c(-1.0, 0.0, 1.0)) +
  scale_y_continuous(limits = c(-1, 219), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(colour = '#7570B3', x = Pass_BAS_full_Detectin_glm.FN$lci, y = Pass_BAS_full_Detectin_glm.FN$Position, xend = Pass_BAS_full_Detectin_glm.FN$uci, yend = Pass_BAS_full_Detectin_glm.FN$Position, size = 0.4) + 
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=grp, fontface = 1), hjust = "left", x =-1.16, vjust=0.5, size = 1.2) +
  labs(x = "Intercept estimates by flight number",
       y = "") +
  annotate("text", x = 0.5, y = 0.65, size = 2.4, label = "italic(N_Detected)", parse = TRUE)
Fig_Air_Numb_A3

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Numb_A3.png", width = 6, height = 24, units = "cm", Fig_Air_Numb_A3, dpi = 600)




#Plots: FF Interceptions X Number
Pass_BAS_full_DD_total_FF_glm.FN <- subset(Pass_BAS_full_DD_total_FF_glm.ranef, grpvar == "FlightNumber:FlightOrigin")
Pass_BAS_full_DD_total_FF_glm.FN$lci <- Pass_BAS_full_DD_total_FF_glm.FN$condval - 1.96*(Pass_BAS_full_DD_total_FF_glm.FN$condsd)
Pass_BAS_full_DD_total_FF_glm.FN$uci <- Pass_BAS_full_DD_total_FF_glm.FN$condval + 1.96*(Pass_BAS_full_DD_total_FF_glm.FN$condsd)
Pass_BAS_full_DD_total_FF_glm.FN <- Pass_BAS_full_DD_total_FF_glm.FN[order(Pass_BAS_full_DD_total_FF_glm.FN$condval,decreasing=TRUE),]
Pass_BAS_full_DD_total_FF_glm.FN$Position <- c(217:1)

Fig_Air_Numb_B1 <- ggplot(Pass_BAS_full_DD_total_FF_glm.FN, aes(x = condval, y = Position)) +
  scale_x_continuous(limits = c(-1.2, 1.0), expand = c(0, 0), breaks=c(-1.0, 0.0, 1.0)) +
  scale_y_continuous(limits = c(-1, 219), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(colour = '#1B9E77', x = Pass_BAS_full_DD_total_FF_glm.FN$lci, y = Pass_BAS_full_DD_total_FF_glm.FN$Position, xend = Pass_BAS_full_DD_total_FF_glm.FN$uci, yend = Pass_BAS_full_DD_total_FF_glm.FN$Position, size = 0.4) + 
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=grp, fontface = 1), hjust = "left", x =-1.16, vjust=0.5, size = 1.2) +
  labs(x = "Intercept estimates by flight number",
       y = "") +
  annotate("text", x = 0.5, y = 0.65, size = 2.4, label = "italic(N_Total_FF)", parse = TRUE)
Fig_Air_Numb_B1

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Numb_B1.png", width = 6, height = 24, units = "cm", Fig_Air_Numb_B1, dpi = 600)


Pass_BAS_full_Declarin_FF_glm.FN <- subset(Pass_BAS_full_Declarin_FF_glm.ranef, grpvar == "FlightNumber:FlightOrigin")
Pass_BAS_full_Declarin_FF_glm.FN$lci <- Pass_BAS_full_Declarin_FF_glm.FN$condval - 1.96*(Pass_BAS_full_Declarin_FF_glm.FN$condsd)
Pass_BAS_full_Declarin_FF_glm.FN$uci <- Pass_BAS_full_Declarin_FF_glm.FN$condval + 1.96*(Pass_BAS_full_Declarin_FF_glm.FN$condsd)
Pass_BAS_full_Declarin_FF_glm.FN <- Pass_BAS_full_Declarin_FF_glm.FN[order(Pass_BAS_full_Declarin_FF_glm.FN$condval,decreasing=TRUE),]
Pass_BAS_full_Declarin_FF_glm.FN$Position <- c(217:1)
#summary(Pass_BAS_full_Declarin_FF_glm.FN)

Fig_Air_Numb_B2 <- ggplot(Pass_BAS_full_Declarin_FF_glm.FN, aes(x = condval, y = Position)) +
  scale_x_continuous(limits = c(-2.1, 1.5), expand = c(0, 0), breaks=c(-1.5, 0.0, 1.5)) +
  scale_y_continuous(limits = c(-1, 219), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(colour = '#D95F02', x = Pass_BAS_full_Declarin_FF_glm.FN$lci, y = Pass_BAS_full_Declarin_FF_glm.FN$Position, xend = Pass_BAS_full_Declarin_FF_glm.FN$uci, yend = Pass_BAS_full_Declarin_FF_glm.FN$Position, size = 0.4) + 
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=grp, fontface = 1), hjust = "left", x =-2.04, vjust=0.5, size = 1.2) +
  labs(x = "Intercept estimates by flight number",
       y = "") +
  annotate("text", x = 0.75, y = 0.65, size = 2.4, label = "italic(N_Declared_FF)", parse = TRUE)
Fig_Air_Numb_B2

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Numb_B2.png", width = 6, height = 24, units = "cm", Fig_Air_Numb_B2, dpi = 600)



Pass_BAS_full_Detectin_FF_glm.FN <- subset(Pass_BAS_full_Detectin_FF_glm.ranef, grpvar == "FlightNumber:FlightOrigin")
Pass_BAS_full_Detectin_FF_glm.FN$lci <- Pass_BAS_full_Detectin_FF_glm.FN$condval - 1.96*(Pass_BAS_full_Detectin_FF_glm.FN$condsd)
Pass_BAS_full_Detectin_FF_glm.FN$uci <- Pass_BAS_full_Detectin_FF_glm.FN$condval + 1.96*(Pass_BAS_full_Detectin_FF_glm.FN$condsd)
Pass_BAS_full_Detectin_FF_glm.FN <- Pass_BAS_full_Detectin_FF_glm.FN[order(Pass_BAS_full_Detectin_FF_glm.FN$condval,decreasing=TRUE),]
Pass_BAS_full_Detectin_FF_glm.FN$Position <- c(217:1)
#summary(Pass_BAS_full_Detectin_FF_glm.FN)

Fig_Air_Numb_B3 <- ggplot(Pass_BAS_full_Detectin_FF_glm.FN, aes(x = condval, y = Position)) +
  scale_x_continuous(limits = c(-1.35, 1.1), expand = c(0, 0), breaks=c(-1.1, 0.0, 1.1)) +
  scale_y_continuous(limits = c(-1, 219), expand = c(0, 0), breaks=NULL) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 7, colour = "black"), 
        axis.line.x = element_line(colour = "black", size = 0.5),
        panel.background = element_rect(fill = "white"),
        axis.title.x  = element_text(size=9, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(colour = '#7570B3', x = Pass_BAS_full_Detectin_FF_glm.FN$lci, y = Pass_BAS_full_Detectin_FF_glm.FN$Position, xend = Pass_BAS_full_Detectin_FF_glm.FN$uci, yend = Pass_BAS_full_Detectin_FF_glm.FN$Position, size = 0.4) + 
  geom_point(colour = 'black', shape = 19, size = 1) +
  geom_text(aes(label=grp, fontface = 1), hjust = "left", x =-1.31, vjust=0.5, size = 1.2) +
  labs(x = "Intercept estimates by flight number",
       y = "") +
  annotate("text", x = 0.55, y = 0.65, size = 2.4, label = "italic(N_Detected_FF)", parse = TRUE)
Fig_Air_Numb_B3


#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Air_Numb_B3.png", width = 6, height = 24, units = "cm", Fig_Air_Numb_B3, dpi = 600)



#### 3A. Table: Model descriptions and fit checks ####


perf_DD_total_glm <-model_performance(Pass_BAS_full_DD_total_glm)
perf_Declarin_glm <-model_performance(Pass_BAS_full_Declarin_glm)
perf_Detectin_glm <-model_performance(Pass_BAS_full_Detectin_glm)

perf_DD_total_FF_glm <-model_performance(Pass_BAS_full_DD_total_FF_glm)
perf_Declarin_FF_glm <-model_performance(Pass_BAS_full_Declarin_FF_glm)
perf_Detectin_FF_glm <-model_performance(Pass_BAS_full_Detectin_FF_glm)

mod_performance_glm <- rbind(perf_DD_total_glm, perf_Declarin_glm, perf_Detectin_glm,
                             perf_DD_total_FF_glm, perf_Declarin_FF_glm, perf_Detectin_FF_glm)

rownames(mod_performance_glm) <- c("Pass_BAS_full_DD_total_glm","Pass_BAS_full_Declarin_glm", "Pass_BAS_full_Detectin_glm",
                                   "Pass_BAS_full_DD_total_FF_glm", "Pass_BAS_full_Declarin_FF_glm", "Pass_BAS_full_Detectin_FF_glm")

#write.csv(mod_performance_glm, "~/CEBRA_AirInterventions/outputs_visualisations/mod_performance_glm.csv")  



#r2_nakagawa(Pass_BAS_full_DD_total_glm)
#r2_nakagawa(Pass_BAS_full_Declarin_glm)
#r2_nakagawa(Pass_BAS_full_Detectin_glm)
#r2_nakagawa(Pass_BAS_full_DD_total_FF_glm)
#r2_nakagawa(Pass_BAS_full_Declarin_FF_glm)
#r2_nakagawa(Pass_BAS_full_Detectin_FF_glm)

#calculating CIs for variance components
varcis_DD_total_glm <- confint(Pass_BAS_full_DD_total_glm)


#Estimating proportional variation associated with random effect.
#a - Proportional variance associated with random effects (R2cond - R2marg)
#0.570456927 - 0.543097431 = 0.0273595

#b - Proportion of random effect variance associated with origin (Var(origin)/Var(origin+routes))
#0.004200524 / (0.004200524 + 0.042823712) = 0.08932679

#c - Proportion of random effect variance associated with routes (Var(routes)/Var(flights+routes))
#0.042823712 / (0.004200524 + 0.042823712) = 0.9106732

#Proportion of total variance associated with origin (a*b)
#0.0273595 * 0.08932679 = 0.002443936

#Proportion of random effect variance associated with routes (Var(routes)/Var(flights+routes))
#0.9106732 * 0.0273595 = 0.02491556


#### 3B. Tables: Model summary ####

table1a <- as.data.frame(summary(Pass_BAS_full_DD_total_glm)$coefficients)
table1b <- as.data.frame(summary(Pass_BAS_full_Declarin_glm)$coefficients)
table1c <- as.data.frame(summary(Pass_BAS_full_Detectin_glm)$coefficients)

table1a$Estimate <- format(round(table1a$Estimate, digits = 3), nsmall = 3)
table1b$Estimate <- format(round(table1b$Estimate, digits = 3), nsmall = 3)
table1c$Estimate <- format(round(table1c$Estimate, digits = 3), nsmall = 3)
table1a$`Std. Error` <- format(round(table1a$`Std. Error`, digits = 3), nsmall = 3)
table1b$`Std. Error` <- format(round(table1b$`Std. Error`, digits = 3), nsmall = 3)
table1c$`Std. Error` <- format(round(table1c$`Std. Error`, digits = 3), nsmall = 3)
table1a$`z value` <- format(round(table1a$`z value`, digits = 3), nsmall = 3)
table1b$`z value` <- format(round(table1b$`z value`, digits = 3), nsmall = 3)
table1c$`z value` <- format(round(table1c$`z value`, digits = 3), nsmall = 3)

table1a$P <- case_when(table1a$`Pr(>|z|)` < 0.001 ~ "P < 0.001")
table1a$`Pr(>|z|)` <- as.character(format(round(table1a$`Pr(>|z|)`, digits = 3), nsmall = 3))
table1a$`Pr(>|z|)` <- case_when(table1a$`Pr(>|z|)` == "0.000" ~ "P < 0.001",
                                .default = table1a$`Pr(>|z|)`)
table1b$P <- case_when(table1b$`Pr(>|z|)` < 0.001 ~ "P < 0.001")
table1b$`Pr(>|z|)` <- as.character(format(round(table1b$`Pr(>|z|)`, digits = 3), nsmall = 3))
table1b$`Pr(>|z|)` <- case_when(table1b$`Pr(>|z|)` == "0.000" ~ "P < 0.001",
                                .default = table1b$`Pr(>|z|)`)
table1c$P <- case_when(table1c$`Pr(>|z|)` < 0.001 ~ "P < 0.001")
table1c$`Pr(>|z|)` <- as.character(format(round(table1c$`Pr(>|z|)`, digits = 3), nsmall = 3))
table1c$`Pr(>|z|)` <- case_when(table1c$`Pr(>|z|)` == "0.000" ~ "P < 0.001",
                                .default = table1c$`Pr(>|z|)`)

table1a$Model_param <- paste("  - ", rownames(table1a), sep = "")
table1b$Model_param <- paste("  - ", rownames(table1b), sep = "")
table1c$Model_param <- paste("  - ", rownames(table1c), sep = "")
table1a <- table1a[,c(6,1,2,3,4)]
table1b <- table1b[,c(6,1,2,3,4)]
table1c <- table1c[,c(6,1,2,3,4)]

table1a$Var <- ""
table1b$Var <- ""
table1c$Var <- ""

colnames(table1a) <- c("Model_param", "Estimate", "S.E.", "z", "P", "Var")
colnames(table1b) <- c("Model_param", "Estimate", "S.E.", "z", "P", "Var")
colnames(table1c) <- c("Model_param", "Estimate", "S.E.", "z", "P", "Var")

table1d <- as.data.frame(summary(Pass_BAS_full_DD_total_glm)$varcor)
table1e <- as.data.frame(summary(Pass_BAS_full_Declarin_glm)$varcor)
table1f <- as.data.frame(summary(Pass_BAS_full_Detectin_glm)$varcor)

table1d$fill <- ""
table1e$fill <- ""
table1f$fill <- ""

table1d[,c(2,3,5)] <- ""
table1e[,c(2,3,5)] <- ""
table1f[,c(2,3,5)] <- ""

table1d <- table1d[,c(1,2,3,5,6,4)] 
table1e <- table1e[,c(1,2,3,5,6,4)] 
table1f <- table1f[,c(1,2,3,5,6,4)] 

colnames(table1d) <- colnames(table1a)
colnames(table1e) <- colnames(table1b)
colnames(table1f) <- colnames(table1c)

table1d$Model_param <- paste("  (1|", table1d$Model_param, sep = "")
table1e$Model_param <- paste("  (1|", table1e$Model_param, sep = "")
table1f$Model_param <- paste("  (1|", table1f$Model_param, sep = "")

table1d$Model_param <- paste(table1d$Model_param, ")", sep = "")
table1e$Model_param <- paste(table1e$Model_param, ")", sep = "")
table1f$Model_param <- paste(table1f$Model_param, ")", sep = "")

table1d$Var <- format(round(table1d$Var, digits = 3), nsmall = 3)
table1e$Var <- format(round(table1e$Var, digits = 3), nsmall = 3)
table1f$Var <- format(round(table1f$Var, digits = 3), nsmall = 3)

table1g <- NULL
table1g$Model_param <- "Model: N_Total"
table1g <- as.data.frame(table1g)
table1g$Estimate <- ""
table1g$`S.E.` <- ""
table1g$z <- ""
table1g$P <- ""
table1g$Var <- ""

table1h <- NULL
table1h$Model_param <- "Model: N_Declarations"
table1h <- as.data.frame(table1h)
table1h$Estimate <- ""
table1h$`S.E.` <- ""
table1h$z <- ""
table1h$P <- ""
table1h$Var <- ""

table1i <- NULL
table1i$Model_param <- "Model: N_Detections"
table1i <- as.data.frame(table1i)
table1i$Estimate <- ""
table1i$`S.E.` <- ""
table1i$z <- ""
table1i$P <- ""
table1i$Var <- ""

table1 <- rbind(table1g, table1d, table1a, 
                table1h, table1e, table1b,
                table1i, table1f, table1c)


print(xtable(table1, 
    caption = c("Glm, fixed effect estimates and random effect variance components for BRM interception models (total, declared and detected)."), 
    label = "table_suppmainmods", align = c("l","l","c","c","c","c","c")), include.rownames=FALSE, caption.placement = "top")



table2a <- as.data.frame(summary(Pass_BAS_full_DD_total_FF_glm)$coefficients)
table2b <- as.data.frame(summary(Pass_BAS_full_Declarin_FF_glm)$coefficients)
table2c <- as.data.frame(summary(Pass_BAS_full_Detectin_FF_glm)$coefficients)

table2a$Estimate <- format(round(table2a$Estimate, digits = 3), nsmall = 3)
table2b$Estimate <- format(round(table2b$Estimate, digits = 3), nsmall = 3)
table2c$Estimate <- format(round(table2c$Estimate, digits = 3), nsmall = 3)
table2a$`Std. Error` <- format(round(table2a$`Std. Error`, digits = 3), nsmall = 3)
table2b$`Std. Error` <- format(round(table2b$`Std. Error`, digits = 3), nsmall = 3)
table2c$`Std. Error` <- format(round(table2c$`Std. Error`, digits = 3), nsmall = 3)
table2a$`z value` <- format(round(table2a$`z value`, digits = 3), nsmall = 3)
table2b$`z value` <- format(round(table2b$`z value`, digits = 3), nsmall = 3)
table2c$`z value` <- format(round(table2c$`z value`, digits = 3), nsmall = 3)

table2a$P <- case_when(table2a$`Pr(>|z|)` < 0.001 ~ "P < 0.001")
table2a$`Pr(>|z|)` <- as.character(format(round(table2a$`Pr(>|z|)`, digits = 3), nsmall = 3))
table2a$`Pr(>|z|)` <- case_when(table2a$`Pr(>|z|)` == "0.000" ~ "P < 0.001",
                                .default = table2a$`Pr(>|z|)`)
table2b$P <- case_when(table2b$`Pr(>|z|)` < 0.001 ~ "P < 0.001")
table2b$`Pr(>|z|)` <- as.character(format(round(table2b$`Pr(>|z|)`, digits = 3), nsmall = 3))
table2b$`Pr(>|z|)` <- case_when(table2b$`Pr(>|z|)` == "0.000" ~ "P < 0.001",
                                .default = table2b$`Pr(>|z|)`)
table2c$P <- case_when(table2c$`Pr(>|z|)` < 0.001 ~ "P < 0.001")
table2c$`Pr(>|z|)` <- as.character(format(round(table2c$`Pr(>|z|)`, digits = 3), nsmall = 3))
table2c$`Pr(>|z|)` <- case_when(table2c$`Pr(>|z|)` == "0.000" ~ "P < 0.001",
                                .default = table2c$`Pr(>|z|)`)

table2a$Model_param <- paste("  - ", rownames(table2a), sep = "")
table2b$Model_param <- paste("  - ", rownames(table2b), sep = "")
table2c$Model_param <- paste("  - ", rownames(table2c), sep = "")
table2a <- table2a[,c(6,1,2,3,4)]
table2b <- table2b[,c(6,1,2,3,4)]
table2c <- table2c[,c(6,1,2,3,4)]

table2a$Var <- ""
table2b$Var <- ""
table2c$Var <- ""

colnames(table2a) <- c("Model_param", "Estimate", "S.E.", "z", "P", "Var")
colnames(table2b) <- c("Model_param", "Estimate", "S.E.", "z", "P", "Var")
colnames(table2c) <- c("Model_param", "Estimate", "S.E.", "z", "P", "Var")

table2d <- as.data.frame(summary(Pass_BAS_full_DD_total_FF_glm)$varcor)
table2e <- as.data.frame(summary(Pass_BAS_full_Declarin_FF_glm)$varcor)
table2f <- as.data.frame(summary(Pass_BAS_full_Detectin_FF_glm)$varcor)

table2d$fill <- ""
table2e$fill <- ""
table2f$fill <- ""

table2d[,c(2,3,5)] <- ""
table2e[,c(2,3,5)] <- ""
table2f[,c(2,3,5)] <- ""

table2d <- table2d[,c(1,2,3,5,6,4)] 
table2e <- table2e[,c(1,2,3,5,6,4)] 
table2f <- table2f[,c(1,2,3,5,6,4)] 

colnames(table2d) <- colnames(table2a)
colnames(table2e) <- colnames(table2b)
colnames(table2f) <- colnames(table2c)

table2d$Model_param <- paste("  (1|", table2d$Model_param, sep = "")
table2e$Model_param <- paste("  (1|", table2e$Model_param, sep = "")
table2f$Model_param <- paste("  (1|", table2f$Model_param, sep = "")

table2d$Model_param <- paste(table2d$Model_param, ")", sep = "")
table2e$Model_param <- paste(table2e$Model_param, ")", sep = "")
table2f$Model_param <- paste(table2f$Model_param, ")", sep = "")

table2d$Var <- format(round(table2d$Var, digits = 3), nsmall = 3)
table2e$Var <- format(round(table2e$Var, digits = 3), nsmall = 3)
table2f$Var <- format(round(table2f$Var, digits = 3), nsmall = 3)

table2g <- NULL
table2g$Model_param <- "Model: N_Total_FF"
table2g <- as.data.frame(table2g)
table2g$Estimate <- ""
table2g$`S.E.` <- ""
table2g$z <- ""
table2g$P <- ""
table2g$Var <- ""

table2h <- NULL
table2h$Model_param <- "Model: N_Declarations_FF"
table2h <- as.data.frame(table2h)
table2h$Estimate <- ""
table2h$`S.E.` <- ""
table2h$z <- ""
table2h$P <- ""
table2h$Var <- ""

table2i <- NULL
table2i$Model_param <- "Model: N_Detections_FF"
table2i <- as.data.frame(table2i)
table2i$Estimate <- ""
table2i$`S.E.` <- ""
table2i$z <- ""
table2i$P <- ""
table2i$Var <- ""

table2 <- rbind(table2g, table2d, table2a, 
                table2h, table2e, table2b,
                table2i, table2f, table2c)


print(xtable(table2, 
             caption = c("Glm, fixed effect estimates and random effect variance components for FF host interception models (total, declared and detected)."), 
             label = "table_suppmainmods2", align = c("l","l","c","c","c","c","c")), include.rownames=FALSE, caption.placement = "top")




