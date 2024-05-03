# Study: Border biosecurity interceptions for air passengers – assessing intervention methods and analytic tools
#
# Code authored by: Nicholas Moran, CEBRA, University of Melbourne
#
# Date: May 2024



#Loading required packages:
library(tidyverse); library(data.table); library(lubridate); library(xtable)
library(lme4); library(lmerTest);  library(car); library(performance); library(emmeans)



#### S.1. BRM FF Host interception compositions ####

#Data manually anonymised from "passengers-compliance-report_anonymised.csv"
Pass_BAS_comdat <- read.csv('~/CEBRA_AirInterventions/Pass_BAB_commdat_danon.csv', strip.white = TRUE)
Pass_BAS_comdat_FF <- read.csv('~/CEBRA_AirInterventions/Pass_BAB_commdat_FF_danon.csv', strip.white = TRUE)

Pass_BAS_comdat <- as_tibble(Pass_BAS_comdat)
Pass_BAS_comdat$ArrivalTime <- ymd_hms(Pass_BAS_comdat$ArrivalTime)

#summary(Pass_BAS_comdat$ArrivalTime)

Pass_BAS_comdat_2019_2023 <- subset(Pass_BAS_comdat, ArrivalTime >= ymd_hms('2019-01-01 00:00:00.00'))
Pass_BAS_comdat_2019_2023 <- subset(Pass_BAS_comdat_2019_2023, ArrivalTime <= ymd_hms("2023-09-02 00:00:00.00"))
#summary(Pass_BAS_comdat_2019_2023$ArrivalTime)

Pass_BAS_comdat_forsupps <- subset(Pass_BAS_comdat_FF, ActivityId %in% Pass_BAS_dat_processed$ActivityId)


# - Final counts for summary data
#nrow(Pass_BAS_dat_processed) #59917
#sum(Pass_BAS_dat_processed$N_Total_FF) #43803
#sum(Pass_BAS_dat_processed$N_Total) #66675

#nrow(Pass_BAS_comdat_forsupps) #66675


#Plot: Commodity composition
Pass_BAS_comdat_2019_2023$CommodityType <- case_when(
  Pass_BAS_comdat_2019_2023$CommodityType %in% c("Animal Materials", "Seafood (Non-viable)") ~ "Animal products",
  Pass_BAS_comdat_2019_2023$CommodityType %in% c("Cut Flowers") ~ "Cut flowers",
  Pass_BAS_comdat_2019_2023$CommodityType %in% c("Fruit & Veg") ~ "Fruit & vegetables",
  Pass_BAS_comdat_2019_2023$CommodityType %in% c("Live Animals") ~ "Live animals",
  Pass_BAS_comdat_2019_2023$CommodityType %in% c("Nursery Stock") ~ "Nursery stock",
  Pass_BAS_comdat_2019_2023$CommodityType %in% c("Seeds/Grain") ~ "Seeds/grain",
  Pass_BAS_comdat_2019_2023$CommodityType %in% c("Sporting Equipment") ~ "Sporting equipment",
  .default = Pass_BAS_comdat_2019_2023$CommodityType
)

Pass_BAS_intercepttype <- as.data.frame(table(Pass_BAS_comdat_2019_2023$CommodityType))
Pass_BAS_intercepttype$Percentage <- 100*(Pass_BAS_intercepttype$Freq/nrow(Pass_BAS_comdat_2019_2023))

Pass_BAS_intercepttype <- Pass_BAS_intercepttype[order(Pass_BAS_intercepttype$Freq,decreasing=TRUE),]
Pass_BAS_intercepttype$Var1 <- ordered(Pass_BAS_intercepttype$Var1, levels = Pass_BAS_intercepttype$Var1)



Fig_Supps1A <- ggplot(Pass_BAS_intercepttype, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(alpha = 0.85,stat="identity", width=1, color="grey15") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Dark2") +
  #  geom_text(aes(y = Freq, label = Freq), size=5) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        panel.border = element_rect(colour = "white", fill=NA, size = 1),
        panel.background = element_rect(fill = "white"),
        legend.margin=margin(0,0,0,0),
        legend.key.size = unit(1.5,"line"))
Fig_Supps1A

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Supps1A.png", width = 16, height = 12, units = "cm", Fig_Supps1A, dpi = 600)




#Summary data: Interceptions by Commodity Type
Pass_BAS_interceptspec1 <- subset(Pass_BAS_comdat_2019_2023, CommodityType == "Animal products")
Pass_BAS_interceptspec1 <- as.data.frame(table(Pass_BAS_interceptspec1$Commodity))
Pass_BAS_interceptspec1$CommodityType <- "Animal products"

Pass_BAS_interceptspec2 <- subset(Pass_BAS_comdat_2019_2023, CommodityType == "Cut flowers")
Pass_BAS_interceptspec2 <- as.data.frame(table(Pass_BAS_interceptspec2$Commodity))
Pass_BAS_interceptspec2$CommodityType <- "Cut flowers"

Pass_BAS_interceptspec3 <- subset(Pass_BAS_comdat_2019_2023, CommodityType == "Fruit & vegetables")
Pass_BAS_interceptspec3 <- as.data.frame(table(Pass_BAS_interceptspec3$Commodity))
Pass_BAS_interceptspec3$CommodityType <- "Fruit & vegetables"

Pass_BAS_interceptspec4 <- subset(Pass_BAS_comdat_2019_2023, CommodityType == "Live animals")
Pass_BAS_interceptspec4 <- as.data.frame(table(Pass_BAS_interceptspec4$Commodity))
Pass_BAS_interceptspec4$CommodityType <- "Live animals"

Pass_BAS_interceptspec5 <- subset(Pass_BAS_comdat_2019_2023, CommodityType == "Nursery stock")
Pass_BAS_interceptspec5 <- as.data.frame(table(Pass_BAS_interceptspec5$Commodity))
Pass_BAS_interceptspec5$CommodityType <- "Nursery stock"

Pass_BAS_interceptspec6 <- subset(Pass_BAS_comdat_2019_2023, CommodityType == "Seeds/grain")
Pass_BAS_interceptspec6 <- as.data.frame(table(Pass_BAS_interceptspec6$Commodity))
Pass_BAS_interceptspec6$CommodityType <- "Seeds/grain"

Pass_BAS_interceptspec7 <- subset(Pass_BAS_comdat_2019_2023, CommodityType == "Sporting equipment")
Pass_BAS_interceptspec7 <- as.data.frame(table(Pass_BAS_interceptspec7$Commodity))
Pass_BAS_interceptspec7$CommodityType <- "Sporting equipment"

Pass_BAS_interceptspec <- rbind(Pass_BAS_interceptspec1,Pass_BAS_interceptspec2,Pass_BAS_interceptspec3,Pass_BAS_interceptspec4,Pass_BAS_interceptspec5,Pass_BAS_interceptspec6,Pass_BAS_interceptspec7)

#write.csv(Pass_BAS_interceptspec, "~/CEBRA_AirInterventions/outputs_visualisations/Interception_compositions.csv")



#Pass_BAS_comdat_FF
Pass_BAS_comdat_FF_forsupps <- subset(Pass_BAS_comdat_FF, ActivityId %in% Pass_BAS_dat_processed$ActivityId)
#nrow(Pass_BAS_comdat_FF_forsupps) #43803
#sum(Pass_BAS_dat_processed$N_Total_FF) #43803

#table(Pass_BAS_comdat_FF_forsupps$Commodity)
Pass_BAS_comdat_FF_forsupps$Commodity <- case_when(
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("APPLES", "APPLES (TOFFEE)") ~ "Apples",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("AVOCADOS") ~ "Avocados",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("BANANAS") ~ "Bananas",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("BLUEBERRIES") ~ "Blueberries",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("CHERRY TOMATOES","TOMATOES") ~ "Tomatoes",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("GRAPES") ~ "Grapes",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("LEMONS","Lemons/Limes","LIMES") ~ "Lemons/limes",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("MANDARINS") ~ "Mandarins",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("ORANGES") ~ "Oranges",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("PEARS") ~ "Pears",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("STRAWBERRIES") ~ "Strawberries",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("NECTARINES") ~ "Nectarines",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("MANGOES") ~ "Mangoes",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("KIWI FRUIT") ~ "Kiwi fruit",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("PLUMS") ~ "Plums",
  Pass_BAS_comdat_FF_forsupps$Commodity %in% c("BABACO","BLACKBERRIES","BOYSENBERRIES","CUMQUAT","CUSTARD APPLES","DATES","Durian","Eggplant","FEIJOAS (prohibited)","FIGS","GOOSEBERRIES","GRAPEFRUIT","GUAVAS (prohibited)","JEW PLUM","KUMQUATS","LOQUATS","LYCHEES","Mangosteen","Mixed Berries","Nashi Pear","OLIVES","PAPAYA","PAWPAWS","PEPPERS","PERSIMMONS","POMEGRANATES","QUINCES","TAMARILLO","TANGELOS","TANGERINES","PASSIONFRUIT","APRICOTS","CAPSICUMS","CAPSICUMS(Tri Colour)","CHILLIES","RASPBERRIES","CHERRY","PEACHES","FRUIT SALAD") ~ "Other/rare",
  .default = Pass_BAS_comdat_FF_forsupps$Commodity
)

Pass_BAS_interceptcomm<- as.data.frame(table(Pass_BAS_comdat_FF_forsupps$Commodity))
Pass_BAS_interceptcomm$Percentage <- 100*(Pass_BAS_interceptcomm$Freq/nrow(Pass_BAS_comdat_FF_forsupps))

Pass_BAS_interceptcomm$Var1 <- ordered(Pass_BAS_interceptcomm$Var1, levels = c("Apples", "Bananas", "Mandarins", "Oranges", "Grapes", "Pears", "Strawberries", "Blueberries", "Tomatoes", "Lemons/limes", "Avocados", "Nectarines", "Mangoes", "Kiwi fruit","Plums", "Other/rare"))

Fig_Supps1B <- ggplot(Pass_BAS_interceptcomm, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(alpha = 1, stat="identity", width=1, color="grey15") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("#A6CEE3","#1F78B4","#FF66AD","#E744C7",
                               "#B2DF8A","#33A02C","#FB9A99","#E31A1C",
                               "#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A",
                               "#FFFF99","#E3E339","grey50","grey30")) +
  #  geom_text(aes(y = Freq, label = Freq), size=5) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        panel.border = element_rect(colour = "white", fill=NA, size = 1),
        panel.background = element_rect(fill = "white"),
        legend.margin=margin(0,0,0,0),
        legend.key.size = unit(1.2,"line"))
Fig_Supps1B

#ggsave("~/CEBRA_AirInterventions/outputs_visualisations/Fig_Supps1B.png", width = 16, height = 12, units = "cm", Fig_Supps1B, dpi = 600)




#### S.2. Time factor sensitivity checks (additional analyses not for inclusion) ####

#Preparing time variable
Pass_BAS_dat_processed.mod$ArrivalTime <- ymd_hms(Pass_BAS_dat_processed.mod$ArrivalTime)
#summary(Pass_BAS_dat_processed.mod$ArrivalTime)

Pass_BAS_dat_processed.mod_2019 <- subset(Pass_BAS_dat_processed.mod, ArrivalTime <= ymd_hms('2019-12-31 23:59:59.9999'))
#summary(Pass_BAS_dat_processed.mod_2019$ArrivalTime)

Pass_BAS_dat_processed.mod_2020 <- subset(Pass_BAS_dat_processed.mod, ArrivalTime >= ymd_hms('2020-01-01 00:00:00.0000'))
Pass_BAS_dat_processed.mod_2020 <- subset(Pass_BAS_dat_processed.mod_2020, ArrivalTime <= ymd_hms('2020-12-31 23:59:59.9999'))
#summary(Pass_BAS_dat_processed.mod_2020$ArrivalTime)

Pass_BAS_dat_processed.mod_2021 <- subset(Pass_BAS_dat_processed.mod, ArrivalTime >= ymd_hms('2021-01-01 00:00:00.0000'))
Pass_BAS_dat_processed.mod_2021 <- subset(Pass_BAS_dat_processed.mod_2021, ArrivalTime <= ymd_hms('2021-12-31 23:59:59.9999'))
#summary(Pass_BAS_dat_processed.mod_2021$ArrivalTime)

Pass_BAS_dat_processed.mod_2022 <- subset(Pass_BAS_dat_processed.mod, ArrivalTime >= ymd_hms('2022-01-01 00:00:00.0000'))
Pass_BAS_dat_processed.mod_2022 <- subset(Pass_BAS_dat_processed.mod_2022, ArrivalTime <= ymd_hms('2022-12-31 23:59:59.9999'))
#summary(Pass_BAS_dat_processed.mod_2022$ArrivalTime)

Pass_BAS_dat_processed.mod_2023 <- subset(Pass_BAS_dat_processed.mod, ArrivalTime >= ymd_hms('2023-01-01 00:00:00.0000'))
Pass_BAS_dat_processed.mod_2023 <- subset(Pass_BAS_dat_processed.mod_2023, ArrivalTime <= ymd_hms('2023-12-31 23:59:59.9999'))
#summary(Pass_BAS_dat_processed.mod_2023$ArrivalTime)

Pass_BAS_dat_processed.mod_2019$Year <- "2019"
Pass_BAS_dat_processed.mod_2020$Year <- "2020"
Pass_BAS_dat_processed.mod_2021$Year <- "2021"
Pass_BAS_dat_processed.mod_2022$Year <- "2022"
Pass_BAS_dat_processed.mod_2023$Year <- "2023"

Pass_BAS_dat_processed.mod_time <- rbind(Pass_BAS_dat_processed.mod_2019,
                                         Pass_BAS_dat_processed.mod_2020,
                                         Pass_BAS_dat_processed.mod_2021,
                                         Pass_BAS_dat_processed.mod_2022,
                                         Pass_BAS_dat_processed.mod_2023)
#nrow(Pass_BAS_dat_processed.mod)
#nrow(Pass_BAS_dat_processed.mod_time)

Pass_BAS_dat_processed.mod_time$Year <- as.factor(Pass_BAS_dat_processed.mod_time$Year)


#Model 1T, glmer implementation with time
#Pass_BAS_full_DD_total_glm_time <- glmer(N_Total ~ 1 + Location + Regime + sqrt.BagSearchCount.Z + sqrt.PassengerCount.Z + (1|FlightOrigin/FlightNumber) + (1|Year), family = poisson, data=Pass_BAS_dat_processed.mod_time, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

#save(Pass_BAS_full_DD_total_glm_time, file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_glm_time.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_glm.RData")
load(file = "~/CEBRA_AirInterventions/models/Pass_BAS_full_DD_total_glm_time.RData")

#Comparing summary outputs. 
summary(Pass_BAS_full_DD_total_glm)
summary(Pass_BAS_full_DD_total_glm_time)
# - Year explains some, but the lowest amount of variance for included random effects.
# - Quantative output for fixed effects are very similar. So deviations from the main analyses

#Plotting random effect estimates, shows no systematic influence of time on FlightNumber or FlightOrigin variables. 
plot(subset(as.data.frame(ranef(Pass_BAS_full_DD_total_glm)), grpvar == "FlightNumber:FlightOrigin")$condval,
     subset(as.data.frame(ranef(Pass_BAS_full_DD_total_glm_time)), grpvar == "FlightNumber:FlightOrigin")$condval)
plot(subset(as.data.frame(ranef(Pass_BAS_full_DD_total_glm)), grpvar == "FlightOrigin")$condval,
     subset(as.data.frame(ranef(Pass_BAS_full_DD_total_glm_time)), grpvar == "FlightOrigin")$condval)

