Materials for "Border biosecurity interceptions for air passengers – assessing intervention methods and analytic tools." (in press, Neobiota, 2025)

Critical files required for reproducing outputs include the following: 

OSF Storage
 - MS_21O_supplementarymaterials_v1.0.pdf, including the following:
	- Appendix A.  Composition of BRM and FF host interceptions
	- Appendix B.  Supplementary model design and output details

GitHub Storage:
 - Pass_BAS_dat_processed_danon.csv -- anonymised interception databases, response variable columns = N_Declarations, N_Detections, N_Total, N_Declarations_FF, N_Detections_FF, N_Total_FF, 1 row = 1 arrival
 - Pass_BAS_commdat_danon.csv -- anonymised interception databases for each interception, 1 row = 1 intereception. Response variables above calulated based on InterventionType, CommodityType & Commodity columns. 
 - Pass_BAS_comdat_FF_danon.csv -- as above, subsetted to FF host items only. 
 - 1_modelling_and_visualisations.R (data preparation for modelling, model runs, and data visualisation for main analyses)
 - 2_sensitivity_analyses.R (data preparation for modelling, model runs, and data visualisation for sensitivity analyses)
 - 3_supplementary_analyses.R (additional non-essential supporting scripts)
 - folder: models (saved models .RData files, and model effect parameters .csv files)
 - folder: outputs_visualisations (figures and additional quantative model outputs)
