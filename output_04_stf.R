# Script information ------------------------------------------------------

# run short term forecast (stf) 

# Authors: María José Zúñiga 

# Date: 2024

# In the catch sceanrios table:
#   change value for the advised catches
#   change value for quota defined by SP and PT

# In the fbar_scenarios:
#   change/include scenario for the HCR at the end

# Check which HCR need to be capped

# load libraries ----------------------------------------------------------

library(icesTAF)
library(FLCore)
library(FLash)

set.seed(23456)

# load FLStock object ------------------------------------------------------

load("./output/toFLR/S0/ane27aStock.RData")
#load reference points
#load("./boot/initial/data/ane27aStock_refPoints.R")

# Define the assessment/interim year ---------------------------------------
ass.yr <- endyr <- as.numeric(pil.stock@range[5])

# Definition of the stock-recruitment relationship (SRR) -------------------

# Geometric mean recruitment (last 5 years, including the interim year)
mean_rec <- exp(mean(log(FLCore::rec(pil.stock)[,FLCore::ac((endyr-4):(endyr))])))
# set up an FLSR object with a geometric mean model
pil.stock_sr <- as.FLSR(pil.stock, model="geomean")
params(pil.stock_sr)['a',] <- mean_rec

# Do a 2 year forecast
pil.stock_stf <- FLash::stf(pil.stock, nyears = 2, wts.nyears=1)

# Now the stock goes up to endyr+2
summary(pil.stock_stf)


harvest(pil.stock_stf)[,ac((endyr+1):(endyr+2))] <- harvest(pil.stock_stf)[,ac(endyr-1)] 
landings(pil.stock_stf)[,ac((endyr+1):(endyr+2))] <- NA
landings.n(pil.stock_stf)[,ac((endyr):(endyr+2))] <- 1.0
catch(pil.stock_stf)[,ac((endyr+1):(endyr+2))] <- NA

# Do several F scenarios

fbar_multiplier <- seq(from = 0.05, to = 1.3, by = 0.005)
Fstatus_quo <- FLCore::fbar(pil.stock)[,FLCore::ac(ass.yr),drop=T]

# As we already have the projection for the interim year, we use the multiplier already in the first projection year 
# F scenarios, same F as in interim year and Flim (=0.26)
fbar_scenarios <- cbind(c(fbar_multiplier,Fstatus_quo,pil278c9a_ref.pts$Fpa,pil278c9a_ref.pts$Flim),c(fbar_multiplier,Fstatus_quo,pil278c9a_ref.pts$Fpa,pil278c9a_ref.pts$Flim))
colnames(fbar_scenarios) <- c(ass.yr+1,ass.yr+2)

# There are various results we want to extract from the STF
# Like predicted Catch, SSB and the relative change in these
# The following is what we calculate in the STECF Med. WG
# Make an empty matrix in which to store the results
stf_results <- matrix(NA,nrow = nrow(fbar_scenarios),ncol = 12)
# Set some column names
final_year <- endyr
colnames(stf_results) <- c(paste('B1+',final_year,sep="_"),
                           paste0('F',endyr),
                           paste('Catch',final_year,sep="_"),
                           paste('B1+',final_year+1,sep="_"),
                           #'Fsq','Fmult',
                           'F',
                           paste('Catch',final_year+1,sep="_"), 
                           paste('B1+',final_year+2,sep="_"),
                           paste('Catch',final_year+2,sep="_"),
                           paste('Change_B1+_',final_year+1,'-',final_year+2,'(%)',sep=""),
                           paste('Change_Catch_',final_year-1,'-',final_year+1,'(%)',sep=""),
                           paste('Change_Advice_',final_year,'-',final_year+1,'(%)',sep=""),
                           paste('Change_Quota_',final_year,'-',final_year+1,'(%)',sep=""))
head(stf_results)


# Loop over the scenarios (each row in the fbar_scenarios table)
for (scenario in 1:nrow(fbar_scenarios)) {
  cat("Scenario: ", scenario, "\n")
  # Make a target object withe F values for that scenario
  ctrl_target <- data.frame(year = (endyr+1):(endyr+2),
                            quantity = "f",
                            val = fbar_scenarios[scenario,])
  # Set the control object - year, quantity and value for the moment
  ctrl_f <- fwdControl(ctrl_target)
  # ctrl_target
  # Run the forward projection.
  if (exists("pil.stock_fwd")) { rm(pil.stock_fwd); gc() }
  pil.stock_fwd <- FLash::fwd(pil.stock_stf, ctrl = ctrl_f, sr = pil.stock_sr)

  # Fill results table
  stf_results[scenario,1] <- stock(pil.stock_fwd)[,ac(final_year)]   # Interim year B1+
  stf_results[scenario,2] <- mean(harvest(pil.stock_fwd)[ac(2:5),ac(final_year)])
  stf_results[scenario,3] <- catch(pil.stock_fwd)[,ac(final_year)]   # Interim year catch stf year
  stf_results[scenario,4] <- stock(pil.stock_fwd)[,ac(final_year+1)] # 1st stf year B1+
  #stf_results[scenario,5] <- fbar_status_quo # f status quo
  #stf_results[scenario,6] <- fbar_multiplier[scenario] # F_multiplier
  stf_results[scenario,5] <- fbar_scenarios[scenario,1]
  stf_results[scenario,6] <- catch(pil.stock_fwd)[,ac(final_year+1)] # 1st stf year catch
  stf_results[scenario,7] <- stock(pil.stock_fwd)[,ac(final_year+2)] # 2nd stf year B1+
  stf_results[scenario,8] <- catch(pil.stock_fwd)[,ac(final_year+2)] # 2nd stf year Catch
  
  # Change in SSB and Catch
  stf_results[scenario,9] <- (stock(pil.stock_fwd)[,ac(final_year+2)]-stock(pil.stock_fwd)[,ac(final_year+1)])/stock(pil.stock_fwd)[,ac(final_year+1)]*100 # change in B1+ in last two stf years
  stf_results[scenario,10] <- (catch(pil.stock_fwd)[,ac(final_year+1)]-catch(pil.stock_fwd)[,ac(final_year-1)])/catch(pil.stock_fwd)[,ac(final_year-1)]*100 # change in catch from final year+1, to 2nd to last stf year (final year-1)
  stf_results[scenario,11] <- (catch(pil.stock_fwd)[,ac(final_year+1)]-43841)/43841*100 # advised catches in final year + 1 compared to advised catches for final year (catches in advice sheet)
  #stf_results[scenario,12] <- (catch(pil.stock_fwd)[,ac(final_year+1)]-catch(pil.stock_fwd)[,ac(final_year)])/catch(pil.stock_fwd)[,ac(final_year)]*100 # change in quota Advised catches in final year+1 compared to the sum of national quotas for Portugal and Spain for final year (legislation)
  stf_results[scenario,12] <- (catch(pil.stock_fwd)[,ac(final_year+1)]-56604)/56604*100 # This year the sum of quotas is not our assumption in the model.
  }

# export this if necessary
write.csv(stf_results,file="./output/stf/pil.27.8c9a_F_scenarios.csv",row.names=F)

#B1+ at the beginning of the management year
b.datyr <- stock(pil.stock_fwd[,ac(ass.yr+1)])

#Specify the biomass and fishing mortality trigger points of the HCR
Bloss <- 112943
Flow <- 0.064
Ftarget <- 0.12
Bmsy <- 252523
Bpa <- 446331

Brefs <- c(Bloss,Bmsy,Bpa)

# Find where the SSB (Age structured) OR Biomass (Aggregated) in relation to Btrig points (for HCR of the MP)
b.pos <- findInterval(b.datyr, Brefs)
Ftg   <- drop(ifelse(b.pos==0,0,ifelse(b.pos == 1,(0-Flow)*Bloss/(Bmsy-Bloss) + (Flow/(Bmsy-Bloss))*b.datyr,
                                  ifelse(b.pos == 2,(Flow-(Ftarget-Flow)*Bmsy/(Bpa-Bmsy)) + ((Ftarget-Flow)/(Bpa-Bmsy))*b.datyr,Ftarget))))

#Ftg is 0.1054525;
ftg_scenario <- cbind(Ftg,Ftg)
colnames(ftg_scenario) <- c(ass.yr+1,ass.yr+2)

ftg_results <- matrix(NA,nrow = nrow(ftg_scenario),ncol = 12)

# Set some column names
final_year <- endyr
colnames(ftg_results) <- c(paste('B1+',final_year,sep="_"),
                             paste0('F',endyr),
                             paste('Catch',final_year,sep="_"),
                             paste('B1+',final_year+1,sep="_"),
                             #'Fsq','Fmult',
                             'F',
                             paste('Catch',final_year+1,sep="_"), 
                             paste('B1+',final_year+2,sep="_"),
                             paste('Catch',final_year+2,sep="_"),
                             paste('Change_B1+_',final_year+1,'-',final_year+2,'(%)',sep=""),
                             paste('Change_Catch_',final_year-1,'-',final_year+1,'(%)',sep=""),
                             paste('Change_Advice_',final_year,'-',final_year+1,'(%)',sep=""),
                             paste('Change_Quota_',final_year,'-',final_year+1,'(%)',sep=""))
head(ftg_results)

for (scenario in 1:nrow(ftg_scenario)) {
  cat("Scenario: ", scenario, "\n")
  # Make a target object withe F values for that scenario
  ctrl_target <- data.frame(year = (endyr+1):(endyr+2),
                            quantity = "f",
                            val = ftg_scenario[scenario,])
  # Set the control object - year, quantity and value for the moment
  ctrl_f <- FLash::fwdControl(ctrl_target)
  # ctrl_target
  # Run the forward projection. 
  pil.stock_fwd <- FLash::fwd(pil.stock_stf, ctrl = ctrl_f, sr = pil.stock_sr)
  
  # Fill results table
  ftg_results[scenario,1] <- stock(pil.stock_fwd)[,ac(final_year)]   # Interim year B1+
  ftg_results[scenario,2] <- mean(harvest(pil.stock_fwd)[ac(2:5),ac(final_year)])
  ftg_results[scenario,3] <- catch(pil.stock_fwd)[,ac(final_year)]   # Interim year catch stf year
  ftg_results[scenario,4] <- stock(pil.stock_fwd)[,ac(final_year+1)] # 1st stf year B1+
  ftg_results[scenario,5] <- fbar(pil.stock_fwd)[,ac(final_year+1)]
  ftg_results[scenario,6] <- catch(pil.stock_fwd)[,ac(final_year+1)] # 1st stf year catch
  ftg_results[scenario,7] <- stock(pil.stock_fwd)[,ac(final_year+2)] # 2nd stf year B1+
  ftg_results[scenario,8] <- catch(pil.stock_fwd)[,ac(final_year+2)] # 2nd stf year Catch
  
  # Change in SSB and Catch
  ftg_results[scenario,9] <- (stock(pil.stock_fwd)[,ac(final_year+2)]-stock(pil.stock_fwd)[,ac(final_year+1)])/stock(pil.stock_fwd)[,ac(final_year+1)]*100 # change in B1+ in last two stf years
  ftg_results[scenario,10] <- (catch(pil.stock_fwd)[,ac(final_year+1)]-catch(pil.stock_fwd)[,ac(final_year-1)])/catch(pil.stock_fwd)[,ac(final_year-1)]*100 # change in catch from true year, to 2nd to last stf year
  ftg_results[scenario,11] <- (catch(pil.stock_fwd)[,ac(final_year+1)]-43841)/43841*100 # change in catch from true year, to 2nd to last stf year
  ftg_results[scenario,12] <- (catch(pil.stock_fwd)[,ac(final_year+1)]-56604)/56604*100 # change in catch from true year, to 2nd to last stf year
}

# merge stf_results with catch_results

stf_final <- dplyr::bind_rows(as.data.frame(stf_results),as.data.frame(ftg_results))

# Ftg corresponds to a max of catches
as.data.frame(ftg_results)$Catch_2024

# This means that this year only the HCR 30, 35 and 40 are capped.
# Run stf for this scenarios


#catch equal to 30,35 and 40 kt

catch_scenarios <- cbind(seq(30000,40000,by=5000),seq(30000,40000,by=5000))
colnames(catch_scenarios) <- c(ass.yr+1,ass.yr+2)

catch_results <- matrix(NA,nrow = nrow(catch_scenarios),ncol = 12)

# Set some column names
final_year <- endyr
colnames(catch_results) <- c(paste('B1+',final_year,sep="_"),
                             paste0('F',endyr),
                             paste('Catch',final_year,sep="_"),
                             paste('B1+',final_year+1,sep="_"),
                             #'Fsq','Fmult',
                             'F',
                             paste('Catch',final_year+1,sep="_"), 
                             paste('B1+',final_year+2,sep="_"),
                             paste('Catch',final_year+2,sep="_"),
                             paste('Change_B1+_',final_year+1,'-',final_year+2,'(%)',sep=""),
                             paste('Change_Catch_',final_year-1,'-',final_year+1,'(%)',sep=""),
                             paste('Change_Advice_',final_year,'-',final_year+1,'(%)',sep=""),
                             paste('Change_Quota_',final_year,'-',final_year+1,'(%)',sep=""))
head(catch_results)

for (scenario in 1:nrow(catch_scenarios)) {
  cat("Scenario: ", scenario, "\n")
  # Make a target object withe F values for that scenario
  ctrl_target <- data.frame(year = (endyr+1):(endyr+2),
                            quantity = "landings",
                            val = catch_scenarios[scenario,])
  # Set the control object - year, quantity and value for the moment
  ctrl_f <- FLash::fwdControl(ctrl_target)
  # ctrl_target
  # Run the forward projection. 
  pil.stock_fwd <- FLash::fwd(pil.stock_stf, ctrl = ctrl_f, sr = pil.stock_sr)
  
  # Fill results table
  catch_results[scenario,1] <- stock(pil.stock_fwd)[,ac(final_year)]   # Interim year B1+
  catch_results[scenario,2] <- mean(harvest(pil.stock_fwd)[ac(2:5),ac(final_year)])
  catch_results[scenario,3] <- catch(pil.stock_fwd)[,ac(final_year)]   # Interim year catch stf year
  catch_results[scenario,4] <- stock(pil.stock_fwd)[,ac(final_year+1)] # 1st stf year B1+
  catch_results[scenario,5] <- fbar(pil.stock_fwd)[,ac(final_year+1)]
  catch_results[scenario,6] <- catch(pil.stock_fwd)[,ac(final_year+1)] # 1st stf year catch
  catch_results[scenario,7] <- stock(pil.stock_fwd)[,ac(final_year+2)] # 2nd stf year B1+
  catch_results[scenario,8] <- catch(pil.stock_fwd)[,ac(final_year+2)] # 2nd stf year Catch
  
  # Change in SSB and Catch
  catch_results[scenario,9] <- (stock(pil.stock_fwd)[,ac(final_year+2)]-stock(pil.stock_fwd)[,ac(final_year+1)])/stock(pil.stock_fwd)[,ac(final_year+1)]*100 # change in B1+ in last two stf years
  catch_results[scenario,10] <- (catch(pil.stock_fwd)[,ac(final_year+1)]-catch(pil.stock_fwd)[,ac(final_year-1)])/catch(pil.stock_fwd)[,ac(final_year-1)]*100 # change in catch from true year, to 2nd to last stf year
  catch_results[scenario,11] <- (catch(pil.stock_fwd)[,ac(final_year+1)]-43841)/43841*100 # change in catch from true year, to 2nd to last stf year
  catch_results[scenario,12] <- (catch(pil.stock_fwd)[,ac(final_year+1)]-56604)/56604*100 # change in catch from true year, to 2nd to last stf year
}


# merge stf_results with catch_results

stf_final <- dplyr::bind_rows(stf_final,as.data.frame(catch_results))

# export this if necessary
write.csv(stf_final,file="./output/stf/pil.27.8c9a_scenarios.csv",row.names=F)


# Save data
save(stf_final,pil.stock_stf,pil.stock_fwd,final_year,mean_rec,Ftg,Fstatus_quo,file="output/stf/STF_pil27.8c9a.RData")



# End of script -----------------------------------------------------------
rm(list = ls())


