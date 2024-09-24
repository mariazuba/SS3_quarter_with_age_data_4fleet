# Script information ------------------------------------------------------

# create FLStock from stock assessment output 

# Authors: María José Zúñiga 
# Date: 2024

# Load libraries --------------------------------------------------------------
# install.packages("ss3om", repos=structure(
#   c(CRAN="https://cloud.r-project.org/", FLR="https://flr-project.org/R")))

library(ss3om)
library(tidyverse)
mkdir("output/toFLR/S0")
# Load FLR object from folder containing the output of the SS3 run ------------

dir <- "model/run/S0"

ass.yr <- lubridate::year(Sys.Date())

# To create an FLStock object from a model run, we can call function readFLSss3

ane27a.stock <- ss3om::readFLSss3(dir, name = "ane27a", desc = paste0('Advice for ', ass.yr))

out <- readOutputss3(dir, repfile = "Report.sso", compfile = "CompReport.sso")

# Changes needed  -------------------------------------------------------------

# Maturity should be wtatage when Fleet == -2 %/% wtatage when Fleet == 0 
# instead of wtatage when Fleet == -2 %/% wtatage when Fleet == -1

mat(ane27a.stock) <- (mat(ane27a.stock)*catch.wt(ane27a.stock))/stock.wt(ane27a.stock)

mat(ane27a.stock)[1,] <- 0.0 #to remove NA's

# for the STF we assume that maturity in the last year is the mean of the last 6 years
mat(ane27a.stock)[,ac(out$endyr)] <- rowMeans(mat(ane27a.stock)[,ac((out$endyr-6):(out$endyr-1))])

# Min, Max Fbar

ane27a.stock@range[['minfbar']] <- 1
ane27a.stock@range[['maxfbar']] <- 3

# Harvest should take in consideration Fbar

selectivity <- out$ageselex%>%
  dplyr::filter(Fleet == 1 & Factor == "Asel2" & Yr %in% out$startyr:out$endyr)%>%
  dplyr::select("Yr",matches("[0-9]+")) # porque de 0 a 9???

f.apical <- out$derived_quants%>%
  dplyr::filter(grepl("F_\\d", Label))%>%
  dplyr::select(Label,Value)%>%
  tidyr::separate(Label, into = c("Variable","Yr"),sep = "_",remove = TRUE,extra="drop")

harvest <- f.apical$Value*selectivity[,c("0","1","2","3")]%>%
  dplyr::mutate(refF = rowMeans(dplyr::select(.,"1","3")))
#harvest$Year <- f.apical$Yr


harvest=FLQuant(unname(as.matrix(t(harvest[,-c(5)]))),quant="age",units="f")

dimnames(harvest)[[2]]=rep(as.character(out$startyr:(out$endyr)),each=4)
dimnames(harvest)[[1]]=as.character(min(out$agebins):max(out$agebins))

#harvest(ane27a.stock) <- harvest

units(m.spwn(ane27a.stock)) <- "f"

# Discards.wt should be zero
discards.wt(ane27a.stock) <- discards.n(ane27a.stock)

# save the objects created ------------------------------------------------

save(ane27a.stock, file="output/toFLR/S0/ane27aStock.RData")

# End of script -----------------------------------------------------------

rm(list = ls())


