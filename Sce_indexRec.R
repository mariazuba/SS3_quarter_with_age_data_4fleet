# Catchability scenarios
library(icesTAF)
library(flextable)
library(r4ss)

boot<-"boot/initial/data/run/"
list.files(boot)

# run scenarios
ESCs<-c("S1.0_4FLEETS_SelECO",
        "S4FLEETS_SelECO_Mage_M1.3",
        "S4FLEETS_SelECO_Mage_M1.4",
        "S4FLEETS_SelECO_Mage_M1.5")

old.esc<-"S1.0_4FLEETS_SelECO"
dat <- r4ss::SS_read(dir = paste0(boot ,old.esc))
dat$ctl$age_selex_parms #OK
dat$ctl$natM
#'*===========================================================================*
old.esc<-"S1.0_4FLEETS_SelECO"
new.esc<-"S4FLEETS_SelECO_Mage_M1.3"
dirnew<-paste0(boot,new.esc)
dat <- r4ss::SS_read(dir = paste0(boot ,new.esc))
dat$ctl$age_selex_parms #OK
#'*===========================================================================*
old.esc<-"S1.0_4FLEETS_SelECO"
new.esc<-"S4FLEETS_SelECO_Mage_M1.4"
dirnew<-paste0(boot,new.esc)
dat <- r4ss::SS_read(dir = paste0(boot ,new.esc))
dat$ctl$MG_parms
#'*===========================================================================*
old.esc<-"S4FLEETS_SelECO_Mage"
new.esc<-"S4FLEETS_SelECO_Mage_M1.5"
dirnew<-paste0(boot,new.esc)
dat <- r4ss::SS_read(dir = paste0(boot ,new.esc))
dat$ctl$age_selex_parms
dat$ctl$MG_parms


for(i in 2:length(ESCs)){
  boot<-"boot/initial/data/run/"
  ESCs<-c("S1.0_4FLEETS_SelECO",
          "S4FLEETS_SelECO_Mage_M1.3",
          "S4FLEETS_SelECO_Mage_M1.4",
          "S4FLEETS_SelECO_Mage_M1.5")
  esc<-ESCs[i]
  write(esc, file = paste0(boot,"Esc.txt"))
  sourceTAF("bootstrap")
  sourceTAF("data")
  sourceTAF("model_01_run")
  sourceTAF("output_01_run")
  sourceTAF("report_01_run")
}




#'*============================================================================*
dir.create("report/run/comparison/Mnew", recursive = TRUE)
path_mod<-"report/run/comparison/Mnew"

# Create a dataframe with the scenarios
list.files("boot/initial/data/run/" )
ESCs<-c("S1.0_4FLEETS_SelECO",
        "S4FLEETS_SelECO_Mage_M1.3",
        "S4FLEETS_SelECO_Mage_M1.4",
        "S4FLEETS_SelECO_Mage_M1.5")

scenarios <- data.frame(
  Scenario = ESCs,
  Description =  c("Mfix: age-0=2.97, age-1=1.33, age-2=1.33, age-3=1.33",
                   "Mfix: age-0=2.97, age-1=1.3, Mestimate: age-2 and age-3",
                   "Mfix: age-0=2.97, age-1=1.4, Mestimate: age-2 and age-3",
                   "Mfix: age-0=2.97, age-1=1.5, Mestimate: age-2 and age-3")
)

ft0 <- flextable(scenarios)
ft0 <- colformat_double(ft0, digits=1, na_str = "")
ft0 <- colformat_num(ft0,big.mark = "", na_str = "")
ft0 <- align(ft0,part = "header", align = "center") 
ft0 <- fontsize(ft0, size = 8, part = "body")
ft0 <- autofit(ft0)

invisible(save_as_image(ft0, path = paste0(path_mod, "/tb_scenarios_Mnew.png")))



#run retrospective
# for(i in 1:length(ESCs)){
#   boot<-"boot/initial/data/run/" 
#   ESCs<-c("S1.0_4FLEETS", "S1.0_4FLEETS_q1PEL","S1.0_4FLEETS_q1ECO", "S1.0_4FLEETS_q1BOCA", "S1.0_4FLEETS_q1ECOREC")
#   esc<-ESCs[i]
#   write(esc, file = paste0(boot,"Esc.txt"))
#   sourceTAF("bootstrap")
#   sourceTAF("data")
#   sourceTAF("model_02_retro")
#   sourceTAF("output_02_retro")
#   sourceTAF("report_02_retro")
# }


#'*--------------------------------------------------------------------------*
esc<-c("S1.0_4FLEETS_SelECO",
       "S4FLEETS_SelECO_Mage_M1.3",
       "S4FLEETS_SelECO_Mage_M1.4",
       "S4FLEETS_SelECO_Mage_M1.5")
replist<-list()
diag<-list()
params_est<-list()
Calc_Q<-list()
M<-list()
for(i in 1:length(esc)){
  Esc<-esc[i]
  load(paste0("output/run/",Esc,"/output.RData"))
  replist[[Esc]]<-output
  
  
  diag[[Esc]]<-data.frame(ESC=Esc,
                          convergency=output$maximum_gradient_component,
                          AIC=as.numeric(2*dim(output$estimated_non_dev_parameters)[1]+2*output$likelihoods_used[1,1]),
                          Total_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "TOTAL"],
                          Survey_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "Survey"],
                          Age_like=output$likelihoods_used$values[rownames(output$likelihoods_used) == "Age_comp"],
                          RMSE_index=jaba_cpue$RMSE.perc[jaba_cpue$indices == "Combined"],
                          RMSE_age=jaba_age$RMSE.perc[jaba_age$indices == "Combined"])
  
  params <- output$estimated_non_dev_parameters %>%
    rownames_to_column(var = "Parameter")
  
  params_est[[Esc]] <- params %>% 
    select(c(Parameter,Value))
  
  Calc_Q[[Esc]] <-output$cpue 
  
  M[[Esc]]<-output$Natural_Mortality[4,13:16]
}

diagsSS<-plyr::ldply(diag,data.frame)
diagsSS<-diagsSS %>% select(-ESC)
parmSS<-plyr::ldply(params_est,data.frame)
Q_SS<-plyr::ldply(Calc_Q,data.frame)
M_SS<-plyr::ldply(M,data.frame)

ft1.1.1<-M_SS %>% flextable()
invisible(save_as_image(ft1.1.1, path = paste0(path_mod,"/tb_M.png")))

df_diagsSS <-   pivot_longer(diagsSS, 
                             cols = c(convergency, AIC,Total_like, Survey_like, Age_like, RMSE_index, RMSE_age),
                             names_to = "Metric", 
                             values_to = "Value")

df1_diagsSS <- pivot_wider(df_diagsSS, names_from = .id, values_from = Value)

df_parmSS <- pivot_wider(parmSS, names_from = .id, values_from = Value)

Qdata<-Q_SS %>% select(c(".id","Yr","Fleet_name","Vuln_bio","Obs","Exp","Calc_Q"))

#results
path_mod<-"report/run/comparison/Mnew"
ft1 <- flextable(cbind(df1_diagsSS["Metric"], round(df1_diagsSS[,-which(names(df1_diagsSS) == "Metric")], 4)))
ft1 <- colformat_double(ft1, digits=1, na_str = "")
ft1 <- colformat_num(ft1,big.mark = "", na_str = "")
ft1 <- align(ft1,part = "header", align = "center") 
ft1 <- fontsize(ft1, size = 8, part = "header")
ft1 <- autofit(ft1)
invisible(save_as_image(ft1, path = paste0(path_mod,"/tb_Diagstics.png")))

ft2 <- flextable(cbind(df_parmSS["Parameter"], round(df_parmSS[,-which(names(df_parmSS) == "Parameter")], 3)))
ft2 <- colformat_double(ft2, digits=1, na_str = "")
ft2 <- colformat_num(ft2,big.mark = "", na_str = "")
ft2 <- align(ft2,part = "header", align = "center") 
ft2 <- fontsize(ft2, size = 8, part = "header")
ft2 <- autofit(ft2)
invisible(save_as_image(ft2, path = paste0(path_mod,"/tb_Parameters.png")))

Qdata$Fleet_name <- factor(Qdata$Fleet_name, 
                           levels = c("PELAGO", "ECOCADIZ", "BOCADEVA", "ECORECLUTAS"))

# Reordenar la columna .id en orden descendente
Qdata$.id <- Qdata$.id <- factor(Qdata$.id,
                                 levels = c("S1.0_4FLEETS_SelECO",
                                            "S4FLEETS_SelECO_Mage_M1.3",
                                            "S4FLEETS_SelECO_Mage_M1.4",
                                            "S4FLEETS_SelECO_Mage_M1.5"))


# Crear el gráfico
fig_q <- Qdata %>%
  ggplot(aes(x=Fleet_name, y=Calc_Q, colour=.id)) +
  geom_point(aes(size = ifelse(.id == "S1", 4, 2)), shape = 21, stroke = 1.5,
             fill = ifelse(Qdata$.id == "S1", "black", NA)) + 
  # Puntos más grandes para S1 y normales para los demás
  labs(x="Surveys", y="Catchability", title="") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right") +
  guides(size = "none",  # Eliminar la leyenda del tamaño
         colour = guide_legend(override.aes = list(size = c(4, rep(2, length(levels(Qdata$.id))-1)))))

ggsave(file.path(paste0("report/run/comparison/Mnew","/fig_catchability2.png")), fig_q,  width=8, height=5)



mod.sum <- SSsummarize(replist)

SSplotComparisons(mod.sum, subplots=c(13,2,8,10),indexPlotEach = T,
                  legendlabels = esc,pwidth = 5,
                  pheight = 3,png=TRUE,plotdir="report/run/comparison/Mnew",legendloc='topleft')

