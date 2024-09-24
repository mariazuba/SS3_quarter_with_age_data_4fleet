# Natural Mortality
# libraries ------------------------------------------------------
library(r4ss) 
library(icesTAF)

# directorios ----
old_wd <- getwd()

run.dir<-"model/run/S1"
Mconst<-seq(0.1,4,0.1)

for(i in 1:length(Mconst)){
esc<-paste0("M",Mconst[i])


like.dir<-paste0("model/like/M/",esc)
# delete  folder named "retro" 
system(paste("rm -r", shQuote(like.dir)))

dir.create(like.dir, recursive = TRUE)

copy_SS_inputs(dir.old = run.dir, 
               dir.new =  like.dir,
               copy_exe = FALSE,
               verbose = FALSE)

ctl <-r4ss::SS_readctl(file = paste0(like.dir,"/control.SS"),datlist =paste0(like.dir,"/data.SS"),verbose = FALSE)

ctl$natM["natM1", c("Age_0","Age_1","Age_2","Age_3")] <- c(Mconst[i]+0.9,rep(Mconst[i],3))

r4ss::SS_writectl(ctllist=ctl, outfile = paste0(like.dir,"/control.SS"),overwrite=T,verbose = F)

cp("boot/software/ss3", like.dir)
wd <- like.dir
system(wd)
system(paste0("chmod 755 ",wd,"/ss3"))
r4ss::run(dir=wd, exe="ss3", skipfinished=FALSE, show_in_console =T)

#'*------------------------------------------------------------------------------------------*
setwd(old_wd)
}

rm(list=ls())



