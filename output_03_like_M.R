# likehood natural mortality




library(icesTAF)
library(r4ss)
library(tidyverse)
library(lubridate)
library(ss3diags)
wd <- getwd()


run_esc<-"boot/data/run/" 
list.files(run_esc, full.names = TRUE)
esc<-readLines(paste0(run_esc,"Esc.txt")) 


report<-"report/run/"
path_rep<-paste0(report,esc)

like_M<-"model/like/M"
ls_likeM<-list.files(like_M)

Mconst<-as.numeric(gsub("M", "", ls_likeM))

TotallikeM<-list()
for(i in 1:length(Mconst)){
  esc<-paste0("M",Mconst[i])
  like.dir<-paste0("model/like/M/",esc)


output <- r4ss::SS_output(dir = like.dir,forecast=FALSE)
TotallikeM[[esc]]<-output$likelihoods_used$values[rownames(output$likelihoods_used) == "TOTAL"]
}

likeM<-plyr::ldply(TotallikeM,data.frame)
colnames(likeM)<-c("esc","like")
likeM$esc <- as.numeric(sub("M", "", likeM$esc))

fig1<-ggplot(likeM,aes(x=esc,y=like))+geom_point()+geom_line()+
  labs(title = "",
       x = "M",
       y = "Likelihood") +
theme(panel.grid=element_line(color=NA)) +
  theme(plot.title = element_text(size =10),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 10),
        panel.background = element_rect(colour="gray",fill = "gray99"),
        strip.background = element_rect(colour = "gray", fill = "gray99"),
        axis.text.x = element_text(angle = 0), # Rotar etiquetas del eje X
        axis.text.y = element_text(size = 10),  # Tamaño de etiquetas del eje Y
        axis.title.x = element_text(margin = margin(t = 10)),  # Margen para el título del eje X
        axis.title.y = element_text(margin = margin(r = 10)))+
  scale_x_continuous(breaks = seq(min(likeM$esc), max(likeM$esc), by = 0.2))
ggsave(file.path(paste0(path_rep,"/fig_like_M.png")), fig1,  width=7, height=4)


