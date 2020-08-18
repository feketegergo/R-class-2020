rm(list=ls())

if (!require("tidyverse")) install.packages("tidyverse");
    library(tidyverse)

if (!require("dplyr")) install.packages("dplyr");
    library(dplyr)

if (!require("ggplot2")) install.packages("ggplot2");
library(ggplot2)


# A kezeletlen esetben az OD  várható értéke 1.2 és a méréshez tartozó szórás 0.25
# AMP kezelés mellett az OD várható értéke 1.1 és a hozzá tartozó szórás 0.4

outdir<-paste0("D:/R_workspace/R-class-2020/script/Dori/out/understanding-of-FDR")
dir.create(outdir, recursive = TRUE)

tbl1<-tibble(experiment_id=character(),p=numeric(),type=character())

for( i in 1:5000)

{

    NT<-rnorm(3,1.2,0.15)
    AMP<-rnorm(3,0.25,0.8)
    
    ttest<-t.test (NT,AMP,conf.level = 0.95)
    
    tbl1<-tbl1 %>% add_row(p=ttest$p.value,experiment_id=sprintf("exp_%04.f",i),type="NT-AMP")
    
}


#-----------------generate plots

#hisztogram nagy binekkel

ggplot(tbl1,aes(p))+
    geom_histogram(bins = 20,boundary=0)+
    geom_vline(aes(xintercept = 0.05),colour="red",linetype="dashed",size=1)

filename1 <- paste0("nagybin.pdf")
ggsave(path=outdir,filename=filename1, width =  200 , height = 100 ,units = "mm") 


#hisztogram kis binekkel

ggplot(tbl1,aes(p))+
    geom_histogram(bins = 100,boundary=0)+
    geom_vline(aes(xintercept = 0.05),colour="red",linetype="dashed",size=1)

filename2 <- paste0("kicsibin.pdf")
ggsave(path=outdir,filename=filename2, width =  200 , height = 100 ,units = "mm") 


