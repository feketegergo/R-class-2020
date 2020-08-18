rm(list=ls())


if (!require("tidyverse")) install.packages("tidyverse");
library(tidyverse)

if (!require("dplyr")) install.packages("dplyr");
library(dplyr)

if (!require("ggplot2")) install.packages("ggplot2");
library(ggplot2)


#a két minta más-más eloszlásból származik

tbl3<-tibble(experiment_id=character(),p=numeric(),type=character())
NT<-rnorm(5,1.2,0.25)

for( i in 1:1000)
  
{
  
  AMP<-rnorm(5,1.2,0.25)
  
  ttest3<-t.test (NT,AMP,conf.level = 0.95)
  
  tbl3<-tbl3 %>% add_row(p=ttest3$p.value,experiment_id=sprintf("exp_%04.f",i),type="NT-AMP")
  
}

#hogy néz ki az eloszlás?

ggplot(tbl3,aes(p))+
  geom_histogram(bins = 20,boundary=0)+
  geom_vline(aes(xintercept = 0.05),colour="red",linetype="dashed",size=1)


tbl3<-tbl3 %>% mutate(is_significant=case_when(p<0.05 ~ 1,
                                               TRUE ~ 0))
no_of_significant_ps_3<-sum(tbl3$is_significant)/1000*100

sprintf("A p-értékek %.02f százaléka szignifikáns, ha csak egyszer mértünk kontrollt, és ahhoz hasonlítjuk az összes mérésünket",no_of_significant_ps_3)

