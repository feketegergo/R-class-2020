rm(list=ls())


if (!require("tidyverse")) install.packages("tidyverse");
    library(tidyverse)

if (!require("dplyr")) install.packages("dplyr");
    library(dplyr)

if (!require("ggplot2")) install.packages("ggplot2");
library(ggplot2)


#a két minta más-más eloszlásból származik

tbl1<-tibble(experiment_id=character(),p=numeric(),type=character())

for( i in 1:5000)

{

    NT<-rnorm(4,1.2,0.15)
    AMP<-rnorm(5,0.25,0.8)
    
    ttest1<-t.test (NT,AMP,conf.level = 0.95)
    
    tbl1<-tbl1 %>% add_row(p=ttest1$p.value,experiment_id=sprintf("exp_%04.f",i),type="NT-AMP")
    
}

#A két minta ugyanabból az eloszlásból származik

tbl2<-tibble(experiment_id=character(),p=numeric(),type=character())

for( i in 1:5000)
    
{
    
    NT<-rnorm(4,1.2,0.15)
    AMP<-rnorm(5,1.2,0.15)
    
    ttest2<-t.test (NT,AMP,conf.level = 0.95)
    
    tbl2<-tbl2 %>% add_row(p=ttest2$p.value,experiment_id=sprintf("exp_%04.f",i),type="NT-AMP")
    
}
    
#számoljuk meg, hogy a p-értékek hány százaléka szignifikáns egyik ill. másik szimuláció esetén!

tbl1<-tbl1 %>% mutate(is_significant=case_when(p<0.05 ~1,
                                                  TRUE ~ 0))
no_of_significant_ps_1<-sum(tbl1$is_significant)/5000*100

sprintf("A p-értékek %.02f százaléka szignifikáns, ha a két minta eltérő eloszlásból származik",no_of_significant_ps_1)

tbl2<-tbl2 %>% mutate(is_significant=case_when(p<0.05 ~1,
                                                 TRUE ~ 0))
no_of_significant_ps_2<-sum(tbl2$is_significant)/5000*100

sprintf("A p-értékek %.02f százaléka szignifikáns, ha a két minta ugyanabból az eloszlásból származik",no_of_significant_ps_2)


