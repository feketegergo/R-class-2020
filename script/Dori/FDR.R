rm(list=ls())

library(tidyverse)

# A kezeletlen esetben az OD  várható értéke 1.2 és a méréshez tartozó szórás 0.25
# AMP kezelés mellett az OD várható értéke 1.1 és a hozzá tartozó szórás 0.4

pvalues<-data.frame()

for( i in 1:3)
{

    kezeletlen<-rnorm(3,1.2,0.25)
    kezelt<-rnorm(3,1.1,0.4)
    
    ttest<-t.test (kezeletlen,kezelt,conf.level = 0.95)
    
    pvalues[i,1]<-ttest[3]

}

