rm(list=ls())


if (!require("tidyverse")) install.packages("tidyverse");
library(tidyverse)

if (!require("dplyr")) install.packages("dplyr");
library(dplyr)

if (!require("ggplot2")) install.packages("ggplot2");
library(ggplot2)

# Szimuláljatok úgy adatokat, hogy legyen 400 kezelt-VS-kezeletlen, és 4000 kezeletlen-VS-kezeletlen minta p�r.

#400 minta - kezelt vs kezeletlen

tbl1<-tibble(experiment_id=character(),p=numeric(),type=character())
outdir<-paste0("D:/R_workspace/R-class-2020/script/Dori/out/understanding-of-FDR")


for( i in 1:400)
  
{
  
  NT<-rnorm(4,1.2,0.15)
  AMP<-rnorm(5,0.25,0.8)
  
  ttest1<-t.test (NT,AMP,conf.level = 0.95)
  
  tbl1<-tbl1 %>% add_row(p=ttest1$p.value,experiment_id=sprintf("exp_%04.f",i),
                         type="operative")
                         
  
}

#4000 minta, ahol kezeletlen vs kezeletlen

tbl2<-tibble(experiment_id=character(),p=numeric(),type=character())

for( i in 1:4000)
  
{
  
  NT<-rnorm(4,1.2,0.15)
  AMP<-rnorm(4,1.2,0.15)
  
  ttest2<-t.test (NT,AMP,conf.level = 0.95)
  
  tbl2<-tbl2 %>% add_row(p=ttest2$p.value,experiment_id=sprintf("exp_%04.f",i),
                         type="no_effect")

}

total_table<-tbl1 %>%  bind_rows(tbl2)

# hisztogram rajzolása

ggplot(total_table,aes(p,fill=type))+
  geom_histogram(bins = 100,boundary=0,position = "identity",alpha=0.8)+
  geom_vline(aes(xintercept = 0.05),colour="red",linetype="dashed",size=1)+
  scale_fill_manual(values=c("no_effect"="blue","operative"="lightpink2"))+
  labs(fill = "valódi típus")+
  geom_text(aes(x=0.03,y=50, label="p=0.05"), colour="red", angle=90, size=4)+
  ggtitle("raw p-values")

filename1 <- paste0("overlay.pdf")
ggsave(path=outdir,filename=filename1, width =  200 , height = 100 ,units = "mm") 

#FDR korrekció

total_table <- total_table %>% 
  mutate(p_fdr=p.adjust(p, method="fdr"))


rawp<-ggplot(total_table,aes(p,fill=type))+
  geom_histogram(bins = 100,boundary=0,position = "identity",alpha=0.5)+
  geom_vline(aes(xintercept = 0.05),colour="red",linetype="dashed",size=1)+
  scale_fill_manual(values=c("no_effect"="darkturquoise","operative"="deeppink"))+
  labs(fill = "valódi típus")+
  ggtitle("raw p-values")


FDRp<-ggplot(total_table,aes(p_fdr,fill=type))+
  geom_histogram(bins = 100,boundary=0,position = "identity",alpha=0.5)+
  geom_vline(aes(xintercept = 0.05),colour="red",linetype="dashed",size=1)+
  scale_fill_manual(values=c("no_effect"="darkturquoise","operative"="deeppink"))+
  labs(fill = "valódi típus")+
  ggtitle("FDR")

#install.packages("cowplot")

library(cowplot)
ggdraw()+
  draw_plot(rawp,x = 0.0, y = 0.5, width = 1, height = 0.5)+
  draw_plot(FDRp,x = 0.0, y = 0.0, width = 1, height = 0.5)

filename2 <- paste0("allP_histograms.pdf")

ggsave(path=outdir,filename=filename2,width =  18 , height = 18 ,units = "in")

#Melyek az ábrán a TRUE-Pozitív, FALSE-Pozitív, TRUE-Negatív és FALSE-Negatív esetek?

#TRUE-Pozitív

TRUE_positive <- total_table %>% 
  filter(p<0.05,type=="operative") %>% 
  count()

sprintf("A TRUE-Pozitív esetek száma: %.0f" ,TRUE_positive$n)

#FALSE-Pozitív

FALSE_positive <- total_table %>% 
  filter(p<0.05,type=="no_effect") %>% 
  count()

sprintf("A FALSE-Pozitív esetek száma: %.0f" ,FALSE_positive$n)

#TRUE-Negative

TRUE_negative <- total_table %>% 
  filter(p>0.05,type=="no_effect") %>% 
  count()

sprintf("A TRUE-negative esetek száma: %.0f" ,TRUE_negative$n)

#FALSE-Negative

FALSE_negative <- total_table %>% 
  filter(p>0.05,type=="operative") %>% 
  count()

sprintf("A FALSE-negatív esetek száma: %.0f" ,FALSE_negative$n)


#Ugyanezek FDR korrekció után

#TRUE-Pozitív

TRUE_positive_fdr <- total_table %>% 
  filter(p_fdr<0.05,type=="operative") %>% 
  count()

sprintf("A TRUE-Pozitív esetek száma FDR korrekció után: %.0f" ,TRUE_positive_fdr$n)

#FALSE-Pozitív

FALSE_positive_fdr <- total_table %>% 
  filter(p_fdr<0.05,type=="no_effect") %>% 
  count()

sprintf("A FALSE-Pozitív esetek száma FDR korrekció után: %.0f" ,FALSE_positive_fdr$n)

#TRUE-Negative

TRUE_negative_fdr <- total_table %>% 
  filter(p_fdr>0.05,type=="no_effect") %>% 
  count()

sprintf("A TRUE-negative esetek száma FDR korrekció után: %.0f" ,TRUE_negative_fdr$n)

#FALSE-Negative

FALSE_negative_fdr <- total_table %>% 
  filter(p_fdr>0.05,type=="operative") %>% 
  count()

sprintf("A FALSE-negatív esetek száma FDR korrekció után: %.0f" ,FALSE_negative_fdr$n)

#Számoljátok ki precision-t és recall-t!

#precision:  fraction of relevant instances among the retrieved instances ~ true positives/identified as positives
#recall (sensitivity):proportion of positives that are correctly identified ~ true positives/ all positives (regardless of beeing identified or not)


precision <- TRUE_positive$n/(TRUE_positive$n+FALSE_positive$n)
sensitivity <- TRUE_positive$n/(TRUE_positive$n+FALSE_negative$n)

precision_FDR <- TRUE_positive_fdr$n/(TRUE_positive_fdr$n+FALSE_positive_fdr$n)
sensitivity_FDR <- TRUE_positive_fdr$n/(TRUE_positive_fdr$n+FALSE_negative_fdr$n)


summary<-tibble(precision=numeric(),sensitivity=numeric(),is_FDR=numeric())
summary<-summary %>% add_row(precision,sensitivity) %>% 
                     add_row(precision=precision_FDR,sensitivity=sensitivity_FDR)
summary$is_FDR<-c(0,1)
