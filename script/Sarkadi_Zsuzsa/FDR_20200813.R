library(tidyverse)
#Normális eloszlású random számot az rnorm() függvénnyel tudtok generálni
#A kezeletlen esetben az OD  várható értéke 1.2 és a méréshez tartozó szórás 0.25
#3-3 szimulált mintát
kezeletlen <- rnorm(3, mean=1.2, sd=0.25)

#AMP kezelés mellett az OD várható értéke 1.1 és a hozzá tartozó szórás 0.4
kezeltAMP <- rnorm(3, mean=1.1, sd=0.4)

t.test(kezeletlen, kezeltAMP)

#15-15 minta
kezeletlen <- rnorm(15, mean=1.2, sd=0.25)
kezeltAMP <- rnorm(15, mean=1.1, sd=0.4)
t.test(kezeletlen, kezeltAMP)
#10 kezelt és 3 kezeletlen
kezeletlen <- rnorm(3, mean=1.2, sd=0.25)
kezeltAMP <- rnorm(10, mean=1.1, sd=0.4)
t.test(kezeletlen, kezeltAMP)
# kezelt is kezeletlen
kezeletlen <- rnorm(15, mean=1.2, sd=0.25)
kezeltAMP <- rnorm(15, mean=1.2, sd=0.25)
t.test(kezeletlen, kezeltAMP)


#Alfeladat3: N=5000  t-testet úgy hogy mindig vesztek 3-3 NT és AMP mintát, mindegyikre kiszámoljátok a p-értéket. Az eredményeket tegyétek be egy N soros táblázatba . 

tbl1 <- tibble(experiment_id=character(), p=numeric(), type=character())


for(i in 1:5000) {
  kezeletlen <- rnorm(3, mean=1.4, sd=0.25)
  kezeltAMP <- rnorm(3, mean=1.1, sd=0.4)
  result_ttest <- t.test(kezeletlen, kezeltAMP)
  tbl1 <- tbl1 %>% add_row(experiment_id=sprintf("exp_%03i",i), 
                           p=result_ttest$p.value, type="NT-AMP")
}

#Feladat 4
library(ggplot2)
tbl1 %>%
  ggplot(aes(x=p))+geom_histogram(color="black", fill="white")+
  geom_vline(aes(xintercept = 0.05), color="red", linetype="dashed", size=1) 
  ggsave("out/undersatndig-of-FDR/histogram_pvalue.jpg")
  
tbl1 %>%
    ggplot(aes(x=p))+geom_histogram(color="black", fill="white", binwidth = 0.10)+
    geom_vline(aes(xintercept = 0.05), color="red", linetype="dashed", size=1)
ggsave("out/undersatndig-of-FDR/histogram_pvalue_largebin.jpg")

#Alfeladat 5
tbl2 <- tibble(experiment_id=character(), p=numeric(), type=character())
for(i in 1:5000) {
  kezeletlen <- rnorm(4, mean=1.3, sd=0.25)
  kezeltAMP <- rnorm(5, mean=1.3, sd=0.25)
  result_ttest <- t.test(kezeletlen, kezeltAMP)
  tbl2 <- tbl2 %>% add_row(experiment_id=sprintf("exp_%03i",i), 
                           p=result_ttest$p.value, type="NT-AMP")
}

tbl2 %>%
  ggplot(aes(x=p))+geom_histogram(color="black", fill="white")+
  geom_vline(aes(xintercept = 0.05), color="red", linetype="dashed", size=1) 
ggsave("out/undersatndig-of-FDR/histogram_pvalue_2.jpg")

tbl2 %>%
  ggplot(aes(x=p))+geom_histogram(color="black", fill="white", binwidth = 0.10)+
  geom_vline(aes(xintercept = 0.05), color="red", linetype="dashed", size=1)
ggsave("out/undersatndig-of-FDR/histogram_pvalue_largebin_2.jpg")

#Alfeladat 6
#p értékek hány százaléka szignifikáns ( <0.05) az egyik és a másik szimuláció esetén?

signif_1<- tbl1 %>% 
  count(p<0.05) 

583/5000
# result: 11.7%

signif_2<- tbl2 %>% 
  count(p<0.05) 
231/5000
# result 4.6%

#Alfeladat7

tbl3 <- tibble(experiment_id=character(), p=numeric(), type=character())


for(i in 1:1000) {
  kezeletlen <- rnorm(2, mean=1.4, sd=0.25)
  kezeltAMP <- rnorm(5, mean=1.4, sd=0.25)
  result_ttest <- t.test(kezeletlen, kezeltAMP)
  tbl3 <- tbl3 %>% add_row(experiment_id=sprintf("exp_%03i",i), 
                           p=result_ttest$p.value, type="NT-AMP")
}
tbl3 %>%
  ggplot(aes(x=p))+geom_histogram(color="black", fill="white")+
  geom_vline(aes(xintercept = 0.05), color="red", linetype="dashed", size=1) 

signif_3<- tbl3 %>% 
  count(p<0.05) 
63/1000
# result 6.9%

#Alfeladat8
#Szimuláljatok úgy adatokat, hogy legyen 400 kezelt-VS-kezeletlen, és 4000 kezeletlen-VS-kezeletlen minta pár.

tbl4 <- tibble(experiment_id=character(), p=numeric(), type=character())

for(i in 1:400) {
  kezeletlen <- rnorm(3, mean=2, sd=0.25)
  kezeltAMP <- rnorm(3, mean=1.1, sd=0.4)
  result_ttest1 <- t.test(kezeletlen, kezeltAMP)
  tbl4 <- tbl4 %>% add_row(experiment_id=sprintf("exp_%03i",i), 
                           p=result_ttest1$p.value, type="NT-hatasos")
}
for(i in 1:4000) {
  kezeletlen <- rnorm(3, mean=1.4, sd=0.25)
  kezeltAMP <- rnorm(3, mean=1.4, sd=0.25)
  result_ttest2 <- t.test(kezeletlen, kezeltAMP)
  tbl4 <- tbl4 %>% add_row(experiment_id=sprintf("exp_%03i",i), 
                           p=result_ttest2$p.value, type="NT-hatastalan")
}
tbl4 %>%
  ggplot(aes(x=p, fill=type))+geom_histogram(bins=100)+
  geom_vline(aes(xintercept = 0.05), color="red", linetype="dashed", size=1) 
ggsave("out/undersatndig-of-FDR/histogram_pvalue_feladat8.jpg")

#Alfeladat9
tbl4 <- tbl4 %>% 
  mutate(p_fdr=p.adjust(p, method="fdr"))
tbl4 %>%
  ggplot(aes(x=p_fdr, fill=type))+geom_histogram(bins=100)+
  geom_vline(aes(xintercept = 0.05), color="red", linetype="dashed", size=1) 
ggsave("out/undersatndig-of-FDR/histogram_p_fdr_feladat9.jpg")

#TRUE POSITIVE
TRUE_POSITIVE <- tbl4 %>% 
  filter(p<0.05) %>% 
  count(type=="NT-hatasos")
TRUE_POSITIVE[2,2]
#239

FALSE_POSITIVE <- tbl4 %>% 
  filter(p<0.05) %>% 
  count(type=="NT-hatastalan")
FALSE_POSITIVE[2,2]
#134

TRUE_NEGATIVE <- tbl4 %>% 
  filter(p>0.05) %>% 
  count(type=="NT-hatastalan")
TRUE_NEGATIVE[2,2]
#3886

FALSE_NEGATIVE <- tbl4 %>% 
  filter(p>0.05) %>% 
  count(type=="NT-hatasos")
FALSE_NEGATIVE[2,2]
#161
