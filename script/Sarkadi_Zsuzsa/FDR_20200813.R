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
kezeletlen <- rnorm(3, mean=1.2, sd=0.25)
kezeltAMP <- rnorm(3, mean=1.1, sd=0.4)

tbl1 <- tibble(experiment_id=character(), p=numeric(), type=character())


for(i in 1:5000) {
  kezeletlen <- rnorm(3, mean=1.2, sd=0.25)
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
