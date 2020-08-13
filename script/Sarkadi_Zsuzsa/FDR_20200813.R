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
  sprintf("exp_%03d",tbl1[i] )
}
i
p=t.test(kezeletlen, kezeltAMP)$p.value    
sprintf("exp_%03d",i )

