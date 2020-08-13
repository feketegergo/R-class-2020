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


