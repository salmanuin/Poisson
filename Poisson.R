#Source Code Program R skripsi 2024 ANALISIS REGRESI POISSON, 
#REGRESI BINOMIAL NEGATIF DAN REGRESI HURDLE BINOMIAL NEGATIF
#PADA DATA OVERDISPERSI
#(STUDI KASUS : JUMLAH KASUS KEMATIAN PENDERITA DEMAM BERDARAH
# DENGUE DI PROVINSI JAWA TIMUR TAHUN 2022)
#SALMAN AL-FARISI
#19106010037
#UIN SUNAN KALIJAGA YOGYAKARTA
library(car)
library(AER)
library(MASS)
library(pscl)
#1 Mengimpor data
View(DBD2022)
str(DBD2022)

#2 Statistik deskriptif
summary(DBD2022)
var(DBD2022)

#3 Uji distribusi Poisson
ks.test(DBD2022$Y,"ppois", mean(DBD2022$Y))

#4 Uji Multikolinearitas
model = lm(Y~X1+X2+X3+X4+X5+X6+X7, data = DBD2022)
vif(model)

#5 Estimasi Parameter regresi Poisson
m<- glm(Y~X1+X3+X4+X5+X6+X7, family="poisson", data=DBD2022)
summary(m)
pR2(m)
qchisq(0.95, 6)

#6 Uji overdispersi
dispersiontest(m)

#7 Estimasi parameter regresi Binomial Negatif
m2<- glm.nb(Y~X1+X3+X4+X5+X6+X7, data=DBD2022)
summary(m2)
pR2(m2)
qchisq(0.95, 6)

#8 Estimasi parameter regresi Hurdle Binomial Negatif
m3<-hurdle(Y~X1+X3+X4+X5+X6+X7, data=DBD2022)
summary(m3)
pR2(m3)
qchisq(0.95, 6)
AIC(m3)
