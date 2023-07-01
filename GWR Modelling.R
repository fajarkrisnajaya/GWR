
#Library Analysis
library(dplyr)
library(ggplot2)
library(tidyverse)
library(easyGgplot2)
library(GWmodel)
library(readxl)

#Read Data
data = read.csv("gwr_data.csv")
View(data)

# memeriksa tipe data
str(data)

# statistik deskriptif
summary(data)
str(data)

y =  data$Persentase_NEET    
x1 = data$Angka_Melek_Huruf 
x2 = data$Remaja_dengan_TIK 


# Regresi Linier Berganda
#Model Regresi OLS
model_linier = lm(formula = y~x1+x2, data = data)
sum = summary(model_linier)
print(sum)
cat("AIC = ",AIC(model_linier))
cat("\nR2 = ",sum$r.squared)

#Kesimpulan ; pvalue < alpha | 1.995e-08 < 0.05 | Tolak H0
#Kesimpulan ; Terdapat paling sedikit ada 1 variabel x mempengaruhi y

#Uji Normalitas Test
library(nortest)

ks_y  = lillie.test(y)
ks_x1 = lillie.test(x1)
ks_x2 = lillie.test(x2)
ks_x3 = lillie.test(x3)
ks_x4 = lillie.test(x4)
ks_x5 = lillie.test(x5)
ks_x6 = lillie.test(x6)

pval_ks_test = c(ks_y$p.value,ks_x1$p.value,ks_x2$p.value,ks_x3$p.value,ks_x4$p.value,ks_x5$p.value,ks_x6$p.value)
variabel = colnames(data[,3:9])
ks_result = data.frame(variabel,pval_ks_test)
ks_result$pval_ks_test = format(ks_result$pval_ks_test,scientific = FALSE)
print(ks_result)

#Variabel dist normal (x1,x2,x3,x4x,x6) | Variabel tidak dist normal (y,x5)

plot(model_linier) #Normal Q-Q
lillie.test(model_linier[['residuals']])

#nilai Signifikasi untuk nilai residualnya (0.8215) > 0.05 maka disimpulkan model regresi telah memenuhi asumsi normalitas.

# Uji Autokorelasi
library(car)
dwt(model_linier)

# pvalue uji < alpha (0.05) sehingga tolak H0 : tidak terdapat autokorelasi

# Uji Multikolinearitas
# VIF
vif(model_linier)
# rata-rata VIF
mean(vif(model_linier))

# Karena model tidak memiliki VIF lebih besar dari 10, jadi ini menunjukkan tidak ada multikolinearitas dalam data, juga rata-rata VIF adalah sekitar 1.64, jadi tidak ada bias dalam model.


# Uji Heterokedastisitas
library(skedastic)
glejser(model_linier)

# Karena nilai pvalue (0.7924852) > 0,05 maka dapat dikatakan model persamaan regresi tidak mengalami heteroskedastisitas atau model regresi mengalami homoskedastisitas


library(spdep)
library(sf)
library(mapview)

df <- as_tibble(data)
df_spasial <- st_as_sf(df, coords = c("Long", "Lat"))
df_spasial_sp <- as(df_spasial, "Spatial")
df_spasial_sp


# Uji Dependensi Spasial
coords <- coordinates(df_spasial_sp)
bobot <- nb2listw(knn2nb(knearneigh(coords)))
moran.test(df_spasial_sp$Avg_Suhu, bobot, alternative="greater")

# Didapatkan p-value= 0.9562 karena nilai pvalue > 0,05 maka dapat gagal tolak H0 berarti kurang adanya bukti autokorelasi spasial.

# Nilai Euclidean antar titik wilayah
# -----Import Peta-----
library(spdep)
library(rgdal)
library(raster)
peta<-readOGR(dsn="D:/OneDrive/Universitas Airlangga/SEMESTER 6/Analisis Data Spasial/M5_Regresi_Spasial/Data/indonesia", layer="BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL")

peta 
df = data
colnames(df)[1] <- "PROVINSI"
df$PROVINSI <- toupper(df$PROVINSI)
peta = merge(peta,df, by.x = "PROVINSI", by.y = "PROVINSI")
peta

k=16
colfunc <- colorRampPalette(c("blue","violet","yellow")) 
color <- colfunc(k)
library(sp)
spplot(peta, "Avg_Suhu", col.regions=color, sub="Rata-rata Suhu")
spplot(peta, "Curah_hujan", col.regions=color, sub="Curah Hujan (mm)")
spplot(peta, "Hari_hujan", col.regions=color, sub="Jumlah Hari Hujan")
spplot(peta, "Kecepatan_angin", col.regions=color, sub="Kecepatan Angin (m/det)")

spplot(peta, "Kelembaban", col.regions=color, sub="Kelembapan")
spplot(peta, "Tekanan_udara", col.regions=color, sub="Tekanan Udara (mb)")
spplot(peta, "Penyinaran_matahari", col.regions=color, sub="Lama Penyinaran Matahari (%)")











# Nilai Euclidean antar titik wilayah
library(GWmodel)

euclidean <- gw.dist(coords)
View(euclidean)

# Nilai Bandwidth Optimal
library(spgwr)
library(spdep)
library(spatialreg)
library(gwrr)

#pembobot GWR yang digunakan adalah pembobot kernel gaussian
gwr_adapt <-gwr.sel(y~x1+x2+x3+x4+x5, df_spasial_sp,adapt=TRUE,gweight = gwr.Gauss)
gwr_adapt #nilai optimal

# Matriks Pembobot Lokasi
bobot <- knn2nb(knearneigh(coords,k=4))
matriks_bobot <- nb2mat(bobot, zero.policy=TRUE)
rownames(matriks_bobot) = df$Provinsi
colnames(matriks_bobot) = df$Provinsi
View(matriks_bobot)

# Estimasi Parameter GWR
gwr.fit <- gwr(y~x1+x2+x3+x4+x5+x6, data = df_spasial_sp, adapt=gwr_adapt, se.fit=T, hatmatrix=T, gweight = gwr.Gauss)
gwr.fit

# Output model GWR
df_gwr = as.data.frame(gwr.fit$SDF)
rownames(df_gwr) = df_spasial$Provinsi
View(df_gwr)

# Export hasil model GWR
library("writexl")
write_xlsx(df_gwr,"Model GWR.xlsx")