# Install and load the necessary package
library(GWmodel) 
library(mapview)
library(readxl)

library(dplyr)
library(ggplot2)
library(tidyverse)
library(easyGgplot2)
library(GWmodel)
library(readxl)



df = read_excel("count_dataset.xlsx")
#df <- read.csv("count_dataset.csv",sep=";")
df
# Prepare your data
df$lon <- as.numeric(df$lon)
df$lat <- as.numeric(df$lat)

df$jumlah_pus <- as.numeric(df$jumlah_pus)
df$jumlah_miskin <- as.numeric(df$jumlah_miskin)
df$lama_sekolah <- as.numeric(df$lama_sekolah)
df$akh <- as.numeric(df$akh)
df$ipm <- as.numeric(df$ipm)
coordinates(df) <- c("lon", "lat")

library(GWmodel) 

DM<-gw.dist(dp.locat=coordinates(df))
DM

y <- df$jumlah_pus
x1 <- df$jumlah_miskin
x2 <- df$lama_sekolah 
x3 <- df$akh
x4 <- df$ipm
formula_regresi = y~x1+x2+x3+x4


bw.gwr <- bw.ggwr(formula_regresi,  
                  data = df,
                  family = "poisson",
                  approach = "AICc",
                  kernel = "gaussian", 
                  adaptive = TRUE,
                  dMat = DM )
bw.gwr


bgwr.res <- ggwr.basic(formula_regresi, 
                       data =df,
                       family = "poisson",
                       bw = bw.gwr, 
                       kernel = "gaussian", 
                       adaptive = TRUE,
                       dMat = DM)

bgwr.res


bw.gwr <- bw.ggwr(formula_regresi,  
                  data = df,
                  family = "poisson",
                  approach = "AICc",
                  kernel = "bisquare", 
                  adaptive = TRUE,
                  dMat = DM )
bw.gwr


bgwr.res <- ggwr.basic(formula_regresi, 
                       data =df,
                       family = "poisson",
                       bw = bw.gwr, 
                       kernel = "bisquare", 
                       adaptive = TRUE,
                       dMat = DM)

bgwr.res

library(tidyverse)
library(spdep)
library(sf)
library(mapview)
df <- as_tibble(df)
df_spasial <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
df_spasial_sp <- as(df_spasial, "Spatial")
mapview(df_spasial[,c("kabkot","jumlah_pus")], zcol = "jumlah_pus", cex="jumlah_pus", layer.name="kabkot", alpha.regions = 0.7,zoom = "fit",legend = TRUE)



par(mfrow = c(2, 2))

# Plot Jumlah Pasangan Usia Subur Menurut Kabupaten/Kota vs x1
plot(x1, y, main = "Jumlah Pasangan Usia Subur vs jumlah_miskin", xlab = "jumlah_miskin", ylab = "Jumlah Pasangan Usia Subur")

# Plot Jumlah Pasangan Usia Subur Menurut Kabupaten/Kota vs x2
plot(x2, y, main = "Jumlah Pasangan Usia Subur vs lama_sekolah", xlab = "lama_sekolah", ylab = "Jumlah Pasangan Usia Subur")

# Plot Jumlah Pasangan Usia Subur Menurut Kabupaten/Kota vs x3
plot(x3, y, main = "Jumlah Pasangan Usia Subur vs akh", xlab = "akh", ylab = "Jumlah Pasangan Usia Subur")

# Plot Jumlah Pasangan Usia Subur Menurut Kabupaten/Kota vs x4
plot(x4, y, main = "Jumlah Pasangan Usia Subur vs ipm", xlab = "ipm", ylab = "Jumlah Pasangan Usia Subur")



############OLS######################
data =df
model_linier = lm(formula = y~x1+x2+x3+x4, data = data)
sum = summary(model_linier)
print(sum)
cat("AIC = ",AIC(model_linier))
cat("\nR2 = ",sum$r.squared)

library(nortest)

ks_y  = lillie.test(y)
ks_x1 = lillie.test(x1)
ks_x2 = lillie.test(x2)
ks_x3 = lillie.test(x3)
ks_x4 = lillie.test(x4)

pval_ks_test = c(ks_y$p.value,ks_x1$p.value,ks_x2$p.value)
variabel = colnames(data[,2:7])
ks_result = data.frame(variabel,pval_ks_test)
ks_result$pval_ks_test = format(ks_result$pval_ks_test,scientific = FALSE)
print(ks_result)

print(ks_x4)

pval_ks_test



# Uji Multikolinearitas
# VIF
vif(model_linier)
# rata-rata VIF
mean(vif(model_linier))



# Uji Heterokedastisitas
library(skedastic)
glejser(model_linier)



library(spdep)
library(sf)
library(mapview)

df <- as_tibble(data)
df_spasial <- st_as_sf(df, coords = c("Long", "Lat"))
df_spasial_sp <- as(df_spasial, "Spatial")
df_spasial_sp


coords <- coordinates(df_spasial_sp)
bobot <- nb2listw(knn2nb(knearneigh(coords)))
moran.test(df_spasial_sp$jumlah_pus , bobot, alternative="greater")


#another plot
library(spdep)
library(rgdal)
library(raster)
peta<-readOGR(dsn="batas_administrasi_kabupaten_kota_polygon_120020180327153112", layer="batas_administrasi_kabupaten_kota_polygon_120020180327153112")

peta 
df = data
colnames(df)[1] <- "kabupaten"
df$kabupaten <- toupper(df$kabupaten)
peta = merge(peta,df, by.x = "kabupaten", by.y = "kabupaten")
peta


k=16
colfunc <- colorRampPalette(c("blue","violet","yellow")) 
color <- colfunc(k)
library(sp)

spplot(peta, "jumlah_miskin", sub="jumlah_miskin")

view(peta@data)

