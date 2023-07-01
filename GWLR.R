#Import Data
library(readxl)
binary_df = read_excel("Data Binary.xlsx")
binary_df
str(binary_df)


#Logistic Regression
library(lmtest)
library(car)
library(pscl)
logistic_reg = glm(Y~X1+X2+X3+X4+X6, data = binary_df, family = "binomial")
summary(logistic_reg)
#calculate McFadden's R-squared for model
pR2(logistic_reg)['McFadden']

# Menghitung probabibiltas tingkat kemiskinan tinggi (1)
probabilities <- predict(logistic_reg, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

# uji asumsi linearitas
# Select only numeric predictors
library(dplyr)
library(ggplot2)
library(broom)
library(tidyverse)

#Drop Variabel Non-Predictor dan Yang Tidak Signifikan
mydata <- subset(binary_df, select = -c(`Kota/Kabupaten (2021)`,Y,X5,lon,lat))
predictors <- colnames(mydata)

# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# uji asumsi multikolinearitas
car::vif(logistic_reg)

# Uji Nilai Paling Berpengaruh, disini mengecek apakah ada nilai yang berpengaruh secara ekstreme yang dapat mempengaruhi hasil regresi logistik
plot(logistic_reg, which = 4, id.n = 3)
# Extract model results
model.data <- augment(logistic_reg) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Y), alpha = .5) +
  theme_bw()

model.data %>% 
  filter(abs(.std.resid) > 3)

#Drop Variabel Yang Tidak Signifikan
binary_df <- subset(binary_df, select = -c(X5))

# == GWLR ==
library(sp)
binary_df.sp.GWR=binary_df
coordinates(binary_df.sp.GWR) <-binary_df[,c("lat","lon")]#kolom menyatakan letak Long-Lat
class(binary_df.sp.GWR)  
head(binary_df.sp.GWR)

library(GWmodel)
DM = gw.dist(dp.locat = coordinates(binary_df.sp.GWR))

#Pemilihan Bandwidth Optimal
##Fixed Gaussian
DM = gw.dist(dp.locat = coordinates(binary_df.sp.GWR))
bw.gwr.fg = bw.ggwr(Y ~ X1+X2+X3+X4+X6,
                    data = binary_df.sp.GWR,
                    family = "binomial",
                    approach = "AICc",
                    kernel = "gaussian",
                    adaptive = FALSE,
                    dMat = DM)
bgwr.res.fg = ggwr.basic(Y ~ X1+X2+X3+X4+X6,
                         data = binary_df.sp.GWR,
                         family = "binomial",
                         bw = bw.gwr.fg, 
                         kernel = "gaussian", 
                         adaptive = FALSE,
                         dMat = DM)
bgwr.res.fg
#Adaptive Gaussian
DM = gw.dist(dp.locat = coordinates(binary_df.sp.GWR))
bw.gwr.ag = bw.ggwr(Y ~ X1+X2+X3+X4+X6,
                    data = binary_df.sp.GWR,
                    family = "binomial",
                    approach = "AICc",
                    kernel = "gaussian",
                    adaptive = TRUE,
                    dMat = DM)
bgwr.res.ag = ggwr.basic(Y ~ X1+X2+X3+X4+X6,
                         data = binary_df.sp.GWR,
                         family = "binomial",
                         bw = bw.gwr.ag, 
                         kernel = "gaussian", 
                         adaptive = TRUE,
                         dMat = DM)
bgwr.res.ag
#Adaptive Tricube
DM = gw.dist(dp.locat = coordinates(binary_df.sp.GWR)) #bandiwth optimum
bw.gwr.at = bw.ggwr(Y ~ X1+X2+X3+X4+X6,
                    data = binary_df.sp.GWR,
                    family = "binomial",
                    approach = "AICc",
                    kernel = "tricube",
                    adaptive = TRUE,
                    dMat = DM)
bgwr.res.at = ggwr.basic(Y ~ X1+X2+X3+X4+X6,
                         data = binary_df.sp.GWR,
                         family = "binomial",
                         bw = bw.gwr.at, 
                         kernel = "tricube", 
                         adaptive = TRUE,
                         dMat = DM)
bgwr.res.at
#Adaptive Bisquare
DM = gw.dist(dp.locat = coordinates(binary_df.sp.GWR))
bw.gwr.ab = bw.ggwr(Y ~ X1+X2+X3+X4+X6,
                    data = binary_df.sp.GWR,
                    family = "binomial",
                    approach = "AICc",
                    kernel = "bisquare",
                    adaptive = TRUE,
                    dMat = DM)
bgwr.res.ab = ggwr.basic(Y ~ X1+X2+X3+X4+X6,
                         data = binary_df.sp.GWR,
                         family = "binomial",
                         bw = bw.gwr.ab, 
                         kernel = "bisquare", 
                         adaptive = TRUE,
                         dMat = DM)
bgwr.res.ab

#Berdasarkan nilai AIC dan pseudo R^2, bandwidth optimal adalah Fixed Gaussian
##Fixed Gaussian
DM = gw.dist(dp.locat = coordinates(binary_df.sp.GWR))
bw.gwr.fg = bw.ggwr(Y ~ X1+X2+X3+X4+X6,
                    data = binary_df.sp.GWR,
                    family = "binomial",
                    approach = "AICc",
                    kernel = "gaussian",
                    adaptive = FALSE,
                    dMat = DM)
bgwr.res.fg = ggwr.basic(Y ~ X1+X2+X3+X4+X6,
                         data = binary_df.sp.GWR,
                         family = "binomial",
                         bw = bw.gwr.fg, 
                         kernel = "gaussian", 
                         adaptive = FALSE,
                         dMat = DM)
  bgwr.res.fg

#Model Regresi Logistik
pR2(logistic_reg)['McFadden']
AIC(logistic_reg)
#Model GWLR
bgwr.res.fg

#Uji Signifikansi Parameter Tiap Model
#Estimasi Parameter
bgwr.res.fg$SDF
#Mengubah T-Value menjadi P-Value
intercept = bgwr.res.fg$SDF$Intercept
X1 = bgwr.res.fg$SDF$X1_TV
X2 = bgwr.res.fg$SDF$X2_TV
X3 = bgwr.res.fg$SDF$X3_TV
X4 = bgwr.res.fg$SDF$X4_TV
X6 = bgwr.res.fg$SDF$X6_TV
p_valX1 = 2*pt(abs(X1),df=22,lower.tail = FALSE)
p_valX1
p_valX2 = 2*pt(abs(X2),df=22,lower.tail = FALSE)
p_valX2
p_valX3 = 2*pt(abs(X3),df=22,lower.tail = FALSE)
p_valX3
p_valX4 = 2*pt(abs(X4),df=22,lower.tail = FALSE)
p_valX4
p_valX6 = 2*pt(abs(X6),df=22,lower.tail = FALSE)
p_valX6
#Export Hasil Pemodelan
library(writexl)
outputGWLR = data.frame(binary_df$`Kota/Kabupaten (2021)`,intercept,X1,X2,X3,X4,X6,p_valX1,p_valX2,p_valX3,p_valX4,p_valX6)
writexl::write_xlsx(outputGWLR,"output Model GWLR.xlsx")
