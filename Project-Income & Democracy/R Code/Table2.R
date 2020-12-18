#### Table 2 -----------------------------------------------------------------
#### 1. Five-year data #### 
# (1) Pooled OLS 
library("lmtest")
library("plm")
library("texreg")
setwd("/Users/nicoleyin88/Documents/1. Panel Data/0. Final Project/1. Code/")
data1 <- read.csv("5yr_panel.csv", header=TRUE)
democ1 <- pdata.frame(data1, index="country")
Democracy.5yr <- democ1$fhpolrigaug
Income.5yr <- democ1$lrgdpch
pols <- plm(Democracy.5yr ~ lag(Democracy.5yr) + lag(Income.5yr) + year -1, democ1, index = c("country", "year"), 
            model = "pooling", subset = sample == 1)
coeftest(pols, vcov=vcovHC)

# (2) Fixed effects OLS: Two Ways  
fe1 <- plm(Democracy.5yr ~ lag(Democracy.5yr) + lag(Income.5yr), democ1, index = c("country", "year"), model = "within",  
           effect="twoways", subset = sample == 1)
coeftest(fe1, vcov=vcovHC)

# (3) Anderson-Hsiao IV
hsiao <- plm(diff(Democracy.5yr) ~ lag(diff(Democracy.5yr)) + lag(diff(Income.5yr)) + year - 1 | 
               lag(Democracy.5yr, 2) + lag(Income.5yr, 2) + year - 1, 
             democ1, index = c("country", "year"), model = "pooling", subset = sample == 1)
coeftest(hsiao, vcov=vcovHC)

# (4) Arellano-Bond GMM 
# i) Arellano-Bond GMM 
gmm1 <- pgmm(Democracy.5yr ~ lag(Democracy.5yr) + lag(Income.5yr) | lag(Democracy.5yr, 2)| lag(Income.5yr, 2), 
             democ1, index=c("country", "year"), 
             model="onestep", effect="twoways", subset = sample == 1)
coeftest(gmm1, vcov=vcovHC)

# (5) Fixed effects OLS
fe2 <- plm(Democracy.5yr ~ lag(Income.5yr), democ1, index = c("country", "year"), model = "within",  
           effect="twoways", subset = sample == 1)
fe2.coef <- coeftest(fe2, vcov=vcovHC)

#### 2. Annual data #### 
# (6) Fixed effects OLS 
data2 <- read.csv("annual_panel.csv", header=TRUE)
democ2 <- pdata.frame(data2, index="country")
Democracy.annual <- democ2$fhpolrigaug
Income.annual <- democ2$lrgdpch
fe3 <- plm(Democracy.annual ~ lag(Democracy.annual) + lag(Income.annual), democ2, index = c("country", "year"), model = "within", 
           effect="twoways", subset = sample == 1)
coeftest(fe3, vcov=vcovHC)

#### 3. Ten-year data #### 
# (7) Fixed effects OLS 
data3 <- read.csv("10yr_panel.csv", header=TRUE)
democ3 <- pdata.frame(data3, index="country")
Democracy.10yr <- democ3$fhpolrigaug
Income.10yr <- democ3$lrgdpch
fe4 <- plm(Democracy.10yr ~ lag(Democracy.10yr) + lag(Income.10yr), democ3, index = c("country", "year"), model = "within", 
           effect="twoways", subset = sample == 1)
coeftest(fe4, vcov=vcovHC)
summary(fe4)
#### 4. Twenty-year data #### 
# (9) Fixed effects OLS 
data4 <- read.csv("20yr_panel.csv", header=TRUE)
democ4 <- pdata.frame(data4, index="country")
Democracy.20yr <- democ4$fhpolrigaug
Income.20yr <- democ4$lrgdpch
fe5 <- plm(Democracy.20yr ~ lag(Democracy.20yr) + lag(Income.20yr), democ4, index = c("country", "year"), model = "within", 
           effect="twoways", subset = sample == 1)
coeftest(fe5, vcov=vcovHC)

screenreg(list("Pooled OLS (1)" = pols.coef, "Fixed effects (2)" = fe1.coef, 
               "Anderson-Hsiao IV (3)" = hsiao.coef, "Arellano-Bond GMM (4)" = gmm1.coef,
               "Fixed effects OLS (5)" = fe2.coef, "Fixed effects OLS (6)" = fe3.coef,
               "Fixed effects OLS (7)" = fe4.coef, "Fixed effects OLS (9)" = fe5.coef))
