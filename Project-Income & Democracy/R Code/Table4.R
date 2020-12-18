#### Table 4 -----------------------------------------------------------------
#### Five-year data #### 
#### 1. Balanced panel, 1970-2000 
# (1) Fixed effects OLS 
data1.2 <- read.csv("Table4_1.csv", header=TRUE)
democ1.2 <- pdata.frame(data1.2, index = "country")
democ1.2 <- na.omit(democ1.2)
Democracy.1970_2000 <- democ1.2$fhpolrigaug
Income.1970_2000 <- democ1.2$lrgdpch
fe1.2 <- plm(Democracy.1970_2000 ~ lag(Democracy.1970_2000) + lag(Income.1970_2000), democ1.2,  index = c("country", "year"),
             model = "within", effect="twoways") 
fe1.2.coef <- coeftest(fe1.2, vcov=vcovHC)

# (2) Arellano Bond GMM 
gmm1.2 <- pgmm(Democracy.1970_2000 ~ lag(Democracy.1970_2000) + lag(Income.1970_2000) | 
                 lag(Democracy.1970_2000, 2:99)| lag(Income.1970_2000, 2),
               democ1.2, index=c("country", "year"), 
               model="onestep", effect="twoways")
gmm1.2.coef <- coeftest(gmm1.2, vcov=vcovHC)

#### 2. Base sample, 1960-2000, without former socialist countries
# (3) Fixed effects OLS 
data2.2 <- read.csv("Table4_2.csv", header=TRUE)
democ2.2 <- pdata.frame(data2.2, index = "country")
Democracy.no_socialist <- democ2.2$fhpolrigaug
Income.no_socialist <- democ2.2$lrgdpch
fe2.2 <- plm(Democracy.no_socialist ~ lag(Democracy.no_socialist) + lag(Income.no_socialist), democ2.2,  index = c("country", "year"),
             model = "within", effect="twoways") 
fe2.2.coef <- coeftest(fe2.2, vcov=vcovHC)

# (4) Arellano Bond GMM 
gmm2.2 <- pgmm(Democracy.no_socialist ~ lag(Democracy.no_socialist) + lag(Income.no_socialist) | 
                 lag(Democracy.no_socialist, 2:99)| lag(Income.no_socialist, 2),
               democ2.2, index=c("country", "year"), 
               model="onestep", effect="twoways")
gmm2.2.coef <- coeftest(gmm2.2, vcov=vcovHC)

#### 3. Base sample, 1960-2000 
data3.2 <- read.csv("Table4_3.csv", header=TRUE)
democ3.2 <- pdata.frame(data3.2, index = "country")
Democracy.1960_2000 <- democ3.2$fhpolrigaug
Income.1960_2000 <- democ3.2$lrgdpch
# (5) Fixed effects OLS 
fe3.2 <- plm(Democracy.1960_2000 ~ lag(Democracy.1960_2000) + lag(Income.1960_2000) + lag(lpop) + lag(medage) 
             + lag(age_veryyoung) + lag(age_young) + lag(age_midage) + lag(age_old), 
             democ3.2, index = c("country", "year"), model = "within", 
             effect="twoways", subset = sample == 1)
fe3.2.coef <- coeftest(fe3.2, vcov=vcovHC)

# (7) Fixed effects OLS 
fe4.2 <- plm(Democracy.1960_2000 ~ lag(Democracy.1960_2000) + lag(Income.1960_2000) + lag(education) + lag(lpop) + lag(medage) 
             + lag(age_veryyoung) + lag(age_young) + lag(age_midage) + lag(age_old), 
             democ3.2, index = c("country", "year"), model = "within", 
             effect="twoways", subset = sample == 1)
fe4.2.coef <- coeftest(fe4.2, vcov=vcovHC)

screenreg(list("Fixed effects OLS (1)" = fe1.2.coef, "Arellano-Bond GMM (2)" = gmm1.2.coef, 
               "Fixed effects (3)" = fe2.2.coef, "Arellano-Bond GMM (4)" = gmm2.2.coef,
               "Fixed effects OLS (5)" = fe3.2.coef, "Fixed effects OLS (7)" = fe4.2.coef))
