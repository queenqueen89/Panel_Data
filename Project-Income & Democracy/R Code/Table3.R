#### Table 3 -----------------------------------------------------------------
#### 1. Five-year data #### 
# (1) Pooled OLS 
data1.1 <- read.csv("5yr_panel.csv", header=TRUE)
democ1.1 <- pdata.frame(data1.1, index="country")
Democracy1.5yr <- democ1.1$polity4
Income1.5yr <- democ1.1$lrgdpch
pols.1 <- plm(Democracy1.5yr ~ lag(Democracy1.5yr) + lag(Income1.5yr) + year -1, democ1.1, index = c("country", "year"), model = "pooling", subset = sample == 1)
pols.1.coef <- coeftest(pols.1, vcov=vcovHC)

# (2) Fixed effects OLS: Two Ways  
fe1.1 <- plm(Democracy1.5yr ~ lag(Democracy1.5yr) + lag(Income1.5yr), democ1.1,
             index = c("country", "year"), model = "within", 
             effect="twoways", subset = sample == 1)
fe1.1.coef <- coeftest(fe1.1, vcov=vcovHC)

# (3) Anderson-Hsiao IV
hsiao.1 <- plm(diff(Democracy1.5yr) ~ lag(diff(Democracy1.5yr)) + lag(diff(Income1.5yr)) + year - 1 | lag(Democracy1.5yr, 2) + lag(Income1.5yr, 2) + year - 1, 
               democ1.1, index = c("country", "year"), model = "pooling", subset = sample == 1)
hsiao.1.coef <- coeftest(hsiao.1, vcov=vcovHC)

# (4) Arellano-Bond GMM 
# i) Arellano-Bond GMM 
gmm1.1 <- pgmm(Democracy1.5yr ~ lag(Democracy1.5yr) + lag(Income1.5yr) | lag(Democracy1.5yr, 2:99)| lag(Income1.5yr, 2), democ1.1, index=c("country", "year"), 
               model="onestep", effect="twoways", subset = sample == 1)
gmm1.1.coef <- coeftest(gmm1.1, vcov=vcovHC)

# (5) Fixed effects OLS
fe2.1 <- plm(Democracy1.5yr ~ lag(Income1.5yr), democ1.1, index = c("country", "year"), model = "within", effect="twoways", subset = sample == 1)
fe2.1.coef <- coeftest(fe2.1, vcov=vcovHC)

#### 2. Annual data #### 
# (6) Fixed effects OLS 
data2.1 <- read.csv("annual_panel.csv", header=TRUE)
democ2.1 <- pdata.frame(data2.1, index="country")
Democracy1.annual <- democ2.1$polity4
Income1.annual <- democ2.1$lrgdpch
fe3.1 <- plm(Democracy1.annual ~ lag(Democracy1.annual) + lag(Income1.annual), democ2.1, index = c("country", "year"), model = "within", 
             effect="twoways", subset = sample == 1)
fe3.1.coef <- coeftest(fe3.1, vcov=vcovHC)

#### 3. Ten-year data #### 
# (7) Fixed effects OLS
data3.1 <- read.csv("10yr_panel.csv", header=TRUE)
democ3.1 <- pdata.frame(data3.1, index="country")
Democracy1.10yr <- democ3.1$polity4
Income1.10yr <- democ3.1$lrgdpch
fe4.1 <- plm(Democracy1.10yr ~ lag(Democracy1.10yr) + lag(Income1.10yr), democ3.1, index = c("country", "year"), model = "within", 
             effect="twoways", subset = sample == 1)
fe4.1.coef <- coeftest(fe4.1, vcov=vcovHC)

#### 4. Twenty-year data #### 
# (9) Fixed effects OLS 
data4.1 <- read.csv("20yr_panel.csv", header=TRUE)
democ4.1 <- pdata.frame(data4.1, index="country")
Democracy1.20yr <- democ4.1$polity4
Income1.20yr <- democ4.1$lrgdpch
fe5.1 <- plm(Democracy1.20yr ~ lag(Democracy1.20yr) + lag(Income1.20yr), democ4.1, index = c("country", "year"), model = "within", 
             effect="twoways", subset = sample == 1)
fe5.1.coef <- coeftest(fe5.1, vcov=vcovHC)

screenreg(list("Pooled OLS (1)" = pols.1.coef, "Fixed effects (2)" = fe1.1.coef, "Anderson-Hsiao IV (3)" = hsiao.1.coef, "Arellano-Bond GMM (4)" = gmm1.1.coef,
               "Fixed effects OLS (5)" = fe2.1.coef, "Fixed effects OLS (6)" = fe3.1.coef, "Fixed effects OLS (7)" = fe4.1.coef, "Fixed effects OLS (9)" = fe5.1.coef))
