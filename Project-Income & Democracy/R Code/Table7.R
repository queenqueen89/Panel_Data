#### Table 7 -----------------------------------------------------------------
#### 1. Panel A #### 
# (1) Pooled PLS 
data1.5 <- read.csv("25yr_panel.csv", header=TRUE)
democ1.5 <- pdata.frame(data1.5, index="country")
Democracy.25yr <- democ1.5$polity4
Income.25yr <- democ1.5$lrgdpmad
pols1.5 <- plm(Democracy.25yr ~ lag(Democracy.25yr) + lag(Income.25yr) + year -1, democ1.5,
               index = "madid", model = "pooling", subset = sample == 1)
pols1.5.coef <- coeftest(pols1.5, vcov=vcovHC)

# (2) Fixed Effects OLS 
fe1.5 <- plm(Democracy.25yr ~ lag(Democracy.25yr) + lag(Income.25yr), democ1.5,
             index = "madid", model = "within", 
             effect="twoways", subset = sample == 1)
fe.1.5.coef <- coeftest(fe1.5, vcov=vcovHC)

# (3) Arellano-Bond GMM 
gmm1.5 <- pgmm(Democracy.25yr ~ lag(Democracy.25yr) + lag(Income.25yr) | 
                 lag(Democracy.25yr, 2:99)| lag(Income.25yr, 2),
               democ1.5, index=c("country", "year"), 
               model="onestep", effect="twoways", subset = sample == 1)
gmm1.5.coef <- coeftest(gmm1.5, vcov=vcovHC)

# (4) Fixed Effects OLS 
fe2.5 <- plm(Democracy.25yr ~ lag(Income.25yr), democ1.5,
             index = "madid", model = "within", 
             effect="twoways", subset = sample == 1)
fe2.5.cocef <- coeftest(fe2.5, vcov=vcovHC)

# (5) Fixed Effects OLS 
data2.5 <- read.csv("Table7_PanelA_col5.csv", header=TRUE)
democ2.5 <- pdata.frame(data2.5, index="country")
Democracy1.25yr <- democ2.5$polity4
Income1.25yr <- democ2.5$lrgdpmad
fe3.5 <- plm(Democracy1.25yr ~ lag(Democracy1.25yr) + lag(Income1.25yr), democ2.5,
             index = "madid", model = "within", 
             effect="twoways")
fe3.5.coef <- coeftest(fe3.5, vcov=vcovHC)

#### 2. Panel B #### 
# (1) Pooled PLS 
data3.5 <- read.csv("50yr_panel.csv", header=TRUE)
democ3.5 <- pdata.frame(data3.5, index="country")
Democracy.50yr <- democ3.5$polity4
Income.50yr <- democ3.5$lrgdpmad
pols2.5 <- plm(Democracy.50yr ~ lag(Democracy.50yr) + lag(Income.50yr) + year -1, democ3.5,
               index = "madid", model = "pooling", subset = sample == 1)
pols2.5.coef <- coeftest(pols2.5, vcov=vcovHC)

# (2) Fixed Effects OLS 
fe4.5 <- plm(Democracy.50yr ~ lag(Democracy.50yr) + lag(Income.50yr), democ3.5,
             index = "madid", model = "within", 
             effect="twoways", subset = sample == 1)
fe4.5.coef <- coeftest(fe4.5, vcov=vcovHC)

# (3) Arellano-Bond GMM 
gmm2.5 <- pgmm(Democracy.50yr ~ lag(Democracy.50yr) + lag(Income.50yr) | 
                 lag(Democracy.50yr, 2:99)| lag(Income.50yr, 2),
               democ3.5, index=c("country", "year"), 
               model="onestep", effect="twoways", subset = sample == 1)
gmm2.5.coef <- coeftest(gmm2.5, vcov=vcovHC)

# (4) Fixed Effects OLS 
fe5.5 <- plm(Democracy.50yr ~ lag(Income.50yr), democ3.5,
             index = "madid", model = "within", 
             effect="twoways", subset = sample == 1)
fe5.5.coef <- coeftest(fe5.5, vcov=vcovHC)

# (5) Fixed Effects OLS 
data4.5 <- read.csv("Table7_PanelB_col5.csv")
democ4.5 <- pdata.frame(data4.5, index="country")
Democracy1.50yr <- democ4.5$polity4
Income1.50yr <- democ4.5$lrgdpmad
fe6.5 <- plm(Democracy1.50yr ~ lag(Democracy1.50yr) + lag(Income1.50yr), democ4.5,
             index = "madid", model = "within", 
             effect="twoways")
fe6.5.coef <- coeftest(fe6.5, vcov=vcovHC)

screenreg(list("Pooled OLS (1)" = pols1.5.coef, "Fixed effects OLS (2)" = fe.1.5.coef, 
               "Arellano-Bond GMM (3)" = gmm1.5.coef, "Fiexed effects OLS (4)" = fe2.5.cocef,
               "Fixed effects OLS (5)" = fe3.5.coef))

screenreg(list("Pooled OLS (1)" = pols2.5.coef, "Fixed effects OLS (2)" = fe4.5.coef, 
               "Arellano-Bond GMM (3)" = gmm2.5.coef, "Fiexed effects OLS (4)" = fe5.5.coef,
               "Fixed effects OLS (5)" = fe6.5.coef))

