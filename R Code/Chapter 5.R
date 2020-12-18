#### Example 5-1: Clustered SE for Pooled Models ####
library("plm")
library("lmtest")
library("sandwich")
data("Produc", package = "plm")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: study the gross state product

# 2. Data: A panel of 48 observations from 1970 to 1986

# 3. Variables
# (1) Independent Variables
# pcap: public capital stock
# pc: private capital stock
# emp: labor input measured by the employment in non–agricultural payrolls
# unemp: state unemployment rate

# (2) Dependent Variables
# gsp: gross state product

####----------------------------------------------------------------------------------------------------

#### Clustered SE (robust SE) for POLS Model #### 
# 1. Create Formula 
fm <- log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp

# 2. Clustered SE for POLS by lm()
lmmod <- lm(fm, Produc)
coeftest(lmmod, vcov = vcovHC)        

# 3. Clustered SE for POLS model by plm()
plmmod <- plm(fm, Produc, model = "pooling")
summary(plmmod, vcov = vcovHC)

# 4. Results 
# (1) The coefficients are the same. 
# (2) SEs are different. This is b/c the classes of the model objects to be tested are different, and so are the default
#     settings of the vcovHC for lm() and plm() methods.
####----------------------------------------------------------------------------------------------------

#### Example 5-2: Clustered SE for Non-Panel Data ####
data("Hedonic", package = "plm")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description #### 
# 1. Topic: consider the median values of owner-occupied homes

# 2. Data: cross-section of 506 observations from 92 towns in Boston area 

# 3. Variables
# (1) Independent Variables
# crim: crime rate
# zn: proportion of 25,000 square feet residential lots
# indus: proportion of no–retail business acres
# chas: is the tract bounds the Charles River?
# nox:annual average nitrogen oxide concentration in parts per hundred million  
# rm: average number of rooms
# age: proportion of owner units built prior to 1940
# dis: weighted distances to five employment centers in the Boston area
# rad: index of accessibility to radial highways
# tax: full value property tax rate ($/$10,000)
# ptratio: pupil/teacher ratio
# blacks: proportion of blacks in the population
# lstat: proportion of population that is lower status

# (2) Dependent Variable
# mv: median value of owner–occupied homes

#### Comments #### 
# 1. Clustering can occur in non-panel data. 
#    Whenever a grouping index of some sort is provided and there's reason to believe that errors are 
#    dependent within groups defined by that index, the clustered SE can be employed to account for 
#    heteroscedasticity across groups and for within group correlation of any kind, not limited to proper 
#    serial correlation in time. 
# 2. if some regressors are observed at group level, e.g., when adding local GDP to individual data drawn 
#    from different geographical units, then SE have to be adjusted for intra-group correlation.
####----------------------------------------------------------------------------------------------------

#### Clustered SE and White SE for OLS Model 
# 1. Create Formula 
hfm <- mv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + blacks + lstat

# 2. OLS model with lm()
hlmmod <- lm(hfm, Hedonic)

# 3. OLS model with plm()
hplmmod <- plm(hfm, Hedonic, model = "pooling", index = "townid")       # cluster by town "townid"

# 4. Compare White SE, Clustered SE
sign.tab <- cbind(coef(hlmmod), coeftest(hlmmod, vcov = vcovHC)[,4],    # White SE 
                  coeftest(hplmmod, vcov = vcovHC)[, 4])                # Clustered SE
dimnames(sign.tab)[[2]] <- c("Coefficient", "p-values, HC", "p-val., cluster")
round(sign.tab, 3)

# 5. Results 
# chas, rm, blacks are not significant anymore after clustering by town (in step 3). 
####----------------------------------------------------------------------------------------------------
#### Example 5-3: Newey-West and Double-Clustering Estimators ####
data("Produc", package = "plm")

####----------------------------------------------------------------------------------------------------

####  Newey-West and Double-Clustering ####
# 1. Create Formula 
fm <- log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp

# 2. POLS use plm()
plmmod <- plm(fm, Produc, model = "pooling")

# 3. SCC Clustering
coeftest(plmmod, vcov=vcovSCC)

# 4. Double Clustering 
coeftest(plmmod, vcov=vcovDC)

# 5. Comments
# (1) One might want to account for both spatial correlation between states in same time period and 
# for the serial correlation within the same state and across different ones.
# (2) SCC clustering accounts for both spatial correlation in the same time period and
# serial correlation within the same state and across different states as well. 
# (3) DC clustering accounts for persistent shocks (individual, time-invariant error components)
# and for cross-sectional or spatial correlation. 
####----------------------------------------------------------------------------------------------------

# Double Clustering with 4 Periods of Unweighted Shocks
myvcovDCS <- function(x, maxlag = NULL, ...) {
  w1 <- function(j, maxlag) 1
  VsccL.1 <- vcovSCC(x, maxlag = maxlag, wj = w1, ...)
  Vcx <- vcovHC(x, cluster = "group", method = "arellano", ...)
  VnwL.1 <- vcovSCC(x, maxlag = maxlag, inner = "white", wj = w1, ...)
  return(VsccL.1 + Vcx - VnwL.1)
}
coeftest(plmmod, vcov=function(x) myvcovDCS(x, maxlag = 4))
####----------------------------------------------------------------------------------------------------

#### Example 5-4: Compute An Array of SEs #### 
library("plm")
library("lmtest")
library("sandwich")
data("Produc", package = "plm")
####----------------------------------------------------------------------------------------------------

#### Compute An Array of SEs #### 
# 1. Create Formula 
fm <- log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp
plmmod <- plm(fm, Produc, model = "pooling")

# 2. Define a Vector of Covariance Functions 
Vw <- function(x) vcovHC(x, method = "white1")     
Vcx <- function(x) vcovHC(x, cluster = "group", method = "arellano")
Vct <- function(x) vcovHC(x, cluster = "time", method = "arellano")
Vcxt <- function(x) Vcx(x) + Vct(x) - Vw(x)
Vct.L <- function(x) vcovSCC(x, wj = function(j, maxlag) 1)
Vnw.L <- function(x) vcovNW(x)
Vscc.L <- function(x) vcovSCC(x)
Vcxt.L<- function(x) Vct.L(x) + Vcx(x) - vcovNW(x, wj = function(j, maxlag) 1)

# 3. Put Covariance Estimates in A Cector
vcovs <- c(vcov, Vw, Vcx, Vct, Vcxt, Vct.L, Vnw.L, Vscc.L, Vcxt.L)
names(vcovs) <- c("OLS", "Vw", "Vcx", "Vct", "Vcxt", "Vct.L", "Vnw.L", "Vscc.L", "Vcxt.L")

# 4. Create A Table of All Covariance Estimates
cfrtab <- function(mod, vcovs, ...) {
  cfrtab <- matrix(nrow = length(coef(mod)), ncol = 1 + length(vcovs))
  dimnames(cfrtab) <- list(names(coef(mod)),
                           c("Coefficient", paste("s.e.", names(vcovs))))
  cfrtab[,1] <- coef(mod)
  for(i in 1:length(vcovs)) {
    myvcov = vcovs[[i]]
    cfrtab[ , 1 + i] <- sqrt(diag(myvcov(mod)))
  }
  return(t(round(cfrtab, 4)))
}

cfrtab(plmmod, vcovs)

# 5. Comment
# The additive nature of the 3 basic components V(WH), V(CX), V(CT) allow researcher to infer on relative 
# importance of each clustering dimension by looking at the contribution of each to SE estimate, so that
# if V(CX) < V(CT) < V(CXT), this is evidence of important cross-sectional correlation. 
####----------------------------------------------------------------------------------------------------

#### Example 5-5: RE and Robust Covariances ####
# 1. Replicate previous table now using RE as the specification
replmmod <- plm(fm, Produc)
cfrtab(replmmod, vcovs)

# 2. Results
# The relative magnitute of SE under group clustering wrt the others was hinting at error correlation in time. 
# The previous table is replicated on RE specification. 
# The cross-sectional dependence component becomes relatively more important when accounting for time persistence
# in the model through random individual (country) effects. 
####----------------------------------------------------------------------------------------------------

#### Example 5-6: Time Fixed Effect model ####
library("pcse")
library("plm")
library("lmtest")
data("agl", package = "pcse")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: estimate a model where economic performance is related to political and labor 
# organization variables. They control for trade openness of countries toward other OECD
# and for lagged growth, instrumented through an auxiliary regression.

# 2. Data: A panel with 240 observations (1970-1975) from 16 countries

# 3. Variables 
# (1) Independent Variables 
# lagg1: An instrument for lagged growth rates constructed with an auxilary regression.  
# opengdp: weighted OECD demand
# openex: weighted OECD export
# openimp: weighted OECD import
# central: labor organization index
# leftc: "Left" cabinet composition

# (2) Dependent Variables 
# growth: the OECD growth rate 
####----------------------------------------------------------------------------------------------------

#### FE Model with Time Effect #### 
# 1. Create Formula 
fm <- growth ~ lagg1 + opengdp + openex + openimp + central * leftc

# 2. FE Model with Time Effect 
aglmod <- plm(fm, agl, model = "within", effect = "time")

# 3. Panel Corrected SE
coeftest(aglmod, vcov=vcovBK)
####----------------------------------------------------------------------------------------------------

#### Example 5-7: Testing with Robust Covarince Matrices ####
library("plm")
library("lmtest")
library("sandwich")

# 1. Create Matrix with vcov as Parameter
# (1) Method 1
coeftest(plmmod, vcov = vcovHC(plmmod, type = "HC3"))

# (2) Method 2
coeftest(plmmod, vcov = function(x) vcovHC(x, type = "HC3"))

# 2. Comment
# For some test, e.g.,for multiple model comparisons by waldtest, one should always 
# provide a function.
####----------------------------------------------------------------------------------------------------

#### Example 5-8: Testing with Robust Covariance Matrices ####
library("plm")
library("lmtest")
library("sandwich")
library("car")
data("Parity", package = "plm")
head(Parity$country)
####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: PPP regression on a "long" panel of quarterly data

# 2. Data: A panel of 104 quarterly observations (1973Q1-1998Q4) from 17 countries

# 3. Variables 
# (1) Independent Variables 
# ld: log price differential vs. USD

# (2) Dependent Variables 
# ls: log spot exchange rate vs. USD

####----------------------------------------------------------------------------------------------------

#### 
# 1. Create Formula 
fm <- ls ~ ld

# 2. Two-way FE
pppmod <- plm(fm, data = Parity, effect = "twoways")

# 3. Wald Test (H0: β=1) (use linearHypothesis())
linearHypothesis(pppmod, "ld = 1", vcov = vcov)    

# 4. OLS, One-way Time FE, One-way Country FE, Two-way FE
Vw <- function(x) vcovHC(x, method = "white1")
Vcx <- function(x) vcovHC(x, cluster = "group", method = "arellano")
Vct <- function(x) vcovHC(x, cluster = "time", method = "arellano")
Vcxt <- function(x) Vcx(x) + Vct(x) - Vw(x)
Vct.L <- function(x) vcovSCC(x, wj = function(j, maxlag) 1)
Vnw.L <- function(x) vcovNW(x)
Vscc.L <- function(x) vcovSCC(x)
Vcxt.L<- function(x) Vct.L(x) + Vcx(x) - vcovNW(x, wj = function(j, maxlag) 1)

# 5. Creates Table for the 4 Models
vcovs <- c(vcov, Vw, Vcx, Vct, Vcxt, Vct.L, Vnw.L, Vscc.L, Vcxt.L)
names(vcovs) <- c("OLS", "Vw", "Vcx", "Vct", "Vcxt", "Vct.L", "Vnw.L", "Vscc.L", "Vcxt.L")
tttab <- matrix(nrow = 4, ncol = length(vcovs))
dimnames(tttab) <- list(c("Pooled OLS","Time FE","Country FE","Two-way FE"), names(vcovs))

pppmod.ols <- plm(fm, data = Parity, model = "pooling")
for(i in 1:length(vcovs)) {
  tttab[1, i] <- linearHypothesis(pppmod.ols, "ld = 1",
                                  vcov = vcovs[[i]])[2, 4]
}

pppmod.tfe <- plm(fm, data = Parity, effect = "time")
for(i in 1:length(vcovs)) {
  tttab[2, i] <- linearHypothesis(pppmod.tfe, "ld = 1",
                                  vcov = vcovs[[i]])[2, 4]
}

pppmod.cfe <- plm(fm, data = Parity, effect = "individual")
for(i in 1:length(vcovs)) {
  tttab[3, i] <- linearHypothesis(pppmod.cfe, "ld = 1",
                                  vcov = vcovs[[i]])[2, 4]
}
pppmod.2fe <- plm(fm, data = Parity, effect = "twoways")
for(i in 1:length(vcovs)) {
  tttab[4, i] <- linearHypothesis(pppmod.2fe, "ld = 1",
                                  vcov = vcovs[[i]])[2, 4]
}

print(t(round(tttab, 6)))
####----------------------------------------------------------------------------------------------------

#### Example 5-9: Regression-Based Hausman Test ####
data("Grunfeld", package = "plm")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: estimate the investment

# 2. Data: A panel of 10 observational units (firms) from 1935 to 1954

# 3. Variables 
# (1) Independent Variables 
# value: value of the firm
# capital: stock of plant and equipment

# (2) Dependent Variables 
# inv: gross Investment
####----------------------------------------------------------------------------------------------------

#### Hausman Test: Chisq and Auxiliary Versions 
# 1. Hausman Test: chisq (use phtest()) 
phtest(inv ~ value + capital, data = Grunfeld)     # default is 'chisq'

# 2. Hausman Test: auxillary 
phtest(inv ~ value + capital, data = Grunfeld, method = "aux")      # specify 'aux'

# 3. Result 
# The results from the regression-based and original Hausman test are consistent: both support RE hypothesis. 
####----------------------------------------------------------------------------------------------------

#### Example 5-10: Robust Hausman Test ####
data("RDSpillovers", package = "pder")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: study the returns of own R&D in production fuction of European firms 

# 2. Data: a cross-section of 119 industries from 1980 to 2005

# 3. Variables 
# (1) Independent Variables 
# lnl: log of labour input
# lnk: log of physical capital stock
# lnrd: log of RD capital stock

# (2) Dependent Variables 
# lny: log output

####----------------------------------------------------------------------------------------------------

#### 
# 1. Create Panel Data  
pehs <- pdata.frame(RDSpillovers, index = c("id", "year"))

# 2. Create Formula 
ehsfm <- lny ~ lnl + lnk + lnrd

# 3. Hausman Test: Homo
phtest(ehsfm, pehs, method = "aux")

# 4. Hausman Test: Robust Covariance
phtest(ehsfm, pehs, method = "aux", vcov = vcovHC) 

# 5. Robust version of Hausman test doesn't reject RE hypothesis. 
####----------------------------------------------------------------------------------------------------

#### Example 5-11:Generalized GLS Estimator #### 
library("plm")
data("EmplUK", package = "plm")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: study the employment in UK 

# 2. Data: An unbalanced panel of 140 observations (1976-1984)

# 3. Variables 
# (1) Independent Variables 
# wage: wages 
# capital: capital

# (2) Dependent Variable 
# emp: employment

#4. Comment 
# This data is has relatively big random sample of firms observed over a limited number of years.
####----------------------------------------------------------------------------------------------------

#### Generalized GLS Model
# 1. Generalized GLS Model (use pggls()) 
gglsmod <- pggls(log(emp) ~ log(wage) + log(capital), data = EmplUK, model = "pooling")
summary(gglsmod)

# 2. Table of Correlations Between Residuals
round(gglsmod$sigma, 3)   

# 3. Comments 
# (1) This framework allows error covariance structure inside every group of observations to 
#     be fully unrestricted and is robust against any type of intra-group heteroscedasticity 
#     and serial correlation. 
# (2) The pggls() is similar to plm(). The difference is that the estimate of the group 
#     covariance matrix of errors (sigma, a matrix) is reported in model objects instead of usual
#     estimated variances of the two error components. 
# (3) correlation between pairs of residuals (in time) for same individual don't die out 
#     with distance in time. The estimated error covariance very much resembles RE stucture, 
#     with strong prevalence of individual variance component ση^2 over σν^2.  
####----------------------------------------------------------------------------------------------------

#### Example 5-12: FE GLS estimator #### 
# 1. FE Generalized GLS
feglsmod <- pggls(log(emp) ~ log(wage) + log(capital), data = EmplUK, model = "within")
summary(feglsmod)

# 2. Hausman test
phtest(feglsmod, gglsmod)

# 3. Result 
# FE is preferred. 
####----------------------------------------------------------------------------------------------------

#### Example 5-13: FD GLS Estimator #### 
# 1. FD Generalized GLS
fdglsmod <- pggls(log(emp) ~ log(wage) + log(capital), data = EmplUK, model = "fd")
summary(fdglsmod)
####----------------------------------------------------------------------------------------------------

#### Example 5-14: Generalized GLS Estimator ####
library("plm")
library("lmtest")
data("RiceFarms", package = "splm")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: study rice production in Indonesia

# 2. Data: a panel of 171 observations

# 3. Variables 
# (1) Independent Variables
# seed: seed in kilogram
# urea: urea in kilogram
# phosphate: phosphate in kilogram
# totlabor: total labor (excluding harvest labor)
# size: the total area cultivated with rice, measured in hectares
# pesticide: pesticide cost in Rupiah
# varieties: one of 'trad' (traditional varieties), 'high' (high yielding varieties) and 'mixed' (mixed varieties)
# region: one of 'wargabinangun', 'langan', 'gunungwangi', 'malausma', 'sukaambit', 'ciwangi'
# time: marked from 1-6 and repeated

# (2) Dependent Variables
# goutput: gross output of rice in kg
####----------------------------------------------------------------------------------------------------

#### Generalized GLS estimator #### 
# 1. Transform Some Variables 
RiceFarms <- transform(RiceFarms, 
                       phosphate = phosphate / 1000,
                       pesticide = as.numeric(pesticide > 0))

# 2. Create Formula 
fm <- log(goutput) ~ log(seed) + log(urea) + phosphate + log(totlabor) + log(size) + pesticide + varieties + region + time

# 3. Generalized GLS model (use pggls()) 
gglsmodrice <- pggls(fm, RiceFarms, model = "pooling", index = "id")    # clustered by id (equivalent to time) 
summary(gglsmodrice)

# 4. Joint restriction test
library("lmtest")
waldtest(gglsmodrice, "region")

# 5. FE Generalized GLS model
feglsmodrice <- pggls(update(fm, . ~ . - region), RiceFarms, index = "id")

# 6. Hausman test
phtest(gglsmodrice, feglsmodrice)

# 7. Hausman Test: Omit Regional FE
phtest(pggls(update(fm, . ~ . - region), RiceFarms, 
             model = "pooling", index = "id"), feglsmodrice)

# 8. Results
# (1) (step 2:  added region+time) The random sampling assumption seems to be reasonable within regions, but one might 
#     suspect observations from same region to share some common characteristics: include 
#     5 regional FE and 5 time FE. 
# (2) from Step 3: regions don't seem to be so important.
# (3) from step 5: the results don't seem to change much when adding individual FE. 
# (4) from step 6: Hausman test doesn't reject. 
# (5) from step 7: even omitting regional FE, pggls() still passes Hausman test. The 171
#     rice farms can be actually seen as random draws from same population w/out need for 
#     either individual or regional FE. 
####----------------------------------------------------------------------------------------------------

#### Example 5-15: Generalized GLS estimator ####
library("plm")
library("lmtest")
data("RDSpillovers", package = "pder")

####----------------------------------------------------------------------------------------------------

#### Generalized GLS estimator #### 
# 1. Create Formula 
fm <- lny ~ lnl + lnk + lnrd

# 2. Generalized GLS model
gglsmodehs <- pggls(fm, RDSpillovers, model = "pooling")
coeftest(gglsmodehs)

# 3. FE generalized GLS model
feglsmodehs <- pggls(fm, RDSpillovers, model = "within")
coeftest(feglsmodehs)

# 4. Hausman test
phtest(gglsmodehs, feglsmodehs)

# 5. FD Generalized GLS
fdglsmodehs <- pggls(fm, RDSpillovers, model = "fd")

# 6. Residuals from FE GLS 
fee <- resid(feglsmodehs)     

# 7. Create Data Frame with FE GLS Residuals 
dbfee <- data.frame(fee=fee, id=attr(fee, "index")[[1]])

# 8. Estimate Pooled Autoregressive Model
coeftest(plm(fee~lag(fee)+lag(fee,2), dbfee, model = "p", index="id"))

# 9. Residuals from FD GLS 
fde <- resid(fdglsmodehs)     

# 10. Create Data From with FD GLS Residuals
dbfde <- data.frame(fde=fde, id=attr(fde, "index")[[1]])     

# 11. Estimate Pooled Autoregressive Model
coeftest(plm(fde~lag(fde)+lag(fde,2), dbfde, model = "p", index="id"))

# 12. FD GLS Model Test 
coeftest(fdglsmodehs)
####----------------------------------------------------------------------------------------------------