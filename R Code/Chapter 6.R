#### Example 6-1: Within 2SLS Estimator ####
library("plm")
data("SeatBelt", package = "pder")

####----------------------------------------------------------------------------------------------------
#### Data & Variable Description
# 1. Topic: the influence of using seat belts on #  of deaths on roads

# 2. Data: include yearly 765 total observations (1983-1997) from 51 states (including DC)

# 3. Variables 
# (1) Independent Variables: 
#   i)Endogenous
#     usage (rate of seat belt usage)  

#   ii)Exogenous
# percapin (median income in current US dollars)
# unemp (unemployment rate)
# meanage (mean age)
# precentb (% of African-Americans in the state population)
# densrur (traffic density rural: registered vehicles per unit length of urban roads in miles)
# precenth (% of people of Hispanic origin in the state population)
# densurb (traffic density urban: registered vehicles per unit length of urban roads in miles)
# viopcap (number of violent crimes per capita)
# proppcap (number of property crimes per capita)
# vmtrural (vehicle miles traveled on rural roads)
# vmturban (vehicle miles traveled on urban roads)
# fueltax (fuel tax)
# lim65 (dummy: lim65=1 if 65 miles per hour speed limit)
# lim70p (dummy: lim70p=1 if 70 miles per hour or above speed limit)
# mlda21 (dummy: mlda21=1 for a min legal drinking age of 21 years)
# bac08 (dummy: bac08=1 for a max of 0.08 blood alcohol content) 

# (2) Dependent Variables 
# occfat (# of vehicle occupants  killed on road per mile)
# noccfat (# of non vehicle occupants killed on road per mile)

# (3) IV's 
# ds (dummy: ds=1 for the periods in which the state had a secondary-enforcement mandatory seat belt law, 
# or a primary-enforcement law that preceded by a secondary-enforcement law)
# dp (dummy: dp=1 for the periods in which the state had a primary-enforcement mandatory seat belt law 
# that was not preceded by a secondary-enforcement law)
# dsp (dummy: dsp=1 for the periods in which the state had a primary-enforcement mandatory seat belt law 
# that was preceded by a secondary enforcement law)

# (4) Other Variables (used in transforming dependent variables)
# farsocc (# of traffic fatalities of vehicle occupants)   
# farsnocc (# of traffic fatalities of non vehicle occupants (pedestrians & bicyclists))

# Note: 
# vehicle occupants are the # of occupants in a motor vehicle including the driver.

#### Two Concerns
# 1. behavior compensation theory: using seat belt makes the driver more confident and leads him to adopt a less prudent driving behavior
# 2. endogeneity problem: if driving conditions get worse, both seat belt use and mortality increase
#    leads to IV model 
####----------------------------------------------------------------------------------------------------

#### Estimate "occfat" (Vehicle Occupants Killed)  
# 1. Create Variable "occfat" 
SeatBelt$occfat <- with(SeatBelt, log(farsocc / (vmtrural + vmturban)))   # Use with() to transform part of data and create new variable "occfat"

# 2. Set Up Model 
# Formula 1: reg y on x1, x2, x3; after "|", reg y on x1, x3, z (the instrument)
y ~ x1 + x2 + x3 | x1 + x3 + z  
# Formula 2: reg y on x1, x2, x3; after " | "  use "." operator to construct 2nd part of formula by updating the 1st part  
# "- x2" to exclude the endogenous variable x2, and "+ z"  to include the instrument z 
y ~ x1 + x2 + x3 | . - x2 + z  
# They're same formulas: When there're too many regressors, better to use the 2nd formula so that you don't have to list all regressors again.

# 3. OLS Model
ols <- plm(occfat ~ log(usage) + log(percapin) + log(unemp) + log(meanage) + log(precentb) + log(precenth)+ log(densrur) + log(densurb) 
           + log(viopcap) + log(proppcap) + log(vmtrural) + log(vmturban) + log(fueltax) + lim65 + lim70p + mlda21 + bac08, SeatBelt,
           effect = "time")
# all variables (expect for dummies) are in logs to avoid heteroscedasticity problems. We can do so because all obervations are positive values. 

# 4. FE Model
fe <- update(ols, effect="twoways")

# 5. IV FE Model
ivfe <- update(fe, . ~ . | . - log(usage) + ds + dp +dsp)       # where ".~." means use the same formula as in fe

# 6. OLS IV Model 
olsiv <- update(ols, . ~ . | . - log(usage) + ds + dp +dsp)

# 7. Compare The Four Models
rbind(ols = coef(summary(ols))[1,], fe = coef(summary(fe))[1, ], ivfe = coef(summary(ivfe))[1, ], olsiv = coef(summary(olsiv))[1,])

# 8. Results
# OLS: coefficient on log(usage) is positive, which doesn't make sense.
#     this may be due to the endogeneity issue and behavior compensation theory. 

# FE: coefficient on log(usage) is negative. But the true effect may be larger. 
#     although the correlation between individual effect and covariate is taken into account
#     the problem of correlation between idiosyncratic error and covariate remains. 

# FE IV: after using IV, the coefficient on log(usage) becomes larger.

# OLS IV: coefficients on log(usage) is positive, which doesn't make sense either. 
# Even control for endogeneity, there might be some other issues we didn't account for. For example, the correlation between idiosyncratic error and covariates. 
####----------------------------------------------------------------------------------------------------

#### Estimate Non Vehicle Occupants Killed 
# 1. Create Variable "noccfat" 
SeatBelt$noccfat <- with(SeatBelt, log(farsnocc / (vmtrural + vmturban)))     

# 2. IV FE Model 
nivfe <- update(ivfe, noccfat ~ . | .)
# "noccfat ~ " means keep all others except for changing the dependent variable to "noccfat"
coef(summary(nivfe))[1,]

# 3. Result
# This tests the behavior compensation theory
# The coefficient on log(usage) is negative, which proved the theory.
####----------------------------------------------------------------------------------------------------

#### Why include more IV's endogenous variable? (overidentification)? 
# 1. If only include one IV
ivfe2 <- update(fe, . ~ . | . - log(usage) + ds)  

ivfe3 <- update(fe, . ~ . | . - log(usage) + dp)

ivfe4 <- update(fe, . ~ . | . - log(usage) + dsp)

rbind(ivfe1 = coef(summary(ivfe))[1, ],
      ivfe2 = coef(summary(ivfe2))[1, ],
      ivfe3 = coef(summary(ivfe3))[1, ],
      ivfe4 = coef(summary(ivfe4))[1, ])

# 2. Result 
# Coefficient on log(usage) is large (in absolute value) with ivfe model, and only this coefficient is significant (smallest p-value)
# Overidentification in this case provides more information about the fit of the model? 
####----------------------------------------------------------------------------------------------------

#### What if not take logs for all variables? 
# 1. Histograms
par(mfrow=c(3,5))
hist(SeatBelt$farsocc, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$farsnocc, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$usage, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$percapin, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$unemp, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$meanage, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$precentb, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$precenth, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$densrur, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$densurb, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$viopcap, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$proppcap, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$vmtrural, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$vmturban, freq=F, col="steelblue", prob=T) 
hist(SeatBelt$fueltax, freq=F, col="steelblue", prob=T) 

# 2. An Alternative Universe 
SeatBelt$occfat2 <- with(SeatBelt, (farsocc)/(vmtrural + vmturban))
ols2 <- plm(occfat2 ~ usage + percapin + unemp + meanage +
               precentb + precenth+ densrur +
               densurb + viopcap + proppcap +
               vmtrural + vmturban + fueltax +
               lim65 + lim70p + mlda21 + bac08, SeatBelt,
            effect = "time")

fe2 <- update(ols2, effect="twoways")

ivfe5 <- update(fe2, . ~ . | . - usage + ds + dp +dsp)   

olsiv2 <- update(ols2, . ~ . | . - usage + ds + dp +dsp)

# 3. Look at the Four Models 
rbind(ols2 = coef(ols2), fe2 = coef(fe2), ivfe5 = coef(ivfe5), olsiv2 = coef(olsiv2))

# 4. Result: 
# all coefficients on log(usage) are insignificantly small numbers. 
# because there's heteroscedasticity issue. The observations are very skewed based on the histograms.
#  Using logs also makes it easier to interprete the result: for example in the IV FE model, by increasing log(usage) 1%, the fatality rate on road decreases by 13.34%. 
####----------------------------------------------------------------------------------------------------

#### What if not use time effect in ols? 
# 1. OLS without time effect
ols3 <- plm(occfat ~ log(usage) + log(percapin) + log(unemp) + log(meanage) +
               log(precentb) + log(precenth)+ log(densrur) +
               log(densurb) + log(viopcap) + log(proppcap) +
               log(vmtrural) + log(vmturban) + log(fueltax) +
               lim65 + lim70p + mlda21 + bac08, SeatBelt)
coef(summary(ols3))[1,]

# 2. Result
# The coefficient on log(usage) becomes negative. Why? 
####----------------------------------------------------------------------------------------------------

#### How do we know endogeneity exists? 
# 1. Hausman Test
phtest(ols, ivfe)

# 2. Result
# Endogeneity exists. IV FE model is better.
####----------------------------------------------------------------------------------------------------

#### Example 6-2: EC2SLS Estimator (Error Component 2SLS) ####
library("plm")
data("ForeignTrade", package = "pder")

####----------------------------------------------------------------------------------------------------
#### Data & Variable Description
# 1. Topic: determinants of international trade for developing countries and measure of price and income elasticities

# 2. Data: include yearly 744 total observations (1963-1986) from 31 countries

# 3. Variables
# (1) Independent Variables
#   i)Exogenous
# pmcpi (log of imports price divided by domestic cpi)
# gnp (real GNP per capita)
# lag(imports) (nominal imports deflated by the unit value of exports per capita)
# lag(resimp) (official foreign reserves (in $) divided by nominal imports) 

# (2) IV's
# lag(consump) (domestic consumption per capita)
# lag(cpi) (domestic CPI, base year is 1980 = 100)
# lag(income) (domestic disposable income per capita)
# lag(px) (unit value of exports) 
# lag(reserves) (official foreign reserves)
# lag(exports) (nominal exports deflated by the unit value of exports per capita)
# lag(gnp)
# pm (unit value of imports) 
# lag(invest) (domestic fixed gross investment per capita)
# lag(money) (domestic money supply per capita)
# gnpw (real genp for USA per capita)
# pw (US producer's price index)
# trend (trend dummy, 1964=1)
# pgnp (trend real GNP per capita calculated by fitting linear trend yit*=y0iexp(gi t))

# (3) Dependent Variable 
# imports 

# Note
# all variables are per capital and in logs to avoid heteroscedasticity problems.
# one-period lag of the response in included in covariate in every equation. 
# All variables are already being log transoformed, that's why you don't see log in the formula. 
####----------------------------------------------------------------------------------------------------

#### Estimate The Import Demand: EC2SLS Model
# 1. FE Model   
w1 <- plm(imports ~ pmcpi + gnp + lag(imports) + lag(resimp)        # before the "|", the 1st argument indicates exogenous variables;
          | lag(consump) + lag(cpi) + lag(income) + lag(gnp) + pm +    # after the "|", the 2nd argument indicates IVs. 
             lag(invest) + lag(money) + gnpw + pw + lag(reserves) + 
             lag(exports) + trend + pgnp + lag(px), ForeignTrade, model = "within")

# 2. EC2SLS Model (Error Component 2SLS)
r1 <- update(w1, model = "random", random.method = "nerlove", random.dfcor = c(1, 1), inst.method = "baltagi")

# 3. Hausman test
# H0: no correlation between IV and individual effects (FE more efficient)
phtest(r1, w1)

# 4. Result 
# FE model is more efficient because no correlation between IV and individual effects
####----------------------------------------------------------------------------------------------------

#### Estimate The Import Demand: KL Model (Kinal and Lahiri)
# 1. Two types of IV Used in KL Model: 
# (1) doubly IV: those uncorrelated with individual effects and can be used twice using within and between transformations
# (2) simply iV: those correlated with the individual effects and can only be used in within transformations

# 2. KL Model 
r1b <- plm(imports ~ pmcpi + gnp + lag(imports) + lag(resimp) 
           | lag(consump) + lag(cpi) + lag(income) + lag(px) +      # after the "|", the 2nd argument indicates the doubly IV, the 3rd are the simply IV
              lag(reserves) + lag(exports) | lag(gnp) + pm +
              lag(invest) + lag(money) + gnpw + pw + trend + pgnp,
           ForeignTrade, model = "random", inst.method = "baltagi",
           random.method = "nerlove", random.dfcor = c(1, 1))

# 3. Hausman Test
# H0: no correlation between IV and individual effects (FE more efficient)
phtest(w1, r1b)

# 4. Result 
# KL model (GLS estimator) is more efficient. 

# 5. Compare FE and KL Models 
rbind(within = coef(w1), ec2sls = coef(r1b)[-1])     # here -1 means exclude first coefficient

# 6. Result
# KL model (GLS estimator) and FE model give similar results.
####----------------------------------------------------------------------------------------------------

#### Measures of Elasticity from FE, EC2SLS, KL Models
# ST elasticity of imports demand is given by prie (pmcpi) coefficient
# LT elasticity is obtained by dividing ST elasticity by (1 - coefficient of lagged imports)
# 1. Find elasticities 
elast <- sapply(list(w1, r1, r1b), function(x) c(coef(x)["pmcpi"], coef(x)["pmcpi"] / (1 - coef(x)["lag(imports)"])))
dimnames(elast) <- list(c("ST", "LT"), c("w1", "r1", "r1b"))
elast

# 2. Compare SD from FE and EC2SLS Models 
rbind(within = coef(summary(w1))[, 2], ec2sls = coef(summary(r1b))[-1, 2])

# 3. Result 
# GLS estimator coefficients have much lower SD becasue it exploits part of inter-individual variation. 
####----------------------------------------------------------------------------------------------------

#### Example 6-3: Hausman-Taylor Estimator ####
library("plm")
data("TradeEU", package = "pder")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: analyze international trade in Europe (gravity model)

# 2. Data: include yearly 3822 total observations (1960-2001) in Europe

# 3. Variables
# (1) Independent Variables
# i) Exogenous 
# dist: the geographical distance between capital cities
# sim: a measure of similarity between two trading countries 
# cee: a dummy equal to 1 when both belong to European Community
# emu: a dummy equal to 1 when both adopt the common currency
# bor: a dummy equal to 1 when the trading partners share a border

#  ii) Endogenous 
# lan: a dummy equal to 1 when both speak the same language

# (2) Dependent Variables
# trade: the sum of logged exports and imports, bilateral trade flow

# (3) IVs 
# rer: the logged bilateral real exchange rate
# gdp: the sum of the logged real GDPs
# rlf: a measure of relative factor endowments
####----------------------------------------------------------------------------------------------------

#### Example 6-4: ####
# 1. POLS Model
ols <- plm(trade ~ gdp + dist + rer + rlf + sim + cee + emu + bor + lan, TradeEU,
           model = "pooling", index = c("pair", "year"))

# 2. FE Model 
fe <- update(ols, model="within")

# 3. RE Model 
re <- update(fe, model="random")

# 4. Hausman test
phtest(re, fe)

# 5. Hausman-Taylor Estimator (IV: "rer")
ht1 <- plm(trade ~ gdp + dist + rer + rlf + sim + cee + emu + bor + lan |
              rer + dist + bor | gdp + rlf + sim + cee + emu + lan,
           data = TradeEU, model = "random", index = c("pair", "year"),
           inst.method = "baltagi", random.method = "ht")

# 6. Hausman-Taylor Estimator (IV: rer, dgp, rlf) 
ht2 <- update(ht1, trade ~ gdp + dist + rer + rlf + sim + cee + emu + bor + lan |
                 rer + gdp + rlf + dist + bor | sim + cee + emu + lan)

# 7. Hausman Test 
phtest(ht1, fe)
phtest(ht2, fe)

# 8. Hausman-Taylor Estimator: am Method
ht2am <- update(ht2, inst.method = "am")
phtest(ht2am, fe)

# 9. Create Table Compare 6 Models 
library("texreg")
texreg(list(ols, fe, re, ht1, ht2, ht2am),
       custom.model.names = c("OLS", "FE", "RE", "HT1", "HT2", "AM2"),
       caption = "Estimations of the gravity model.", label = "table:gravity", 
       custom.gof.names = c("R$ ̂ 2$", "Adj. R$ ̂ 2$", "Num. obs.", "s\\_idios",  
                            "s\\_id"),
       scriptsize = FALSE)
####----------------------------------------------------------------------------------------------------

#### Example 6-4:Error Components 3SLS ####
library("plm")
data("ForeignTrade", package = "pder")

####----------------------------------------------------------------------------------------------------

#### Error Components 3SLS #### 
# 1. KL Model
r1b <- plm(imports ~ pmcpi + gnp + lag(imports) + lag(resimp) |
              lag(consump) + lag(cpi) + lag(income) + lag(px) +
              lag(reserves) + lag(exports) | lag(gnp) + pm +
              lag(invest) + lag(money) + gnpw + pw + trend + pgnp,
           ForeignTrade, model = "random", inst.method = "baltagi",
           random.method = "nerlove", random.dfcor = c(1, 1))

# 2. Formula for Import 
eqimp <- imports ~ pmcpi + gnp + lag(imports) + lag(resimp) | 
   lag(consump) + lag(cpi) + lag(income) + lag(px) + lag(reserves) + lag(exports) | 
   lag(gnp) + pm + lag(invest) + lag(money) + gnpw + pw + trend + pgnp

# 3. Formula for Export 
eqexp <- exports ~ pxpw + gnpw + lag(exports) |
   lag(gnp) + pw + lag(consump) + pm + lag(px) + lag(cpi) |
   lag(money) + gnpw + pgnp + pop + lag(invest) + lag(income) + lag(reserves) + exrate

# 4. Error Components 3SLS model
r12 <- plm(list(import.demand = eqimp,
                export.demand = eqexp),
           data = ForeignTrade, index = 31, model = "random",
           inst.method = "baltagi", random.method = "nerlove", random.dfcor = c(1, 1))
summary(r12)

# 5. Compares the EC2SLS estimate from example 6-2 with the EC3SLS estimate
rbind(ec2sls = coef(summary(r1b))[-1, 2],
      ec3sls = coef(summary(r12), "import.demand")[-1, 2])
####----------------------------------------------------------------------------------------------------