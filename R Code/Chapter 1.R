#### Example 1-1: Individual Heterogeneity ####
library("plm")                      # "plm" is for pooled linear model 
library("lmtest")
library("car")
data("Fatalities", package="AER")

####----------------------------------------------------------------------------------------------------
#### Data & Variable Description
# 1. Topic: whether taxing alcoholics can reduce the road's death toll

# 2. Data: include 336 total observations (1982-1988) from 48 states (excluding Alaska & Hawaii)

# 3. Variables 
# (1) Inependent Variable
# beertax: tax on case of beer.
# state: factor indicating state.

# (2) Dependent Variables 
# frate: fatality rate, that is, the number of traffic accidents per 10,000 people living in a state in a specific year)  

# (3) Other Variables 
# fatal: Number of vehicle fatalities (used for transforming "frate"). 
# pop: population (used for transforming "frate"). 
# year: factor indicating year (used for subset). 

#### Concern
# This is a good example of the importance of individual heterogeneity and time effects in a panel setting.
# We suspect the presence of unobserved heterogeneity
####----------------------------------------------------------------------------------------------------

#### Estimate "frate"  for 1982
# 1. Create Variable "frate" 
Fatalities$frate <- with(Fatalities, fatal / pop * 10000)

# 2. Create Formula 
fm <- frate ~ beertax     

# 3. Linear Model  for 1982
model82 <- lm(fm, Fatalities, subset = year == 1982)    
summary(model82)   # report coefficients, R2, residual, etc.
coeftest(model82)     # report coefficients and their p-values

# 4. Results
# Coefficient on beertax (only) in 1982 is positive but insignificant. (taxing beer seems to increase fatality)
####----------------------------------------------------------------------------------------------------

#### Estimate "frate"  for 1988
# 1. Linear Model  for 1988
model88 <- update(model82, subset = year == 1988)
summary(model82)   
coeftest(model88)     

# 2. Results 
# Coefficient on beertax in 1988 becomes significant and positive. (taxing beer seems to increase fatality)
# It turns out between 1982-1988, only coefficient on beertax in 1982 is insignificant. The rest of coefficients are significant.
####----------------------------------------------------------------------------------------------------

#### POLS Model
# 1. POLS Model
pols <- plm(fm, Fatalities, model="pooling")
summary(pols) 
coeftest(pols)

# 2. Results 
# Coefficients are positive and signficant (taxing beer seems to increase fatality). 
# This may be due to not considering individual effect (here, it is state effect). 
# We suspect the presence of unobserved heterogeneity.
####----------------------------------------------------------------------------------------------------

#### Difference Model 
# 1. Five-Year Difference Model      (ΔYn,t = Yn,t - Yn,t-5) 
diff.model <- plm(diff(frate, 5) ~ diff(beertax, 5), Fatalities, model="pooling")
coeftest(diff.model)   # report coefficients and their p-values
coef(diff.model)        # report only coefficients

# 2. Results 
# The coefficient on beertax is now negative. 
# After controlling for state heterogeneity (state effect), higher taxation is associated with lower fatalities.
####----------------------------------------------------------------------------------------------------

#### LSDV Model (Least Squares Dummy Variable) - Equivalent to FE Model
# 1. LSDV Model 
lsdv.fm <- update(fm, . ~ . + state - 1)      # Update formula "fm" to "frate ~ beertax + state". Here "-1" means no intercept
lsdv.model <- lm(lsdv.fm, Fatalities)
coef(lsdv.model)[1]

# 2. Results 
# The coefficient on beertax is negative. 
# After controlling for state heterogeneity (time-invariant unobservables), higher taxation is associated with lower fatalities.
####----------------------------------------------------------------------------------------------------

#### FE Model 
# 1. FE Model 
fe.model <- plm(fm, Fatalities)
coeftest(fe.model)

# 2. Results 
# The coefficient on beertax is negative. 
# After controlling for state heterogeneity (time-invariant unobservables), higher taxation is associated with lower fatalities.
# FE requires minimal assumptions on the nature of heterogeneity. It is one of the simplest and most robust specifications. 
####----------------------------------------------------------------------------------------------------

#### Example 1-2: No Heterogeneity ####
library("pder")
library("plm")
data("Tileries", package = "pder")

####----------------------------------------------------------------------------------------------------
#### Data & Variable Description
# 1. Topic: estimate the tilery production in Egypt

# 2. Data: include weekly (total 22 weeks) observations of 25 firms (483 total observations) in Egypt

# 3. Variables 
# (1) Independent Variables 
# labor: labor hours 
# machine: machine hours 

# (2) Dependent Variable
# output: output

# (3) Other Variables 
# fayoum: one of the areas called "fayoum"
####----------------------------------------------------------------------------------------------------

#### FE Model
# 1. FE Model  for area fayoum
egypt.fe <- plm(log(output) ~ log(labor) + machine, data=Tileries, subset=area=="fayoum")
coef(summary(egypt.fe))
####----------------------------------------------------------------------------------------------------

#### POLS Model
# 1. POLS Model
egypt.pols <- plm(log(output) ~ log(labor) + machine, data = Tileries, model = "pooling", subset = area == "fayoum")
coef(summary(egypt.pols))

# 2. Results
# Effects of log(labor) and machine on output are similar from POLS Model and FE Model. 
# Reason: the individual units are rather homogenous (log(labor) values are similar), and technology is standard (machine values are similar)
# Hence here unobserved heterogeneity isn't an issue. 
####----------------------------------------------------------------------------------------------------

#### Example 1-3: Linear Regressions ####

####----------------------------------------------------------------------------------------------------
#### Data & Variable Description
# 1. Topic: whether taxing alcoholics can reduce the road's death toll

# 2. Data: include 336 total observations (1982-1988) from 48 states (excluding Alaska & Hawaii)

# 3. Variables 
# (1) Inependent Variable
# beertax: tax on case of beer.
# state: factor indicating state.

# (2) Dependent Variables 
# frate: fatality rate, that is, the number of traffic accidents per 10,000 people living in a state in a specific year)  
####----------------------------------------------------------------------------------------------------

#### OLS Model (by hand)
# 1. Create Vectors "X", "Y"
y <- Fatalities$frate
X <- cbind(1, Fatalities$beertax)

# 2. Solve for β_hat 
beta.hat <- solve(crossprod(X), crossprod(X,y))  # solve for β_hat in (X'X)β=X'y
beta.hat

# 3. Compare with OLS Model
mod <- lm(frate ~ beertax, Fatalities)
coef(mod)

# 4. Results 
# OLS model (by hand) and OLS model have the same results.
####----------------------------------------------------------------------------------------------------

#### LSDV Model
LSDVmod <- lm(frate ~ beertax + state - 1, Fatalities)
coef(LSDVmod)["beertax"]
####----------------------------------------------------------------------------------------------------

#### OLS Model (for the time-demeaned "frate" on the time-demeaned "beertax") 
# 1. Attach data
attach(Fatalities)    # attach() brings the data to the user level, so that not need to type Fatalities$ to extract data all the time.

# 2. Create time-demeaned "frate.tilde" and "beertax.tilde"
frate.tilde <- frate - rep(tapply(frate, state, mean),
                           each = length(unique(year)))
beertax.tilde <- beertax - rep(tapply(beertax, state, mean),
                               each = length(unique(year)))

# 3. OLS Model (time-demeaned)
lm(frate.tilde ~ beertax.tilde - 1)
summary(plm(fm, Fatalities))

# 4. Detach data
detach(Fatalities)

# 5. Results
# This gives results similar to FE model.
####----------------------------------------------------------------------------------------------------

#### Example 1-4: Explicit Within Transformation ####
within.model <- plm(Within(frate) ~ Within(beertax) - 1, data=Fatalities, model = "pooling")     # use within() function.
coef(within.model)
####----------------------------------------------------------------------------------------------------

#### Example 1-5: Wald Test with user-supplied Covariance ####

####----------------------------------------------------------------------------------------------------
#### Data & Variable Description
# 1. Topic: estimate the tilery production in Egypt

# 2. Data: include weekly (total 22 weeks) observations of 25 firms (483 total observations) in Egypt

# 3. Variables 
# (1) Independent Variables 
# labor: labor hours 
# machine: machine hours 

# (2) Dependent Variable
# output: output

# (3) Other Variables 
# fayoum: one of the areas called "fayoum"
####----------------------------------------------------------------------------------------------------

### POLS Model
# 1. Formula 
til.fm <- log(output) ~ log(labor) + log(machine)

# 2. POLS Model (use lm() function)
lm.mod <- lm(til.fm, data = Tileries, subset = area == "fayoum")

# 3. Wald Test (homoskedasticity)
# Assume Cobb-Douglas, test H0: CRS or H0: γ1+γ2=1. 
lht(lm.mod, "log(labor) + log(machine) = 1")       # lht() is the linear hypothesis test.

# 4. Wald Test (allow heteroskedasticity)
lht(lm.mod, "log(labor) + log(machine) = 1", vcov=vcovHC)     # vcovHC means allow heteroskedasticity.

# 5. Compare both Wald Tests 
# the qualitative findings are not changed. 
# but a different covariance estimator has been employed. 
####----------------------------------------------------------------------------------------------------

#### Example 1-6: User-supplied covariance ####
# 1. POLS Model (use plm(function))
plm.mod <- plm(til.fm, data = Tileries, subset = area == "fayoum")

# 2. Wald Test (allow heteroskedasticity)
lht(plm.mod, "log(labor) + log(machine) = 1", vcov = vcovHC)