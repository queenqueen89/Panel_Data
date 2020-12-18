#### Example 4-1: F Test and LM (Lagrange Multiplier) Test ####
library("plm")
library("splm")
data("RiceFarms", package="splm")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: test H0: no individual effects

# 2. Data: include a panel of 171 observations

# 3. Variables 
# (1) Independent Variables
# seed: seed in kilogram
# totlabor: total labor (excluding harvest labor)
# size: the total area cultivated with rice, measured in hectares

# (2) Dependent Variables
# goutput: gross output of rice in kg
####----------------------------------------------------------------------------------------------------

#### F Test (default one-way: H0: no individual effects) 
# 1. Create Panel Data 
Rice <- pdata.frame(RiceFarms, index = "id")

# 2. FE Model 
rice.w <- plm(log(goutput) ~ log(seed) + log(totlabor) + log(size), Rice)

# 3. POLS Model
rice.p <- update(rice.w, model = "pooling")

# 4. Two-ways FE Model
rice.wd <- plm(log(goutput) ~ log(seed) + log(totlabor) + log(size), Rice, effect = "twoways")

# 5. F test 
# (1) Method 1
pFtest(rice.w, rice.p)      # since pols not include individual effect, fe does.

# (2) Method 2
pFtest(log(goutput) ~ log(seed) + log(totlabor) + log(size), Rice)

# 6. Results: 
# Reject H0, there are individual effects 
####----------------------------------------------------------------------------------------------------

#### F Test (two-way: H0: no individual and time effects) 
# 1. Method 1
pFtest(rice.w, rice.p)      # since pols doesn't include individual and time effects, wd does.

# 2. Method 2
pFtest(log(goutput) ~ log(seed) + log(totlabor) + log(size), Rice,effect = "twoways")

# 3. Results 
# Reject H0, there are individual and time effects 
####----------------------------------------------------------------------------------------------------

#### F test (H0: no time effects)  
# 1. F test
pFtest(rice.wd, rice.w)   # since fe doesn't include time effects, wd does.

# 2. Result 
# Reject H0, there are time effects
####----------------------------------------------------------------------------------------------------

#### LM Test (default: H0: no individual effects)
# 1. LM Test (use (plmtest())
# (1) Method 1 
plmtest(rice.p)      # since pols doesn't include individual effects 

# (2) Method 2 
plmtest(log(goutput)~log(seed)+log(totlabor)+log(size), Rice)

# 2. Results 
# Reject H0, there're individual effects 
####----------------------------------------------------------------------------------------------------

#### LM Test (H0: no time effects)
# 1. LM Test 
plmtest(rice.p, effect = "time")

# 2. Result: 
# Reject H0, there're time effects 
####----------------------------------------------------------------------------------------------------

#### LM Test (H0: no individual and time effects)
# 1. Method 1: LM Test 
plmtest(rice.p, effect = "twoways")

# 2. Method 2: 
plmtest(rice.p, effect = "twoways", type = "kw")     # type="kw" is developed by King and Wu 

# 3. Method 3: 
plmtest(rice.p, effect = "twoways", type = "ghm")    # type ="ghm" is developed by Gourieroux et al.

# 4. Result
# Reject H0, there're individual and time effects 
####----------------------------------------------------------------------------------------------------

#### Example 4-2: Hausman Test ####
data("RiceFarms", package = "splm")
####----------------------------------------------------------------------------------------------------

#### Test whether use RE or FE Model
# 1. Create panel data 
Rice <- pdata.frame(RiceFarms, index = "id")

# 2. FE Model 
rice.w <- plm(log(goutput) ~ log(seed) + log(totlabor) + log(size), Rice)

# 3. RE Model 
rice.r <- update(rice.w, model = "random")

# 4. Test whether use RE or FE Model (H0: no correlation between regressors and individual effects)
# (1) Method 1: Hausman test (use phtest()) 
phtest(rice.w, rice.r)
# result: not reject H0

# (2) Method 2: Mundlak Test
# Takes the difference between the within and between estimators
# Between Model
rice.b <- update(rice.w, model = "between")
cp <- intersect(names(coef(rice.b)), names(coef(rice.w)))
dcoef <- coef(rice.w)[cp] - coef(rice.b)[cp]
V <- vcov(rice.w)[cp, cp] + vcov(rice.b)[cp, cp]
as.numeric(t(dcoef) %*% solve(V) %*% dcoef)
cor(fixef(rice.w), between(log(Rice$goutput)))  # between the individual effects and individual means of the explanatory variable
# result: not reject H0

# (3) Method 3: Chamberlain test (use piest()) 
piest(log(goutput) ~ log(seed) + log(totlabor) + log(size), RiceFarms, index = "id")
# result: not reject H0 at 0.01

# (4) Method 4: Angrist and Newey test.
print(aneweytest(log(goutput) ~ log(seed) + log(totlabor) + log(size), RiceFarms, index = "id")) 
# result: reject H0
####----------------------------------------------------------------------------------------------------

#### Example 4-3: Unobserved effects test ####
data("RiceFarms", package="plm")
Rice <- pdata.frame(RiceFarms, index = "id")

####----------------------------------------------------------------------------------------------------
#### Wooldridge's Test (H0: no unobserved individual effects) 
# 1. Wooldridge's Test (use pwtest())
pwtest(log(goutput) ~ log(seed) + log(totlabor) + log(size), Rice)    

# 2. Result
# reject H0, there are individual effects 
####----------------------------------------------------------------------------------------------------

#### Example 4-4: LM Test for RE and/or Serial Correlation ####
data("RiceFarms", package="plm")
Rice <- pdata.frame(RiceFarms, index = "id")

####----------------------------------------------------------------------------------------------------
#### LM Test (for RE and the presence of serial correlation) 
# 1. Create Formula
fm <- log(goutput) ~ log(seed) + log(totlabor) + log(size)

# 2. Create 2x3 Matrix 
bsy.LM <- matrix(ncol=3, nrow = 2) 

# 3. LM Test (use pbsytest())
# Three Methods of LM Test
# (1) 'J' method: sets normalality and homoskedasticity
# (2) 'RE'method: for RE Model
# (3) 'AR' method: for AR(1) model
tests <- c("J", "RE", "AR")
dimnames(bsy.LM) <- list(c("LM test", "p-value"), tests)
for(i in tests) {
  mytest <- pbsytest(fm, data = Rice, test = i)
  bsy.LM[1:2, i] <- c(mytest$statistic, mytest$p.value)
}

# 4. Compare 3 methods of LM Test 
round(bsy.LM, 6)

# 5. Results 
# The robust tests allow us to discriminate between time-invariance error persistence (RE) 
# and time-decaying persistence (autoregressive error). 
# Conclude: p-value on 'RE' method is insignificant, use RE Model. 
####----------------------------------------------------------------------------------------------------

#### Optimal Conditional Test (use pbltest())
# 1. Optimal Conditional Test (H0: no serial correlation)
pbltest(fm, Rice, alternative = "onesided")   # allows for FE of any magnitude. using residuals of FE MLE.

# 2. Result
# Reject H0. There's serial correlation. 
####----------------------------------------------------------------------------------------------------

#### Example 4-5: Likelihood Ratio Tests ####
library("nlme")
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

#### RE(GLS) Model, MLE 
# 1. RE(GLS) Model
reGLS <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
summary(reGLS)
# 2. MLE 
reML <- lme(inv ~ value + capital, data = Grunfeld, random = ~1 | firm)   # random = ~1 sets number of random effects to 1, the intercept

# 3. Compare RE(GLS) Model, MLE 
# (1) Method 1
rbind(coef(reGLS), fixef(reML))

# (2) Method 2
library("stargazer")
stargazer(coef(reGLS), fixef(reML),
          dep.var.labels = "investment",
          covariate.labels = c("intercept", "value", "capital"),
          title = c("FE Coefficients", "MLE Coefficients"),
          no.space = TRUE,
          type = "text"
)
####----------------------------------------------------------------------------------------------------

#### Groupwise Structures of Time Dependence (must specify correlation structure)
# 1. Linear Model (restricted: with AR(1) errors)
lmAR1ML <- gls(inv ~ value + capital, data = Grunfeld, correlation = corAR1(0, form = ~ year | firm))    
summary(lmAR1ML)

# 2. RE Model (restricted: with AR(1) errors)
reAR1ML <- lme(inv ~ value + capital, data = Grunfeld, random = ~ 1 | firm, correlation = corAR1(0, form = ~ year | firm))   # restricted model
summary(reAR1ML)

# 3. Linear Model  (unrestricted: no correlation)
lmML <- gls(inv ~ value + capital, data = Grunfeld)     # unrestricted model

# 4. RE Model (unrestricted: no correlation)
reML <- lme(inv ~ value + capital, data = Grunfeld, random = ~1 | firm) 

# 5. Compare Restricted and Unrestricted Models (use anova())
# (1) Compare Linear Model unrestricted and restricted 
anova(lmML, lmAR1ML)

# (2) Compare RE Model unrestricted and restricted
anova(reML, reAR1ML)

# 6. Likelihood Ratio Test for RE 
anova(lmML, reML)

# 6. Likelihood Ratio Test for RE sub AR(1) errors
anova(lmAR1ML, reAR1ML)
####----------------------------------------------------------------------------------------------------

#### Example 4-6: Breusch-Godfrey and Durbin Watson tests ####
# 1. RE Model 
rice.re <- plm(fm, Rice, model='random')

# 2. Breusch-Godfrey test (re-estimate relevant quasi-demeaned model by OLS and apply these tests to the residuals) 
pbgtest(rice.re, order = 2)    # (use pbgtest())

# 3. Durbin-Watson test 
pdwtest(rice.re, order = 2)    # (use pdwtest())
####----------------------------------------------------------------------------------------------------

#### Example 4-7: serial correlation tests for FE models ####
data("EmplUK", package = "plm")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: study employment in UK 

# 2. Data: An unbalanced panel of 140 observations from 1976 to 1984 in UK

# 3. Variables 
# (1) Independent Variables 
# wage: wages  
# capital: capital 

# (2) Dependent Variables 
# emp: employment
####----------------------------------------------------------------------------------------------------

#### Wooldridge's Within-based Serial Correlation Test
# 1. Wooldridge's Test (H0: no serial correlation)
pwartest(log(emp) ~ log(wage) + log(capital), data = EmplUK)   

# 2. Result 
# reject H0, there's serial correlation
####----------------------------------------------------------------------------------------------------

#### Example 4-8: Wooldridge's First Difference Test####
# 1. Wooldridge's Test (H0: no serial correlation in FD errors)
pwfdtest(log(emp) ~ log(wage) + log(capital), data = EmplUK)    # default: h0 = "fd"

# 2. Result: 
# not reject H0, no serial correlation. 

# 3.  Wooldridge's Test (H0: no serial correlation in FE errors) 
pwfdtest(log(emp) ~ log(wage) + log(capital), data = EmplUK, h0 = "fe")    # specify: h0 = "fe"

# 4. Result: 
# here reject H0. 
# The original residuals show evidence of serial correlation, which disappears after FD. 
####----------------------------------------------------------------------------------------------------

#### Example 4-9: Wooldridge's First Difference Test ####
data("RiceFarms", package="plm")

####----------------------------------------------------------------------------------------------------

#### Wooldridge's First Difference Test
# 1. Create panel data 
Rice <- pdata.frame(RiceFarms, index = "id")

# 2. Create formula 
fm <- log(goutput) ~ log(seed) + log(totlabor) + log(size)

# 3. Create 2x2 Matrix 
W.fd <- matrix(ncol = 2, nrow =2)

# 4. Wooldridge's First Difference Test (calculate by hand)
H0 <- c("fd", "fe")
dimnames(W.fd) <- list(c("test", "p-value"), H0)
for(i in H0) {
  mytest <- pwfdtest(fm, Rice, h0 = i)
  W.fd[1, i] <- mytest$statistic
  W.fd[2, i] <- mytest$p.value
}
round(W.fd, 6)

# 5. Result 
# Both rejected. The truth lies in the middle. 
# In this case, whichever is chosen will have serially correlated errors. 
# It's better to use an autocorrelation-robust covariance matrix. 
####----------------------------------------------------------------------------------------------------

#### Example 4-10: Tests for Cross-sectional Dependence ####
library("pder")
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

# 4. Comments 
# (1) They account for common factors and spillover effects
# (2) They find evidence that when controlling for such features, effect of own R&D isn't significant any more.
#     And conclude that value of R&D derived from a complex mix of own sources and spillovers from other firms. 
# (3) They estimate various specifications of a standard production fn. (output is fn. of labor and capital)
#     augmented with R&D expenditure.
####----------------------------------------------------------------------------------------------------

# processes are present, relating individuals in a way depending on a measure of distance. 

#### Tests for Cross-sectional Dependence 
# Def: Cross-sectional Dependence: another type of serial correlation
# ex: idiosyncratic errors are correlated across individuals not time cov(Vit, Vjt) ≠ 0)
# 1. Create Formula 
fm.rds <- lny ~ lnl + lnk + lnrd

# 2. Tests for Cross-sectional Dependence (H0: no cross-sectional dependence) 
# (1) Method 1: Pairwise-Correlations-Based Test (use pcdtest()) 
pcdtest(fm.rds, RDSpillovers)      # only individual effect 

# (2) Method 2: Use Residuals to Test (if a different model specification is consistent)
rds.2fe <- plm(fm.rds, RDSpillovers, model = "within", effect = "twoways")    # both individual and time effect 
pcdtest(rds.2fe)

# 3. Comparing the 2 Tests
cbind("rho" = pcdtest(rds.2fe, test = "rho")$statistic,      # use $statistic to get z score
      "|rho|"= pcdtest(rds.2fe, test = "absrho")$statistic)
####----------------------------------------------------------------------------------------------------

#### Example 4-11: Cross-Sectional Dependence Test for a pseries ####
data("HousePricesUS", package = "pder")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: analyze changes in real house prices to assess to which extent they're driven by 
#           foundatmentals like disposable per capita income, net borrowing costs, and pop growth. 

# 2. Data: yearly 1421 total observations of 49 regions (1976 to 2003) in US 

# 3. Variables 
# price: real house price index, 1980=100
# region: region index 
####----------------------------------------------------------------------------------------------------

#### Cross-Sectional Dependence Test
# 1. Create panel data 
php <- pdata.frame(HousePricesUS)

# 2. Comparing the 2 Tests 
cbind("rho" = pcdtest(diff(log(php$price)), test = "rho")$statistic, 
      "|rho|" = pcdtest(diff(log(php$price)), test = "absrho")$statistic)

# 3. Result 
# overall averages of rho are large and close to each other, indicating substantial positive correlation. 

# 4. Create a correlation table (see correlation between regions) 
regions.names <- c("New Engl", "Mideast", "Southeast", "Great Lks",
                   "Plains", "Southwest", "Rocky Mnt", "Far West")
corr.table.hp <- cortab(diff(log(php$price)), grouping = php$region,     
                        groupnames = regions.names)
colnames(corr.table.hp) <- substr(rownames(corr.table.hp), 1, 5)
round(corr.table.hp, 2)

# 5. Result 
# To investigate whether this behavior is geographically uniform or not, one can drill down to regional level. 
# Based on table, there's evidence of factor-related dependence: common shocks to technology stimulate growth 
# in the most advanced states irrespective of geographic proximity. 

# 6. Cross-Sectional Dependence Test (H0: no cross-sectional dependence) (using AR(2) model)
pcdtest(diff(log(price)) ~ diff(lag(log(price))) + diff(lag(log(price), 2)), data = php)

# 7. Result 
# reject H0, there's cross-sectional dependence. 
####----------------------------------------------------------------------------------------------------