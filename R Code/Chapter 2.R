#### Example 2-1: Within Estimator (FE Model) #### 
library("plm")
library("pglm")
library("stargazer")
data("TobinQ", package = "pder")
####----------------------------------------------------------------------------------------------------
#### Data & Variable Description
# 1. Topic: test Tobin's theory of investment: 
# the main variable that explains investment is the ratio between value of firm and replacement cost of its physical capital (ratio Tobin Q). 
# If the Tobin Q is greater than 1, then the profitability of investment is greater than its cost, so the investment is valuable. 

# 2. Data: include yearly 188 observations (1951-1981)  (6580 in total) in the US

# 3. Variables 
# Independent Variables 
# qn: Tobin's Q

# Dependent Variable 
# ikn: investment divided by capital 

# (3) Other Variables 
# cusip: compustat's identifying number 
# year: year
####----------------------------------------------------------------------------------------------------

#### Code (allow for plotting  results for some examples later on)
baw <- FALSE
library("ggplot2")
plotplm <- function(x, N = 10, seed = 1, lgth = 0.1){
  mydata <- model.frame(x)
  onames <- names(mydata)
  names(mydata) <- c("y", "x")
  LGTH <- (max(mydata$x) - min(mydata$x)) ^ 2 +
    (max(mydata$y) - min(mydata$y)) ^ 2
  lgth <- lgth * sqrt(LGTH) / 2
  seed <- set.seed(seed)
  theids <- sample(unique(index(mydata)[[1]]), N)
  small <- subset(mydata, index(mydata)[[1]] %in% theids)
  small <- cbind(small, id = index(small)[[1]])
  ymean <- with(small, tapply(y, id, mean)[as.character(theids)])
  xmean <- with(small, tapply(x, id, mean)[as.character(theids)])
  within <- update(x, model = "within")
  alpha <- mean(mydata[[1]]) - coef(within) * mean(mydata[[2]])
  beta <- as.numeric(coef(within))
  random <- update(within, model = "random")
  between <- update(within, model = "between")
  ols <- update(within, model = "pooling")
  FE <- fixef(within)[as.character(theids)]
  DATA <- data.frame(id = names(FE), FE = as.numeric(FE), slope = beta,
                     xmean = xmean, ymean = ymean,
                     xmin = xmean - lgth / sqrt(1 + beta ^ 2),
                     xmax = xmean + lgth / sqrt(1 + beta ^ 2),
                     ymin = ymean - lgth * beta / sqrt(1 + beta ^ 2),
                     ymax = ymean + lgth * beta / sqrt(1 + beta ^ 2))
  MODELS <- data.frame(models = c("ols", "random", "within", "between"),
                       intercept = c(coef(ols)[1], coef(random)[1], alpha, coef(between)[1]),
                       slope = c(coef(ols)[2], coef(random)[2], coef(within), coef(between)[2]))
  if (! baw){
    ggplot(data = small, aes(x = x, y = y, color = id)) + geom_point(size = 1) +
      geom_segment(aes(x = xmin, xend = xmax, y = ymin, yend = ymax, color = id), data = DATA) +
      geom_abline(aes(intercept = intercept, slope = slope, lty = models), data = MODELS) +
      geom_point(aes(x = xmean, y = ymean, color = id), size = 2, shape = 13, data = DATA) +
      xlab(onames[2]) + ylab(onames[1]) +
      theme(legend.text = element_text(size = 10),
            legend.title= element_text(size = 12),
            axis.title = element_text(size = 12))
  } else {
    ggplot(data = small, aes(x = x, y = y)) + geom_point(size = 1, aes(shape = id)) +
      geom_segment(aes(x = xmin, xend = xmax, y = ymin, yend = ymax), data = DATA) +
      geom_abline(aes(intercept = intercept, slope = slope, lty = models), data = MODELS) +
      geom_point(aes(x = xmean, y = ymean, shape = id), size = 2,  data = DATA) +
      scale_shape_manual(values=1:N) +
      xlab(onames[2]) + ylab(onames[1]) +
      theme(legend.text = element_text(size = 10),
            legend.title= element_text(size = 12),
            axis.title = element_text(size = 12))
  }
}
####----------------------------------------------------------------------------------------------------

#### Create Panel Data (use pdata.frame())
# 1. Default Setting 
pTobinQ <- pdata.frame(TobinQ)    # default: 1st column is individual index, 2nd column is time index

# 2. Order by individual 
pTobinQa <- pdata.frame(TobinQ, index = 188)     # Use index=188 to order data by individual, since number of obervations is 188

# 3. Order by 'cusip'
pTobinQb <- pdata.frame(TobinQ, index = c('cusip'))   # Use index-c('cusip') to order data by 'cusip'

# 4. Order by 'cusip' and 'year'
pTobinQc <- pdata.frame(TobinQ, index = c('cusip', 'year'))      # Use index-c('cusip', 'year') to order data by 'cusip' and 'year'

# 5. Dimension of data (use pdim())
pdim(pTobinQ)     # inspects the individual and time dimensions (find n and T)
####----------------------------------------------------------------------------------------------------

#### POLS, FE Model, Between Model 
# 1. Create Formula 
Qeq <- ikn ~ qn

# 2. POLS, FE Model, Between Model 
Q.pooling <- plm(Qeq, pTobinQ, model = "pooling")
Q.within <- update(Q.pooling, model = "within")
Q.between <- update(Q.pooling, model = "between")

# 3. Compare the three models 
# Method 1
coef(summary(Q.pooling))[2]
coef(summary(Q.within))[1]
coef(summary(Q.between))[2]

# Method 2
models <- list(POLS = Q.pooling, FE = Q.within, Between = Q.between)
sapply(models, coef)
####----------------------------------------------------------------------------------------------------

#### Three Types of Within Estimator 
# 1. Individual effects (use fixef() function)
# the default option returns the individual intercepts
head(fixef(Q.within))

# 2. Individual effects in deviations from 1st individual (use type = "dfirst")
head(fixef(Q.within, type = "dfirst"))

# 3. Individual effects in deviations from their mean (use type = "dmean")
head(fixef(Q.within, type = "dmean"))
####----------------------------------------------------------------------------------------------------

#### LSDV Model (equivalent to within estimator)
lsdv <- lm(ikn ~ qn + factor(cusip), pTobinQ)
head(coef(lsdv))
####----------------------------------------------------------------------------------------------------

#### Example 2-2: RE Model #### 
#### RE Model: Four Ways 
# 1. RE Model (use random.method = "swar")
Q.swar <- plm(Qeq, pTobinQ, model = "random", random.method = "swar")   # RE Model developed by Swamy and Arora

# 2. RE Model (use random.method = "walhus")
Q.walhus <- update(Q.swar, random.method = "walhus")

# 3. RE Model (use random.method = "amemiya")
Q.amemiya <- update(Q.swar, random.method = "amemiya")

# 4. RE Model (use random.method = "nerlove")
Q.nerlove <- update(Q.swar, random.method = "nerlove")

# 5. Compare results from the 4 models 
Q.models <- list(swar = Q.swar, walhus = Q.walhus, amemiya = Q.amemiya, nerlove = Q.nerlove)
sapply(Q.models, function(x) ercomp(x)$theta)    # sapply() extract theta 
sapply(Q.models, coef)                           # sapply() extract coefficients 

# 6. Results 
# (1) The individual effect share is 0.275 (about 1/4). 
# (2) θ is about 0.735 (or 73%). θ is the part of individual mean that's removed from each variable for GLS estimator. 
#     The high θ value is due to large time dimension (T=35). 
# (3) This means GLS estimator is closer to within estimator (θ=1, since 100% mean is subtracted in FE model)
#     than to the OLS estimator (θ=0, since 0% mean is subtracted in OLS model)
####----------------------------------------------------------------------------------------------------

#### Error Components: 7 methods
# 1. Method 1 "swar" 
summary(Q.swar)$ercomp       # get coefficients and error components 

# 2. Method 2 "walhus"
summary(Q.walhus)$ercomp

# 3. Method 3 "amemiya"
summary(Q.amemiya)$ercomp

# 4. Method 4 "amemiya"
Q.nerlove

# 5. Method 5 (use ercomp() function)
ercomp(Qeq, pTobinQ)    # formula + data 

# 6. Method 6 (use ercomp() function)
ercomp(Q.swar)         # model 

# 7. Method 7 (use random.models = c("within", "between"))
# random.models = c("within", "between"):use within residuals to get σν, use between residuals to get σl  
# random.dfcor indicates the denominator of the 2 quadratic forms
# 0 is used when the # of observations used is (NT,N)
# 1 is used when the numerators of the theoretical formuals are used (N(T-1),N)
# 2 is used when the number of estimated parameters are deduced (N(T-1)-K,N-K-1)
Q.swar2 <- plm(Qeq, pTobinQ, model = "random", random.models = c("within", "between"),random.dfcor = c(2, 2))
summary(Q.swar2)$ercomp            
####----------------------------------------------------------------------------------------------------

#### Example 2-3: Comparison of estimators ####
# 1.Compare POLS, FE Model, Between Model, RE (swar method)
sapply(list(POLS = Q.pooling, FE = Q.within, Between = Q.between, RE = Q.swar), 
       function(x) coef(summary(x))["qn", c("Estimate", "Std. Error")])

# 2. Results  
# GLS (RE) estimator is slightly lower than FE estimator, but are similar. 
# GLS (RE) estimator is much lower than Between estimator
# OLS estimator seems to be the most efficient, but it is biased if individual effects are present. 
####----------------------------------------------------------------------------------------------------

#### Calculate POLS estimator by hand
# 1. Share of variances for "qn"
summary(pTobinQ$qn)

# 2. Sum of squares for FE Model, Between Model, SST for "qn"
SST.fe <- sum(Within(pTobinQ$qn) ^ 2)
SST.between <- sum((Between(pTobinQ$qn) - mean(pTobinQ$qn)) ^ 2)
SST <- sum( (pTobinQ$qn - mean(pTobinQ$qn)) ^ 2)

# 3. Weight of FE model       
weight.fe <- print(SST.fe/SST) 

# 4. POLS estimator
weight.fe * coef(Q.within)[["qn"]] + (1 - weight.fe) * coef(Q.between)[["qn"]]
####----------------------------------------------------------------------------------------------------

#### Calculate GLS estimator by hand 
# 1. Estimate Parameter φ using the residuals of Between and FE estimators       
T <- 35
N <- 188
smxt2 <- deviance(Q.between) * T / (N - 2)
sidios2 <- deviance(Q.within) / (N * (T - 1) - 1)
phi <- sqrt(sidios2 / smxt2)

# 2. Weight of FE model 
weight.fe <- SST.fe / (SST.fe + phi^2 * SST.between)
weight.fe

# 3. GLS estimator
weight.fe * coef(Q.within)[["qn"]] + (1 - weight.fe) * coef(Q.between)[["qn"]]
####----------------------------------------------------------------------------------------------------

#### Example 2-4: Simple Linear Model ####
data("ForeignTrade", package = "pder")

####----------------------------------------------------------------------------------------------------
#### Data & Variable Description
# 1. Topic: analyze the link between imports and the national product. 

# 2. Data: include yearly 744 total observations (1963-1986) from 31 countries

# 3. Variables
# (1) Independent Variable 
# gnp:real GNP per capita 

# (2) Dependent Variable 
# imports: nominal imports deflated by the unit value of exports per capita 
####----------------------------------------------------------------------------------------------------

#### FE, RE, POLS, Between Models 
# 1. Create panel data 
trade <- pdata.frame(ForeignTrade)
summary(trade$gnp)    # Share of variances for "gnp", where"id" is individual effect, "time" is time effect. 

# 2.Variances of the error components
ercomp(imports ~ gnp, trade)

# 3. Compare the 4 models 
models <- c("within", "random", "pooling", "between")
sapply(models, function(x) coef(plm(imports ~ gnp, trade, model = x))["gnp"])

# 4. Plot the 4 models 
plotplm(plm(imports~gnp, ForeignTrade), N = 10, seed = 4, lgth = .05)
####----------------------------------------------------------------------------------------------------

#### Example 2-5:Simple linear model ####
data("TurkishBanks", package = "pder")

####----------------------------------------------------------------------------------------------------
#### Data & Variable Description
# 1. Topic: analyze production costs of banks

# 2. Data: include yearly 583 total observations (1990-2000) from 53 banks in Turkey. 

# 3. Variables
# (1) Independent Variable 
# output: output, total loans

# (2) Dependent Variable 
# cost: total cost
####----------------------------------------------------------------------------------------------------

#### FE, RE, POLS, Between Models 
# 1. Create panel data without missing values 
TurkishBanks <- na.omit(TurkishBanks)     # na.omit() function deals with missing value
TB <- pdata.frame(TurkishBanks)

# 2. Share of variances for "output"
summary(log(TB$output))    # where"id" is individual effect, "time" is time effect. 

# 3. Variance of error component 
ercomp(log(cost) ~ log(output), TB)

# 4. Compare the 4 models 
sapply(models, function(x) coef(plm(log(cost) ~ log(output), TB, model = x))["log(output)"])

# 5. Results
# (1) For output, variation is mainly inter-individual (85%). 
# (2) But for the error, share of individual and idiosyncratic effect are similar (40% and 60%).
#     That's why OLS and Between estimators are very close. 
# (3) GLS estimator is about half way between OLS and within estimators because the transformation
#     removes about 65% of individual mean. 

# 6. Plot 
plotplm(plm(log(cost)~log(output), TB), N = 8)
####----------------------------------------------------------------------------------------------------

#### Example 2-6: Simple Linear Model ####
data("TexasElectr", package = "pder")
####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: study the production cost of electric firms in Texas

# 2. Data: include yearly 180 total observations (1966-1983) from 10 firms in Texas

# 3. Variables
# (1) Independent Variables
# output: output 

# (2) Dependent Variables
# cost: total production cost

# (3) Other Variables 
# explab: expense in labor
# expfuel: expense in fuel
# expcap: expense in capital
####----------------------------------------------------------------------------------------------------

#### FE, RE, POLS, Between Models 
# 1. Create "cost" variable 
TexasElectr$cost <- with(TexasElectr, explab + expfuel + expcap)

# 2. Create panel data 
TE <- pdata.frame(TexasElectr)

# 2. Share of variances for "output"
summary(log(TE$output))

# 3. Variance of error component 
ercomp(log(cost) ~ log(output), TE)

# 4. Compare the 4 models 
sapply(models, function(x) coef(plm(log(cost) ~ log(output), TE, model = x))["log(output)"])

# 5. Results 
# The variation in output is mainly inter-individual (82%). 
# But for error, shares of idiosyncratic and individual variance are 99% and 1%. 
# So only a very small part of individual mean is removed while using RE (GLS) estimator.
# So RE (GLS) and OLS estimators are almost equal. 
# The Within estimator is much higher b/c individual effects and output are negatively correlated. 

# 6. Plot 
plotplm(plm(log(cost)~log(output), TexasElectr), N = 8)
####----------------------------------------------------------------------------------------------------

#### Example 2-7: Simple Linear Model ####
data("DemocracyIncome25", package = "pder")

####----------------------------------------------------------------------------------------------------
#### Data & Variable Description
# 1. Topic: analyze the dynamic causal relationship between wealth and democracy

# 2. Data: include yearly 175 total observations (1850-2000) from 25 countries

# 3. Variables
# (1) Independent Variables
# income: the log of the gdp per capita

# (2) Dependent Variables
# democracy: democracy index  
####----------------------------------------------------------------------------------------------------

#### FE, RE, POLS, Between Models 
# 1. Create panel data 
DI <- pdata.frame(DemocracyIncome25)

# 2. Share of variances for "income"
summary(lag(DI$income))

# 3. Variances of error component 
ercomp(democracy ~ lag(income), DI)

# 4. Compare the 4 models 
sapply(models, function(x)
  coef(plm(democracy ~ lag(income), DI, model = x))["lag(income)"])

# 5. Results 
# Share of inter-individual for income and error are weak (43% and 21%). 
# 41% of individual mean is removed from variables to get RE (GLS) estimator. 
# no obvious correlation between individual effects and income. 
# consequently, the 4 estimators are very close. 

# 6. Plot 
plotplm(plm(democracy~lag(log(income)), DemocracyIncome25), N = 8)
####----------------------------------------------------------------------------------------------------

#### Example 2-8: Two-ways effect model #### 
#### RE Model 4 Methods (two-ways effect model)
# 1. RE Model two-ways 
Q.models2 <- lapply(Q.models, function(x) update(x, effect = "twoways"))   # Apply A Function Over A List Or Vector

# 2. SD of 3 components of error term 
sapply(Q.models2, function(x) sqrt(ercomp(x)$sigma2))

# 3. Theta of error term
sapply(Q.models2, function(x) ercomp(x)$theta)
####----------------------------------------------------------------------------------------------------

#### Example 2-9: Multiple Linear Model #### 
data("UnionWage", package = "pglm")

####----------------------------------------------------------------------------------------------------
#### Data & Variable Description
# 1. Topic: investigate the imact of union negotiations on wages and potential endogeneity of "union"

# 2. Data: include yearly 545 total individuals (1980-1987) of 4360 total
#    observations in US

# 3. Variables
# (1) Independent Variables
# union: does the wage is set by collective bargaining
# school: years of schooling
# exper: the experience, computed as age - 6 - schooling
# com: one of black, hisp and other
# rural: does the individual lives in a rural area ?
# married: is the individual married ?
# health: does the individual has health disability ?
# region: the region, one of NorthEast, NothernCentral, South and other
# sector: one of agricultural, mining, construction, trade, transportation, 
#         finance, businessrepair, personalservice, entertainment, manufacturing, 
#         pro.rel.service, pub.admin
# occ: one of proftech, manoffpro, sales, clerical, craftfor, operative, 
#      laborfarm, farmlabor, service

# (2) Dependent Variables
# wage: hourly wage in US dollars
####----------------------------------------------------------------------------------------------------

#### FE and OLS Models 
# 1. Create variable exper2 
UnionWage$exper2 <- with(UnionWage, exper ^ 2)

# 2. FE Model
# (1) FE (without "occ" variable )
wages.within1 <- plm(wage ~ union + school + exper + exper2 +
                       com + rural + married + health +
                       region + sector, UnionWage)

# (2) FE (with "occ" variable )
wages.within2 <- plm(wage ~ union + school + exper + exper2 +
                       com + rural + married + health +
                       region + sector + occ, UnionWage)    
# 3. POLS Model 
# (1) POLS (without "occ" variable )
wages.pooling1 <- update(wages.within1, model = "pooling")

# (2) FE (with "occ" variable )
wages.pooling2 <- update(wages.within2, model = "pooling")   # add "occ" variable 

# 4. Create Table (use stargazer())
stargazer(wages.pooling2, wages.pooling1, wages.within2, wages.within1,
          omit = c("region", "sector", "occ"),
          omit.labels = c("region dummies", "sector dummies", "occupation dummies"),
          column.labels = c("pooling estimation", "within estimation"),
          column.separate = c(2, 2),
          dep.var.labels = "log of hourly wage",
          covariate.labels = c("union membership", "education years",
                               "experience years", "experience years squared",
                               "black", "hispanic", "rural residence",
                               "married", "health problems",
                               "Intercept"),
          omit.stat = c("adj.rsq", "f"),
          title = "Wage equation",
          label = "tab:wagesresult",
          no.space = TRUE,
          type = "text"
)
####----------------------------------------------------------------------------------------------------