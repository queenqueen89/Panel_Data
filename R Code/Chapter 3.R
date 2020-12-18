#### Example 3-1:Unbalanced Panel #### 
library("plm")
library("dplyr")
library("pglm")
library("texreg")

data("Tileries", package = "pder")
head(Tileries, 3)     # list first 3 rows of data
pdim(Tileries)        # unbalanced data

####----------------------------------------------------------------------------------------------------
#### Data & Variable Description
# 1. Topic: estimate the tilery Cobb-Douglas production in Egypt

# 2. Data: include weekly (total 22 weeks) observations of 25 firms (483 total observations) in Egypt

# 3. Variables 
# (1) Independent Variables 
# labor: labor hours 
# machine: machine hours 
# id: firm id

# (2) Dependent Variable
# output: output
####----------------------------------------------------------------------------------------------------

#### FE Models 3 Versions 
# 1. Create panel data 
Tileries <- pdata.frame(Tileries)

# 2. Create Formula 
Oeq <- log(output) ~ log(labor) + log(machine)

# 3. Version 1: FE Mode
plm.within <- plm(Oeq, Tileries, model = "within")
coef(plm.within)
tab_model(plm.within, terms = "log(labor)")

# 4. Version 2: OLS
y <- log(Tileries$output) 
x1 <- log(Tileries$labor)
x2 <- log(Tileries$machine)
lm.within <- lm((I(y - Between(y)) ~ I(x1 - Between(x1)) + I(x2 - Between(x2)) -1))    # I(x^2) would return a vector of values raised by power 2. 

# 5. Version 3: LDSV version
lm.lsdv <- lm(log(output) ~ log(labor) + log(machine) + factor(id), Tileries)    # use factor() to include "id" and "week" b/c they're not numeric.

# 6. Compare the 3 Versions 
# (1) Method 1
fe.models <- list(FE = plm.within, POLS = lm.within, LSDV = lm.lsdv)
sapply(fe.models, function(x) coef(summary(x)))

# (2) Method 2
library("stargazer")
stargazer(coef(plm.within), coef(lm.within), coef(lm.lsdv)[2:3],
          dep.var.labels = "output",
          covariate.labels = c("log(labor)", "log(machine)"),
          title = c("FE Version", "OLS Version", "LSDV Version"),
          label = "tab:wagesresult",
          no.space = TRUE,
          type = "text"
)

# (3) Method 3 
coef(plm.within)
coef(lm.within)
coef(lm.lsdv)[2:3]

# 7. Results 
# We get same results from the 3 methods 
####----------------------------------------------------------------------------------------------------

#### One-way RE model
# 1. One-way RE model 
tile.re <- plm(log(output) ~ log(labor) + log(machine), Tileries, model="random")
summary(tile.re)
coef(tile.re)[2:3]

# 2. Results 
# (1) the transformation parameter (theta) is now individual specific: 
# it depends on number of available observations for every individual. 
# theta has a range of values: between 0.49 and 0.60. 
# (2) share of individual and idiosyncratic is 19% and 81%.  
####----------------------------------------------------------------------------------------------------

#### Two-ways RE model: 3 Versions
# 1. Version 1: RE Model 
plm.within2 <- plm(log(output) ~ log(labor) + log(machine),Tileries, effect = "twoways")

# 2. Version 2: LSDV
lm.lsdv2 <- lm(log(output) ~ log(labor) + log(machine) + factor(id) + factor(week), Tileries)   # add "factor(id)" and "factor(week)"

# 3. Version 3: OLS
y <- y - Between(y, "individual") - Between(y, "time") + mean(y)        # remove individual and time means, use formula 
x1 <- x1 - Between(x1, "individual") - Between(x1, "time") + mean(x1)
x2 <- x2 - Between(x2, "individual") - Between(x2, "time") + mean(x2)
lm.within2 <- lm(y ~ x1 + x2 - 1)

# 4. Compare the 3 Versions 
stargazer(coef(plm.within2), coef(lm.within2), coef(lm.lsdv2)[2:3],
          dep.var.labels = "output",
          covariate.labels = c("log(labor)", "log(machine)"),
          title = c("FE Version", "OLS Version", "LSDV Version"),
          label = "tab:wagesresult",
          no.space = TRUE,
          type = "text"
)

# 5. Results 
# You get same results from the 3 methods.
####----------------------------------------------------------------------------------------------------

#### Two-ways RE Model: 3 Methods
# 1. RE Model (random.method = "walhus")
walhus <- plm(Oeq, Tileries, model = "random", random.method = "walhus", effect = "twoways")

# 2. RE Medol (random.method = "amemiya")
amemiya <- update(walhus, random.method = "amemiya")

# 3. RE Medol (random.method = "swar")
swar <- update(walhus, random.method = "swar")

# 4. "swar" Variance of Error Component
ercomp(swar)

# 5. Compare SD of Error Component for 3 Models
# (1) Method 1
re.models <- list(walhus = walhus, amemiya = amemiya, swar = swar)
sapply(re.models, function(x) sqrt(ercomp(x)$sigma2))
sapply(re.models, coef)

sqrt(ercomp(amemiya)$sigma2)

# (2) Method 2
stargazer(coef(walhus), coef(amemiya), coef(swar),
          dep.var.labels = "log(output)",
          covariate.labels = c("Intercept","log(labor)", "log(machine)"),
          title = c("Walhus", "Amemiya", "Swar"),
          no.space = TRUE,
          type = "text"
)

stargazer(sqrt(ercomp(walhus)$sigma2), sqrt(ercomp(amemiya)$sigma2), sqrt(ercomp(swar)$sigma2),
          dep.var.labels = "log(output)",
          covariate.labels = c("SD Idiosyncratic", "SD Individual", "SD Time"),
          title = c("Walhus Method", "Amemiya Method", "Swar Method"),
          no.space = TRUE,
          type = "text"
)

# 6. Results
# Shares of individual, time, idiosyncratic are 18.5%, 4.7%, 76.8% (lower than one-way). 
####----------------------------------------------------------------------------------------------------

#### Example 3-2: SUR Estimation #### 
library("plm")
library("dplyr")
data("TexasElectr", package="pder")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: study the production cost of electric firms in Texas

# 2. Data: include yearly 180 total observations (1966-1983) from 10 firms in Texas

# 3. Variables
# (1) Other Variables 
# output: output 
# pfuel: price of fuel
# plab: price of labor 
# pcap: price of capital
# explab: expense in labor
# expfuel: expense in fuel
# expcap: expense in capital

# (2) Dependent Variables
# cost: total production cost
####----------------------------------------------------------------------------------------------------

#### RE Model (estimate production cost)
# 1. Log Prices
TexasElectr <- mutate(TexasElectr,                            # mutate() adds new variables and preserves existing ones
                      pf = log(pfuel / mean(pfuel)),
                      pl = log(plab / mean(plab)) - pf,
                      pk = log(pcap / mean(pcap)) - pf)

# 2. Log Output
TexasElectr <- mutate(TexasElectr, q = log(output / mean(output)))

# 3. Total Cost and Factor Shares
TexasElectr <- mutate(TexasElectr,
                      C = expfuel + explab + expcap,
                      sl = explab / C,
                      sk = expcap / C,
                      C = log(C / mean(C)) - pf)

# 4. Squared Terms and Interaction Terms
TexasElectr <- mutate(TexasElectr,
                      pll = 1/2 * pl ^ 2,
                      plk = pl * pk,
                      pkk = 1/2 * pk ^ 2,
                      qq = 1/2 * q ^ 2)

# 5. Formula: Total Cost and Factor Shares
cost <- C ~ pl + pk + q + pll + plk + pkk + qq   # total cost 
shlab <- sl ~ pl + pk                            # labor shares
shcap <- sk ~ pl + pkn                           # capital shares

# 6. Create Restriction Matrix
# (1) Create 6x14 Matrix (all elements are 0's)
R <- matrix(0, nrow = 6, ncol = 14)     # 6 rows b/c total 6 restrictions, 14 columns b/c total 14 variables 

# (2) Assign Values 1 and -1 to some elements
R[1, 2] <- R[2, 3] <- R[3, 5] <- R[4, 6] <- R[5, 6] <- R[6, 7] <- 1
R[1, 9] <- R[2, 12] <- R[3, 10] <- R[4, 11] <- R[5, 13] <- R[6, 14] <- -1
R

# 7. RE Model 
z <- plm(list(cost = C ~ pl + pk + q + pll + plk + pkk + qq,
              shlab = sl ~ pl + pk,
              shcap = sk ~ pl + pk),
         TexasElectr, model = "random",
         restrict.matrix = R)
summary(z)
####----------------------------------------------------------------------------------------------------

#### Example 3-3: MLE ####  
library("pglm")
library("splm")
data("RiceFarms", package = "splm")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: study the production of rice

# 2. Data: include a panel of 171 observations

# 3. Variables
# (1) Independent Variables
# seed: seed in kilogram
# totlabor: total labor (excluding harvest labor)
# size: the total area cultivated with rice, measured in hectares

# (2) Dependent Variables
# goutput: gross output of rice in kg
####----------------------------------------------------------------------------------------------------

#### MLE, RE Model
# 1. Create Panel data 
Rice <- pdata.frame(RiceFarms, index="id")

# 2. MLE (use pglm())
rice.ml <- pglm(log(goutput) ~ log(seed) + log(totlabor) + log(size),data=Rice, family=gaussian)   # use family = gaussian, specify distribution of errors is normal 
summary(rice.ml)
coef(rice.ml)[2:4]

# 3. RE Model
rice.re <- plm(log(goutput) ~ log(seed) + log(totlabor) + log(size),data=Rice,method="random")
coef(rice.re)

# 4. Compare the 2 Models 
stargazer(coef(rice.ml)[2:4], coef(rice.re),
          dep.var.labels = "goutput",
          covariate.labels = c("log(seed)", "log(totlabor)","log(size)"),
          title = c("MLE", "RE"),
          no.space = TRUE,
          type = "text"
)

# 5. Results 
# MLE and Re results are very similar. 
####----------------------------------------------------------------------------------------------------

#### Example 3-4: Nested Error Component Model #### 
library("plm")
library("texreg")
data("RiceFarms", package="plm")
head(RiceFarms, 2)     # find the identifier variable "id" in a dataset 

# Create Panel Data: 3 Methods to order with "id" 
R1 <- pdata.frame(RiceFarms, index = c(id = "id", time = NULL, group = "region"))
R2 <- pdata.frame(RiceFarms, index = c(id = "id", group = "region"))
R3 <- pdata.frame(RiceFarms, index = c("id", group = "region"))
head(index(R1))
head(index(R2))
head(index(R3))
head(R1)
####----------------------------------------------------------------------------------------------------
data("Produc", package = "plm")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: study the gross state product

# 2. Data: A panel of 48 observations from 1970 to 1986

# 3. Variables
# (1) Independent Variables
# pc: private capital stock
# emp: labor input measured by the employment in non–agricultural payrolls
# hwy: highway and streets
# water: water and sewer facilities
# util: other public buildings and structures
# unemp: state unemployment rate

# (2) Dependent Variables
# gsp:  gross state product

####----------------------------------------------------------------------------------------------------

#### Nested Effects RE Model: 4 Methods (use effect = "nested")
# 1. Method 1: RE swar
nswar <- plm(log(gsp) ~ log(pc) + log(emp) + log(hwy) + log(water) + log(util) + unemp, data = Produc,
             model = "random", effect = "nested", random.method = "swar", index = c(group = "region"))

# 2. Method 2: RE amemiya 
namem <- update(nswar, random.method = "amemiya")

# 3. Method 3: RE walhus 
nwalhus <- update(nswar, random.method = "walhus")

# 4. Method 4: RE swar individual 
iswar <- update(nswar, effect = "individual")

# 5. FE individual
iwith <- update(nswar, model = "within", effect = "individual")

# 6. Compare 4 RE models and 1 FE model
screenreg(list("FE Individual" = iwith, "RE Individual" = iswar,                  # screenreg() converts regression output to an ASCII table
               "Nested Swar" = nswar, "Nested Walhus" = nwalhus,
               "Nested Amemiya" = namem), digits = 3)
####----------------------------------------------------------------------------------------------------