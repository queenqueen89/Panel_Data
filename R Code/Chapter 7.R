#### Example 7-1: Description of Data ####
library("plm")
library("ggplot2")
data("DemocracyIncome", package = "pder")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description
# 1. Topic: study the causal relationship between the level of wealth and that of democracy in a country.

# 2. Data: 5-yearly total 2321 observations of 211 countries (1950-2000)

# 3. Variables 
# (1) Independent Variables: 
# income: the log of the gdp per capita
# year: the starting year of the 5-years period

# (2) Dependent Variable: 
# democracy: democracy index
####----------------------------------------------------------------------------------------------------

#### Plot Figure 7.1 ####
# 1. Choose data for years 2000-2004, include & order variables by "democracy, income, country."  
di2000 <- subset(DemocracyIncome, year == "2000-2004", select = c("democracy", "income", "country"))

# 2. Exclude missing values 
di2000 <- na.omit(di2000)

# 3. Convert Objects into Character Values (use as.character())
di2000$country <- as.character(di2000$country)

# 4. Replace countries with NA (if have missing values for "democracy" or "income")
di2000$country[- c(2,5, 23, 16, 17, 22, 71,  125, 37, 43, 44, 79, 98, 105, 50, 120,  81, 129, 57, 58,99)] <- NA

# 5. Plot the Data
ggplot(di2000, aes(income, democracy, label = country)) + 
  geom_point(size = 1) + 
  geom_text(aes(y= democracy + sample(0.5 * c(-1, 1), nrow(di2000), replace = TRUE)), size = 2) +
  theme(legend.text = element_text(size = 8), 
        legend.title= element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8))
####----------------------------------------------------------------------------------------------------

#### Example 7-2: Time Effects Within Model #### 
# 1. POLS  Model 1 (include time dummy)
pols1 <- plm(democracy ~ lag(democracy) + lag(income) + year - 1,      # "+ year" is to include year dummy
           DemocracyIncome, index = c("country", "year"),              # "-1" is to exclude intercept in the regression 
           model = "pooling", subset = sample == 1)                    # "subset = sample == 1" is to use subsets where vaule of sample is 1.
 
# 2. POLS Model 2 (include time effect)
pols2 <- plm(democracy ~ lag(democracy) + lag(income), 
           DemocracyIncome, index = c("country", "year"), 
           model = "within", effect = "time",
           subset = sample == 1)

# 3. Coefficients Table  
library("stargazer")
stargazer(coef(summary(pols1))[1:2,], coef(summary(pols2))[1:2,],
          dep.var.labels = "democracy",
          title = c("POLS 1", "POLS 2"),
          no.space = TRUE,
          type = "text"
)

# 4. Results 
# (1) "democracy" variable shows high persistence, with coefficient of 0.71 (last year's democracy index has a 
#     strong effect on current democracy index). 
# (2) But POLS estimator suffers from positive bias. 
# (3) On the other hand, lag(income) seems to exert a significantly positive influence on democracy.
####----------------------------------------------------------------------------------------------------

#### Example 7-3: Two-Ways Within Model ####
# 1. Two-Ways FE Model
fe <- update(pols2, effect = "twoways")
coef(summary(fe))

# 2. Results 
# (1) Coefficient of democracy is now smaller (0.38 < 0.71). 
#     This is expected b/c FE estimator is biased downward and POLS vice versa. 
# (2) After introducing individual effects, coefficient of income is close to 0.
####----------------------------------------------------------------------------------------------------

#### Example 7-4: Anderson and Hsiao Estimator ####
# 1.Anderson and Hsiao Estimator (include time dummy)
ahsiao <- plm(diff(democracy) ~ lag(diff(democracy)) + lag(diff(income)) + year - 1  | 
                lag(democracy, 2) + lag(income, 2) + year - 1,         # "The IVs are democracy(t-2) and income(t-2).
              DemocracyIncome, index = c("country", "year"), model = "pooling", subset = sample == 1)
coef(summary(ahsiao))[1:2,]

# 2. Results 
# (1) One expects the estimated coefficient of democracy to be comprised between FE Model (biased downward)
#     and POLS (biaded upward). Here coefficient of democracy is 0.47 (between 0.38 and 0.61) 
# (2) Here regressors are lag(diff(democracy)) not lag(democracy). Since it's on level and linear regression, 
#     the coefficient interpretation doesn't change. 
####----------------------------------------------------------------------------------------------------

#### Example 7-5: Difference GMM Estimator ####
# 1. Difference GMM Estimator (model = "onestep")
gmm1 <- pgmm(democracy ~ lag(democracy) + lag(income) |      # first part is the regression 
                lag(democracy, 2:99) |                       # second part are GMM instruments: include lags 2 to 99
                lag(income, 2),                              # third part are normal instruments: income(t-2)
              DemocracyIncome, index=c("country", "year"), 
              model = "onestep", effect = "twoways", subset = sample == 1)
coef(summary(gmm1))

# 2. Difference GMM Estimator (model = "twostep")
gmm2 <- update(gmm1, model = "twosteps")
coef(summary(gmm2))

# 3. Results 
# (1) For nth individual, GMM instruments matrix (17x17) has the form: 
#     [yn1 0   0  0 0 0 0 0  0   0   0  ... 0 ]
#     [0  yn1 yn2   0 0 0 0  0   0   0  ... 0 ]
#     [               ...                     ]
#     [0   0   0  0 0 0 0 0 yn1 yn2 yn3 ...yn9]
# We have 0.5*(11-1)*(11-2) GMM Instruments, since there're 11 5-year periods.
# (2) We have 9 time dummies. 
# (3) We have 1 normal IV income(t-2)
# (4) In total, we have 55 IVs (J=55)
# (5) The results are near those of Anderson and Hsiao Model. 

# 4. Comments on Difference GMM Estimator
# (1) Formula: it has 3 parts: 
#  i) part 1: regressino 
#  ii) part 2: GMM instruments 
#  iii) part 3: normal instruments 

# (2) Model: it can be estimated either in:
#  i) onestep
#  ii) or twosteps 

# (3) Effect: it can be either: 
#   i) individual (eliminated in differencing)
#  ii) or twoways (indicator variables for each period are added as "normal" instruments)
####----------------------------------------------------------------------------------------------------

#### Example 7-6: Instruments Proliferation (IVs grows super fast) #### 
data("DemocracyIncome25", package = "pder")

####----------------------------------------------------------------------------------------------------

#### Data & Variable Description (DemocracyIncome25)
# 1. Topic: study the causal relationship between the level of wealth and that of democracy in a country.

# 2. Data: 25-yearly total 175 observations of 25 countries (1850-2000)

# 3. Variables 
# (1) Independent Variables: 
# income: the log of the gdp per capita
# year: the starting year of the 5-years period

# (2) Dependent Variable: 
# democracy: democracy index
####----------------------------------------------------------------------------------------------------

#### Difference GMM Estimator #### 
# 1. Difference GMM Estimator (lag 2:99)
diff25 <- pgmm(democracy ~ lag(democracy) + lag(income) |
                 lag(democracy, 2:99) + lag(income, 2:99),      # GMM instruments: include lags 2 to 99
               DemocracyIncome25, model = "twosteps")

# 2. Difference GMM Estimator(lag 2:4, index, effect="twoways", subset)
diff25lim <- pgmm(democracy ~ lag(democracy) + lag(income) | 
                    lag(democracy, 2:4)+ lag(income, 2:4),       # GMM instruments: include lags 2 to 4
                  DemocracyIncome, index=c("country", "year"), 
                  model="twosteps", effect="twoways", subset = sample == 1)

# 3. Difference GMM Estimator (lag 2:99, index, effect="twoways", subset)
diff25coll <- pgmm(democracy ~ lag(democracy) + lag(income) | 
                     lag(democracy, 2:99)+ lag(income, 2:99),    # GMM instruments: include lags 2 to 99
                   DemocracyIncome, index=c("country", "year"), 
                   model="twosteps", effect="twoways", subset = sample == 1,
                   collapse = TRUE)

# 4. Compare the 3 Models 
sapply(list(diff25, diff25lim, diff25coll), function(x) coef(x)[1:2])

# 5. Results 
# (1) For each GMM instrument, there are 0.5*(7-1)*(7-2) = 15 instruments. 
#     So two GMM instruments have total 30 instruments.  
# (2) There are 5 time dummies. 
# (3) In total, we have 35 instruments (J=35). 
# (4) The coefficients from 3 models are similar. 
#     The proliferation of instruments is not an important issue in this example. 
####----------------------------------------------------------------------------------------------------

#### Figure 7-2 ####
hatpi <-function(x, sr = 1){
  k <- (1-x)^2/(1-x^2)
  (x-1) * k / (sr^2 + k)
}
xs <- seq(0, 1, 0.01)
dd <- data.frame(x = rep(xs, 2), y = c(hatpi(xs), xs- 1), 
                 var = rep(c("bm1", "pi"), each = length(xs)))

ggplot(dd, aes(x, y)) + geom_line(aes(lty = var)) + 
  scale_y_continuous(limits = c(-1, 0)) + 
  xlab(expression(rho)) + ylab("") + 
  scale_linetype_manual(values = 1:2, breaks = c("bm1", "pi"),
                        labels = list(expression(pi),
                                      expression(rho-1)))+
  theme(legend.text = element_text(size = 6), 
        legend.title= element_blank(),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6))
####----------------------------------------------------------------------------------------------------

#### Figure 7.3 ####
set.seed(2)
snu <- 0.2
beta <- 0.8
T <- 30L
nu1 <- rnorm(T, sd = snu)
nu2 <- rnorm(T, sd = snu)
mu1 <- 1
mu2 <- 2
y1 <- y2 <- Ey1 <- Ey2 <- rep(0, T)
y1[1] <- 1
y2[1] <- 6
Ey1[1] <- Ey2[1] <- NA
for(t in 2:T){
  y1[t] <- y1[t-1] * beta + mu1 + nu1[t]
  Ey1[t] <- y1[t-1] * beta
  y2[t] <- y2[t-1] * beta + mu2 + nu2[t]
  Ey2[t] <- y2[t-1] * beta
}
dd <- data.frame(x = rep(1:30, 4), y = c(y1, y2, Ey1, Ey2),
                 var = rep(c("y", "E(y)"), each = 30 * 2),
                 id = rep(rep(letters[1:2], each = 30), 2), 
                 case = "Case 1")
set.seed(2)
snu <- 0.2
beta <- 0.8
T <- 30L
nu1 <- rnorm(T, sd = snu)
nu2 <- rnorm(T, sd = snu)
mu1 <- 1
mu2 <- 2
y1 <- y2 <- Ey1 <- Ey2 <- rep(0, T)
Ey1[1] <- Ey2[1] <- NA
for(t in 2:T){
  y1[t] <- y1[t-1] * beta + mu1 + nu1[t]
  Ey1[t] <- y1[t-1] * beta
  y2[t] <- y2[t-1] * beta + mu2 + nu2[t]
  Ey2[t] <- y2[t-1] * beta
}
dd2 <- data.frame(x = rep(1:30, 4), y = c(y1, y2, Ey1, Ey2),
                  var = rep(c("y", "E(y)"), each = 30 * 2),
                  id = rep(rep(letters[1:2], each = 30), 2),
                  case = "Case 2")
dd <- rbind(dd, dd2)

gp <- ggplot(dd, aes(x=x, y = y, lty = var, colour = id)) + 
  geom_line() + facet_wrap(~ case) + 
  xlab("") + ylab("") +
  theme(legend.text = element_text(size = 6), 
        legend.title = element_blank(),
        axis.title = element_text(size = 8))
gp

####----------------------------------------------------------------------------------------------------
  
#### Example 7-7: System GMM ####
# 1. System GMM 
sys2 <- pgmm(democracy ~ lag(democracy) + lag(income) | 
               lag(democracy, 2:99)| lag(income, 2),
             DemocracyIncome, index = c("country", "year"), 
             model = "twosteps", effect = "twoways",
             transformation = "ld")      # "ld" for level and difference. # default is "d" for difference.
coef(summary(sys2))

# 2. Results 
# The coefficients from System GMM and Difference GMM are close. 
# The income coefficient is now significantly positive and much larger than before. 
####----------------------------------------------------------------------------------------------------

#### Example 7-8: Robust Estimation of Covariance Matrix ####
# 1. Classical Variance
sqrt(diag(vcov(gmm2)))[1:2]      

# 2. Robust Variance
sqrt(diag(vcovHC(gmm2)))[1:2]     

####----------------------------------------------------------------------------------------------------

#### Example 7-9: Sargan-Hansen Test ####
# 1. Sargan-Hansen Test for Difference GMM (H0: moments are valid )
sargan(gmm2)

# 2. Results 
# It has J=55, K=11, where
# J=55 include 45 GMM estimators, 1 income variable, and 9 time dummies
# K=11 include 1 lagged endogenous variable, 1 income, and 9 time dummies
# df = J-K = 44. 
# The hypothesis of moments' validity is not rejected.

# 3. Sargan-Hansen Test for System GMM
sargan(sys2)

# 4. Results 
# It has J=66, K=12, where
# J=66 include 45 GMM estimators, 1 income variable, and 9 time dummies, 1 more instrument, 10 supplementary instruments.
# K=11 include 1 lagged endogenous variable, 1 income, and 9 time dummies, 1 more instrument.
# df = J-K = 54. 
# The hypothesis of moments' validity is not rejected.

# 5. Compare Sargan Test for 3 Difference GMM Estimators   
sapply(list(diff25, diff25lim, diff25coll), function(x) sargan(x)[["p.value"]])

# 6. Results 
# (1) Hansen-Sargan test is very sensitive to the problem of instrument proliferation. 
# (2) p-value of this test tends to be very high, leading to non-rejecting the validity of moment conditions, 
#     when the same test performed on models more parsimonious in terms of instruments may lead to opposite conclusions. 
# (3) the p-value for diff25 model (using all moment conditions) is near 1, while those of the other models are much lower; 
# (4) in particular, the model limiting the number of lags to 3, the hypothesis of instruments validity is rejected 
#     at the 10% significance level. 
####----------------------------------------------------------------------------------------------------

#### Example 7-10: Autocorrelation Test ####
# 1. Autocorrelation Test (H0: no serial correlation) 
mtest(gmm2, order = 2)     # order=2 is set according to the preceding remark

# 2. Results 
# Not reject the hypothesis.
####----------------------------------------------------------------------------------------------------
