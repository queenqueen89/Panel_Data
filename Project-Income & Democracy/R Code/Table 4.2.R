#### Table 4.2 -----------------------------------------------------------------
library(quantreg)  # for quantile regression 
library("IVQR")      # for quantile IV regression 
library(rqpd)      # for quantile fixed effects regression 

setwd("/Users/nicoleyin88/Documents/1. Panel Data/0. Final Project/1. Code/")
data1 <- read.csv("5yr_panel.csv", header=TRUE)
democ1 <- data.frame(data1)
Democracy <- democ1$polity4
Income <- democ1$lrgdpch

#### 1. Pooled OLS 
# (1 ) Model 1
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
model1 <- rq(Democracy ~ lag(Income) + lag(lpop), taus, democ1)
coef(model1)[2:3,]

mode1l_mu <- lm(Democracy ~ lag(Income) + lag(lpop), democ1)
coef(mode1l_mu)[2:3]

# (2) Model 2 with Region Effects
model2 <- rq(Democracy ~ lag(Income) + lag(lpop) + as.factor(country), taus, democ1)
coef(model2)[2:3,]

model2_mu <- lm(Democracy ~ lag(Income) + lag(lpop) + as.factor(country), democ1)
coef(model2_mu)[2:3]

# (3) Model 3 with Region and Year Effects
model3 <- rq(Democracy ~ lag(Income) + lag(lpop) + as.factor(country) + as.factor(year), taus, democ1)
coef(model3)[2:3,]

model3_mu <- lm(Democracy ~ lag(Income) + lag(lpop) + as.factor(country) + as.factor(year), democ1)
coef(model3_mu)[2:3]

library(stargazer)
coeff <- list(coef(model1)[2:3,],
              coef(model2)[2:3,],
              coef(model3)[2:3,])
stargazer(model1,model2,model3,
          coef=coeff)

#### 2. IV: Trade-weighted world income 
# (4) Model 4
model4 <- ivqr(Democracy ~ lag(Income) | lag(worldincome,2) | lag(lpop), taus, 
               grid = seq(-2,2,0.2), data = democ1)
coef(model4)[2,]

# (5) Model 5

# (6) Model 6

#### 3. Fixed Effects
# (7) Model 7


# (8) Model 8 with Region Effects
fe2 <- rqpd(Democracy ~ lag(Income) + lag(lpop) | as.factor(country), 
            panel(method="pfe", tau=c(0.1, 0.25, 0.5, 0.75, 0.9), 
                  tauw=rep(1/5, 5)), data=democ1)
fe2.coef <- c(fe2$coef[2], fe2$coef[5], fe2$coef[8], fe2$coef[11], fe2$coef[14])
fe2.coef

fe2_mu <- felm(Democracy ~ lag(Income) + lag(lpop) + as.factor(country), data=democ1)
coef(fe2_mu)[2:3]

# (9) Model 9 with Region and Year Effects
fe3 <- rqpd(Democracy ~ lag(Income) + lag(lpop) | as.factor(year), 
            panel(method="pfe", taus=c(0.1, 0.25, 0.5, 0.75, 0.9), 
                  tauw=rep(1/5, 5)), data=democ1)
fe3.coef <- c(fe3$coef[2], fe3$coef[5], fe3$coef[8], fe3$coef[11],fe3$coef[14])
fe3.coef

fe3_mu <- felm(Democracy ~ lag(Income) + lag(lpop) + as.factor(country) + as.factor(year), data=democ1)
coef(fe3_mu)[2:3]
