#### Table 5 -----------------------------------------------------------------

# (1) Pooled OLS 
data1.3 <- read.csv("5yr_panel.csv", header=TRUE)
democ1.3 <- pdata.frame(data1.3, index="country")
Democracy.IV_saving <- democ1.3$fhpolrigaug
Income.IV_saving <- democ1.3$lrgdpch
pols.3 <- plm(Democracy.IV_saving ~ lag(Income.IV_saving) + year -1 | .- lag(Income.IV_saving) + lag(nsave),
              democ1.3, index = c("year", "country"), model = "pooling", subset = sample == 1)
pols.3.coef <- coeftest(pols.3, vcov=vcovHC)

# (2) Fixed effects OLS 
fe1.3 <- plm(Democracy.IV_saving ~ lag(Income.IV_saving) | .- lag(Income.IV_saving) + lag(nsave),
             democ1.3, index = c("country", "year"), model = "within", 
             effect="twoways", subset = sample == 1)
fe1.3.coef <- coeftest(fe1.3)

# (3) Fixed effects OLS 
fe2.3 <- plm(Democracy.IV_saving ~ lag(Democracy.IV_saving) + lag(Income.IV_saving) | .- lag(Income.IV_saving) + lag(nsave),
             democ1.3, index = c("country", "year"), model = "within", 
             effect="twoways", subset = sample == 1)
fe2.3.coef <- coeftest(fe2.3, vcov=vcovHC)

screenreg(list("Pooled OLS (1)" = pols.3.coef, "Fixed effects (2)" = fe1.3.coef, "Fixed effects (3)" = fe2.3.coef))
