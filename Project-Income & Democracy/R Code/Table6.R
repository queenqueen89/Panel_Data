#### Table 6 -----------------------------------------------------------------

# (1) Pooled OLS 
data1.4 <- read.csv("5yr_panel.csv", header=TRUE)
democ1.4 <- pdata.frame(data1.4, index="country")
Democracy.IV.world_income <- democ1.4$fhpolrigaug
Income.IV.world_income <- democ1.4$lrgdpch
pols.4 <- plm(Democracy.IV.world_income ~ lag(Income.IV.world_income) + year -1 | .- lag(Income.IV.world_income) + lag(worldincome),
              democ1.4, index = c("year", "country"), model = "pooling", subset = sample == 1)
pols.4.coef <- coeftest(pols.4, vcov=vcovHC)

# (2) Fixed effects OLS 
fe1.4 <- plm(Democracy.IV.world_income ~ lag(Income.IV.world_income) | .- lag(Income.IV.world_income) + lag(worldincome),
             democ1.4, index = c("country", "year"), model = "within", 
             effect="twoways", subset = sample == 1)
fe1.4.coef <- coeftest(fe1.4)

# (3) Fixed effects OLS 
fe2.4 <- plm(Democracy.IV.world_income ~ lag(Democracy.IV.world_income) + lag(Income.IV.world_income) | .- lag(Income.IV.world_income) + lag(worldincome),
             democ1.4, index = c("country", "year"), model = "within", 
             effect="twoways", subset = sample == 1)
fe2.4.coef <- coeftest(fe2.4)

screenreg(list("Pooled OLS (1)" = pols.4.coef, "Fixed effects (2)" = fe1.4.coef, "Fixed effects (3)" = fe2.4.coef))

