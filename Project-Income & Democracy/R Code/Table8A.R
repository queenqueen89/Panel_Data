#### Table 8A -----------------------------------------------------------------

# (1) OLS
data500.1 <- read.csv("500yr_panel.csv", header=TRUE)
democ500.1 <- pdata.frame(data500.1, index="country")
Democracy.500yr <- democ500.1$democ
Income.500yr <- democ500.1$growth
OLS1.1 <- plm(Democracy.500yr ~ Income.500yr, democ500.1, index = "madid", model = "pooling", subset = world == 1)
OLS1.1.coef <- coeftest(OLS1.1, vcov=vcovHC)

# (2) OLS
OLS2.1 <- plm(Democracy.500yr ~ Income.500yr + consfirstaug + indcent, 
              democ500.1, index = "madid", 
              model = "pooling", subset = world == 1)
OLS2.1.coef <- coeftest(OLS2.1, vcov=vcovHC)

# (3) OLS
OLS3.1 <- plm(Democracy.500yr ~ Income.500yr + rel_catho80 
              + rel_muslim80 + rel_protmg80 , 
              democ500.1, index = "madid", 
              model = "pooling", subset = world == 1)
OLS3.1.coef <- coeftest(OLS3.1, vcov=vcovHC)

# (4) OLS
OLS4.1 <- plm(Democracy.500yr ~ Income.500yr + consfirstaug + indcent + rel_catho80 
              + rel_muslim80 + rel_protmg80, 
              democ500.1, index = "madid", 
              model = "pooling", subset = world == 1)
OLS4.1.coef <- coeftest(OLS4.1, vcov=vcovHC)

screenreg(list("OLS (1)" = OLS1.1.coef, "OLS (2)" = OLS2.1.coef, 
               "OLS (3)" = OLS3.1.coef, "OLS (4)" = OLS4.1.coef))