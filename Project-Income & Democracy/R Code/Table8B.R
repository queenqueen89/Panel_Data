#### Table 8B -----------------------------------------------------------------
# (1) OLS
data500.2 <- read.csv("Table8B.csv")
democ500.2 <- pdata.frame(data500.2, index="country")
Democracy1.500yr <- democ500.2$democ
Income1.500yr <- democ500.2$growth
OLS1.2 <- plm(Democracy1.500yr ~ Income1.500yr, democ500.2, index = "madid", model = "pooling")
OLS1.2.coef <- coeftest(OLS1.2, vcov=vcovHC)

# (2) OLS
OLS2.2 <- plm(Democracy1.500yr ~ Income1.500yr + consfirstaug + indcent, 
              democ500.2, index = "madid", 
              model = "pooling")
OLS2.2.coef <- coeftest(OLS2.2, vcov=vcovHC)

# (3) OLS
OLS3.2 <- plm(Democracy1.500yr ~ Income1.500yr + rel_catho80 
              + rel_muslim80 + rel_protmg80 , 
              democ500.2, index = "madid", 
              model = "pooling")
OLS3.2.coef <- coeftest(OLS3.2, vcov=vcovHC)

# (4) OLS
OLS4.2 <- plm(Democracy1.500yr ~ Income1.500yr + consfirstaug + indcent + rel_catho80 
              + rel_muslim80 + rel_protmg80, 
              democ500.2, index = "madid", 
              model = "pooling")
OLS4.2.coef <- coeftest(OLS4.2, vcov=vcovHC)

# (5) OLS
OLS5.2 <- plm(Democracy1.500yr ~ Income1.500yr + lpd1500s,
              democ500.2, index = "madid",
              model = "pooling")
OLS5.2.coef <- coeftest(OLS5.2, vcov=vcovHC)

# (6) OLS
OLS6.2 <- plm(Democracy1.500yr ~ Income1.500yr + consfirstaug + indcent + lpd1500s,
              democ500.2, index = "madid",
              model = "pooling")
OLS6.2.coef <- coeftest(OLS6.2, vcov=vcovHC)

# (7) OLS
OLS7.2 <- plm(Democracy1.500yr ~ Income1.500yr + consfirstaug + indcent + rel_catho80 
              + rel_muslim80 + rel_protmg80 + lpd1500s, 
              democ500.2, index = "madid", 
              model = "pooling")
OLS7.2.coef <- coeftest(OLS7.2, vcov=vcovHC)

screenreg(list("OLS (1)" = OLS1.2.coef, "OLS (2)" = OLS2.2.coef, "OLS (3)" = OLS3.2.coef, "OLS (4)" = OLS4.2.coef, 
               "OLS (5)" = OLS5.2.coef, "OLS (6)" = OLS6.2.coef, "OLS (7)" = OLS7.2.coef))
