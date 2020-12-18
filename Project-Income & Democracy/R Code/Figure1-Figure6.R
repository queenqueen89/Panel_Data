#### Figure 1 -----------------------------------------------------------------------
# Load the data 
library(readxl)
data_fig1 <- read_excel("AER.xls", sheet = 8)
fhpolrigaug <- data_fig1$fhpolrigaug
lrgdpch <- data_fig1$lrgdpch

# Linear Regression
fig1 <- lm(fhpolrigaug ~ lrgdpch)

# Plot figure 1 
plot(fhpolrigaug ~ lrgdpch,
     xlab = "Log GDP per capita (Penn World Tables)",
     ylab = "Freedom House measure of democracy",
     xlim = c(6,10), ylim = c(0,1))
abline(fig1, col="steelblue")

#### Figure 2 -----------------------------------------------------------------------
# Load the data 
data_fig2 <- read_excel("AER.xls", sheet = 9)
s5lrgdpch <- data_fig2$s5lrgdpch
s5fhpolrigaug <- data_fig2$s5fhpolrigaug

# Linear Regression
fig2 <- lm(s5fhpolrigaug ~ s5lrgdpch)

# Plot figure 2 
plot(s5fhpolrigaug ~ s5lrgdpch,
     xlab = "Change in Log GDP per capita (Penn World Tables)",
     ylab = "Change in Freedom House measure of democracy",
     xlim = c(-1,2), ylim = c(-1,1),
     cex = 0.5)
abline(fig2, col="steelblue")

#### Figure 3 -----------------------------------------------------------------------
# Load the data 
data_fig3 <- read_excel("AER.xls", sheet = 10)
s5lrgdpch <- data_fig3$s5lrgdpch
s5polity4 <- data_fig3$s5polity4

# Linear Regression
fig3 <- lm(s5polity4 ~ s5lrgdpch)

# Plot figure 3
plot(s5polity4 ~ s5lrgdpch,
     xlab = "Change in Log GDP per capita (Penn World Tables)",
     ylab = "Change in Polity measure of democracy",
     xlim = c(-1,2), ylim = c(-1,1),
     cex = 0.5)
abline(fig3, col="steelblue")

#### Figure 4 -----------------------------------------------------------------------
# Load the data 
data_fig4 <- read_excel("AER.xls", sheet = 11)
s2lrgdpmadalt <- data_fig4$s2lrgdpmadalt
s2polity4 <- data_fig4$s2polity4

# Linear Regression
fig4 <- lm(s2polity4 ~ s2lrgdpmadalt)

# Plot figure 4 
plot(s2polity4 ~ s2lrgdpmadalt,
     xlab = "Change in Log GDP per capita (Maddison)",
     ylab = "Change in Polity measure of democracy",
     xlim = c(0,2.5), ylim = c(-0.5,1),
     cex = 0.5)
abline(fig4, col="steelblue")

#### Figure 5 -----------------------------------------------------------------------
# Load the data 
data_fig5 <- read_excel("AER.xls", sheet = 12)
democ <- data_fig5$democ
growth <- data_fig5$growth

# Linear Regression
fig5 <- lm(democ ~ growth)

# Plot figure 5
plot(democ ~ growth,
     xlab = "Change in Log GDP per capita",
     ylab = "Change in Democracy",
     xlim = c(0,4), ylim = c(0,1),
     cex = 0.5)
abline(fig5, col="steelblue")

#### Figure 6 -----------------------------------------------------------------------
# Load the data 
data_fig6 <- read_excel("AER.xls", sheet = 13)
democresid <- data_fig6$democresid
growthresid <- data_fig6$growthresid

# Linear Regression
fig6 <- lm(democresid ~ growthresid)

# Plot figure 6
plot(democresid ~ growthresid,
     xlab = "Change in Log GDP per capita independent of historical factors",
     ylab = "Change in Democracy independent of historical factors",
     xlim = c(-2,3), ylim = c(-0.5,0.5),
     cex = 0.5)
abline(fig6, col="steelblue")
#### End -----------------------------------------------------------------------
