#### PROBLEMS #### 
# Need to display everything in a big table







setwd("/Users/nicoleyin88/Documents/1. Panel Data/0. Final Project/1. Code/")
library(readxl)
library(psych)
library(lmtest)
library(plm)

#### Table 1 -----------------------------------------------------------------------
#### 1. Panel A #### 
# (1) All countries 
# i) Freedom House measure of democracy(t-1)
data1 <- read_excel("Table1.xls", sheet = 1)
describe(lag(data1$fhpolrigaug))[3:4]

# ii) Log GDP per capita(t-1)
describe(lag(data1$lrgdpch))[3:4]

# (2) High-income countries 
# i) Freedom House measure of democracy(t-1)
data1_high <- read_excel("Table1.xls", sheet = 2)
describe(lag(data1_high$fhpolrigaug))[3:4]

# ii) Log GDP per capita(t-1)
describe(lag(data1_high$lrgdpch))[3:4]

# (3) Low-income countries 
# i) Freedom House measure of democracy(t-1)
data1_low <- read_excel("Table1.xls", sheet = 3)
describe(lag(data1_low$fhpolrigaug))[3:4]

# ii) Log GDP per capita(t-1)
describe(lag(data1_low$lrgdpch))[3:4]

#### 2. Panel B #### 
# (1) All countries 
# Polity measure of democracy(t-1)
describe(lag(data1$polity4))[3:4]

# (2) High-income countries 
describe(lag(data1_high$polity4))[3:4]

# (3) Low-income countries 
describe(lag(data1_low$polity4))[3:4]

#### 3. Panel C #### 
# (1) All countries 
# i) Log population(t-1)
describe(lag(data1$lpop))[3:4]

# ii) Education(t-1)
describe(lag(data1$education))[3:4]

# (2) High-income countries 
# i) Log population(t-1)
describe(lag(data1_high$lpop))[3:4]

# ii) Education(t-1)
describe(lag(data1_high$education))[3:4]

# (3) Low-income countries 
# i) Log population(t-1)
describe(lag(data1_low$lpop))[3:4]

# ii) Education(t-1)
describe(lag(data1_low$education))[3:4]

#### 4. Panel D #### 
# (1) All countries 
# Savings rate(t-2)
describe(lag(data1$nsave,2))[3:4]

# (2) High-income countries 
# Savings rate(t-2)
describe(lag(data1_high$nsave,2))[3:4]

# (3) Low-income countries 
# Savings rate(t-2)
describe(lag(data1_low$nsave,2))[3:4]

#### 5. Panel E #### 
# (1) All countries 
# Trade-weighted log GDP(t-1)
describe(lag(data1$worldincome,1))[3:4]

# (2) High-income countries 
# Trade-weighted log GDP(t-1)
describe(lag(data1_high$worldincome,1))[3:4]

# (3) Low-income countries 
# Trade-weighted log GDP(t-1)
describe(lag(data1_low$worldincome,1))[3:4]
#### End -----------------------------------------------------------------------