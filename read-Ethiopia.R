library(haven)
library(dplyr)
source('utils.R')

oldwd <- getwd()
setwd("C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/Ethiopia LSMS 2013/STATA/Household")

# Mobile phone information is in section 10, number 8
# "How many of this [ITEM] does your household own? IF NONE RECORD 0"
sect10 <- read_dta('sect10_hh_w2.dta')
# this is a data frame with 184,170 rows, but only 5,262 unique households
# each combination of 35 assets and 5,262 hh gets its own row
eth_mobile <- sect10[sect10$hh_s10q0a=='Mobile Telephone',
                     c('household_id','household_id2','hh_s10q01')]

# Electricity is in section 9, number 20
# "How many times did the household faced electric power failure/interruption 
# at least lasting for one hour during last week?"
# 1 = don't use electricity
# 2+ = data about interruptions
sect9 <- read_dta('sect9_hh_w2.dta')
eth_elect <- sect9 %>% select(household_id,household_id2,pw2,saq01,hh_s9q19_a)

j <- plyr::join(eth_elect,eth_mobile,by=c('household_id','household_id2'))

eth_2013 <- data.frame(mobile=j$hh_s10q01>0,
                       elect=j$hh_s9q19_a < 4,
                       adm0='Ethiopia',
                       adm1=labelled_to_str(j$saq01),
                       weight=j$pw2)

# Penetration drops when we weight correctly - areas with poor access were
# probably under-sampled.
mean(eth_2013$mobile) # 54%
w_mean(eth_2013$mobile,eth_2012$weight) # 46%
# World Bank says 27.3 subscriptions per 100 people in 2013
# http://data.worldbank.org/indicator/IT.CEL.SETS.P2?locations=ET
# Could still be consistent given households with >1 person

mean(eth_2013$elect,na.rm=TRUE) # 42%
w_mean(eth_2013$elect,eth_2012$weight) # 24.7%
# Compares well with country average at
# http://data.worldbank.org/indicator/EG.ELC.ACCS.ZS?locations=ET
# which gives me 26.6% for 2012

# The number I really care about
mean(eth_2013$mobile & !eth_2013$elect,na.rm=TRUE) # 19.6%
w_mean(eth_2013$mobile & !eth_2013$elect,eth_2013$weight) # 25.8%

setwd(oldwd)
rm(eth_elect,eth_mobile,j,sect9,sect10)


