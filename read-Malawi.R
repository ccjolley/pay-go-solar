library(haven)
source('utils.R')

oldwd <- getwd()
setwd("C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/Malawi LSMS 2013/Household")

# Mobile phone information is in question F34:
# "How many working cell phones in total does your household own?
# IF NONE, RECORD 0"

modf <- read_dta('HH_MOD_F.dta')

# sanity checks
# str(modf$hh_f34)
# sum(is.na(modf$hh_f34))
# mean(modf$hh_f34 > 0,na.rm=TRUE) # 53.8% penetration

# Electric information is in question F19:
# "Do you have electricity working in your dwelling?"

# sanity checks
# str(modf$hh_f19)
# sum(is.na(modf$hh_f19))
# mean(modf$hh_f19==1) # 14.6% electrification

moda <- read_dta('HH_MOD_A_FILT.dta')



mwi_2013 <- data.frame(mobile=modf$hh_f34>0,
                       elect=modf$hh_f19==1,
                       adm0='Malawi',
                       adm1=labelled_to_str(moda$region),
                       #adm2=labelled_to_str(moda$district),
                       weight=moda$hh_wgt)

# Penetration drops when we weight correctly - areas with poor access were
# probably under-sampled.
mean(mwi_2013$mobile,na.rm=TRUE) # 53.8%
w_mean(mwi_2013$mobile,mwi_2013$weight) # 46.7% 
# WB says 33.5 subscriptions per 100 people in 2013
# http://data.worldbank.org/indicator/IT.CEL.SETS.P2?locations=MW
mean(mwi_2013$elect,na.rm=TRUE) # 14.6%
w_mean(mwi_2013$elect,mwi_2013$weight) # 9.3%
# WB says 9.8% in 2012; pretty close
# http://data.worldbank.org/indicator/EG.ELC.ACCS.ZS?locations=MW

# The number I really care about
mean(mwi_2013$mobile & !mwi_2013$elect,na.rm=TRUE) # 39.5%
w_mean(mwi_2013$mobile & !mwi_2013$elect,mwi_2013$weight) # 37.6%


setwd(oldwd)
rm(moda,modf,oldwd)
