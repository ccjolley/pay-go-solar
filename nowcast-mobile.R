library(ggplot2)
library(llamar)
library(XLConnect)
source('read-DHS.R')


###############################################################################
# To what extent does the "sigmoidal" wealth/mobile relationship hold
# up for PA countries?
###############################################################################
wealth_mobile <- function(df,text) {
  df %>% removeAttributes %>%
    ggplot(aes(x=wealth,y=mobile)) +
    geom_jitter(size=5,color='tomato',alpha=0.05,width=0) +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    ggtitle(text) +
    theme_classic() 
}

wealth_mobile(rwa_2015,'Rwanda 2015')

###############################################################################
# Compare different countries at a single point in time
###############################################################################
mobile_compare <- function(a,title='') {
  # a is a big data frame coming out of rbind
  ggplot(a,aes(x=wealth,y=mobile,group=label,color=label)) +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    ggtitle(title) +
    theme_classic()
}

rbind(gha_2014,ken_2014,lbr_2013,nga_2013,sle_2013) %>% 
  removeAttributes %>%
  mobile_compare(title='DHS in 2013-14')
# PROBLEM: sen_2014 seems not to have the same wealth scale as others
# Actually not such a big problem; LSMS countries won't be on the same scale 
# either.

###############################################################################
# Compare changes in one country over time
###############################################################################
rbind(gha_2008,gha_2014) %>% removeAttributes %>%
  mobile_compare(title='Ghana 2008-2014')

rbind(ken_2009,ken_2014) %>% removeAttributes %>%
  mobile_compare(title='Kenya 2008-2014')

rbind(lbr_2007,lbr_2013) %>% removeAttributes %>%
  mobile_compare(title='Liberia 2007-2013')

rbind(nga_2008,nga_2013) %>% removeAttributes %>%
  mobile_compare(title='Nigeria 2008-2013')

rbind(rwa_2010,rwa_2015) %>% removeAttributes %>%
  mobile_compare(title='Rwanda 2010-2015')

rbind(sle_2008,sle_2013) %>% removeAttributes %>%
  mobile_compare(title='Sierra Leone 2008-2013')

rbind(uga_2006,uga_2011) %>% removeAttributes %>%
  mobile_compare(title='Uganda 2006-2011')

rbind(zmb_2007,zmb_2014) %>% removeAttributes %>%
  mobile_compare(title='Zambia 2007-2014')

###############################################################################
# Test agreement between my weighted averages and numbers from DHS API
###############################################################################
# % of households with mobile phone
test <- loadDHS(indicators='HC_HEFF_H_MPH',countries='RW',years=2015)
test$Value
w_mean(rwa_2015$mobile,rwa_2015$weight)
# Glad to see that works.

###############################################################################
# Get as many mobile penetration averages as I can from DHS. Focus on SS
# Africa.
###############################################################################
africa <- c('BJ','BF','BU','CM','CV','CF','TD','CG','CD','CI','EK','ER',
            'ET','GA','GM','GH','GN','KE','LS','LB','MD','MW','ML','MZ','NM',
            'NI','NG','RW','SN','SL','ZA','SD','SZ','TZ','TG','UG','ZM','ZW')

afr_mobile <- loadDHS(indicators='HC_HEFF_H_MPH',
                      countries=paste(africa,collapse=',')) %>%
  select(CountryName,SurveyYear,Value)
# 72 data points. 
write.csv(afr_mobile,'DHS_mobile.csv',row.names=FALSE)

###############################################################################
# Get the corresponding data points out of the (overly-pretty) GSMA Excel 
# reports.
###############################################################################
p2frac <- function(p) {
  # convert a percentage string to a number
  p %>% sub('%','',.) %>% as.numeric / 100
}

s2num <- function(s) {
  # get those commas out of there
  s %>% gsub(',','',.) %>% as.numeric
}

get_gsma <- function(fname,yrs) {
  x <- read.csv(fname,skip=2,stringsAsFactors=FALSE)
  qnames <- paste('Q4.',yrs,sep='')
  market <- x[x$Metric=='Unique subscribers' &
                x$Attribute=='Total','Market..Operator']
  agu <- x[x$Metric=='Growth rate, unique subscribers, annual' &
             x$Attribute=='Total',qnames] %>% t %>% p2frac
  pu <- x[x$Metric=='Market penetration, unique subscribers' &
            x$Attribute=='Total',qnames] %>% t %>% p2frac
  agn <- x[x$Metric=='Growth rate, excluding cellular M2M, annual' &
             x$Attribute=='Total' & x$Market..Operator==market,qnames] %>% t %>% p2frac
  p <- x[x$Metric=='Market penetration' & x$Attribute=='Total' & 
           x$Market..Operator==market,qnames] %>% t %>% p2frac
  h <- x[x$Metric=='Herfindahl-Hirschman Index',qnames] %>% t %>% s2num
  res <- cbind(agu,pu,agn,p,h) %>% as.data.frame
  res$CountryName <- market
  res$SurveyYear <- yrs
  names(res) <- c('ann_growth_uniq','penetration_uniq',
                  'ann_growth','penetration','herfindahl',
                  'CountryName','SurveyYear')
  res
}
dirroot <- 'C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/GSMA/'

gsma <- data.frame()
for (n in afr_mobile$CountryName %>% unique) {
  #print(n)
  fname <- paste0(dirroot,n,'.csv')
  yrs <- afr_mobile[afr_mobile$CountryName==n,'SurveyYear']
  gsma <- rbind(gsma,get_gsma(fname,yrs))
}
gsma[gsma$CountryName=="Congo, Democratic Republic", 'CountryName'] <- "Congo Democratic Republic"
gsma[gsma$CountryName=="C?te d'Ivoire", 'CountryName'] <- "Cote d'Ivoire"

###############################################################################
# How well can the subscriber-level GSMA data predict the household-level
# DHS data? 
###############################################################################

j <- plyr::join(afr_mobile,gsma,by=c('CountryName','SurveyYear')) %>% unique

# First, try linear models, omitting country information
lm(j$Value ~ j$penetration_uniq) %>% summary               # R^2 = 0.7914
lm(j$Value ~ j$penetration) %>% summary                    # R^2 = 0.7672
j2 <- j %>% select(-CountryName)
lm(Value ~ .,j2) %>% summary # R^2 = 0.8329

# I tried to beat this with caret and didn't really succeed. 

# Include country as factor variable
lm(Value ~ CountryName,j) %>% summary            # R^2 = 0.601
lm(Value ~ CountryName+SurveyYear,j) %>% summary # R^2 = 0.948
lm(Value ~ .,j) %>% summary                      # R^2 = 0.977
# In addition to the country factor variable, the annual growth rate
# and penetration of unique subscribers seem to be important. The year stops
# being significant when these are factored in.

###############################################################################
# Project 2016 household-level rates
###############################################################################
pa <- c('Ethiopia','Ghana','Kenya','Liberia','Malawi','Nigeria','Rwanda',
        'Senegal','Sierra Leone','Tanzania','Uganda','Zambia')
gsma_2016 <- data.frame()
for (x in pa) {
  fname <- paste0(dirroot,x,'.csv')
  gsma_2016 <- rbind(gsma_2016,get_gsma(fname,2016))
}
fit <- lm(Value ~ .,j)
newvals <- predict(fit,gsma_2016)
wpred <- gsma_2016 %>% 
  dplyr::select(CountryName,SurveyYear) %>% 
  mutate(Value=newvals,
         Value=ifelse(Value>100,100,Value)) %>%
  rbind(afr_mobile[afr_mobile$CountryName %in% pa,])
ggplot(wpred,aes(x=SurveyYear,y=Value,group=CountryName,color=CountryName)) +
  geom_point(size=4) +
  geom_line(size=2) + 
  theme_classic()

# TODO: Some of the mobile adoption numbers are old -- my hypothesis is that I could 
# get more up-to-date numbers by assuming that new mobile subscribers are added in order
# of decreasing wealth (the wealthiest non-users are the most likely to get new phones)
# and adding in new numbers until the total reaches values similar to what we see from
# GSMA, WB, etc. GSMA probably better due to higher time resolution.

# Need a function to add columns to my existing datasets with estimates of who
# gained cell phones by 2016.