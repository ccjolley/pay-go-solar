library(ggplot2)
library(llamar)

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

# TODO: How do DHS penetration rates compare to those other sources? Can I use them 
# to estimate what a comparable DHS penetration value would be for a certain country and year?
# Linear regression might do it; something from caret might be better.
# The different measurements don't exactly compare apples to apples.

# TODO: Some of the mobile adoption numbers are old -- my hypothesis is that I could 
# get more up-to-date numbers by assuming that new mobile subscribers are added in order
# of decreasing wealth (the wealthiest non-users are the most likely to get new phones)
# and adding in new numbers until the total reaches values similar to what we see from
# GSMA, WB, etc. GSMA probably better due to higher time resolution.

# TODO: Can I get comparable estimates (i.e. weighted averages) using the DHS API from
# other countries/years?