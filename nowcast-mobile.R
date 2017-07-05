library(ggplot2)
library(llamar)
library(XLConnect)
#source('read-DHS.R')
source('utils.R')

###############################################################################
# Get as many mobile penetration averages as I can from DHS. Focus on SS
# Africa.
###############################################################################
africa <- c('BJ','BF','BU','CM','CV','CF','TD','CG','CD','CI','EK','ER',
            'ET','GA','GM','GH','GN','KE','LS','LB','MD','MW','ML','MZ','NM',
            'NI','NG','RW','SN','SL','ZA','SD','SZ','TZ','TG','UG','ZM','ZW')

afr_mobile <- loadDHS(indicators='HC_HEFF_H_MPH',
                      countries=paste(africa,collapse=',')) %>%
  dplyr::select(CountryName,SurveyYear,Value)

###############################################################################
# Get the corresponding data points out of the (overly-pretty) GSMA Excel 
# reports.
###############################################################################
source('read-GSMA.R')

j <- plyr::join(afr_mobile,gsma,by=c('CountryName','SurveyYear')) %>% unique

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

###############################################################################
# Estimate which households acquired a phone between the DHS survey and now.
# Use a combination of geography and wealth to estimate who is most likely
# to add a phone connection.
# If the weighted average is already higher than the target, then nothing will
# be added.
###############################################################################
mobile_now <- function(df,cname) {
  # return df, with a new column df$mobile_now that adds new mobile users in
  # order of decreasing wealth, until the weighted average reaches target
  # from wpred
  target <- wpred[wpred$CountryName==cname & wpred$SurveyYear==2016,'Value']/100
  mod<- df %>% group_by(clust) %>% 
    summarise(clust_avg=mean(mobile,na.rm=TRUE),in_clust=n()) %>%
    plyr::join(df,by='clust') %>%
    mutate(out_avg = clust_avg*in_clust/(in_clust-1) - as.numeric(mobile)/(in_clust-1)) %>%
    dplyr::select(-clust_avg,-in_clust)
  # combine this score with wealth to prioritize likely electrification
  fit <- mod %>% removeAttributes %>%
    glm(mobile~out_avg,data=.,family='binomial')
  mod$pred <- predict(fit,mod,type='response')
  add_us <- mod %>% mutate(n=row_number()) %>%
    filter(mobile==0) %>% arrange(desc(pred)) 
  df$mobile_now <- df$mobile %>% as.numeric
  for (i in add_us$n) {
    if (w_mean(df$mobile_now,df$weight) >= target) {
      break
    }
    df[i,'mobile_now'] <- 1
  }
  df
}

# Now, use predicted values in wpred to add mobile_now columns to my most recent
# DHS datasets
eth_2011 <- mobile_now(eth_2011,'Ethiopia')
gha_2014 <- mobile_now(gha_2014,'Ghana')
ken_2014 <- mobile_now(ken_2014,'Kenya')
lbr_2013 <- mobile_now(lbr_2013,'Liberia')
mwi_2010 <- mobile_now(mwi_2010,'Malawi')
nga_2013 <- mobile_now(nga_2013,'Nigeria')
sen_2014 <- mobile_now(sen_2014,'Senegal')
sle_2013 <- mobile_now(sle_2013,'Sierra Leone')
tza_2016 <- mobile_now(tza_2016,'Tanzania')
uga_2011 <- mobile_now(uga_2011,'Uganda')
rwa_2015 <- mobile_now(rwa_2015,'Rwanda')
zmb_2014 <- mobile_now(zmb_2014,'Zambia')

###############################################################################
# Clean up
###############################################################################

rm(afr_mobile,gsma,gsma_2016,j,wpred,africa,dirroot,fit,fname,n,newvals,pa,x,
   yrs,get_gsma)

