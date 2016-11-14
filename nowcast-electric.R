library(llamar)
library(ggplot2)

###############################################################################
# TODO: I want this file to just contain the code I need to update the data
# frames returned by read-DHS.R to contain estimates of the current 
# electrification status of DHS households. The exploratory stuff that I use
# to justify my models belongs in a separate file (probably an Rmd).
###############################################################################

###############################################################################
# Plot historical household electrification rates from DHS API
###############################################################################

dhs_elect <- loadDHS(indicators='HC_ELEC_H_ELC',
                     countries='ET,GH,KE,LB,MW,NG,RW,SN,SL,TZ,UG,ZM') %>%
  mutate(elect=Value) %>%
  dplyr::select(CountryName,SurveyYear,elect) 

dhs_elect %>% 
  filter(CountryName %in% c('Nigeria','Uganda','Tanzania','Rwanda','Zambia')) %>%
  mutate(elect=elect/100) %>%
  ggplot(aes(x=SurveyYear,y=elect,group=CountryName,color=CountryName)) +
    geom_point(size=4) +
    geom_line(size=2) +
    xlab('Year') +
    ylab('Electricity access') + 
    theme_classic() +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.ticks=element_blank(),
          legend.title=element_blank())

###############################################################################
# Try a linear model to get at electricity access in 2016
###############################################################################

elect_pred <- function(country,year=2016) {
  dhs_elect %>% filter(CountryName==country) %>%
    lm(elect ~ SurveyYear,data=.) %>%
    predict(data.frame(SurveyYear=year))
}
  
newpts <- data.frame(CountryName=unique(dhs_elect$CountryName),
                     SurveyYear=2016)
for (n in newpts$CountryName) {
  newpts[newpts$CountryName==n,'elect'] <- elect_pred(n)
}

rbind(dhs_elect,newpts) %>% # filter(CountryName=='Nigeria') %>%
  ggplot(aes(x=SurveyYear,y=elect,group=CountryName,color=CountryName)) +
  geom_point(size=4) +
  geom_line(size=2) +
  theme_classic()



###############################################################################
# Use a combination of geography and wealth to predict electrification. If
# the weighted electrification rate is already higher than the target, then 
# don't do anything.
###############################################################################
elect_now <- function(df,cname) {
  target <- newpts[newpts$CountryName==cname & newpts$SurveyYear==2016,'elect']/100
  # for each hh, get the fraction of their neighbors who have electricity
  mod<- df %>% group_by(clust) %>% 
    summarise(clust_avg=mean(elect,na.rm=TRUE),in_clust=n()) %>%
    plyr::join(df,by='clust') %>%
    mutate(out_avg = clust_avg*in_clust/(in_clust-1) - as.numeric(elect)/(in_clust-1)) %>%
    select(-clust_avg,-in_clust)
  # combine this score with wealth to prioritize likely electrification
  fit <- mod %>% removeAttributes %>%
    glm(elect~out_avg,data=.,family='binomial')
  mod$pred <- predict(fit,mod,type='response')
  # add new connections until target is reached
  add_us <- mod %>% mutate(n=row_number()) %>%
    filter(elect==0) %>% arrange(desc(pred)) 
  df$elect_now <- df$elect %>% as.numeric
  for (i in add_us$n) {
    if (w_mean(df$elect_now,df$weight) >= target) {
      break
    }
    df[i,'elect_now'] <- 1
  }
  df
}

eth_2011 <- elect_now(eth_2011,'Ethiopia')
gha_2014 <- elect_now(gha_2014,'Ghana')
ken_2014 <- elect_now(ken_2014,'Kenya')
lbr_2013 <- elect_now(lbr_2013,'Liberia')
mwi_2010 <- elect_now(mwi_2010,'Malawi')
nga_2013 <- elect_now(nga_2013,'Nigeria')
sen_2014 <- elect_now(sen_2014,'Senegal')
sle_2013 <- elect_now(sle_2013,'Sierra Leone')
tza_2010 <- elect_now(tza_2010,'Tanzania')
uga_2011 <- elect_now(uga_2011,'Uganda')
rwa_2015 <- elect_now(rwa_2015,'Rwanda')
zmb_2014 <- elect_now(zmb_2014,'Zambia')

###############################################################################
# Plot for data 1-pager. Keep Ghana and Malawi in there to show extremes.
###############################################################################

dhs_elect %>%  
  filter(CountryName %in% c('Nigeria','Ghana','Malawi')) %>%
  ggplot(aes(x=SurveyYear,y=elect,group=CountryName,color=CountryName)) +
  geom_point(size=4) +
  geom_line(size=2) + 
  ylab('Percent of households electrified') +
  xlab('DHS survey year') +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        legend.title=element_blank())


