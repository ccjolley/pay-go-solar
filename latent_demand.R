library(ggplot2)
library(llamar)
library(XLConnect)
library(dplyr)
source('read-DHS.R')
source('utils.R')

source('nowcast-mobile.R')
source('nowcast-electric.R')

###############################################################################
# Weighted country-level averages
###############################################################################
f <- function(df) {
  country <- df[1,'adm0']
  mobile <- w_mean(df$mobile_now,df$weight)
  elect <- w_mean(df$elect_now,df$weight)
  latent <- w_mean(df$mobile_now & !df$elect_now,df$weight)
  data.frame(country=country,mobile=mobile,elect=elect,latent=latent)
}

avgs <- rbind(f(eth_2011),f(gha_2014),f(ken_2014),f(lbr_2013),f(mwi_2010),
              f(nga_2013),f(rwa_2015),f(sen_2014),f(sle_2013),f(tza_2010),
              f(uga_2011),f(zmb_2014))
write.csv(avgs,'avgs.csv',row.names=FALSE)

###############################################################################
# Snazzy bar plot
# I think there's something like what I want to do here;
# http://stackoverflow.com/questions/18774632/how-to-produce-stacked-bars-within-grouped-barchart-in-r
###############################################################################
plotme <- avgs %>% 
  mutate(start=0,
         end=mobile,
         type='mobile') %>%
  select(country,start,end,type) %>%
  rbind(avgs %>% 
          mutate(start=latent,
                 end=latent+elect,
                 type='elect') %>%
          select(country,start,end,type)
  )

ggplot(plotme,aes(x=country,y=end)) +
  geom_bar(stat='identity',aes(fill=type),position='dodge') +
  theme_classic() +
  coord_flip()

# TODO: sort countries by latent
