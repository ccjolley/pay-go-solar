library(ggplot2)
library(dplyr)
library(reshape2)

source('read-DHS.R')
source('utils.R')
source('nowcast-mobile.R')
source('nowcast-electric.R')

###############################################################################
# Weighted country-level averages
###############################################################################
f <- function(df) {
  country <- df[1,'adm0']
  mobile_only <- w_mean(df$mobile_now & !df$elect_now,df$weight)
  elect_only <- w_mean(!df$mobile_now & df$elect_now,df$weight)
  both <- w_mean(df$mobile_now & df$elect_now,df$weight)
  neither <- w_mean(!df$mobile_now & !df$elect_now,df$weight)
  data.frame(country=country,mobile_only=mobile_only,elect_only=elect_only,
             both=both,neither=neither)
}

avgs <- rbind(f(eth_2011),f(gha_2014),f(ken_2014),f(lbr_2013),f(mwi_2010),
              f(nga_2013),f(rwa_2015),f(sen_2014),f(sle_2013),f(tza_2016),
              f(uga_2011),f(zmb_2014))

###############################################################################
# Bar plot
###############################################################################
avgs <- arrange(avgs,desc(mobile_only))
m <- avgs %>%
  mutate(Mobile = mobile_only + both,
         blank = mobile_only,
         Electricity = both + elect_only) %>%
  dplyr::select(country,Mobile,blank,Electricity) %>%
  melt('country') %>%
  mutate(pos = ifelse(variable == 'Mobile',1,2),
         country = factor(country,levels=avgs$country))

ggplot(m, aes(x=pos,y=value,fill=variable)) +
  geom_bar(stat='identity', position='stack') +
  facet_grid(country~.,switch='y') +
  coord_flip() +
  scale_fill_manual(values=c('#6cafcc','#ffffff','#ebe85d'),
                    breaks=c('Mobile','Electricity')) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        strip.text.y = element_text(size = 12, angle = 180,hjust=1),
        strip.background = element_blank())