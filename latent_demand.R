library(ggplot2)
library(llamar)
library(XLConnect)
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
              f(nga_2013),f(rwa_2015),f(sen_2014),f(sle_2013),f(tza_2010),
              f(uga_2011),f(zmb_2014))
write.csv(avgs,'avgs.csv',row.names=FALSE)

###############################################################################
# Snazzy bar plot
###############################################################################
avgs <- arrange(avgs,desc(mobile_only))
m <- avgs %>%
  mutate(Mobile = mobile_only + both,
         blank = mobile_only,
         Electricity = both + elect_only) %>%
  select(country,Mobile,blank,Electricity) %>%
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


###############################################################################
# Overall rankings (for PPT)
###############################################################################

data.frame(country=c('Kenya','South Africa','Tanzania','Uganda','Ghana',
                     'Rwanda','Zambia','Liberia','Malawi','Nigeria',
                     'Senegal','Sierra Leone','Ethiopia'),
           value=c(5,3,3,3,2,2,2,1,1,1,1,1,0)) %>%
  highlight_bar(pcent=FALSE)

###############################################################################
# Charley was interested in a similar-looking plot for mobile money and 
# mobile access
###############################################################################
avgs2 <- avgs %>% mutate(Mobile=mobile_only+both) %>%
  select(country_name=country,Mobile) %>%
  plyr::join(mm_all,by='country_name') %>%
  select(country=country_name,Mobile,Mobile_Money=WP15163_4.1) %>%
  na.omit %>%
  mutate(Mobile_Money = Mobile_Money / 100,
    blank = Mobile - Mobile_Money) %>%
  select(country,Mobile,blank,Mobile_Money) %>%
  arrange(desc(blank))
  
m2 <- avgs2 %>% melt('country') %>%
  mutate(pos = ifelse(variable == 'Mobile',1,2),
         country = factor(country,levels=avgs2$country))
  
ggplot(m2, aes(x=pos,y=value,fill=variable)) +
  geom_bar(stat='identity', position='stack') +
  facet_grid(country~.,switch='y') +
  coord_flip() +
  scale_fill_manual(values=c('mediumorchid','white','aquamarine4'),
                    breaks=c('Mobile','Mobile_Money')) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        strip.text.y = element_text(size = 12, angle = 180,hjust=1),
        strip.background = element_blank())

###############################################################################
# Just because I'm curious, how well do DHS figures on bank account ownership
# (for countries surveyed in 2013-15) agree with the 2014 Findex indicator?
###############################################################################
bank_compare <- data.frame(
  country_name=c('Ghana','Kenya','Nigeria','Rwanda','Senegal','Sierra Leone',
            'Zambia'),
  dhs=c(w_mean(gha_2014$bank,gha_2014$weight), 
        w_mean(ken_2014$bank,ken_2014$weight), 
        w_mean(nga_2013$bank,nga_2013$weight), 
        w_mean(rwa_2015$bank,rwa_2015$weight),
        w_mean(sen_2014$bank,sen_2014$weight),
        w_mean(sle_2013$bank,sle_2013$weight),
        w_mean(zmb_2014$bank,zmb_2014$weight))
) %>%
  plyr::join(gf_wide,by='country_name') %>% 
  select(country=country_name,dhs,findex=WP14887_7.1) %>%
  mutate(dhs=dhs*100)

ggplot(bank_compare,aes(x=dhs,y=findex,label=country)) +
  geom_point(size=4,color='chocolate2') +
  geom_text(vjust=1.2) +
  xlab('DHS: Percent with bank account') +
  ylab('Findex: Percent with account at financial institution') +
  geom_abline(slope=1,intercept=0,color='deepskyblue4',size=1) +
  annotate('text',label='Blue line = equal',color='deepskyblue4',x=45,y=52) +
  scale_x_continuous(limits=c(0,55)) +
  scale_y_continuous(limits=c(0,55)) +
  theme_classic()
  
###############################################################################
# DHS API doesn't actually have any information on bank account ownership. 
# World Bank doesn't seem to have started asking anyone about this until the
# Findex in 2011-14. This means that I can't actually get access to historical
# trends from a single source. What I can do, is test the hypothesis that, 
# at the time the DHS surveys were taken, we can correlate bank account 
# ownership with electricity access.
##############################################################################

f3 <- function(df) {
  country <- df[1,'adm0']
  elect_only <- w_mean(df$elect & !df$bank,df$weight)
  bank_only <- w_mean(!df$elect & df$bank,df$weight)
  both <- w_mean(df$elect & df$bank,df$weight)
  neither <- w_mean(!df$elect & !df$bank,df$weight)
  data.frame(country=country,elect_only=elect_only,bank_only=bank_only,
             both=both,neither=neither)
}

avgs3 <- rbind(f3(eth_2011),f3(gha_2014),f3(ken_2014),f3(lbr_2013),f3(mwi_2010),
              f3(nga_2013),f3(rwa_2015),f3(sen_2014),f3(sle_2013),f3(tza_2010),
              f3(uga_2011),f3(zmb_2014)) %>% 
  arrange(desc(bank_only))

m3 <- avgs3 %>%
  mutate(Electricity = elect_only + both,
         blank = bank_only,
         Bank = both + bank_only) %>%
  select(country,Bank,blank,Electricity) %>%
  melt('country') %>%
  mutate(pos = ifelse(variable == 'Bank',1,2),
         country = factor(country,levels=avgs3$country))

ggplot(m3, aes(x=pos,y=value,fill=variable)) +
  geom_bar(stat='identity', position='stack') +
  facet_grid(country~.,switch='y') +
  coord_flip() +
  scale_fill_manual(values=c('steelblue4','white','gold'),
                    breaks=c('Electricity','Bank')) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        strip.text.y = element_text(size = 12, angle = 180,hjust=1),
        strip.background = element_blank())
