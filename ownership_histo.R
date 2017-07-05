library(ggplot2)
library(ggrepel)
library(dplyr)
library(XLConnect)
source('read-findex.R')

###############################################################################
# Histogram of mobile account ownership rates
###############################################################################
tmp <- gf_wide %>% select(country_name,m_acct=WP15163_4.1) %>%
  na.omit %>%
  filter(!grepl(')',country_name)) %>%
  filter(!grepl('income',country_name)) %>%
  filter(!country_name %in% c('World','South Asia')) %>%
  mutate(m_acct = m_acct/100)

top <- tmp %>% filter(m_acct > 0.02) %>% arrange(m_acct) %>%
  mutate(y = c(28:1,8:1,4:1,3:1,1) / 1.5,
         x=m_acct,
         x=x+ifelse(country_name=='Congo, Dem. Rep.',0.02,0)) 

ggplot(tmp,aes(x=m_acct)) +
  geom_histogram(fill='lightskyblue1',color='gray80',binwidth=0.02) +
  theme_classic() +
  geom_text(data=top,aes(x=x,y=y,label=country_name),hjust=0) +
  xlab('Mobile account ownership rate') +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

###############################################################################
# The histogram might not be the best way to do this. Could I scatter mobile
# account ownership against something else?
###############################################################################

# Try UN 2015 HDI

hdi <- readWorksheetFromFile('C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/Global Findex/2015_statistical_annex_tables_all.xls',1,startRow=9)
names(hdi) <- c('rank','country_name','hdi','x1','life_expect','x2','expect_ed','x3',
                'mean_ed','x4','gni_per_captia','x5','gni_minus_hdi_rank')
hdi <- hdi %>% 
  select(-x1,-x2,-x3,-x4,-x5) %>%
  filter(!is.na(country_name),
         !is.na(hdi)) %>%
  mutate(hdi=as.numeric(hdi)) 

# convern UN to WB names for join
hdi[hdi$country_name=='Bolivia (Plurinational State of)','country_name'] <- 'Bolivia'
hdi[hdi$country_name=='Venezuela (Bolivarian Republic of)','country_name'] <- 'Venezuela, RB'
hdi[hdi$country_name=='Congo','country_name'] <- 'Congo, Rep.'
hdi[hdi$country_name=='Congo (Democratic Republic of the)','country_name'] <- 'Congo, Dem. Rep.'
hdi[hdi$country_name=='Tanzania (United Republic of)','country_name'] <- 'Tanzania'
hdi[hdi$country_name=='Iran (Islamic Republic of)','country_name'] <- 'Iran, Islamic Rep.'
hdi[hdi$country_name=='Egypt','country_name'] <- 'Egypt, Arab Rep.'
hdi[hdi$country_name=='Viet Nam','country_name'] <- 'Vietnam'
hdi[grep('voire',hdi$country_name),'country_name'] <- 'Cote d\'Ivoire'

# Somalia only has a UNDP estimate available 
# see https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index#Countries_missing_from_latest_report
hdi[hdi$country_name=='Somalia','hdi'] <- 0.349

j <- plyr::join(tmp,hdi,by='country_name') %>%
  select(country_name,m_acct,hdi) %>%
  na.omit() %>%
  mutate(label_text = ifelse(m_acct > 0.02,country_name,NA),
         fill = ifelse(country_name %in% c('Nigeria','Rwanda','Tanzania','Uganda','Zambia'),1,2),
         fill = as.factor(fill))

ggplot(j,aes(x=m_acct,y=hdi,label=label_text)) +
  geom_point(size=4,aes(color=fill)) +
  geom_text_repel() +
  theme_classic() +
  scale_x_continuous(labels = scales::percent) +
  xlab('Mobile money account ownership') +
  ylab('UN Human Development Index') +
  theme(axis.ticks=element_blank(),
        axis.text.y = element_blank(),
        legend.position='none')

