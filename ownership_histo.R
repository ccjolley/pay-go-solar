library(ggplot2)
library(ggrepel)
library(dplyr)
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