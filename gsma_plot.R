###############################################################################
# GSMA sample plot
###############################################################################
library(dplyr)
library(ggplot2)

source('read-GSMA.R')

pa <- c('Ethiopia','Ghana','Kenya','Liberia','Malawi','Nigeria','Rwanda',
        'Senegal','Sierra Leone','Tanzania','Uganda','Zambia')
gsma_pa <- plyr::ldply(pa[pa != 'South Africa'],function(x) 
  get_gsma(paste0(dirroot,x,'.csv'),2000:2016))

gsma_pa %>% 
  filter(CountryName %in% c('Nigeria','Uganda','Rwanda','Tanzania','Zambia')) %>%
  ggplot(aes(x=SurveyYear,y=penetration_uniq,group=CountryName,
             color=CountryName)) +
  geom_line(size=2) +
  theme_classic() +
  xlab('Year') +
  ylab('Penetration, unique subscribers') +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.ticks=element_blank(),
        legend.title=element_blank())