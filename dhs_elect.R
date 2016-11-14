###############################################################################
# Plot of electrification growth from DHS API.
###############################################################################

library(dplyr)
library(llamar)
library(ggplot2)

dhs_elect <- loadDHS(indicators='HC_ELEC_H_ELC',
                     countries='NG,RW,TZ,UG,ZM') %>%
  mutate(elect=Value) %>%
  dplyr::select(CountryName,SurveyYear,elect) 

dhs_elect %>% 
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