###############################################################################
# TODO: I'm not entirely sure what this is or whether it works.
###############################################################################

###############################################################################
# What about broad geographic coverage?
###############################################################################

mc <- function(df,thresh=0.5) {
  tmp <- df %>% filter(rural==1) %>%
    group_by(clust) %>%
    summarize(mean_mobile = mean(mobile_now==1,na.rm=TRUE))
  mean(tmp$mean_mobile > thresh,na.rm=TRUE)*100  
}

mobile_coverage <- data.frame(country=c('Ethiopia','Ghana','Kenya','Liberia','Malawi','Nigeria',
                                        'Senegal','Sierra Leone','Tanzania','Uganda','Rwanda',
                                        'Zambia'),
                              value=c(mc(eth_2011),mc(gha_2014),mc(ken_2014),mc(lbr_2013),
                                      mc(mwi_2010),mc(nga_2013),mc(sen_2014),mc(sle_2013),
                                      mc(tza_2010),mc(uga_2011),mc(rwa_2015),mc(zmb_2014)))

highlight_bar(mobile_coverage)

wpred %>% filter(SurveyYear==2016) %>% 
  select(country=CountryName,ownership=Value) %>%
  plyr::join(mobile_coverage,by='country') %>%
  rename(coverage=value) %>%
  ggplot(aes(x=ownership,y=coverage,label=country)) +
  geom_point() +
  geom_smooth(method='lm',alpha=0.3) +
  geom_text(vjust=1) +
  theme_classic()

lbr_2013 %>% filter(rural==1) %>% summarize(m=mean(mobile_now,na.rm=TRUE))
lbr_2013 %>% filter(rural==0) %>% summarize(m=mean(mobile_now,na.rm=TRUE))

uga_2011 %>% filter(rural==1) %>% summarize(m=mean(mobile_now,na.rm=TRUE))
uga_2011 %>% filter(rural==0) %>% summarize(m=mean(mobile_now,na.rm=TRUE))

highlight_bar(mobile_coverage)

###############################################################################
# GSMA sample plots
###############################################################################

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
