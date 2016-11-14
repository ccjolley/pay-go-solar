library(dplyr)
source('utils.R')
source('nowcast-mobile.R') # also calls read-DHS.R

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


