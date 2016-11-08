library(haven)
library(dplyr)

###############################################################################
# TODO: I can't remember; does this actually get used in the map I made for the
# report? If not, I can get rid of this.
###############################################################################

oldwd <- getwd()
setwd("C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/Uganda LSMS 2014")

rp_all <- read_dta('RigaPanel_201504_all.dta')
rp_all %>% select(latitude,longitude) %>% unique %>% nrow # 871 clusters

uga_lsms_elect <- rp_all %>%
  select(latitude,longitude,electricity) %>%
  group_by(latitude,longitude) %>%
  summarize(electricity=mean(electricity,na.rm=TRUE)) 

setwd("C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/PAYG-surveys")
write.csv(uga_lsms_elect,'uga_lsms.csv',row.names=FALSE)
