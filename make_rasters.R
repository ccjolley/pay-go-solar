###############################################################################
# This script doesn't actually *make* rasters; it makes CSV files containing
# lat/long coordinates and averaged values of key variables for each of those
# locations. I used ArcMap to make rasters from these CSV files.
###############################################################################

library(foreign)

source('read-DHS.R')
oldwd <- getwd()

load_geo <- function(dname,fname,joinme) {
  rootdir <- "C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/"
  paste0(rootdir,dname) %>% setwd
  nga_2013_geo <- read.dbf(fname) %>%
    select(lat=LATNUM,long=LONGNUM,clust=DHSCLUST) %>%
    plyr::join(joinme,by='clust') %>% 
    group_by(clust) %>%
    summarize(lat=mean(lat),long=mean(long),mobile=mean(mobile,na.rm=TRUE),
              elect=mean(elect,na.rm=TRUE),bank=mean(bank,na.rm=TRUE),
              wealth=mean(wealth,na.rm=TRUE),adm0=unique(adm0),adm1=unique(adm1),
              label=unique(label),rural=mean(rural,na.rm=TRUE))
}

nga_2013_geo <- load_geo('Nigeria DHS 2013','NGGE6AFL.dbf',nga_2013)
uga_2011_geo <- load_geo('Uganda DHS 2011','UGGE61FL.dbf',uga_2011)
zmb_2014_geo <- load_geo('Zambia DHS 2013-4','ZMGE61FL.dbf',zmb_2014)
tza_2010_geo <- load_geo('Tanzania DHS 2010','TZGE61FL.dbf',tza_2010)
rwa_2015_geo <- load_geo('Rwanda DHS 2015','RWGE71FL.dbf',rwa_2015)

setwd(oldwd)

# output CSV files

write.csv(nga_2013_geo,'nga_dhs_2013.csv',row.names=FALSE)
write.csv(uga_2011_geo,'uga_dhs_2011.csv',row.names=FALSE)
write.csv(zmb_2014_geo,'zmb_dhs_2014.csv',row.names=FALSE)
write.csv(tza_2010_geo,'tza_dhs_2010.csv',row.names=FALSE)
write.csv(rwa_2015_geo,'rwa_dhs_2015.csv',row.names=FALSE)
