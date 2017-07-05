# I want choropleths of electricity and mobile access rates 

library(haven)
library(dplyr)
library(foreign)
setwd("C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/PAYG-surveys")
source('utils.R')
source('read-DHS.R')

load_geo <- function(dname,fname,joinme) {
  rootdir <- "C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/"
  paste0(rootdir,dname) %>% setwd
  raw <- read.dbf(fname) 
  paste0('Max cluster in ',fname,': ',max(raw$DHSCLUST)) %>% print
  paste0('Max cluster to join: ',max(joinme$clust)) %>% print
  n_zero <- raw %>% filter(LATNUM==0,LONGNUM==0) %>% nrow
  paste0('Removed ',n_zero,' rows with lat,long = 0') %>% print
  raw %>%
    dplyr::select(lat=LATNUM,long=LONGNUM,clust=DHSCLUST) %>%
    filter(lat != 0, long != 0) %>%
    plyr::join(joinme,by='clust') %>% 
    group_by(clust) %>%
    summarize(lat=mean(lat),long=mean(long),mobile=mean(mobile,na.rm=TRUE),
              elect=mean(elect,na.rm=TRUE),bank=mean(bank,na.rm=TRUE),
              wealth=mean(wealth,na.rm=TRUE),adm0=unique(adm0),adm1=unique(adm1),
              label=unique(label),rural=mean(rural,na.rm=TRUE),
              weight=sum(weight))
}

adm1_choro <- function(df) {
  df %>%
    group_by(adm1) %>%
    summarize(e=w_mean(elect,weight),
              m=w_mean(mobile,weight))
}

cap1 <- function(x) {
  paste0(toupper(substring(x,1,1)),substring(x,2))
}

cap <- function(x) {
  strsplit(x, " ")[[1]] %>%
    sapply(function(y) ifelse(y %in% c('and','es','the','or'),
                              y,cap1(y))) %>%
    paste(collapse=' ')
}
    
########## Uganda ############

uga_2011 <- load_dhs("Uganda DHS 2011",'Uganda')
uga_2011$adm1 %>% unique

# 10 different adm1 units -- do I have a shapefile for these?
# Natural earth has district-level divisions and groups these into 
# 16 different "regions". 

uga_2011_geo <- load_geo('Uganda DHS 2011','UGGE61FL.dbf',uga_2011)
ggplot(uga_2011_geo,aes(x=long,y=lat,group=adm1,color=adm1)) +
  geom_point(size=3)
write.csv(uga_2011_geo,'uga_dhs_2011.csv',row.names=FALSE)

# get weighted averages of mobile ownership and electric access
# for each "adm1" area

uga_adm <- uga_2011_geo %>%
  group_by(adm1) %>%
  summarize(e=w_mean(elect,weight),
            m=w_mean(mobile,weight))


write.csv(uga_adm,'uga_dhs_adm1.csv',row.names=FALSE)

########## Tanzania ############

tza_2016 <- load_dhs("Tanzania DHS 2016",'Tanzania') %>%
  mutate(adm1 = sapply(adm1,cap),
         adm1 = recode(adm1,
                       'Kaskazini Pemba' = 'Pemba North',
                       'Kaskazini Unguja' = 'Zanzibar North',
                       'Kusini Pemba' = 'Pemba South',
                       'Kusini Unguja' = 'Zanzibar South and Central',
                       'Mjini Magharibi' = 'Zanzibar West'))

tza_2016_geo <- load_geo('Tanzania DHS 2016','TZGE7AFL.dbf',tza_2016) 

ggplot(tza_2016_geo,aes(x=long,y=lat,group=adm1,color=adm1)) +
  geom_point(size=3)

write.csv(tza_2016_geo,'tzn_dhs_2016.csv',row.names=FALSE)

adm1_choro(tza_2016_geo) %>%
  write.csv('tza_dhs_adm1.csv',row.names=FALSE)

########## Nigeria ############

nga_2013 <- load_dhs("Nigeria DHS 2013",'Nigeria')
nga_2013$adm1 %>% unique
# These don't correspond to Nigeria's actual adm1 units; I'll need to
# make a new shapefile that lumps them together.

nga_2013_geo <- load_geo('Nigeria DHS 2013','NGGE6AFL.dbf',nga_2013) 

ggplot(nga_2013_geo,aes(x=long,y=lat,group=adm1,color=adm1)) +
  geom_point(size=3)

write.csv(nga_2013_geo,'nga_dhs_2013.csv',row.names=FALSE)

adm1_choro(nga_2013_geo) %>%
  write.csv('nga_dhs_adm1.csv',row.names=FALSE)

########### Zambia ##############
zmb_2014 <- load_dhs("Zambia DHS 2013-4",'Zambia') %>%
  mutate(adm1 = sapply(adm1,cap),
         adm1 = dplyr::recode(adm1,'North Western' = 'North-Western'))
zmb_2014$adm1 %>% unique
# Works with GADM, not NE

zmb_2014_geo <- load_geo('Zambia DHS 2013-4','ZMGE61FL.dbf',zmb_2014) 

ggplot(zmb_2014_geo,aes(x=long,y=lat,group=adm1,color=adm1)) +
  geom_point(size=3)

write.csv(zmb_2014_geo,'zmb_dhs_2014.csv',row.names=FALSE)

adm1_choro(zmb_2014_geo) %>%
  write.csv('zmb_dhs_adm1.csv',row.names=FALSE)

########### Rwanda ##############

rwa_2015 <- load_dhs("Rwanda DHS 2015",'Rwanda') %>%
  mutate(adm1 = sapply(adm1,cap),
         adm1 = dplyr::recode(adm1,
                'North' = 'Northern',
                'South' = 'Southern',
                'East' = 'Eastern',
                'West' = 'Western'))
rwa_2015$adm1 %>% unique

rwa_2015_geo <- load_geo('Rwanda DHS 2015','RWGE71FL.dbf',rwa_2015) 

ggplot(rwa_2015_geo,aes(x=long,y=lat,group=adm1,color=adm1)) +
  geom_point(size=3)

write.csv(rwa_2015_geo,'rwa_dhs_2015.csv',row.names=FALSE)

adm1_choro(rwa_2015_geo) %>%
  write.csv('rwa_dhs_adm1.csv',row.names=FALSE)
