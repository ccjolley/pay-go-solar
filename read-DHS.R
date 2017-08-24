library(haven)
library(dplyr)
source('utils.R')

###############################################################################
# Get DHS data. The purpose of this file is to read in all of the DHS files
# needed for other visualizations.
#
# mobile = hv243a (0=no, 1=yes)
# electricity = hv206 (0=no, 1=yes)
# wealth = hv271 (divide by 1e5)
# hh weight = hv005 (divide by 1e6)
# adm1 = hv024
###############################################################################
load_dhs <- function(dirname,cname=NULL) {
  rootdir <- "C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/"
  dir <- paste0(rootdir,dirname)
  oldwd <- getwd()
  setwd(dir)
  fname <- list.files(pattern='.+\\.(DTA|dta)')
  tmp_hh <- read_dta(fname)
  if (is.null(cname)) {
    cname <- dirname
  }
  tmp <- data.frame(mobile=tmp_hh$hv243a,
                    elect=tmp_hh$hv206,
                    bank=tmp_hh$hv247,                    
                    wealth=tmp_hh$hv271 / 1e5,
                    adm0=cname,
                    adm1=labelled_to_str(tmp_hh$hv024),
                    weight=tmp_hh$hv005 / 1e6,
                    label=dirname,
                    rural=as.numeric(tmp_hh$hv025 == 2),
                    clust=tmp_hh$hv001)
  names(tmp) <- c('mobile','elect','bank','wealth','adm0','adm1','weight',
                  'label','rural','clust')
  tmp$adm0 <- as.character(tmp$adm0)
  tmp$adm1 <- as.character(tmp$adm1)
  tmp$label <- as.character(tmp$label)
  tmp$mobile[tmp$mobile==9] <- NA
  tmp$elect[tmp$elect==9] <- NA
  tmp$bank[tmp$bank > 1] <- NA
  setwd(oldwd)
  tmp
}

# Most recent DHS for Power Africa countries
eth_2016 <- load_dhs("Ethiopia DHS 2016",'Ethiopia')
gha_2014 <- load_dhs("Ghana DHS 2014",'Ghana')
ken_2014 <- load_dhs("Kenya DHS 2014",'Kenya')
lbr_2013 <- load_dhs("Liberia DHS 2013",'Liberia')
mwi_2016 <- load_dhs("Malawi DHS 2016",'Malawi')
nga_2013 <- load_dhs("Nigeria DHS 2013",'Nigeria')
sen_2015 <- load_dhs("Senegal DHS 2015",'Senegal')
sle_2013 <- load_dhs("Sierra Leone DHS 2013",'Sierra Leone')
tza_2016 <- load_dhs("Tanzania DHS 2016",'Tanzania')
uga_2011 <- load_dhs("Uganda DHS 2011",'Uganda')
rwa_2015 <- load_dhs("Rwanda DHS 2015",'Rwanda')
zmb_2014 <- load_dhs('Zambia DHS 2013-4','Zambia')

# Older DHS surveys containing the same indicators
# eth_2011 <- load_dhs("Ethiopia DHS 2011",'Ethiopia')
# gha_2008 <- load_dhs("Ghana DHS 2008",'Ghana')
# ken_2009 <- load_dhs("Kenya DHS 2008-9",'Kenya')
# lbr_2007 <- load_dhs('Liberia DHS 2007','Liberia')
# mwi_2010 <- load_dhs("Malawi DHS 2010",'Malawi')
# nga_2008 <- load_dhs('Nigeria DHS 2008','Nigeria')
# rwa_2010 <- load_dhs('Rwanda DHS 2010','Rwanda')
# sen_2014 <- load_dhs("Senegal DHS 2014",'Senegal')
# sle_2008 <- load_dhs('Sierra Leone DHS 2008','Sierra Leone')
# uga_2006 <- load_dhs('Uganda DHS 2006','Uganda')
# zmb_2007 <- load_dhs('Zambia DHS 2007','Zambia')

# Make new average CSV for Tanzania 2016
tza_2016 %>% 
  mutate(clust=as.numeric(clust)) %>%
  group_by(clust) %>%
  summarize(mobile=mean(mobile),
            elect=mean(elect),
            bank=mean(bank),
            wealth=mean(wealth),
            adm0=first(adm0),
            adm1=first(adm1),
            weight=mean(weight),
            rural=mean(rural)) %>%
  write.csv('tza_dhs_2016.csv',row.names=FALSE)
