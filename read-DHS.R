library(haven)
library(dplyr)
source('utils.R')

###############################################################################
# get DHS data
# mobile = hv243a (0=no, 1=yes)
# electricity = hv206 (0=no, 1=yes)
# wealth = hv271 (divide by 1e5)
# hh weight = hv005 (divide by 1e6)
# adm1 = hv024
###############################################################################
# TODO: should add a variable to the column with the (median?) survey date
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
                    wealth=tmp_hh$hv271 / 1e5,
                    adm0=cname,
                    adm1=labelled_to_str(tmp_hh$hv024),
                    weight=tmp_hh$hv005 / 1e6,
                    label=dirname,
                    clust=tmp_hh$hv001)
  names(tmp) <- c('mobile','elect','wealth','adm0','adm1','weight','label','clust')
  tmp$adm0 <- as.character(tmp$adm0)
  tmp$adm1 <- as.character(tmp$adm1)
  tmp$label <- as.character(tmp$label)
  tmp$mobile[tmp$mobile==9] <- NA
  tmp$elect[tmp$elect==9] <- NA
  setwd(oldwd)
  tmp
}


eth_2011 <- load_dhs("Ethiopia DHS 2011",'Ethiopia')
gha_2014 <- load_dhs("Ghana DHS 2014",'Ghana')
ken_2014 <- load_dhs("Kenya DHS 2014",'Kenya')
lbr_2013 <- load_dhs("Liberia DHS 2013",'Liberia')
mwi_2010 <- load_dhs("Malawi DHS 2010",'Malawi')
nga_2013 <- load_dhs("Nigeria DHS 2013",'Nigeria')
sen_2014 <- load_dhs("Senegal DHS 2014",'Senegal')
sle_2013 <- load_dhs("Sierra Leone DHS 2013",'Sierra Leone')
tza_2010 <- load_dhs("Tanzania DHS 2010",'Tanzania')
uga_2011 <- load_dhs("Uganda DHS 2011",'Uganda')
rwa_2015 <- load_dhs("Rwanda DHS 2015",'Rwanda')
zmb_2014 <- load_dhs('Zambia DHS 2013-4','Zambia')

# It looks like the indicators I need were also present in DHS-V, so we can 
# go back to older data.
gha_2008 <- load_dhs("Ghana DHS 2008",'Ghana')
ken_2009 <- load_dhs("Kenya DHS 2008-9",'Kenya')
lbr_2007 <- load_dhs('Liberia DHS 2007','Liberia')
nga_2008 <- load_dhs('Nigeria DHS 2008','Nigeria')
rwa_2010 <- load_dhs('Rwanda DHS 2010','Rwanda')
sle_2008 <- load_dhs('Sierra Leone DHS 2008','Sierra Leone')
uga_2006 <- load_dhs('Uganda DHS 2006','Uganda')
zmb_2007 <- load_dhs('Zambia DHS 2007','Zambia')
