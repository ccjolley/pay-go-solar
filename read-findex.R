###############################################################################
# Load and clean Findex data.
# The purpose of this file is to load the Findex data I've already downloaded
# and make it available for further analysis.
#
# TODO: Move composite indices out into a different file; I don't usually need
# them.
# TODO: Make sure that only the relevant variables survive at the end of this
# file.
###############################################################################

library(tidyr)
library(dplyr)

oldwd <- getwd()
setwd("C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/Global Findex")
gf <- read.csv('00d7fbe8-2939-4a51-8972-efb244252327_Data.csv',
               encoding="UTF-8",stringsAsFactors=FALSE)

names(gf) <- c('country_name','country_code','series_name','series_code',
               'val_2011','val_2014','mrv')

# If both w2 and w1 indicators are present, I want the w2 one. If only w1 is present, that's
# the one I want

gf_w2 <- gf %>% 
  filter(country_code != '',
         grepl('\\[w2\\]',gf$series_name)) %>% 
  mutate(series_name = sub(' \\[w2\\]','',series_name))

gf_w1 <- gf %>% 
  filter(country_code != '',
         grepl('\\[w1\\]',gf$series_name)) %>% 
  mutate(series_name = sub(' \\[w1\\]','',series_name))

just1 <- setdiff(gf_w1$series_name,gf_w2$series_name)

gf_clean <- rbind(gf_w2,gf_w1[gf_w1$series_name %in% just1,]) %>%
  select(country_name,series_name,series_code,mrv) %>%
  mutate(mrv = mrv %>% as.character %>% as.numeric)

gf_wide <- gf_clean %>% 
  select(country_name,series_code,mrv) %>%
  spread(series_code,mrv)

key <- gf %>% select(series_name,series_code) %>% unique

rm(gf,gf_w1,gf_w2,just1)
setwd(oldwd)

###############################################################################
# Focus on Power Africa countries (add to interest list as needed)
###############################################################################

pa <- c('Ethiopia','Ghana','Kenya','Malawi','Nigeria','Rwanda',
        'Senegal','Sierra Leone','South Africa','Tanzania','Uganda','Zambia')

# Hardly any information is available for Liberia (only 2 of 477 variables), so
# I'm not including it. 

gf_pa <- gf_wide[gf_wide$country_name %in% pa,]

# Visualization function
pa_plot <- function(code,country=NULL) {
  tmp <- gf_pa[,c('country_name',code)]
  names(tmp) <- c('country','value')
  title <- key[key$series_code==code,'series_name'] %>% 
    gsub(' \\[w2\\]','',.)
  highlight_bar(tmp,title=title,hi_country=country)
}

# pa_plot('WP14887_7.10','Nigeria') # Account at a financial institution, rural
# pa_plot('WP14918.1','Nigeria')    # Bought from a store on credit
# pa_plot('WP15163_4.1','Nigeria')  # Mobile accts
# pa_plot('WP15163_4.8','Nigeria')  # Mobile accts, poorest 40%
# pa_plot('WP14934.1','Nigeria')    # Received domestic remittances
# pa_plot('WP14928.1','Nigeria')    # Sent domestic remittances



