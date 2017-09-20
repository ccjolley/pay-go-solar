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
library(forcats)
library(reshape2)

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

###############################################################################
# The findex_plot function comes from the Shiny app code; I don't know if I 
# also have it in this repo somewhere else. Can't find it if so.
###############################################################################

findex_plot <- function(clist,rlist,code='WP14887_7.1',textsize=4) {
  if (length(c(clist,rlist))==0) { return(NULL) }
  tmp <- plyr::ldply(c(clist,rlist),function(c) {
    v <- gf_wide[gf_wide$country_name==c,code]
    r <- ifelse(c %in% rlist,1,0)
    data.frame(country=c,value=v,region=r)
  }) %>% 
    mutate(plotorder = rank(value) + (1-region)*1000,
           region = as.factor(region)) %>%
    na.omit
  title <- key[key$series_code==code,'series_name'] %>% 
    gsub(' \\[.*\\]','',.) %>%
    gsub(' \\(.*\\)','',.)
  tmp$text <- round(tmp$value) %>% paste0('%')
  text_df <- tmp %>% 
    mutate(x=fct_reorder(country,plotorder)) %>%
    dplyr::select(country,text,value,x) %>% 
    mutate(text=as.character(text),country=as.character(country)) %>%
    melt(id.vars=c('value','x'),value.name='plotme') %>%
    mutate(hjust=ifelse(variable=='country',0,-0.1),
           y=ifelse(variable=='country',max(value)/40,value),
           plotme=as.character(plotme)) %>%
    dplyr::select(plotme,hjust,x,y)
  ggplot(tmp,aes(x=fct_reorder(country,plotorder),y=value)) +
    geom_bar(stat='identity',aes(fill=region)) +
    geom_text(data=text_df,aes(x=x,y=y,hjust=hjust,label=plotme),check_overlap=TRUE,size=textsize) +
    coord_flip() +
    ggtitle(title) +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          legend.position = "none")
}

# plot for September 2017 webinar
findex_plot(clist=c('Kenya','Uganda','Tanzania','Rwanda','Nigeria'),
            rlist=c('Sub-Saharan Africa (developing only)','Low & middle income'),
            code='WP14940_4.1',textsize=4)