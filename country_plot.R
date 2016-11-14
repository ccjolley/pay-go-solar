###############################################################################
# Plot a set of variables for a specific country. When I get around to making
# a Shiny app I'll probably use something like this to make a custom indicator
# comparator.
###############################################################################

library(dplyr)
library(reshape2)
library(forcats)
source('read-findex.R')

country_plot <- function(country,varlist) {
  gf_wide %>%
    filter(country_name==country) %>%
    select(one_of(varlist)) %>%
    melt %>% 
    mutate(series_code=as.character(variable),
           value=value/100) %>%
    plyr::join(key,by='series_code') %>%
    mutate(series_name=gsub(' *\\(.*\\) *','',series_name),
           series_name=gsub(' *\\[.*\\] *','',series_name)) %>%
    ggplot(aes(y=value,x=fct_reorder(variable,value))) +
    geom_bar(stat='identity',fill='goldenrod') +
    geom_text(hjust=0,aes(y=max(value)/50,label=series_name)) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    ggtitle(country) +
    theme_classic() +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.title = element_blank())
}

country_plot('Nigeria',c('WP15163_4.10','WP11672.10','WP11674.10','WP11673.10',
                         'WP15172_4.10','WP14940_4.10','WP15161_1.10'))
country_plot('Uganda',c('WP15163_4.10','WP11672.10','WP11674.10','WP11673.10',
                         'WP15172_4.10','WP14940_4.10','WP15161_1.10'))