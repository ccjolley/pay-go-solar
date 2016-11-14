###############################################################################
# Plot to illustrate the urban-rural gaps for several different indicators.
###############################################################################

library(dplyr)
library(reshape2)
library(forcats)
library(ggplot2)
source('read-findex.R')

gap_plot <- function(cname) {
  gap <- gf_wide %>% 
    filter(country_name==cname) %>%
    select(WP15163_4.1,WP11672.1,WP11674.1,WP11673.1,
           WP15172_4.1,WP14940_4.1,WP15161_1.1,
           WP15163_4.10,WP11672.10,WP11674.10,WP11673.10,
           WP15172_4.10,WP14940_4.10,WP15161_1.10) %>%
    melt %>%
    dplyr::rename(series_code=variable) %>%
    plyr::join(key,by='series_code') %>%
    mutate(series_name=gsub(' *\\(.*\\) *','',series_name),
           series_name=gsub(' *\\[.*\\] *','',series_name)) %>%
    dplyr::arrange(series_name) %>%
    mutate(group=1:7 %>% rep(2) %>% sort) %>%
    rbind(data.frame(series_code=as.character(1:7),value=NA,
                     series_name=NA,group=1:7))
  
  gap <- gap %>% group_by(group) %>% 
    summarize(m_value=mean(value,na.rm=TRUE)) %>%
    plyr::join(gap,by='group') %>%
    mutate(group=factor(group),
           m_value = m_value+ifelse(!grepl(', rural',series_name),1e-6,0),
           m_value = m_value+ifelse(is.na(value),2e-6,0),
           ur = ifelse(grepl(', rural',series_name),'rural',paste0('all ',cname)),
           ur = ifelse(is.na(value),NA,ur),
           series_name=ifelse(grepl(', rural',series_name),NA,series_name)) %>%
    arrange(desc(m_value)) %>%
    mutate(h = nrow(gap)-row_number()+1,
           plabel=value %>% round(digits=1),
           plabel=ifelse(is.na(plabel),NA,paste0(plabel,'%'))) 
  lcut=max(gap$m_value,na.rm=TRUE)/4
  ggplot(gap,aes(x=fct_reorder(series_code,m_value),y=value,fill=group,label=series_name)) +
    geom_bar(stat='identity') +
    coord_flip() +
    theme_classic() +
    ylab('% of respondents') +
    geom_text(aes(x=h+1,y=0),hjust=0) +
    geom_text(aes(label=ur,x=h,y=max(gap$m_value)/35),hjust=0) +
    geom_text(aes(label=plabel,x=h,y=ifelse(value>lcut,value,lcut),
                  hjust=1)) +
    #ggtitle(cname) +
    scale_y_continuous(limits=c(0,2*max(gap$value))) +
    theme(legend.position = "none",
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())
}

gap_plot('Uganda')
gap_plot('Nigeria')
gap_plot('Rwanda')
gap_plot('Tanzania')
gap_plot('Zambia')
