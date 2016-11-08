###############################################################################
# Construct some user personas based on DHS and Findex data
# 
# My methodology here will be to use my previous work to find features of a 
# few of our focus countries that make them special. For these countries,
# who are the people who align with that trend? Who are the people who buck
# the trend?
###############################################################################

library(dplyr)
library(reshape2)
library(haven)
library(llamar)
library(forcats)
source('read-DHS.R')
source('read-findex.R')

###############################################################################
# Nigeria
# * Lowest MM usage
# * Low borrowing rates
# * High electrification rate
###############################################################################

# Nigeria had a very low score for my overall mobile money usage index. Out of
# the minority that do use some form of mobile money, what do they use it for?

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

# Can I use individual-level Findex data to find out more about how these early
# adopters differ from the population generally?

root_dir <- 'C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/Global Findex'
fdx_nga <- read_dta(paste0(root_dir,'/Nigeria 2014/micro_nga.dta'))
# I'm interested in people who either sent or received money with a mobile
# q27c (domestic remit. received through mobile)
# q29c (domestic remit. sent through mobile)
fdx_nga$mob_sr <- fdx_nga$q27c == 1 | fdx_nga$q29c == 1
w_mean(fdx_nga$mob_sr,fdx_nga$wgt) # ~12% of people who responded said yes
mean(!is.na(fdx_nga$mob_sr)) # only 34% answered the question
fisher.test(fdx_nga$mob_sr,fdx_nga$female) # nope
t.test(age ~ mob_sr,fdx_nga) # also not
t.test(inc_q ~ mob_sr,fdx_nga) # no
t.test(educ ~ mob_sr,fdx_nga) # mm users a little more educated
fisher.test(fdx_nga$mob_sr,fdx_nga$saved) # no
fisher.test(fdx_nga$mob_sr,fdx_nga$borrowed) # no
# This is feeling like a rabbit hole, but I should at least use the survey
# package to weight these tests more correctly.



# It seems Nigerians don't like to borrow money much. When they do, how do they
# do it?

borrowing <- c('WP14924_8.10','WP14921.10','WP14922.10','WP14917.10',
               'WP14920.10','WP14918.10','WP11654.10','WP14919.10',
               'WP14923.10')

country_plot('Nigeria',borrowing)
country_plot('Kenya',borrowing)
# Those who borrow money do so overwhelmingly from family and friends. That 
# pattern is similar to Uganda, where overall borrowing rates are much higher.

# Do some of these types of borrowing seem atypical in interesting ways?

# Overall, electrification seems to be fairly high. What's the geography of 
# electrification in Nigeria?

# 
# Uganda
# * More MM usage in rural areas
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
  #paste('lcut=',lcut) %>% print  
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

# 
# Zambia
# 
# 
# Tanzania
# * Highest urban-rural MM gap


gap_plot('Tanzania')
gap_plot('Ghana')
    


# * Highest mobile & unelectrified
# * Low electrification



# 
# Kenya
# * Higest MM usage
# * High mobile penetration
# 
# Ghana
# * Lowest borrowing rates
# * Highest electrification
# * High mobile penetration

country_plot('Ghana',borrowing)
