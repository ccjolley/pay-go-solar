###############################################################################
# TODO: My goal of making data-derived personas didn't really work out. There
# are some other potentially interesting visualizations in here that I should
# keep handy. Re-frame comments away from personas and remove useless stuff.
###############################################################################


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
