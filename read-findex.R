###############################################################################
# Load and clean Findex data -- analysis happens in findex.R
###############################################################################

library(tidyr)
library(dplyr)
library(ggplot2)

oldwd <- getwd()
setwd("C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/Global Findex")
# gf <- read.csv('a0a7494d-73a3-41ab-a382-13a58f4df93a_Data.csv',
#                encoding="UTF-8",stringsAsFactors=FALSE)
gf <- read.csv('00d7fbe8-2939-4a51-8972-efb244252327_Data.csv',
               encoding="UTF-8",stringsAsFactors=FALSE)

names(gf) <- c('country_name','country_code','series_name','series_code','val_2011','val_2014','mrv')

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

# 682 variables for 172 countries. 

# Which countries are missing lots of data?
# row_na <- gf_wide %>% is.na %>% rowSums 
# nmis_country <- data.frame(nmis=row_na,country=gf_wide$country_name) %>%
#   arrange(desc(nmis))
# hist(nmis_country$nmis,breaks=40)
# head(nmis_country)
# tail(nmis_country)

# Which columns are missing lots of data?
# col_na <- gf_wide %>% is.na %>% colSums 
# nmis_series <- data.frame(nmis=col_na,series_code=names(gf_wide)) %>%
#   arrange(desc(nmis))
# hist(nmis_series$nmis,breaks=40)
# head(nmis_series) %>% plyr::join(key,by='series_code')
# tail(nmis_series) %>% plyr::join(key,by='series_code')

rm(gf,gf_w1,gf_w2,just1)

###############################################################################
# Focus on Power Africa countries (add to interest list as needed)
###############################################################################

pa <- c('Ethiopia','Ghana','Kenya','Malawi','Nigeria','Rwanda',
        'Senegal','Sierra Leone','South Africa','Tanzania','Uganda','Zambia')

# Hardly any information is available for Liberia (only 2 of 477 variables), so
# I'm not including it. Also no Findex data for Mozambique.

# Keep the 443 variables that were measured for all countries
gf_pa <- gf_wide[gf_wide$country_name %in% pa,]
#gf_pa <- gf_pa[,colSums(is.na(gf_pa)) < 2] %>% na.omit

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
# Mobile money usage index
###############################################################################

# First, build a nationwide index
# WP15163_4.1	Mobile account (% age 15+) [w2]
# WP11672.1	Mobile phone used to pay bills (% age 15+) [w1]
# WP11674.1	Mobile phone used to receive money (% age 15+) [w1]
# WP11673.1	Mobile phone used to send money (% age 15+) [w1]
# WP15172_4.1	Used a mobile phone to pay for school fees (% age 15+) [w2]
# WP14940_4.1	Used a mobile phone to pay utility bills (% age 15+) [w2]
# WP15161_1.1	Used an account to make a transaction through a mobile phone (% age 15+) [w2]

mm <- gf_wide %>% select(WP15163_4.1,WP11672.1,WP11674.1,WP11673.1,
                         WP15172_4.1,WP14940_4.1,WP15161_1.1)

# to do PCA, I'll need to impute
impute_mm <- function(m) {
  nc <- ncol(m)
  mm_mice <- mice(m,m=1,print=FALSE)
  mm1 <- complete(mm_mice,1)
  pr1 <- prcomp(mm1,center=TRUE,scale=TRUE)
  summary(pr1) %>% print # first PC gets me ~50% of the variance
  m <- m %>% mutate(country_name=gf_wide$country_name,
                    pc1=pr1$x[,1])
  m[rowSums(is.na(m)) == nc,'pc1'] <- NA 
  m
}

mm <- impute_mm(mm)

mm %>% arrange(pc1) %>% head(10) # worst places for mobile money
mm %>% arrange(desc(pc1)) %>% head(10) 
# best places include Kenya, Somalia, Uganda, plus rich countries
# not sure how it imputed for countries with no data

# Rural index
# WP15163_4.10	Mobile account, rural (% age 15+) [w2]
# WP11672.10	Mobile phone used to pay bills, rural (% age 15+) [w1]
# WP11674.10	Mobile phone used to receive money, rural (% age 15+) [w1]
# WP11673.10	Mobile phone used to send money, rural (% age 15+) [w1]
# WP15172_4.10	Used a mobile phone to pay for school fees, rural (% age 15+) [w2]
# WP14940_4.10	Used a mobile phone to pay utility bills, rural (% age 15+) [w2]
# WP15161_1.10	Used an account to make a transaction through a mobile phone, rural (% age 15+) [w2]
mm_rural <- gf_wide %>% select(WP15163_4.10,WP11672.10,WP11674.10,WP11673.10,
                               WP15172_4.10,WP14940_4.10,WP15161_1.10)
mm_rural <- impute_mm(mm_rural)
mm_rural %>% arrange(pc1) %>% head(10) # similar to previous list
mm_rural %>% arrange(desc(pc1)) %>% head(10) # also

mm_gap <- mm_rural %>%
  mutate(acct = mm$WP15163_4.1 - WP15163_4.10,
         bill_pay = mm$WP11672.1 - WP11672.10,
         recv = mm$WP11674.1 - WP11674.10,
         send = mm$WP11673.1 - WP11673.10,
         school_fees = mm$WP15172_4.1 - WP15172_4.10,
         utility = mm$WP14940_4.1 - WP14940_4.10,
         trans = mm$WP15161_1.1 - WP15161_1.10) %>%
  select(acct,bill_pay,recv,send,school_fees,utility,trans)
mm_gap <- impute_mm(mm_gap)

# re-scale so that a score of zero means an average gap of zero
mm_gap$avg_gap <- mm_gap %>% select(acct:trans) %>% rowMeans(na.rm=TRUE)
s <- lm(avg_gap ~ pc1,data=mm_gap) %>% summary
dx <- -s$coefficients[1,1]/s$coefficients[2,1]
qplot(mm_gap$pc1-dx,mm_gap$avg_gap) + geom_smooth() # shift looks right
mm_gap$pc1 <- mm_gap$pc1 - dx

mm_gap %>% arrange(pc1) %>% head(15) # negative values -- higher usage in rural
mm_gap %>% arrange(desc(pc1)) %>% head(15) # rural areas lag (Tanzania)

# No rural split avaiable for these
# WP15175.1	Paid school fees: using a mobile phone (% paying school fees, age 15+) [w2]
# WP14943.1	Paid utility bills: using a mobile phone (% paying utility bills, age 15+) [w2]
# WP14938.1	Received domestic remittances: through a mobile phone (% recipients, age 15+) [w2]
# WP15181.1	Received government transfers: through a mobile phone (% transfer recipients, age 15+) [w2]
# WP15187.1	Received payments for agricultural products: through a mobile phone (% recipients, age 15+) [w2]
# WP14949.1	Received wages: through a mobile phone (% wage recipients, age 15+) [w2]
# WP15170.1	Sent domestic remittances: through a mobile phone (% senders, age 15+) [w2]

mm_all <- mm %>%
  select(WP15163_4.1:WP15161_1.1) %>%
  cbind(gf_wide %>% select(WP15175.1,WP14943.1,WP14938.1,WP15181.1,WP15187.1,
                           WP14949.1,WP15170.1)) %>%
  impute_mm %>%
  mutate(all = pc1,
         gap = mm_gap$'pc1',           # quotes are to avoid namespace issues
         rural = mm_rural$'pc1') %>%
  select(-pc1)

mm_all %>% is.na %>% rowSums %>% table

mm_all[rowSums(is.na(mm_all)) < 3,] %>%
  ggplot(aes(x=all,y=gap,label=country_name)) +
  geom_point() +
  geom_text(vjust=1) +
  geom_vline(xintercept=mm_all[mm_all$country_name=='Low income','all'],color='red') +
  geom_hline(yintercept=mm_all[mm_all$country_name=='Low income','gap'],color='red') +  
  xlab('Mobile money usage') +
  ylab('Urban-rural gap') +
  theme_classic()

rm(mm,mm_gap,mm_rural,dx,s)

###############################################################################
# Histogram of mobile account ownership rates
###############################################################################
tmp <- gf_wide %>% select(country_name,m_acct=WP15163_4.1) %>%
  na.omit %>%
  filter(!grepl(')',country_name)) %>%
  filter(!grepl('income',country_name)) %>%
  filter(!country_name %in% c('World','South Asia')) %>%
  mutate(m_acct = m_acct/100)
# top <- tmp %>% filter(m_acct > 10 | country_name == 'Nigeria') %>% arrange(m_acct) %>%
#   mutate(y = c(1,7:1,4:1,3:1,1) / 200 + 0.01)

top <- tmp %>% filter(m_acct > 0.02) %>% arrange(m_acct) %>%
  mutate(y = c(28:1,8:1,4:1,3:1,1) / 1.5,
         x=m_acct,
         x=x+ifelse(country_name=='Congo, Dem. Rep.',0.02,0)) 

ggplot(tmp,aes(x=m_acct)) +
  geom_histogram(fill='lightskyblue1',color='gray80',binwidth=0.02) +
  theme_classic() +
  geom_text(data=top,aes(x=x,y=y,label=country_name),hjust=0) +
  xlab('Mobile account ownership rate') +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

###############################################################################
# Mobile money scatterplots
###############################################################################
gf_pa %>% select(country_name,m_acct=WP15163_4.1,m_acct_r=WP15163_4.10) %>%
  ggplot(aes(x=m_acct,y=m_acct_r,label=country_name)) +
  geom_point(size=3) +
  geom_text(vjust=1.1) +
  geom_abline(slope=1,intercept=0,color='darkorange4') +
  xlab('Mobile account ownership (national avg)') +
  ylab('Mobile account ownership (rural)') +
  theme_classic()

gf_pa %>% select(country_name,billpay=WP14940_4.1,billpay_r=WP14940_4.10) %>%
  ggplot(aes(x=billpay,y=billpay_r,label=country_name)) +
  geom_point(size=3) +
  geom_text(vjust=1.1) +
  geom_abline(slope=1,intercept=0,color='darkorange4') +
  xlab('Utility bills paid with mobile (national avg)') +
  ylab('Utility bills paid with mobile (rural)') +
  theme_classic()

###############################################################################

setwd(oldwd)