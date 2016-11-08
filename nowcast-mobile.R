library(ggplot2)
library(llamar)
library(XLConnect)
source('read-DHS.R')
source('utils.R')

###############################################################################
# TODO: I want this file to just contain the code I need to update the data
# frames returned by read-DHS.R to contain estimates of the current 
# mobile ownership of DHS households. The exploratory stuff that I use
# to justify my models belongs in a separate file (probably an Rmd).
###############################################################################

###############################################################################
# To what extent does the "sigmoidal" wealth/mobile relationship hold
# up for PA countries?
###############################################################################
# wealth_mobile <- function(df,text) {
#   df %>% removeAttributes %>%
#     ggplot(aes(x=wealth,y=mobile)) +
#     geom_jitter(size=5,color='tomato',alpha=0.05,width=0) +
#     geom_smooth(method = "glm", method.args = list(family = "binomial")) +
#     ggtitle(text) +
#     theme_classic() 
# }
# 
# wealth_mobile(rwa_2015,'Rwanda 2015')

###############################################################################
# Compare different countries at a single point in time
###############################################################################
# p50 <- function(df) {
#   s <- df %>% removeAttributes %>% 
#     glm(mobile~wealth,data=.,family='binomial') %>%
#     summary
#   data.frame(res=-s$coefficients[1,1]/s$coefficients[2,1])
# }
# mobile_compare <- function(a,title='') {
#   # a is (typically) a big data frame coming out of rbind
#   l <- a$label %>% unique
#   if (length(l)==2) {
#     p1 <- a[a$label==l[1],] %>% p50
#     p2 <- a[a$label==l[2],] %>% p50
#     print(paste('p50 difference = ',round(p2-p1,digits=3)))
#   }
#   ggplot(a,aes(x=wealth,y=mobile,group=label,color=label)) +
#     geom_smooth(method = "glm", method.args = list(family = "binomial")) +
#     ggtitle(title) +
#     theme_classic()
# }
# 
# 
# rbind(gha_2014,ken_2014,lbr_2013,nga_2013,sle_2013) %>% 
#   removeAttributes %>%
#   mobile_compare(title='DHS in 2013-14')
# PROBLEM: sen_2014 seems not to have the same wealth scale as others
# Actually not such a big problem; LSMS countries won't be on the same scale 
# either.

###############################################################################
# Compare changes in one country over time
###############################################################################
# rbind(gha_2008,gha_2014) %>% removeAttributes %>%
#   mobile_compare(title='Ghana 2008-2014') # -1.421
# 
# rbind(ken_2009,ken_2014) %>% removeAttributes %>%
#   mobile_compare(title='Kenya 2008-2014') # -1.081
# 
# rbind(lbr_2007,lbr_2013) %>% removeAttributes %>%
#   mobile_compare(title='Liberia 2007-2013') # -0.904
# 
# rbind(nga_2008,nga_2013) %>% removeAttributes %>%
#   mobile_compare(title='Nigeria 2008-2013') # -1.039
# 
# rbind(rwa_2010,rwa_2015) %>% removeAttributes %>%
#   mobile_compare(title='Rwanda 2010-2015') # -0.355, smallest difference
# 
# rbind(sle_2008,sle_2013) %>% removeAttributes %>%
#   mobile_compare(title='Sierra Leone 2008-2013') # -0.749
# 
# rbind(uga_2006,uga_2011) %>% removeAttributes %>%
#   mobile_compare(title='Uganda 2006-2011') # -1.665, largest difference
# 
# rbind(zmb_2007,zmb_2014) %>% removeAttributes %>%
#   mobile_compare(title='Zambia 2007-2014') # -1.313

###############################################################################
# Test agreement between my weighted averages and numbers from DHS API
###############################################################################
# # % of households with mobile phone
# test <- loadDHS(indicators='HC_HEFF_H_MPH',countries='RW',years=2015)
# test$Value
# w_mean(rwa_2015$mobile,rwa_2015$weight)
# # Glad to see that works.

###############################################################################
# Get as many mobile penetration averages as I can from DHS. Focus on SS
# Africa.
###############################################################################
africa <- c('BJ','BF','BU','CM','CV','CF','TD','CG','CD','CI','EK','ER',
            'ET','GA','GM','GH','GN','KE','LS','LB','MD','MW','ML','MZ','NM',
            'NI','NG','RW','SN','SL','ZA','SD','SZ','TZ','TG','UG','ZM','ZW')

afr_mobile <- loadDHS(indicators='HC_HEFF_H_MPH',
                      countries=paste(africa,collapse=',')) %>%
  dplyr::select(CountryName,SurveyYear,Value)
# 72 data points. 
# write.csv(afr_mobile,'DHS_mobile.csv',row.names=FALSE)

###############################################################################
# Get the corresponding data points out of the (overly-pretty) GSMA Excel 
# reports.
###############################################################################
p2frac <- function(p) {
  # convert a percentage string to a number
  p %>% sub('%','',.) %>% as.numeric / 100
}

s2num <- function(s) {
  # get those commas out of there
  s %>% gsub(',','',.) %>% as.numeric
}

get_gsma <- function(fname,yrs) {
  x <- read.csv(fname,skip=2,stringsAsFactors=FALSE)
  qnames <- paste('Q4.',yrs,sep='')
  market <- x[x$Metric=='Unique subscribers' &
                x$Attribute=='Total','Market..Operator']
  agu <- x[x$Metric=='Growth rate, unique subscribers, annual' &
             x$Attribute=='Total',qnames] %>% t %>% p2frac
  pu <- x[x$Metric=='Market penetration, unique subscribers' &
            x$Attribute=='Total',qnames] %>% t %>% p2frac
  agn <- x[x$Metric=='Growth rate, excluding cellular M2M, annual' &
             x$Attribute=='Total' & x$Market..Operator==market,qnames] %>% t %>% p2frac
  p <- x[x$Metric=='Market penetration' & x$Attribute=='Total' & 
           x$Market..Operator==market,qnames] %>% t %>% p2frac
  h <- x[x$Metric=='Herfindahl-Hirschman Index',qnames] %>% t %>% s2num
  res <- cbind(agu,pu,agn,p,h) %>% as.data.frame
  res$CountryName <- market
  res$SurveyYear <- yrs
  names(res) <- c('ann_growth_uniq','penetration_uniq',
                  'ann_growth','penetration','herfindahl',
                  'CountryName','SurveyYear')
  res
}
dirroot <- 'C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/GSMA/'

gsma <- data.frame()
for (n in afr_mobile$CountryName %>% unique) {
  #print(n)
  fname <- paste0(dirroot,n,'.csv')
  yrs <- afr_mobile[afr_mobile$CountryName==n,'SurveyYear']
  gsma <- rbind(gsma,get_gsma(fname,yrs))
}
gsma[gsma$CountryName=="Congo, Democratic Republic", 'CountryName'] <- "Congo Democratic Republic"
gsma[gsma$CountryName=="C?te d'Ivoire", 'CountryName'] <- "Cote d'Ivoire"

###############################################################################
# How well can the subscriber-level GSMA data predict the household-level
# DHS data? 
###############################################################################

j <- plyr::join(afr_mobile,gsma,by=c('CountryName','SurveyYear')) %>% unique
# 
# # First, try linear models, omitting country information
# lm(j$Value ~ j$penetration_uniq) %>% summary               # R^2 = 0.7914
# lm(j$Value ~ j$penetration) %>% summary                    # R^2 = 0.7672
# j2 <- j %>% select(-CountryName)
# lm(Value ~ .,j2) %>% summary # R^2 = 0.8329
# 
# # I tried to beat this with caret and didn't really succeed. 
# 
# # Include country as factor variable
# lm(Value ~ CountryName,j) %>% summary            # R^2 = 0.601
# lm(Value ~ CountryName+SurveyYear,j) %>% summary # R^2 = 0.948
# lm(Value ~ .,j) %>% summary                      # R^2 = 0.977
# # In addition to the country factor variable, the annual growth rate
# # and penetration of unique subscribers seem to be important. The year stops
# # being significant when these are factored in.

###############################################################################
# Project 2016 household-level rates
###############################################################################
pa <- c('Ethiopia','Ghana','Kenya','Liberia','Malawi','Nigeria','Rwanda',
        'Senegal','Sierra Leone','Tanzania','Uganda','Zambia')
gsma_2016 <- data.frame()
for (x in pa) {
  fname <- paste0(dirroot,x,'.csv')
  gsma_2016 <- rbind(gsma_2016,get_gsma(fname,2016))
}
fit <- lm(Value ~ .,j)
newvals <- predict(fit,gsma_2016)
wpred <- gsma_2016 %>% 
  dplyr::select(CountryName,SurveyYear) %>% 
  mutate(Value=newvals,
         Value=ifelse(Value>100,100,Value)) %>%
  rbind(afr_mobile[afr_mobile$CountryName %in% pa,])
# ggplot(wpred,aes(x=SurveyYear,y=Value,group=CountryName,color=CountryName)) +
#   geom_point(size=4) +
#   geom_line(size=2) +
#   theme_classic()

###############################################################################
# Estimate which households acquired a phone between the DHS survey and now.
# Use a combination of geography and wealth to estimate who is most likely
# to add a phone connection.
# If the weighted average is already higher than the target, then nothing will
# be added.
###############################################################################
mobile_now <- function(df,cname) {
  # return df, with a new column df$mobile_now that adds new mobile users in
  # order of decreasing wealth, until the weighted average reaches target
  # from wpred
  target <- wpred[wpred$CountryName==cname & wpred$SurveyYear==2016,'Value']/100
  mod<- df %>% group_by(clust) %>% 
    summarise(clust_avg=mean(mobile,na.rm=TRUE),in_clust=n()) %>%
    plyr::join(df,by='clust') %>%
    mutate(out_avg = clust_avg*in_clust/(in_clust-1) - as.numeric(mobile)/(in_clust-1)) %>%
    select(-clust_avg,-in_clust)
  # combine this score with wealth to prioritize likely electrification
  fit <- mod %>% removeAttributes %>%
    glm(mobile~out_avg,data=.,family='binomial')
  mod$pred <- predict(fit,mod,type='response')
  add_us <- mod %>% mutate(n=row_number()) %>%
    filter(mobile==0) %>% arrange(desc(pred)) 
  df$mobile_now <- df$mobile %>% as.numeric
  for (i in add_us$n) {
    if (w_mean(df$mobile_now,df$weight) >= target) {
      break
    }
    df[i,'mobile_now'] <- 1
  }
  df
}

# Now, use predicted values in wpred to add mobile_now columns to my most recent
# DHS datasets
eth_2011 <- mobile_now(eth_2011,'Ethiopia')
gha_2014 <- mobile_now(gha_2014,'Ghana')
ken_2014 <- mobile_now(ken_2014,'Kenya')
lbr_2013 <- mobile_now(lbr_2013,'Liberia')
mwi_2010 <- mobile_now(mwi_2010,'Malawi')
nga_2013 <- mobile_now(nga_2013,'Nigeria')
sen_2014 <- mobile_now(sen_2014,'Senegal')
sle_2013 <- mobile_now(sle_2013,'Sierra Leone')
tza_2010 <- mobile_now(tza_2010,'Tanzania')
uga_2011 <- mobile_now(uga_2011,'Uganda')
rwa_2015 <- mobile_now(rwa_2015,'Rwanda')
zmb_2014 <- mobile_now(zmb_2014,'Zambia')

###############################################################################
# Clean up
###############################################################################

rm(afr_mobile,gsma,gsma_2016,j,wpred,africa,dirroot,fit,fname,n,newvals,pa,x,
   yrs,get_gsma)

###############################################################################
# Plot for data 1-pager. Keep Ghana and Malawi for comparisons
###############################################################################
library(stringr)

tmp1 <- wpred %>% filter(CountryName %in% c('Nigeria','Ghana','Malawi')) %>%
  mutate(lt = as.factor(ifelse(SurveyYear==2016,2,1)))
tmp2 <- tmp1 %>% filter(SurveyYear < 2016)

ggplot(tmp1,aes(x=SurveyYear,y=Value,group=CountryName,color=CountryName)) +
  geom_point(size=4) +
  geom_line(size=2,linetype=2) +
  geom_line(data=tmp2,aes(x=SurveyYear,y=Value,group=CountryName,
                          color=CountryName),size=2,linetype=1) +
  ylab('Percent of households with mobiles') +
  xlab('DHS survey year') +
  annotate('text',x=2014,y=44,hjust=0,
           label=str_wrap('2016 values predicted based on GSMA data',20)) +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        legend.title=element_blank())

###############################################################################
# What about broad geographic coverage?
###############################################################################

mc <- function(df,thresh=0.5) {
  tmp <- df %>% filter(rural==1) %>%
    group_by(clust) %>%
    summarize(mean_mobile = mean(mobile_now==1,na.rm=TRUE))
  mean(tmp$mean_mobile > thresh,na.rm=TRUE)*100  
}

mobile_coverage <- data.frame(country=c('Ethiopia','Ghana','Kenya','Liberia','Malawi','Nigeria',
                     'Senegal','Sierra Leone','Tanzania','Uganda','Rwanda',
                     'Zambia'),
           value=c(mc(eth_2011),mc(gha_2014),mc(ken_2014),mc(lbr_2013),
                      mc(mwi_2010),mc(nga_2013),mc(sen_2014),mc(sle_2013),
                      mc(tza_2010),mc(uga_2011),mc(rwa_2015),mc(zmb_2014)))
           
highlight_bar(mobile_coverage)

wpred %>% filter(SurveyYear==2016) %>% 
  select(country=CountryName,ownership=Value) %>%
  plyr::join(mobile_coverage,by='country') %>%
  rename(coverage=value) %>%
  ggplot(aes(x=ownership,y=coverage,label=country)) +
  geom_point() +
  geom_smooth(method='lm',alpha=0.3) +
  geom_text(vjust=1) +
  theme_classic()

lbr_2013 %>% filter(rural==1) %>% summarize(m=mean(mobile_now,na.rm=TRUE))
lbr_2013 %>% filter(rural==0) %>% summarize(m=mean(mobile_now,na.rm=TRUE))

uga_2011 %>% filter(rural==1) %>% summarize(m=mean(mobile_now,na.rm=TRUE))
uga_2011 %>% filter(rural==0) %>% summarize(m=mean(mobile_now,na.rm=TRUE))

highlight_bar(mobile_coverage)

###############################################################################
# GSMA sample plots
###############################################################################

gsma_pa <- plyr::ldply(pa[pa != 'South Africa'],function(x) 
  get_gsma(paste0(dirroot,x,'.csv'),2000:2016))

gsma_pa %>% 
  filter(CountryName %in% c('Nigeria','Uganda','Rwanda','Tanzania','Zambia')) %>%
  ggplot(aes(x=SurveyYear,y=penetration_uniq,group=CountryName,
                     color=CountryName)) +
    geom_line(size=2) +
    theme_classic() +
    xlab('Year') +
    ylab('Penetration, unique subscribers') +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.ticks=element_blank(),
          legend.title=element_blank())
   