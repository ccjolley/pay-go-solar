library(tidyr)
library(dplyr)
library(mice)
library(ggplot2)

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
# 682 variables for 172 countries. 

# Which countries are missing lots of data?
row_na <- gf_wide %>% is.na %>% rowSums 
nmis_country <- data.frame(nmis=row_na,country=gf_wide$country_name) %>%
  arrange(desc(nmis))
hist(nmis_country$nmis,breaks=40)
head(nmis_country)
tail(nmis_country)

# Which columns are missing lots of data?
key <- gf %>% select(series_name,series_code) %>% unique
col_na <- gf_wide %>% is.na %>% colSums 
nmis_series <- data.frame(nmis=col_na,series_code=names(gf_wide)) %>%
  arrange(desc(nmis))
hist(nmis_series$nmis,breaks=40)
head(nmis_series) %>% plyr::join(key,by='series_code')
tail(nmis_series) %>% plyr::join(key,by='series_code')

# No good way to get around missing data; let's impute.
system.time(gf_mice <- mice(gf_wide,m=1,method='fastpmm')) 
# running this for 9.87h got me 631 variable imputations ~ 64/hr
# 477 varibles x 5 iterations = 2385 imputations = 37.3h
# doing this properly will have to wait until the weekend;
# it seems that changing algorithms doesn't help much when the dataset
# is wide rather than long; they're all slow

# do manually with medians; not good but will let us do PCA
gf_imp <- gf_wide[,2:ncol(gf_wide)]
for (n in names(gf_imp)) {
  isna <- is.na(gf_imp[,n])
  gf_imp[isna,n] <- median(gf_imp[!isna,n],na.rm=TRUE)
}

pr <- prcomp(gf_imp,center=TRUE,scale=TRUE)
plot(pr)
summary(pr) # First two PC's get me 43.6% of variance; 90% after first 34
qplot(pr$x[,1],pr$x[,2],color=1) +
  guides(color=FALSE) +
  theme_classic() +
  xlab('PC 1') + ylab('PC 2')
# What are our PCs capturing?
gf_wide$country_name[pr$x[,1] > 25] # rich countries
gf_wide$country_name[pr$x[,1] < -17] # poor countries
gf_wide$country_name[pr$x[,2] > 14] # E&E, MENA, LAC
gf_wide$country_name[pr$x[,2] < -30] # Kenya, Uganda


code_trans <- function(x) {
  df <- data.frame(value=x,series_code=names(x))
  plyr::join(df,key,by='series_code')
}
pr$rotation[,1] %>% sort %>% head %>% code_trans
# negative correlation with PC1: using cash, borrowing for medical purposes
pr$rotation[,1] %>% sort %>% tail %>% code_trans
# positive correlation with PC1: debit cards and accounts, even for poor
hist(pr$rotation[,1],breaks=30)

pr$rotation[,2] %>% sort %>% head %>% code_trans
# negative correlation with PC2: Sending domestic remittances
pr$rotation[,2] %>% sort %>% tail %>% code_trans
# Not seeing as clear of a pattern here
hist(pr$rotation[,2],breaks=30)

###############################################################################
# Use inflection in between-cluster variance to choose appropriate number of
# clusters.
###############################################################################
res <- data.frame()
cluster_us <- pr$x %>% as.data.frame %>%
  select(PC1:PC26) 
for (i in 2:30) {
  f <- NULL
  for (j in 1:20) {
    km_j <- kmeans(cluster_us,centers=i)
    f <- c(f,km_j$betweenss/km_j$totss)
  }
  res <- rbind(res,data.frame(i=i,f=mean(f)))
}
qplot(res$i,res$f) + 
  xlab('Number of clusters') +
  ylab('Fraction of between-cluster variance') +
  geom_vline(xintercept=6,color='red') +
  theme_classic() # I'd say it's around k=6

###############################################################################
# Split into clusters and visualize on first two PCs.
###############################################################################
km <- kmeans(cluster_us,centers=6) # totally arbitrary for now
cluster_us$clust <- as.factor(km$cluster)
cluster_us$country_name <- gf_wide$country_name

ggplot(cluster_us,aes(x=PC1,y=PC2,group=clust,color=clust)) +
  geom_point(size=3) +
  theme_classic()

cluster_us[cluster_us$clust==4,'country_name']

###############################################################################
# Find a "typical" country in each cluster, based on its centroid
###############################################################################
country_centroid <- function(clust_num,num_res=1) {
  mid <- km$centers[clust_num,]
  tmp <- cluster_us[cluster_us$clust==clust_num,]
  tmp$d2 <- sapply(1:nrow(tmp), function(i) sum((mid - tmp[i,1:26])^2))
  tmp %>% arrange(d2) %>% head(num_res)
}

country_centroid(1,5) %>% select(country_name,d2)

###############################################################################
# Find variables that distinguish clusters from each other 
###############################################################################
sep_vars <- function(gp1,gp2) {
  # gp1 and gp2 are each data frames
  tmp <- rbind(gp1 %>% mutate(label=1),gp2 %>% mutate(label=0))
  # remove variables with zero variance
  keep_us <- apply(tmp,2,sd) > 0
  tmp <- tmp[,keep_us]
  pvals <- 
  
  plyr::ldply(names(tmp %>% select(-label)),function(x) {
    tt <- t.test(tmp[tmp$label==1,x],tmp[tmp$label==0,x])
    data.frame(series_code=x,m1=tt$estimate[1],m2=tt$estimate[2],pval=tt$p.value)
  }) %>%
    #filter(pval < 0.01/ncol(tmp)) %>%
    arrange(pval) %>%
    plyr::join(key,by='series_code')
}

sep_vars(gf_imp[cluster_us$clust==5,],gf_imp[cluster_us$clust==4,]) %>%
  head(10)

# Primary difference between cluster 1 and cluster 6: More people in cluster 1 
# use the internet for paying bills, shopping, etc.

# Clusters 6 and 2: People in 6 more likely to pay bills using accounts
# at financial institutions, and to have accounts

# Clusters 2 and 3: People in 2 more likely to have debit cards

# Clusters 3 and 5: People in 5 use more domestic remittances; people in 3 
# pay more utility bills

# Clusters 5 and 4: People in 4 more likely to have mobile accounts and use 
# them for domestic remittances and utility bills

sep_vars(gf_imp[cluster_us$PC1 > 0,],gf_imp[cluster_us$PC1 < 0,]) %>% head(10)
sep_vars(gf_imp[cluster_us$PC1 > 10,],gf_imp[cluster_us$PC1 < -10,]) %>% head(10)
# Major PC1 separation is whether people have accounts at financial institutions

sep_vars(gf_imp[cluster_us$PC2 > 0,],gf_imp[cluster_us$PC2 < 0,]) %>% head(10)
sep_vars(gf_imp[cluster_us$PC2 > 10,],gf_imp[cluster_us$PC2 < -10,]) %>% head(10)
# PC2 separation: people in countries with PC2 < 0 save and borrow more than others

###############################################################################
# Focus on Power Africa countries (add to interest list as needed)
###############################################################################

pa <- c('Ethiopia','Ghana','Kenya','Malawi','Nigeria','Rwanda',
        'Senegal','Sierra Leone','South Africa','Tanzania','Uganda','Zambia')

# Hardly any information is available for Liberia (only 2 of 477 variables), so
# I'm not including it.

# Keep the 445 variables that were measured for all countries
gf_pa <- gf_wide[gf_wide$country_name %in% pa,]
gf_pa <- gf_pa[,colSums(is.na(gf_pa)) == 0]

# Visualize
pa_plot <- function(code,country=NULL) {
  tmp <- gf_pa[,c('country_name',code)]
  names(tmp) <- c('country','value')
  title <- key[key$series_code==code,'series_name'] %>% 
    gsub(' \\[w2\\]','',.)
  highlight_bar(tmp,title=title,hi_country=country)
}

pa_plot('WP14887_7.10','Nigeria') # Account at a financial institution, rural
pa_plot('WP14918.1','Nigeria')    # Bought from a store on credit
pa_plot('WP15163_4.1','Nigeria')  # Mobile accts
pa_plot('WP15163_4.8','Nigeria')  # Mobile accts, poorest 40%
pa_plot('WP14934.1','Nigeria')    # Received domestic remittances
pa_plot('WP14928.1','Nigeria')    # Sent domestic remittances

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
  impute_mm

mm_all$gap <- mm_gap$pc1
mm_all %>% is.na %>% rowSums %>% table

mm_all[rowSums(is.na(mm_all)) < 3,] %>%
  ggplot(aes(x=pc1,y=gap,label=country_name)) +
    geom_point() +
    geom_text(vjust=1) +
    geom_vline(xintercept=mm_all[mm_all$country_name=='Low income','pc1'],color='red') +
    geom_hline(yintercept=mm_all[mm_all$country_name=='Low income','gap'],color='red') +  
    xlab('Mobile money usage') +
    ylab('Urban-rural gap') +
    theme_classic()

###############################################################################
# Let's talk about debt (baby). This time I'm not really interested in an
# urban/rural split; I just want to know how willing/able rural people are to 
# use debt financing. 
#
# Rather than doing a complicated PCA thing, I'm going to focus on the two 
# indicators that I think are most relevant.
###############################################################################

# WP14924_8.10	Borrowed any money in the past year, rural (% age 15+) [w2]
# WP14918.10	Borrowed from a store by buying on credit, rural (% age 15+) [w2]

debt <- gf_wide %>% select(country_name,borrowed=WP14924_8.10,
                           store_credit=WP14918.10) %>%
  mutate(mm_usage = mm_all$pc1,
         mm_usage = mm_usage - min(mm_usage,na.rm=TRUE) + 1)

ggplot(debt,aes(x=borrowed,y=mm_usage,label=country_name)) +
  geom_point() +
  geom_text(vjust=1) +
  # geom_vline(xintercept=mm_all[mm_all$country_name=='Low income','pc1'],color='red') +
  # geom_hline(yintercept=mm_all[mm_all$country_name=='Low income','gap'],color='red') +  
  xlab('% Who borrowed money (rural)') +
  ylab('Mobile money usage (log scale)') +
  scale_y_log10() +
  theme_classic()


ggplot(debt,aes(x=store_credit,y=mm_usage,label=country_name)) +
  geom_point() +
  geom_text(vjust=1) +
  # geom_vline(xintercept=mm_all[mm_all$country_name=='Low income','pc1'],color='red') +
  # geom_hline(yintercept=mm_all[mm_all$country_name=='Low income','gap'],color='red') +  
  xlab('% Who bought on credit (rural)') +
  ylab('Mobile money usage (log scale)') +
  scale_y_log10() +
  theme_classic()

###############################################################################
# Plots in the style of the 1-pager
###############################################################################

mm_all %>% filter(country_name %in% pa) %>%
  select(country=country_name,value=pc1) %>%
  highlight_bar(pcent=FALSE)

mm_gap %>% filter(country_name %in% pa) %>%
  select(country=country_name,value=pc1) %>%
  highlight_bar(pcent=FALSE)

debt %>% filter(country_name %in% pa) %>%
  select(country=country_name,value=borrowed) %>%
  highlight_bar()




