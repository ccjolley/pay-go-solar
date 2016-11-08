library(mice)
library(ggplot2)

source('read-findex.R')

###############################################################################
# Clustering of countries based on their Global Findex data. These plots 
# didn't make it into the report, but I still think they're interesting enough
# to keep in the repo.
###############################################################################

###############################################################################
# Imputation and PCA
###############################################################################

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




