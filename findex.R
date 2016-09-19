library(tidyr)
library(dplyr)
library(mice)
library(ggplot2)

setwd("C:/Users/Craig/Desktop/Live projects/Pay-go solar/hh survey data/Global Findex")
gf <- read.csv('a0a7494d-73a3-41ab-a382-13a58f4df93a_Data.csv',
               encoding="UTF-8",stringsAsFactors=FALSE)
names(gf) <- c('country_name','country_code','series_name','series_code','value')

gf_clean <- gf %>% 
  filter(country_code != '',
         value != '..',
         !grepl('WP_time_',gf$series_code)) %>% 
  mutate(value = value %>% as.numeric)

gf_wide <- gf_clean %>% 
  select(-country_code,-series_name) %>%
  spread(series_code,value)
# 477 variables for 172 countries. 

gf_wide %>% na.omit %>% nrow # Only 3 have no missing values

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
system.time(gf_mice <- mice(gf_wide,m=1)) # will probably take overnight
# do manually with medians; not great but will let us do PCA
gf_imp <- gf_wide[,2:ncol(gf_wide)]
for (n in names(gf_imp)) {
  isna <- is.na(gf_imp[,n])
  gf_imp[isna,n] <- median(gf_imp[!isna,n],na.rm=TRUE)
}

pr <- prcomp(gf_imp,center=TRUE,scale=TRUE)
plot(pr)
summary(pr) # First two PC's get me 49.7% of variance; 90% after first 26
qplot(pr$x[,1],pr$x[,2],color=1) +
  guides(color=FALSE) +
  theme_classic() +
  xlab('PC 1') + ylab('PC 2')
# What are our PCs capturing?
gf_wide$country_name[pr$x[,1] > 25] # rich countries
gf_wide$country_name[pr$x[,1] < -17] # poor countries
gf_wide$country_name[pr$x[,2] > 10] # E&E, MENA, LAC
gf_wide$country_name[pr$x[,2] < -30] # Africa


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

# k-means clustering to find groups
cluster_us <- pr$x %>% as.data.frame %>%
  select(PC1:PC26) 
km <- kmeans(cluster_us,centers=5) # totally arbitrary for now
cluster_us$clust <- as.factor(km$cluster)
cluster_us$country_name <- gf_wide$country_name
cluster_us[cluster_us$clust==5,'country_name']

ggplot(cluster_us,aes(x=PC1,y=PC2,group=clust,color=clust)) +
  geom_point(size=3) +
  theme_classic()

# TODO: More rigorous choice of cluster size (variance 'elbow' plot)

# TODO: Find a "typical" country in each cluster, based on its centroid

# TODO: Find variables that distinguish clusters from each other (log regression)