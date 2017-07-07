# My goal here is to come up with something that will draw a (fairly simple?) 
# boundary around areas of high electrification, so that this can be added to my
# population/mobile maps without having to introduce a new color scale.

# Test out on Uganda

library(dplyr)
library(ggplot2)
library(raster)
library(rgeos)
library(rgdal)

uga_data <- read.csv('uga_dhs_2011.csv') %>%
  filter(lat != 0)

uga <- uga_data %>% dplyr::select(lat,long,elect)

# First, visualize as colored dots
ggplot(uga_data,aes(x=long,y=lat,color=elect)) +
  geom_point(size=4) +
  theme_classic()

###############################################################################
# make_elect_shp()
#   Assumes that df will have columns named 'lat','long', and 'elect'
#   wrapper should be a function that will return an array of predictions
###############################################################################
make_elect_shp <- function(df,wrapper,out_name,thresh=0.5) {
  # Raster setup
  xpad <- (max(df$long) - min(df$long))*0.05
  ypad <- (max(df$lat) - min(df$lat))*0.05
  res <- 0.008333333 # same as LandScan
  r1 <- raster(vals=-1,res=res,
               xmn=min(df$long)-xpad,xmx=max(df$long)+xpad,
               ymn=min(df$lat)-ypad,ymx=max(df$lat)+ypad)
  r1pts <- rasterToPoints(r1) 
  # Call wrapper function
  pred <- wrapper(df,r1pts)
  r1 <- setValues(r1,pred)
  r2 <- setValues(r1,pred > 0.5)
  pg1 <- rasterToPolygons(r2,function(x) {x ==1},dissolve=TRUE)
  writeOGR(obj=pg1, dsn=out_name, layer=out_name, driver="ESRI Shapefile")
  r1
}

###############################################################################
# Try KNN first -- keep it simple
###############################################################################
library(FNN)

# Tune k
k_rmse <- sapply(2:15,function(i) {
  knn_i <- knn.reg(uga[,c('lat','long')],y=uga$elect,
                   k=i,algorithm='kd_tree')
  rmse <- (uga$elect - knn_i$pred)^2 %>% mean %>% sqrt
  paste0(i,' neighbors, RMSLE = ',rmse) %>% print
  rmse
})
qplot(2:15,k_rmse)
# Not a ton of variation; best performance with k=7

kr <- knn.reg(uga[,c('lat','long')],y=uga$elect,
              k=7,algorithm='kd_tree')
uga$pred <- kr$pred
qplot(uga$elect,uga$pred)
(uga$elect - uga$pred)^2 %>% mean %>% sqrt 
# RMSE = 0.1768631 - comparable to SVM

knn_wrap <- function(df,pts) {
  my_knn <- knn.reg(train=df[,c('lat','long')],test=pts[,c('y','x')],
                    y=df$elect,k=7,algorithm='kd_tree')
  my_knn$pred
}

knn_raster <- make_elect_shp(uga,knn_wrap,'test')
plot(knn_raster)

###############################################################################
# Just for lulz, try lm. Terrible prediction
###############################################################################
lm_wrap <- function(df,pts) {
  my_lm <- lm(elect~.,data=df)
  pts_rename <- pts %>% as.data.frame %>%
    dplyr::rename(long=x,lat=y)
  predict(my_lm,pts_rename)
}

lm_raster <- make_elect_shp(uga,lm_wrap,'uga_lm')
plot(lm_raster)

###############################################################################
# Curious about SVM now. I'm not going to worry about cross-validation at
# this stage.
###############################################################################
library(e1071)

test_svm <- svm(elect ~ .,uga)
(uga$elect - predict(test_svm,uga))^2 %>% mean %>% sqrt 
# 0.2078456; not as good as KNN

tuneResult <- tune(svm, elect ~ .,  data = uga,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult) # epsilon=0.3, cost=512, RSME = 0.18995 
plot(tuneResult)

tuneResult2 <- tune(svm, elect ~ .,  data = uga,
                   ranges = list(epsilon = seq(0,0.4,0.05), cost = 100*(4:10))
)
print(tuneResult2) # epsilon=0.25, cost=900, RSME=0.18647
plot(tuneResult2)

tuneResult3 <- tune(svm, elect ~ .,  data = uga,
                    ranges = list(epsilon = seq(0.25,0.35,0.01), cost = 100*(8:12))
)
print(tuneResult3) # epsilon=0.32, cost=1200, RSME=0.18552
# can't seem to beat KNN
plot(tuneResult3)

# Now let's see what the raster looks like
svm_wrap <- function(df,pts) {
  my_svm <- svm(elect~.,df,epsilon=0.32,cost=1200)
  pts_rename <- pts %>% as.data.frame %>%
    dplyr::rename(long=x,lat=y)
  predict(my_svm,pts_rename)
}

svm_raster <- make_elect_shp(uga,svm_wrap,'uga_svm')
plot(svm_raster)
# It looks like SVM gives smoother countours than KNN, but is more inclined
# to crazy extrapolations in areas with no data.

###############################################################################
# What about random forests?
###############################################################################
library(randomForest)

test_rf <- randomForest(elect ~ .,data=uga)
(uga$elect - test_rf$predicted)^2 %>% mean %>% sqrt # 0.1600594, beats KNN
# I could mess with parameters some, but stick with defaults for now.

rf_wrap <- function(df,pts) {
  x_train <- df %>% dplyr::select(-elect)
  x_pred <- pts %>% as.data.frame %>%
    dplyr::rename(long=x,lat=y) %>%
    dplyr::select(lat,long)
  my_rf <- randomForest(x=x_train,y=df$elect,
                        xtest=x_pred)
  my_rf$test$predicted
}

rf_raster <- make_elect_shp(uga,rf_wrap,'uga_rf')
plot(rf_raster)
