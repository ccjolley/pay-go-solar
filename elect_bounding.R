# My goal here is to come up with something that will draw a (fairly simple?) 
# boundary around areas of high electrification, so that this can be added to my
# population/mobile maps without having to introduce a new color scale.

# Test out on Uganda

library(dplyr)
library(ggplot2)

uga_data <- read.csv('uga_dhs_2011.csv') %>%
  filter(lat != 0)

uga <- uga_data %>% select(lat,long,elect)

# First, visualize as colored dots
ggplot(uga_data,aes(x=long,y=lat,color=elect)) +
  geom_point(size=4) +
  theme_classic()
# TODO: add borders from shapefile

# Next steps: 
# - Train some model(s) to predict electrification levels based on lat/long
# - Create a raster with dimensions slightly larger than Uganda points
# - Function to fill in raster with predicted values
# - Function to convert this raster with a binary threshold
# - Function to extract polygon shapefile from binary raster (or use ArcGIS?)

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


