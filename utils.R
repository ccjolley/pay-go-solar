library(dplyr)

###############################################################################
# for a labelled vector x, replace numeric value with string labels
###############################################################################
labelled_to_str <- function(x) {
  vals <- x %>% attr('labels')
  names <- vals %>% attr('names') %>% as.character
  df1 <- data.frame(x)
  names(df1) <- 'val'
  df2 <- data.frame(val=vals,name=names)
  j <- plyr::join(df1,df2,by='val')
  j$name %>% as.character
}

###############################################################################
# calculate weighted mean
###############################################################################
w_mean <- function(x,w) {
  tmp <- data.frame(x=x,w=w) %>% na.omit
  sum(tmp$x * tmp$w) / sum(tmp$w)
}

