library(dplyr)

###############################################################################
# TODO: I should move any other frequently-reused code into here, for example
# setting the path for data files. Also using devtools to load llamar, since I
# shouldn't assume that people have it.
###############################################################################

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

###############################################################################
# standardized bar plot
###############################################################################
highlight_bar <- function(df,hi_country=NULL,default_color='#6cafcc',
                          highlight_color='#ebe85d',title=NULL,pcent=TRUE) {
  # assumes that df is a data frame containing two columns, "country" and "value"
  tmp <- arrange(df,value)
  tmp$country <- factor(tmp$country,levels=tmp$country)
  if (pcent) {
    tmp$text <- round(tmp$value) %>% paste0('%')
  } else {
    tmp$text <- round(tmp$value,2)
  }
  tmp$barcolor <- default_color
  if (!is.null(hi_country)) {
    tmp[tmp$country %in% hi_country,'barcolor'] <- highlight_color
  }
  tmp$hj <- ifelse(tmp$value>0,1.1,-0.1)
  ggplot(tmp,aes(x=country,y=value)) +
    geom_bar(stat='identity',fill=tmp$barcolor) +
    geom_text(aes(label=text,hjust=hj)) +
    coord_flip() +
    ggtitle(title) +
    xlab('') + ylab('') +
    theme_classic() +
    theme(axis.ticks = element_blank(),
          axis.text.x = element_blank())
}
