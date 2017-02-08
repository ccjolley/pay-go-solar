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