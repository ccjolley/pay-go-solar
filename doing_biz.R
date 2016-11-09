library(ggplot2)
library(XLConnect)
library(forcats)
library(dplyr)

db <- readWorksheetFromFile("C:/Users/Craig/Desktop/Live projects/Pay-go solar/credit.xlsx",1)
db$fill <- as.factor(c(1,1,1,1,1,2,2))

ggplot(db,aes(x=fct_rev(fct_inorder(Country)),y=legal_rights))  +
  geom_bar(stat='identity',aes(fill=fill)) +
  geom_text(aes(label=legal_rights,y=legal_rights-0.3),hjust=1) +
  geom_text(aes(label=Country,y=0.3),hjust=0) +
  coord_flip() +
  ggtitle('Strength of legal rights index (0-12)') +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "none")

ggplot(db,aes(x=fct_rev(fct_inorder(Country)),y=credit_info))  +
  geom_bar(stat='identity',aes(fill=fill)) +
  geom_text(aes(label=credit_info,y=credit_info-0.2),hjust=1) +
  geom_text(aes(label=Country,y=0.2),hjust=0) +
  coord_flip() +
  ggtitle('Depth of credit information index (0-8)') +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "none")

### Additional plots from Global Findex

source('read-findex.R')
clist <- c('Nigeria','Rwanda','Tanzania','Uganda','Zambia',
           "Sub-Saharan Africa (developing only)","High income: OECD")
gf_focus <- gf_wide[gf_wide$country_name %in% clist,] 

focus_plot <- function(code,pcent=TRUE) {
  tmp <- data.frame()
  for (c in clist) {
    tmp <- rbind(tmp,data.frame(country=c,value=gf_focus[gf_focus$country_name==c,code]))
  }
  tmp <- tmp %>% 
    mutate(country = as.character(country),
           country = ifelse(country=='High income: OECD','OECD high income',country),
           country = ifelse(country=='Sub-Saharan Africa (developing only)','SS Africa',country))
  title <- key[key$series_code==code,'series_name'] %>% 
    gsub(' \\[w2\\]','',.)
  if (pcent) {
    tmp$text <- round(tmp$value) %>% paste0('%')
  } else {
    tmp$text <- round(tmp$value,2)
  }
  tmp$barcolor <- as.factor(c(1,1,1,1,1,2,2))
  tmp$hj <- ifelse(tmp$value>0,1.1,-0.1)
  ggplot(tmp,aes(x=fct_rev(fct_inorder(country)),y=value)) +
    geom_bar(stat='identity',aes(fill=barcolor)) +
    geom_text(aes(label=text,hjust=hj)) +
    geom_text(aes(label=country,y=0.5),hjust=0) +
    coord_flip() +
    ggtitle(title) +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          legend.position = "none")
}

focus_plot('WP14917.1') # Borrowed from a financial inst.
# This function can easily be re-used with any other Findex indicator.
