library(llamar)
library(ggplot2)

###############################################################################
# Plot historical household electrification rates from DHS API
###############################################################################

dhs_elect <- loadDHS(indicators='HC_ELEC_H_ELC',
                     countries='ET,GH,KE,LB,MW,NG,RW,SN,SL,TZ,UG,ZM') %>%
  mutate(elect=Value) %>%
  dplyr::select(CountryName,SurveyYear,elect) 
  
# ggplot(dhs_elect,aes(x=SurveyYear,y=elect,group=CountryName,color=CountryName)) +
#   geom_point(size=4) +
#   geom_line(size=2) + 
#   theme_classic()

###############################################################################
# Try a linear model to get at electricity access in 2016
###############################################################################

# fit <- lm(elect ~ CountryName + SurveyYear,data=dhs_elect) 
# # summary(fit)
# # R^2 = 0.938 -- Good enough to predict
# 
# newpts <- data.frame(CountryName=unique(dhs_elect$CountryName),
#                      SurveyYear=2016)
# newpts <- newpts %>%
#   mutate(elect=predict(fit,newpts))
# 
# rbind(dhs_elect,newpts) %>% 
#   ggplot(aes(x=SurveyYear,y=elect,group=CountryName,color=CountryName)) +
#     geom_point(size=4) +
#     geom_line(size=2) + 
#     theme_classic()

elect_pred <- function(country,year=2016) {
  dhs_elect %>% filter(CountryName==country) %>%
    lm(elect ~ SurveyYear,data=.) %>%
    predict(data.frame(SurveyYear=year))
}
  
newpts <- data.frame(CountryName=unique(dhs_elect$CountryName),
                     SurveyYear=2016)
for (n in newpts$CountryName) {
  newpts[newpts$CountryName==n,'elect'] <- elect_pred(n)
}

# rbind(dhs_elect,newpts) %>% # filter(CountryName=='Nigeria') %>%
#   ggplot(aes(x=SurveyYear,y=elect,group=CountryName,color=CountryName)) +
#   geom_point(size=4) +
#   geom_line(size=2) + 
#   theme_classic()
# # That looks somewhat better

###############################################################################
# Predicting access growth: is there a "sigmoidal" relationship with wealth
# similar to what we saw with mobile adoption?
###############################################################################
# wealth_elect <- function(df,text) {
#   df %>% removeAttributes %>%
#     ggplot(aes(x=wealth,y=elect)) +
#     geom_jitter(size=5,color='tomato',alpha=0.05,width=0) +
#     geom_smooth(method = "glm", method.args = list(family = "binomial")) +
#     ggtitle(text) +
#     theme_classic() 
# }
# 
# wealth_elect(rwa_2015,'Rwanda 2015')
# # At least in Rwanda (with sort of middle-of-the-pack electrification rates),
# # there does seem to be a relationship where the poorest households are 
# # unelectrified.
# wealth_elect(gha_2014,'Ghana 2014')
# # Also some separation in more highly-electrified Ghana

###############################################################################
# Do I see an electric price point coming down the way we did for mobiles?
###############################################################################
# p50 <- function(df) {
#   s <- df %>% removeAttributes %>% 
#     glm(elect~wealth,data=.,family='binomial') %>%
#     summary
#   data.frame(res=-s$coefficients[1,1]/s$coefficients[2,1])
# }
# elect_compare <- function(a,title='') {
#   # a is a big data frame coming out of rbind
#   l <- a$label %>% unique
#   if (length(l)==2) {
#     p1 <- a[a$label==l[1],] %>% p50
#     p2 <- a[a$label==l[2],] %>% p50
#     print(paste('p50 difference = ',round(p2-p1,digits=3)))
#   }
#   ggplot(a,aes(x=wealth,y=elect,group=label,color=label)) +
#     geom_smooth(method = "glm", method.args = list(family = "binomial")) +
#     ggtitle(title) +
#     theme_classic()
# }
# 
# rbind(gha_2008,gha_2014) %>% removeAttributes %>%
#   elect_compare(title='Ghana 2008-2014') # -0.38
# 
# rbind(ken_2009,ken_2014) %>% removeAttributes %>%
#   elect_compare(title='Kenya 2008-2014') # -0.472
# 
# rbind(lbr_2007,lbr_2013) %>% removeAttributes %>%
#   elect_compare(title='Liberia 2007-2013') # -0.38
# 
# rbind(nga_2008,nga_2013) %>% removeAttributes %>%
#   elect_compare(title='Nigeria 2008-2013') # -0.242
# 
# rbind(rwa_2010,rwa_2015) %>% removeAttributes %>%
#   elect_compare(title='Rwanda 2010-2015') # -0.781; bigger than mobile
# 
# rbind(sle_2008,sle_2013) %>% removeAttributes %>%
#   elect_compare(title='Sierra Leone 2008-2013') # 0.027
# 
# # During this period, Rwanda showed pretty substantial growth, while Sierra Leone
# # showed rather little.
# 
# rbind(uga_2006,uga_2011) %>% removeAttributes %>%
#   elect_compare(title='Uganda 2006-2011') # -0.603
# 
# rbind(zmb_2007,zmb_2014) %>% removeAttributes %>%
#   elect_compare(title='Zambia 2007-2014') # -0.544

###############################################################################
# Use a combination of geography and wealth to predict electrification. If
# the weighted electrification rate is already higher than the target, then 
# don't do anything.
###############################################################################
elect_now <- function(df,cname) {
  target <- newpts[newpts$CountryName==cname & newpts$SurveyYear==2016,'elect']/100
  # for each hh, get the fraction of their neighbors who have electricity
  mod<- df %>% group_by(clust) %>% 
    summarise(clust_avg=mean(elect,na.rm=TRUE),in_clust=n()) %>%
    plyr::join(df,by='clust') %>%
    mutate(out_avg = clust_avg*in_clust/(in_clust-1) - as.numeric(elect)/(in_clust-1)) %>%
    select(-clust_avg,-in_clust)
  # combine this score with wealth to prioritize likely electrification
  fit <- mod %>% removeAttributes %>%
    glm(elect~out_avg,data=.,family='binomial')
  mod$pred <- predict(fit,mod,type='response')
  # add new connections until target is reached
  add_us <- mod %>% mutate(n=row_number()) %>%
    filter(elect==0) %>% arrange(desc(pred)) 
  df$elect_now <- df$elect %>% as.numeric
  for (i in add_us$n) {
    if (w_mean(df$elect_now,df$weight) >= target) {
      break
    }
    df[i,'elect_now'] <- 1
  }
  df
}

eth_2011 <- elect_now(eth_2011,'Ethiopia')
gha_2014 <- elect_now(gha_2014,'Ghana')
ken_2014 <- elect_now(ken_2014,'Kenya')
lbr_2013 <- elect_now(lbr_2013,'Liberia')
mwi_2010 <- elect_now(mwi_2010,'Malawi')
nga_2013 <- elect_now(nga_2013,'Nigeria')
sen_2014 <- elect_now(sen_2014,'Senegal')
sle_2013 <- elect_now(sle_2013,'Sierra Leone')
tza_2010 <- elect_now(tza_2010,'Tanzania')
uga_2011 <- elect_now(uga_2011,'Uganda')
rwa_2015 <- elect_now(rwa_2015,'Rwanda')
zmb_2014 <- elect_now(zmb_2014,'Zambia')
