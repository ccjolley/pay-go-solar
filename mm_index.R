###############################################################################
# Mobile money usage index
#
# TODO: Get rid of the urban-rural stuff; there's nothing there
# TODO: Make sure that what is left at the end is a single dataframe with the
#       important outputs.
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
  impute_mm %>%
  mutate(all = pc1,
         gap = mm_gap$'pc1',           # quotes are to avoid namespace issues
         rural = mm_rural$'pc1') %>%
  select(-pc1)

mm_all %>% is.na %>% rowSums %>% table

mm_all[rowSums(is.na(mm_all)) < 3,] %>%
  ggplot(aes(x=all,y=gap,label=country_name)) +
  geom_point() +
  geom_text(vjust=1) +
  geom_vline(xintercept=mm_all[mm_all$country_name=='Low income','all'],color='red') +
  geom_hline(yintercept=mm_all[mm_all$country_name=='Low income','gap'],color='red') +  
  xlab('Mobile money usage') +
  ylab('Urban-rural gap') +
  theme_classic()

rm(mm,mm_gap,mm_rural,dx,s)