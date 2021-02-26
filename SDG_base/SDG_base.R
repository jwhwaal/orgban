
#IMPORT FORBES 2000 (2017)
#opening required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)


#read base data file
SDG_Forbes17 <- read_excel("SDG_Forbes17.xlsx")
fb <- SDG_Forbes17
check <- fb %>% select(GICS4, sectorname) %>% group_by(GICS4)

#calculate numeric variables before converting to factors
fb$srr <- as.numeric(fb$report) + as.numeric(fb$gri4) + as.numeric(fb$iirc) + as.numeric(fb$aa1000) + as.numeric(fb$ass)
summary(fb$srr)


#convert numerical values to factors
fb$GICS4 <- as.factor(fb$GICS4)
fb$gri4 <- as.factor(fb$gri4)
fb$aa1000 <- as.factor(fb$aa1000)
fb$iirc <- as.factor(fb$iirc)
fb$gl <- as.factor(fb$gl)
fb$ass <- as.factor(fb$ass)
fb$ir <- as.factor(fb$ir)
fb$gc <- as.factor(fb$gc)
fb$newsam <- as.factor(fb$newsam)
fb$countrygrpname <- as.factor(fb$countrygrpname)

#make some new variables
fb1 <- fb %>% mutate(GICS2 = as.factor(substr(fb$GICS4,1,4)), 
                     GICS1 = as.factor(substr(fb$GICS4,1,2)),
                     size = log10(marketvalue), # size of company
                     s = log10(sdgf), # log of number of SDG references
                     p = log10(pages), # log of number of SDG referencing pages
                     sdg = ifelse(sdgf>0,1,0),
                     fb1$srr <- as.numeric(fb1$report) + as.numeric(fb1$gri4) + as.numeric(fb1$iirc) + as.numeric(fb1$aa1000) + as.numeric(fb1$ass)) # SDG reference dummy - mentioning at least once
                      # sust reporting rate - number of additional reporting features 
summary(fb1$srr)
error <- fb1[(fb1$srr==10),]

fb1$GICS2 <- factor(fb1$GICS2,
                    levels = c(1010,1510,2010,2020,2030,2510,2520,3010, 3020,3030, 3510,3520,4020,4030,4510,4520,4530,5010,5510),
                    labels = c("1010 Energy", 
                              "1510 Materials",
                              "2010 Capital Goods",
                              "2020 Commercial & Professional Services",
                              "2030 Transportation", 
                              "2510 Automobiles & Components", 
                              "2520 Consumer Durables & Apparel",
                              "2550 Retailing", 
                              "3010 Food & Staples Retailing",
                              "3020 Food, Beverage & Tobacco", 
                              "3030 Household & Personal Products", 
                              "3520 Pharmaceuticals, Biotechnology & Life Sciences", 
                              "4020 Diversified Financials", 
                              "4030 Insurance", 
                              "4510 Software & Services", 
                              "4520 Technology Hardware & Equipment", 
                              "4530 Semiconductors & Semiconductor Equipment", 
                              "5010 Telecommunication Services", 
                              "5510 Utilities"))
fb1$region <- factor(fb1$countrygrpname)

#make various plots
fb1 %>% ggplot(aes(sdgf, GICS2)) + geom_boxplot() #boxplot of sdg-references per GICS2
fb1 %>% ggplot(aes(sdgf, GICS2)) + geom_bar(stat = "identity") #idem, barplot of total sdg-references
fb1 %>% filter(pages>0) %>% ggplot(aes(pages, GICS2)) + geom_boxplot() #boxplot of SR pages per GICS2
fb1 %>% filter(pages>0 & iirc == 0) %>% ggplot(aes(pages)) + geom_histogram(binwidth = 25) # histogam of pages 

fb1 %>% ggplot(aes(size, GICS2)) + geom_boxplot()
fb1 %>% ggplot(aes(size, GICS4)) + 
  geom_boxplot() 
  
#plot SDG references per countrygroup

fb1 %>% filter(sdgf>0 ) %>% 
  drop_na(region) %>% 
  ggplot(aes(region, s)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(color= "blue", size=5)) +
  coord_flip()


#plot log(sdgf) against GICS2 on order 
fb1 %>% filter(sdgf>0 ) %>% 
  drop_na(GICS2) %>% 
  ggplot(aes(x=reorder(GICS2, s, FUN = median, na.rm = TRUE), y=s)) + 
  geom_boxplot() +
  theme(axis.text.y = element_text(color= "blue", size=5)) +
  coord_flip()


fb1 %>% filter(sdgf>0) %>% ggplot(aes(sdgf, newsam)) + geom_boxplot() 


fb1 %>% filter(sdgf>0) %>% ggplot(aes(s, GICS1)) + 
  geom_boxplot() + 
  facet_grid(gc ~ countrygrpname)
fb1 %>% filter(sdgf>0) %>% ggplot(aes(size, s, color = GICS2 )) +
  geom_point() +
  facet_wrap( ~ countrygrpname)
fb1 %>% filter(sdgf>0) %>% ggplot(aes(size, s, color = GICS2 )) +
  geom_point() +
  facet_wrap( ~ countrygrpname)
fb1 %>% ggplot(aes(size, srr, color = countrygrpname )) +
  geom_boxplot() +
  facet_wrap( ~ gc)
fb1 %>% ggplot(aes(log10(pages+1), log10(sales), color = countrygrpname)) +
  geom_point()
fb1 %>% ggplot(aes(log10(sdgf+1), gc, color = GICS2)) +
  geom_boxplot() +
  facet_grid(~ countrygrpname)

library(ggpmisc)
fb1 %>% filter(sdgf>0) %>% ggplot(aes(p, s, color = GICS1 )) +
  geom_boxplot() 
fb1 %>% filter(sdgf>0) %>% ggplot(aes(uai, s, color = countrygrpname )) +
  geom_point() 
fb1 %>% filter(pages>0) %>% group_by(GICS2) %>% summarise(sp = mean(sdg)) %>%
    ggplot(aes(GICS2, sp)) +
  geom_boxplot() 


fb1 %>% ggplot(aes(log10(sdgf+1), gc, color = countrygrpname )) +
  geom_boxplot() +
  geom_jitter(width = .05, alpha = .4) +
  guides(fill = "none") +
  theme_bw() +
  labs(
    x = "log SDGf",
    y = "GC")

fb1 %>% ggplot(aes(log10(pages+1), gc, color = countrygrpname )) +
  geom_boxplot() +
  geom_jitter(width = .05, alpha = .4) +
  guides(fill = "none") +
  theme_bw() +
  labs(
    x = "log pages",
    y = "GC")



#check problems with GICS-classification (NAs)
fb1 %>% select(pkcompany, GICS4) %>% filter(is.na(GICS4))
