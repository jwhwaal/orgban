library(readxl)
library(tidyverse)
claims_2020 <- read_excel("C:/Users/hwwaal/Dropbox/AGROFAIR BIG DATA/QUALITY DATA/claims.xlsx")

cf <- claims_2020
cf <- cf %>% rename(lot = 'Partij no.') %>% 
  mutate(lot = as.numeric(str_sub(lot, 1,5))) %>% left_join(.,lot)


cf$clAF <- cf$clAF %>% as.numeric() %>% replace_na(0)
cf$clPR <- cf$clPR %>% as.numeric() %>% replace_na(0)


cf$claim <- cf$clAF + cf$clPR

cf <- cf %>% l


cf_1 <- cf %>% select(nature, claim, lot, Client) %>%
  rowwise %>% drop_na(any_of(vars)) %>%
  mutate(pn = as.numeric(str_sub(lot,1,5)), C=str_detect(nature, 'CR')) 
  

d<- cf_1$C %>% na.omit()
mean(d)


cf_1 %>%
  ggplot(aes(reorder(nature, -claim, sum), claim)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7))
  
cf_2 <- read_excel("K:/AgroFair - Q1 Kwaliteitsrapporten/Kwaliteitsrapporten - Bananen/Database/database jaarraportage.xlsm")
cf_2 <- cf_2 %>% filter(lot>= min(cf_1$pn, na.rm=TRUE)) %>% 
  select(origin, lot, CR, CRC, CD, CDC) 

dim(cf_1)
dim(cf_2)

cf_3 <- inner_join(cf_1, cf_2, by = c("pn" = "lot")) %>% 
  filter(C==TRUE) %>% 
  mutate(kr = (0.75*CR+1.5*CRC+0.25*CD+0.5*CDC)*100, 
         claim_n = (claim - min(claim))/(max(claim) - min(claim)), 
         kr_n = (kr-min(kr))/(max(kr)-min(kr)))
dim(cf_3)

cf_3 %>% filter(kr<60) %>% ggplot(aes(kr, claim, col = Client)) +
  geom_point() + geom_smooth(method = "gam", formula = y ~ poly(x, 2)) + 
  ggtitle("Claim value  per lot caused by crown rot in EUR") +
  xlab("Crown rot index per lot") + ylab("Claim value") 

#totaal claimschade per jaar
cf_3 %>% summarize(totaal = sum(claim))
cf_3 %>% group_by(Client) %>% 
  summarize(totaal = sum(claim), n = n(), average_claim = mean(claim)) %>% 
  arrange(., desc(average_claim))
cf_3 %>% filter(Client != "CTM / I") %>% summarize(totaal = sum(claim), n= n(), average_claim = mean(claim)) %>%
  arrange(., desc(average_claim))


df_cr <- df %>% select(lot, dd, CR, CRC, CD, CDC, origin) %>% left_join(., cf)
df_cr$claim[is.na(df_cr$claim)] <- 0

df_cr %>% mutate(kr = (CR+CRC+CD+CDC)*100) %>% 
  filter(kr>=0) %>%
  ggplot(aes(kr, claim)) +
  geom_point()
df_cr %>% group_by(origin) %>%
  summarize(avg_claim = mean(claim))






library(broom)


lm_fit <- lm(claim_n ~ kr_n, data=cf_3) #%>% tidy()
summary(lm_fit)

lm_fit <- lm(claim ~ kr, data=cf_3) #%>% tidy()
summary(lm_fit)



write_csv(cf_3, "claim.csv")

lm_poly <- lm(claim_n ~ poly(kr_n, 3, raw=TRUE), data = cf_3)
summary(lm_poly)

