library(readxl)
library(tidyverse)
library(curl)
library(httr)
library(reshape2)
library(lubridate)
library(broom)
library(caret)


#read the most actual quality report database from AgroFair K-drive
df <- read_excel("K:/AgroFair - Q1 Kwaliteitsrapporten/Kwaliteitsrapporten - Bananen/Database/database jaarraportage.xlsm")

setwd("C:/Users/hwwaal/projects/quality")

df %>% filter('pack date' > "2021-12-31") %>% write.csv("quality_data21.csv")

#read historic Piura weather data Piura from meteostat 
url <- "https://bulk.meteostat.net/hourly/84401.csv.gz"

tempdir()
tmp <- tempfile()
curl_download(url, tmp)
download.file("https://bulk.meteostat.net/hourly/84401.csv.gz", destfile = "wd.csv")
wd <- read.csv("wd.csv")


#collect data structure of weather report and create week numbers and week-year numbers (as character), and cutDate as first date of week
names <- read.csv("weather-structure.csv", header=FALSE, sep=";")
vars <- unlist(names$V2)
colnames(wd) <- vars
wd_star <- wd %>% mutate(wk = strftime(date, format = "%V"), 
                         wk_yr =strftime(date, format = "%V-%Y"),
                         week = isoweek(date),
                         week_date = as.Date(cut(as.Date(date), "week")),
                         date = as.Date(date))

#create new dataframe and calculate: 
# remove unused columns
# calculate daily max and min temperatures and daily growth degrees (13 deg threshold)
# calculate average weekly min and max temperatures 
wd_star_star <- wd_star %>% select(-snow, -coco, -wdir, -wspd) %>% 
    group_by(date) %>% 
    mutate(t_max_day = max(temp,na.rm=TRUE), 
           t_min_day = min(temp,na.rm=TRUE),
           grow_d = (t_max_day + t_min_day)/2-13) %>% # denk eraan dat bij lagere temperaturen dan 13 gr er een ifelse in moet: 0 als temp <13 gr
    group_by(wk_yr) %>% 
    mutate(t_max_avg_wk = mean(t_max_day), 
           t_min_avg_wk = mean(t_min_day), 
           rhum_avg_wk = mean(rhum, na.rm=TRUE),
           t_avg_mean = mean(temp, na.rm=TRUE),
           prcp_sum_wk = sum(prcp), na.rm=TRUE)
    
#calculate degree-days per week - zorg ervoor dat er één grow_d per datum wordt opgenomen in het df
periode <- 11 #weken
library(RcppRoll)
dd <- wd_star_star %>%
  aggregate(by = list(wd_star_star$date), max) %>%  #neemt van elke datum de hoogste waarde, nu maar één waarneming per datum
  filter(date > "2015-01-01") %>%
  mutate(degreedays = roll_sum(grow_d, periode*7, align = "right", fill = NA))

#graph of degree-days per day
dd%>% ggplot(aes(wk, degreedays)) +
  geom_boxplot()

#graph of degree-days per day
dd%>% ggplot(aes(date, degreedays)) +
  geom_line()

weather_graph <- wd_star_star %>% ggplot()  + 
    geom_boxplot(aes(x=wk,y=rhum_avg_wk),color='red') + 
    geom_boxplot(aes(x=wk,y=t_avg_mean), color='blue') + 
    geom_boxplot(aes(x=wk,y=t_max_avg_wk), color='dark blue') +
    geom_boxplot(aes(x=wk,y=t_min_avg_wk), color='light blue') +
    geom_boxplot(aes(x=wk,y=prcp_sum_wk), color='purple')
weather_graph +
    ylab('Values (RH t_max, t_avg, t_min, precipitation')+ xlab('week') +
    theme(text = element_text(size=8),
          axis.text.x = element_text(angle=0, hjust=1)) 

#graph in long format
wd_star_star_long <- dd %>% 
  select(wk, wk_yr,week_date, degreedays, t_max_avg_wk ,t_min_avg_wk, rhum_avg_wk,t_avg_mean, prcp_sum_wk) %>%
  gather(key = "weather_attr", value="value", t_max_avg_wk ,t_min_avg_wk, rhum_avg_wk,t_avg_mean, prcp_sum_wk) 


#make a boxplot of weather data since 2015
wd_star_star_long %>% 
  mutate(year = as.numeric(substring(wk_yr,4,7))) %>%
  filter(year >= 2015) %>% 
ggplot(aes(wk, value, col=weather_attr)) +
  geom_boxplot() +
  ylab('Values (RH t_max, t_avg, t_min, precipitation')+ xlab('week') +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=0, hjust=1)) 

#make a line graph of weather data since 2015
wd_star_star_long %>% 
  mutate(year = as.numeric(substring(wk_yr,4,7))) %>%
  filter(year >= 2015) %>% 
  ggplot(aes(week_date, value, col=weather_attr)) +
  geom_line(stat = "identity") +
  ylab('Values (RH t_max, t_avg, t_min, precipitation')+ xlab('week') +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=0, hjust=1)) 

#QUALITY DATA

#rename & create variables & clean some values & calculate sum of crownrot per number of containers per week 
df <- df %>% rename(ca = 'ca/no ca', 
                    pd = 'pack date', 
                    dd = 'date discharge', 
                    line = 'shipping company',
                    maxgrade = 'max grade')
df$dd <- as.Date(df$dd)
df$pd <- as.Date(df$pd)

df <- df %>% select(lot, supplier, line, grower, dd, port, week, vessel, origin, ca, plastic, brand, pd, days, maxgrade, CR, CRC, CD, CDC) %>%
  mutate(bio = ifelse(str_detect(brand, pattern =("BIO|EKO")),1,0), 
                    wk=strftime(pd, format = "%V"),
                    week = isoweek(pd),
                    week_date = floor_date(as.Date(pd), unit="week")) %>%
  group_by(week_date, supplier, ca) %>%
  mutate(CR_avg = sum(CR+CRC)/n(), maxgrade_avg= mean(maxgrade))

# leftjoin the weather data from dd on the pack-date
dd_small <- dd %>% select(date, degreedays, t_max_avg_wk, t_min_avg_wk, rhum_avg_wk, t_avg_mean, prcp_sum_wk)
df <- df %>% left_join(dd_small, by=c("pd" = "date"))


# make shipping line names uniform.
df$line <- str_replace_all(df$line, pattern = "Mediterranean Shipping Company S.A.", "Mediterranean Shipping Company BV")
df$line <- str_replace_all(df$line, pattern = "Hapag-Lloyd Container Line", "Hapag-Lloyd AG")
df$line <- str_replace_all(df$line, pattern = "Hamburg Sud Nederland", "Hamburg Sud")


#convert characters to factors
df$ca <- as.factor(df$ca)
df$origin <-as.factor(df$origin)
df$bio <-as.factor(df$bio)
df$line <- as.factor(df$line)


#slice and filter data
p<- df %>% filter(origin == "Peru" & pd> "2015-01-01" & !is.na(ca)) %>% 
  ggplot(aes(wk, CR+CRC, fill=ca)) +
  geom_col()
p + theme(axis.text=element_text(size=6),
          axis.title=element_text(size=14,face="bold")) 

p2<- df %>% filter(origin == "Peru" & pd> "2015-01-01" & !is.na(ca)) %>% 
  ggplot(aes(wk,CD+CRC, col=ca)) +
  geom_point()
p2  + theme(axis.text=element_text(size=6),
         axis.title=element_text(size=14,face="bold")) +
  facet_wrap(~ supplier)


table <- df %>% filter(origin == "Peru" & pd> "2015-01-01" & !is.na(ca)) %>%
  select(supplier, CR, CRC, ca, CR_avg, supplier) %>%
  group_by(supplier, ca) %>%
  summarize(no_ctrs = n(), avg_cr = sum(CR+CRC)/no_ctrs)
table

df %>% filter(origin == "Peru" & pd> "2015-01-01" & !is.na(ca)) %>%
  ggplot(aes(supplier, CR_avg, fill=ca)) +
  geom_col(position = position_dodge()) +
  coord_flip()


table2 <- df %>% filter(origin == "Dominicaanse Republiek" & pd> "2015-01-01" & !is.na(ca)) %>%
  select(supplier, CR, CRC, ca, wk, lot) %>%
  group_by(supplier, ca) %>%
  summarize(no_ctrs = n(), avg_cr = sum(CRC)/no_ctrs)
table2

table2 %>% ggplot(aes(supplier, avg_cr, fill=ca)) +
  geom_col() +
  coord_flip()
#make various plots
#percentage crownrot per week (sum %CR+CRC / # containers)
table3 <- df %>% filter(origin == "Peru" & pd> "2015-01-01" & !is.na(ca)) %>%
  select(vessel, CR, CRC, ca, wk, lot) %>%
  group_by(wk, ca) %>%
  mutate(no_ctrs = n(), avg_cr = sum(CRC)/no_ctrs) %>%
  ungroup() %>%
ggplot(aes(wk, avg_cr, fill=ca)) +
  geom_col() +
    theme(text = element_text(size=10),
                     axis.text.x = element_text(angle=90, hjust=1)) +
  ylab('Crown rot (weekly average %)')+ xlab('week') +
  ggtitle("Crown rot incidence Peru per week 2015-2020") +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=0, hjust=1)) 
  
table3

table4 <- df %>% filter(origin == "Peru" & pd> "2015-01-01" & !is.na(ca)) %>%
  select(vessel, CR, CRC, ca, wk, lot) %>%
  group_by(vessel, ca) %>%
  summarize(no_ctrs = n(), avg_cr = sum(CRC)/no_ctrs) %>%
  filter(no_ctrs >10)
table4


table4 %>% ggplot(aes(x = reorder(vessel, avg_cr), y=avg_cr, fill=ca)) +
  geom_col() +
  coord_flip() +
  theme(text = element_text(size=7),
                     axis.text.x = element_text(angle=90, hjust=1)) 
  


table5 <- df %>% filter(origin == "Peru" & pd> "2015-01-01" & !is.na(ca)) %>%
  select(line, CR, CRC, ca, wk, lot) %>%
  group_by(line, ca) %>%
  summarize(no_ctrs = n(), avg_cr = sum(CRC)/no_ctrs) %>%
  filter(no_ctrs >10)
table5


table5 %>% ggplot(aes(x = reorder(line, avg_cr), y=avg_cr, fill=ca)) +
  geom_col() +
  coord_flip() +
  theme(text = element_text(size=7),
        axis.text.x = element_text(angle=90, hjust=1)) 

table6 <- df %>% filter(origin == "Peru" & pd> "2015-01-01" & !is.na(ca) & days>10) %>%
  select(line, days, CR, CRC, ca, wk, lot) %>%
  group_by(line, ca) %>%
  summarize(no_ctrs = n(), avg_cr = sum(CRC)/no_ctrs, avg_tt = sum(days/no_ctrs)) %>%
  filter(no_ctrs >10)
table6


table6 %>% ggplot(aes(x = reorder(line, avg_tt), y=avg_tt, fill=ca)) +
  geom_col(position = position_dodge()) +
  coord_flip() +
  theme(text = element_text(size=7),
        axis.text.x = element_text(angle=90, hjust=1)) 

table7 <- df %>% filter(origin == "Peru" & pd> "2015-01-01" & !is.na(ca) & days>10 & days < 50) %>%
  select(line, days, CR, CRC, ca, wk, lot) %>%
  ggplot(aes(days, CRC, col=line)) +
  geom_point() +
  theme(text = element_text(size=7),
        axis.text.x = element_text(angle=90, hjust=1)) 
table7

table8 <- df %>% filter(origin %in% c("Peru", "Ecuador", "Dominicaanse Republiek")
                        & pd> "2015-01-01" & days %in% 15:45 &!is.na(ca)) %>%
  select(vessel, CR, CRC, ca, wk, lot, origin) %>%
  group_by(wk, origin, ca) %>%
  mutate(no_ctrs = n(), avg_cr = sum(CRC)/no_ctrs) %>%
  ungroup() %>%
  ggplot(aes(wk, avg_cr, fill=ca)) +
  geom_col() +
  theme(text = element_text(size=5),
        axis.text.x = element_text(angle=90, hjust=1)) +
  ylab('Crown rot (weekly average %)')+ xlab('week') +
  ggtitle("Crown rot incidence PE/EC/DR per week 2015-2020") +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=, hjust=1)) 

table8 + facet_wrap( ~ origin) + 
  scale_x_discrete(breaks = seq(10, 52, by = 10))





# make a plot of degreedays and crownrot- kroonrot zou beter per week gemiddeld kunnen worden.
df_long <- df %>% 
  mutate(wk = isoweek(pd),
         kroonrot = CR_avg*1000, #for better scaling
         degd = degreedays/10) %>% #for better scaling
  gather(key = "parameter", value="value", kroonrot, t_max_avg_wk ,t_min_avg_wk, rhum_avg_wk,t_avg_mean, degd)
df_long %>% filter(pd > "2015-01-01" & origin == "Peru") %>% 
  ggplot(aes(week_date, value, col=parameter)) +
  geom_point() +
  ylab('Values (RH t_max, t_avg, t_min, precipitation')+ xlab('week') +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=0, hjust=1)) 

df_long %>% ggplot(aes(as.factor(wk), value, col=parameter)) +
  geom_boxplot() +
  ylab('Values ')+ xlab('week') +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=0, hjust=1)) 
df_long %>% filter(pd>"2015-01-01") %>%
ggplot(aes(pd, value, col=parameter)) +
  geom_line(stat="identity") +
  ylab('Values') + xlab('packdate') +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=0, hjust=1)) 

#do linear regression 
lr_data <- df %>% filter(pd >"2015-01-01" & origin == 'Peru' & 
                           supplier %in% c('APPBOSA', 'APOQ', 'VDCHIRA') & !is.na(ca) & days>25) %>%
  group_by(week_date) %>%
  mutate(degreedays_wk = mean(degreedays), crs = sin(2*pi*week/52), crc = cos(2*pi*week/52))
lr_data %>% ggplot(aes(degreedays_wk, CR_avg, col=supplier)) +
  geom_point()
lr_data %>% ggplot(aes(degreedays_wk, CR_avg, col=ca)) +
  geom_point()
write.csv(lr_data, "lr_data.csv")


#linear regression of CR_avg on degreedays (weekly averages)
library(broom)
model1 <- lm(CR_avg ~ degreedays_wk, data=lr_data)
tidy(model1, conf.int = TRUE)
newdata=data.frame(degreedays_wk = seq(500, 1400, 100))
predict.lm(model1, newdata)
plot(predict.lm(model1, newdata))
summary(model1)

model2 <- update(model1, .~. + supplier)
tidy(model2)

model3 <- update(model2, .~. + ca)
tidy(model3)

model4 <- update(model3, .~. + days)
tidy(model4)

model5 <- update(model4, .~. + maxgrade_avg )
tidy(model5)
summary(model5)

model6 <- update(model5, .~. + prcp_sum_wk)
tidy(model6)
model.diag.metrics <- augment(model6)
model.diag.metrics
plot(model6)


ggplot(model.diag.metrics, aes(degreedays_wk, CR_avg)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_point(aes(x = degreedays_wk, y = .fitted), color = "red", size = 0.3)



newdata <- lr_data %>% select(degreedays_wk, supplier, ca, days, maxgrade_avg, week, week_date, dd, CR_avg) %>%
  filter(dd >= "2020-01-01" & dd <= "2020-12-31" & ca == 'NO CA' & days >25)  



ggplot(aes(week, CR_avg), data = pred) +
  geom_point() 

ggplot(aes(CR_avg, CR_hat), data = pred) +
  geom_point() 


model7 <- lm(CR_avg ~ crc + crs + week, data=lr_data)
tidy(model7)
summary(model7)
predict(model7, newdata)

# access the fitted series (for plotting)
fit <- fitted(model7)

df_long

data<- df %>% select(supplier, line, origin, week, ca, plastic, pd, days, maxgrade_avg, CR_avg, week_date) %>%
  mutate(supplier = as.factor(supplier), plastic = as.factor(plastic),  pw = week(pd), crrisk = as.factor(ifelse(CR_avg > 0.02, 1,0))) %>% 
  select(-pd, - CR_avg) %>% drop_na()
x <- data %>% select(-crrisk) as_tibble() %>% remove_rownames()
class(x)
y <- data$crrisk
class(y)
data <- data.frame(y,x)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- data[test_index, ] 
train_set <- data[-test_index, ]     

ks <- seq(1, 100, 10)
f <- sapply(ks, function(k){
  fit <- knn3(y ~ ., data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$y))
  accuracy <- confusionMatrix(data = y_hat, reference = test_set$y)$overall["Accuracy"]
  tibble(ks, accuracy)
})
plot(ks, accuracy)
max(F_1)
ks[which.max(F_1)]

fit <- knn3(y ~ ., data = train_set, k = 5)
  y_hat <- predict(fit, test_set, type = "class") %>% 
  factor(levels = levels(train_set$y))
accuracy <- confusionMatrix(data = y_hat, reference = test_set$y)
accuracy

library(e1071)

model <- train(x,y, 'glm', trControl = trainControl(method = 'cv', number =10))
model

