library(readxl)
library(tidyverse)
library(curl)
library(httr)
library(reshape2)
library(broom)
library(caret)
library(readr)
library(ISOweek)
library(lubridate)

#*******************************************************************************
#* read the weather data of Santiago DR from disk 

W <- W78460 %>% mutate(wk = strftime(date, format = "%V"), 
                  wk_yr =strftime(date, format = "%V-%Y"),
                  week = week(date),
                  week_date = as.Date(cut(as.Date(date), "week")),
                  date = as.Date(date))

#create a function for calculation of temperature sum
# define the time span for the accumulated degree days
span <- 10 #weeks

# make a dataframe with mean and median weather data per day
summ_DR <- W %>% 
    group_by(date) %>%
    summarise(med_temp = median(temp, na.rm = TRUE),
                mean_temp = mean(temp, na.rm = TRUE), # you can also calc mean if you want
                mean_humidity = mean(rhum, na.rm = TRUE),
                mean_rainfall = sum(prcp, na.rm = TRUE))
  # etc for the rest of your vars)  
  
# graph of mean temperature against date
summ_DR %>% filter(date > "2020-01-01") %>%
  ggplot(aes(date, mean_temp)) +
                  geom_line()

#formula to calculate the cumulative sums based on span and cut-off of 13.5
summ_add_DR <- summ_DR %>% filter(date >=  (as.Date("2015-01-01")-span*7)) %>% 
  mutate(grow_temp = (mean_temp - 13.5), cdd = cumsum(grow_temp)) %>%
  mutate(add = cumsum(grow_temp) - lag(cdd, span*7)) %>%
  mutate(crf = cumsum(mean_rainfall)) %>% #calculate cumulative rainfall
  mutate(rfs = cumsum(mean_rainfall) - lag(crf, span*7)) #calculate rainfallsum


# taking out NAs drom data
d1 <- summ_add_DR %>% na.exclude() 

#plot accumulated degree days against date
d1 %>%
  ggplot(aes(date, add)) +
  geom_line() + 
  ylab('Accumulated Degree-Days')+ xlab('date') +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1)) 

# make a dataframe with summed crown rot incidence by pack date
d2 <- df_cl %>% filter(origin == 'Dominicaanse Republiek') %>%
  select(pd, CR, CRC, supplier, days, lot) %>%
  group_by(pd) %>%
  summarize(C = sum(CR+CRC), supplier = supplier, days = days)

# overlay add, cumulative rainfall and crown rot
ggplot() + 
  geom_line(data=d1, aes(x=date, y=add), color='green') + 
  geom_smooth(data=d2, aes(x=pd, y=C*1000+800), color='red',  method = 'loess', span = 0.05)+
  geom_smooth(data=d1, aes(x= date, y = rfs+500), fill = 'blue', method = 'loess', span = 0.05)

# join the crown rot data on the weather data to prepare for a linear regression
data <- d2 %>% left_join(d1, by = c('pd' = 'date'))

# do a normal linear regression
library(broom)
model1 <- data %>% do(tidy(lm(C*100 ~ add + rfs + days + as.factor(supplier), . )))
model1

str(data$C)
#do a fractional response regression
model2 <- glm(C ~ add + rfs, 
              family = binomial,
              data = data)
summary(model2)

data[is.na(data$C),]


#************************************************************************************
#*                                            PERU
#************************************************************************************
write_excel_csv(W84401, "W84401.xls")
getwd()
#*******************************************************************************
#read the weather from meteostat:84401


sapply(84401, read_weather)
W84401 <- read.csv("84401.wd")
names(W84401) <- vars


#* read the weather data of Piura from disk 

W_PE <- W84401 %>% mutate(wk = strftime(date, format = "%V"), 
                       wk_yr =strftime(date, format = "%V-%Y"),
                       week = week(date),
                       week_date = as.Date(cut(as.Date(date), "week")),
                       date = as.Date(date))

#create a function for calculation of temperature sum
# define the time span for the accumulated degree days
span <- 10 #weeks

# make a dataframe with mean and median weather data per day
summ_PE <- W_PE %>% 
  group_by(date) %>%
  summarise(med_temp = median(temp, na.rm = TRUE),
            mean_temp = mean(temp, na.rm = TRUE), # you can also calc mean if you want
            mean_humidity = mean(rhum, na.rm = TRUE),
            mean_rainfall = sum(prcp, na.rm = TRUE))



# graph of mean temperature against date
summ_PE %>% filter(date > "2020-01-01") %>%
  ggplot(aes(date, mean_temp)) +
  geom_line()


#formula to calculate the cumulative sums based on span and cut-off of 13.5
summ_add_PE <- summ_PE %>% filter(date >=  (as.Date("2015-01-01")-span*7)) %>% 
  mutate(grow_temp = (mean_temp - 13.5), cdd = cumsum(grow_temp)) %>%
  mutate(add = cdd - lag(cdd, span*7)) %>%
  mutate(crf = cumsum(mean_rainfall)) %>% #calculate cumulative rainfall
  mutate(rfs = crf - lag(crf, span*7)) #calculate rainfallsum


# taking out NAs from data
d3 <- summ_add_PE %>% na.exclude() 

#plot accumulated degree days against date
d3 %>%
  ggplot(aes(date, add)) +
  geom_line() + 
  ylab('Accumulated Degree-Days')+ xlab('date') +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1)) 

# make a dataframe with summed crown rot incidence by pack date
#d4 <- df_cl %>% filter(origin == 'Peru') %>%
#  select(pd, CR, CRC, supplier, days, ca, lot) %>%
#  mutate(supplier = as.factor(supplier), ca = as.factor(ca)) %>%
#  group_by(pd) %>%
#  summarize(C = sum(CR+CRC), supplier = supplier, days = days, ca = ca, lot=lot)

d4 <- df_cl %>% filter(origin == 'Peru') %>%
  select(pd, CR, CRC, supplier, days, ca, lot) %>%
  mutate(supplier = as.factor(supplier), ca = as.factor(ca), C = CR+2*CRC) 


# overlay add, cumulative rainfall and crown rot
ggplot() + 
  geom_line(data=d3, aes(x=date, y=add), color='green') + 
  geom_smooth(data=d4, aes(x=pd, y=C*2000), color='red',  method = 'loess', span = 0.05)+
  geom_smooth(data=d3, aes(x= date, y = rfs*10), fill = 'blue', method = 'loess', span = 0.05) +
  scale_x_date(name = "date", labels = date) +
  scale_y_continuous(name = "temperature sum °C.d (13.5)", 
                     sec.axis = sec_axis(~.*1, name = "crown rot/rainfall", 
                                         labels = function(b) { paste0(round(b, 0), "%/mm")})) +  
  theme(axis.title.y = element_text(color = "grey"),
    axis.title.y.right = element_text(color = "blue"))

# join the crown rot data on the weather data to prepare for a linear regression
data <- d4 %>% left_join(d3, by = c('pd' = 'date')) %>% filter(pd > "2015-01-01") %>% ungroup()

# do a normal linear regression
library(broom)
model3 <- lm(C ~ add + rfs + days + ca, data = data)
tidy(model3)




summary(data$C)
#do a fractional response regression
model4 <- glm(C ~ add + rfs + days + ca, 
              family = quasipoisson(link = "log"),
              data = data)
summary(model4)
library(lmtest)
library(sandwich)

se_glm_robust = coeftest(model4, vcov = vcovHC(model4, type="HC"))
se_glm_robust

# make a plot of average weekly temperature since start of measurements
tw <- summ_PE %>% mutate(wk = week(date)) %>% group_by(wk) %>%
  summarize(temp_wk = mean(mean_temp, na.rm=TRUE))
plot(tw$wk, tw$temp_wk)


#make a plot of crown rot versus ADD by CA
data %>% filter(C > 0.01) %>% ggplot(aes(add, C, col=ca)) + geom_point()

#make a plot of crown rot versus RFS by CA
data %>% filter(C > 0.01) %>% ggplot(aes(rfs, C, col=ca)) + geom_point()

#make a boxplot of crown rot by CA when add >1000 (below not much difference)
data %>% filter(add > 1000) %>% ggplot(aes(C, col=ca)) + geom_boxplot()

#make a heatmap of add, days and C
#first create bins

data$days <- as.integer(data$days)
data$add <- as.integer(data$add)

lbl_days <- c("11-15", "16-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70")

lbl_add <- c("401-450", "451-500", "501-550", "551-600", "601-650",
             "651-700", "701-750","751-800", "801-850", "851-900","901-950",
             "951-1000", "1001-1050", "1051-1100", "1101-1150", "1151-1200")

lbl_days <- c("11-20", "21-30", "31-40", "41-50", "51-60", "61-70")

lbl_add <- c("401-500", "501-600", "601-700", "701-800", "801-900",
             "901-1000", "1001-1100","1101-1200")


data_hm_PE <- data %>% select(add, days, C, ca, lot) %>%
  mutate(day_bins = cut(days, breaks = seq(10, 70, 10), include.lowest = FALSE, label = lbl_days), 
                add_bins = cut(add, breaks = seq(400,1200, 100), include.lowest = FALSE, label = lbl_add)) %>%
  group_by(day_bins, add_bins, ca) %>%
  summarize(CR_avg = mean(C), n = n())

summary(data_hm_PE$CR_avg)

p2 <- data_hm_PE %>%
  ggplot(aes(x = day_bins, y = add_bins)) +
  geom_tile(aes(fill = CR_avg)) + 
  scale_fill_gradient2(low = "white", high = "blue", 
                       midpoint = 0.1, 
                       breaks = seq(0, 0.2, 0.05), 
                       limits = c(0, 0.2)) +
  labs(title="crownrot Peru by temperature sum and transit time with and without CA",
       x ="transit time (days)", y = "temperature sum (degree.days)") +
  labs(fill = "Avg crown rot ratio") +
  geom_text(aes(label = n), size = 2) +
  facet_grid(. ~ ca)
p2
 
#Kruskall Wallis test for difference of the means
data %>% filter(add > 1000) %>%
group_by(ca) %>%
  summarise(
    count = n(),
    mean = mean(C, na.rm = TRUE),
    sd = sd(C, na.rm = TRUE),
    median = median(C, na.rm = TRUE),
    IQR = IQR(C, na.rm = TRUE)) 

data %>% filter(add > 1000) %>% kruskal.test(C ~ ca, data = .)
            


#************************************************************************************
#*                                            ECUADOR
#************************************************************************************
write_excel_csv(W84370, "W84370.xls") #Tumbes, while Santa Rosa is not available. Rainfall
# in Tumbes is lower (178 versus 418 mm annually).
getwd()
#*******************************************************************************
#read the weather from meteostat:84370
url <- "https://bulk.meteostat.net/hourly/84370.csv.gz"
tempdir()
tmp <- tempfile()
curl_download(url, tmp)
download.file(url, destfile = "84370")




sapply(84370, read_weather)
W84370 <- read.csv("W84370.wd")
names(W84370) <- vars


#* read the weather data of Tumbes from disk 

W_EC <- W84370 %>% mutate(wk = strftime(date, format = "%V"), 
                          wk_yr =strftime(date, format = "%V-%Y"),
                          week = week(date),
                          week_date = as.Date(cut(as.Date(date), "week")),
                          date = as.Date(date))

#create a function for calculation of temperature sum
# define the time span for the accumulated degree days
span <- 10 #weeks

# make a dataframe with mean and median weather data per day
summ_EC <- W_EC %>% 
  group_by(date) %>%
  summarise(med_temp = median(temp, na.rm = TRUE),
            mean_temp = mean(temp, na.rm = TRUE), # you can also calc mean if you want
            mean_humidity = mean(rhum, na.rm = TRUE),
            sum_rainfall = sum(prcp, na.rm = TRUE))



# graph of mean temperature against date
summ_EC %>% filter(date > "2015-01-01") %>%
  ggplot(aes(date, mean_temp)) +
  geom_line()

# graph of rainfall against date : only available after 01-01-2021
summ_EC %>% filter(date > "2015-01-01") %>%
  ggplot(aes(date, sum_rainfall)) +
  geom_line()


#formula to calculate the cumulative sums based on span and cut-off of 13.5
summ_add_EC <- summ_EC %>% filter(date >=  (as.Date("2015-01-01")-span*7)) %>% 
  mutate(grow_temp = (mean_temp - 13.5), cdd = cumsum(grow_temp)) %>%
  mutate(add = cdd - lag(cdd, span*7)) %>%
  mutate(crf = cumsum(sum_rainfall)) %>% #calculate cumulative rainfall
  mutate(rfs = crf - lag(crf, span*7)) #calculate rainfallsum


# taking out NAs from data
d5 <- summ_add_EC %>% na.exclude() 

#plot accumulated degree days against date
d5 %>%
  ggplot(aes(date, add)) +
  geom_line() + 
  ylab('Accumulated Degree-Days')+ xlab('date') +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1)) 

# make a dataframe with summed crown rot incidence by pack date
#d6 <- df_cl %>% filter(origin == 'Peru') %>%
#  select(pd, CR, CRC, supplier, days, ca, lot) %>%
#  mutate(supplier = as.factor(supplier), ca = as.factor(ca)) %>%
#  group_by(pd) %>%
#  summarize(C = sum(CR+CRC), supplier = supplier, days = days, ca = ca, lot=lot)

d6 <- df_cl %>% filter(origin == 'Ecuador' & bio == 1) %>%
  select(pd, CR, CRC, supplier, days, ca, bio, lot) %>%
  mutate(supplier = as.factor(supplier), ca = as.factor(ca), C = CR+2*CRC) 


# overlay add, cumulative rainfall and crown rot: rainfall makes no sense as data only from 01-01-2021
ggplot() + 
  geom_line(data=d5, aes(x=date, y=add), color='green') + 
  geom_smooth(data=d6, aes(x=pd, y=C*1E4), color='red',  method = 'loess', span = 0.05)+
  scale_x_date(name = "date", labels = date) +
  scale_y_continuous(name = "temperature sum °C.d (13.5)", 
                     sec.axis = sec_axis(~./1E3, name = "crown rot", 
                                         labels = function(b) { paste0(round(b, 0), "%")})) +  
  theme(axis.title.y = element_text(color = "grey"),
        axis.title.y.right = element_text(color = "blue"))

# join the crown rot data on the weather data to prepare for a linear regression
data_EC <- d6 %>% left_join(d5, by = c('pd' = 'date')) %>% filter(pd > "2015-01-01") %>% ungroup()

# do a normal linear regression
library(broom)
model5 <- data_EC %>% filter(rfs >= 0) %>% do(tidy(lm(C ~ add +  days + ca, data = .)))
model5
summary(model5)



#do a fractional response regression
model6 <- glm(C ~ add + rfs + days + ca, 
              family = gaussian,
              data = data_EC)
summary(model6)
library(lmtest)
library(sandwich)

se_glm_robust = coeftest(model6, vcov = vcovHC(model6, type="HC"))
se_glm_robust

# make a plot of average weekly temperature since start of measurements
tw <- summ_EC %>% mutate(wk = week(date)) %>% group_by(wk) %>%
  summarize(temp_wk = mean(mean_temp, na.rm=TRUE))
plot(tw$wk, tw$temp_wk)


#make a plot of crown rot versus ADD by CA
data_EC %>% filter(C > 0.01) %>% ggplot(aes(add, C, col=ca)) + geom_point()

#make a plot of crown rot versus RFS by CA
data_EC %>% filter(C > 0.01) %>% ggplot(aes(rfs, C, col=ca)) + geom_point()

#make a boxplot of crown rot by CA when add >1000 (below not much difference)
data_EC %>% filter(add > 1000) %>% ggplot(aes(C, col=ca)) + geom_boxplot()

#make a heatmap of add, days and C
#first create bins

data$days <- as.integer(data_EC$days)
data$add <- as.integer(data_EC$add)

lbl_days <- c("11-15", "16-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70")

lbl_add <- c("401-450", "451-500", "501-550", "551-600", "601-650",
             "651-700", "701-750","751-800", "801-850", "851-900","901-950",
             "951-1000", "1001-1050", "1051-1100", "1101-1150", "1151-1200")

lbl_days <- c("11-20", "21-30", "31-40", "41-50", "51-60", "61-70")

lbl_add <- c("401-500", "501-600", "601-700", "701-800", "801-900",
             "901-1000", "1001-1100","1101-1200")


data_hm_EC <- data_EC %>% select(add, days, C, ca, lot) %>%
  mutate(day_bins = cut(days, breaks = seq(10, 70, 10), include.lowest = FALSE, label = lbl_days), 
         add_bins = cut(add, breaks = seq(400,1200, 100), include.lowest = FALSE, label = lbl_add)) %>%
  group_by(day_bins, add_bins, ca) %>%
  summarize(CR_avg = mean(C), n = n())
summary(data_hm_EC$CR_avg)

p3 <- data_hm_EC %>% drop_na(ca) %>%
  ggplot(aes(x = day_bins, y = add_bins)) +
  geom_tile(aes(fill = CR_avg)) + 
  scale_fill_gradient2(low = "white", high = "purple", 
                       midpoint = 0.025, 
                       breaks = seq(0, 0.15, 0.05), 
                       limits = c(0, 0.2)) +
  labs(title="crown rot EC bio by temperature sum and transit time with and without CA",
       x ="transit time (days)", y = "temperature sum (degree.days)") +
  labs(fill = "Avg crown rot ratio") +
  geom_text(aes(label = n), size = 2) +
  facet_grid(. ~ ca)
p3


data_hm_EC %>% filter(add_bins == c("901-950") & day_bins == c("36-40") & ca == "CA")


#Kruskall Wallis test for difference of the means
data_EC %>% filter(add > 1000) %>%
  group_by(ca) %>%
  summarise(
    count = n(),
    mean = mean(C, na.rm = TRUE),
    sd = sd(C, na.rm = TRUE),
    median = median(C, na.rm = TRUE),
    IQR = IQR(C, na.rm = TRUE)) 

data_EC %>% filter(add > 1000) %>% kruskal.test(C ~ ca, data = .)














     
