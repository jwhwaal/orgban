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
d4 <- df_cl %>% filter(origin == 'Peru') %>%
  select(pd, CR, CRC, supplier, days, ca, lot) %>%
  mutate(supplier = as.factor(supplier), ca = as.factor(ca)) %>%
  group_by(pd) %>%
  summarize(C = sum(CR+CRC), supplier = supplier, days = days, ca = ca, lot=lot)

# overlay add, cumulative rainfall and crown rot
ggplot() + 
  geom_line(data=d3, aes(x=date, y=add), color='green') + 
  geom_smooth(data=d4, aes(x=pd, y=C*1000+500), color='red',  method = 'loess', span = 0.05)+
  geom_smooth(data=d3, aes(x= date, y = rfs+500), fill = 'blue', method = 'loess', span = 0.05)

# join the crown rot data on the weather data to prepare for a linear regression
data <- d4 %>% left_join(d3, by = c('pd' = 'date')) %>% filter(pd > "2015-01-01") %>% ungroup()

# do a normal linear regression
library(broom)
model3 <- lm(C ~ add + rfs + days + ca, data = data)
tidy(model3)




summary(data$C)
#do a fractional response regression
model4 <- glm(C ~ add + days + ca, 
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


data_hm <- data %>% select(add, days, C, ca, lot) %>%
  mutate(day_bins = cut(days, breaks = seq(10, 70, 10), include.lowest = FALSE, label = lbl_days), 
                add_bins = cut(add, breaks = seq(400,1200, 100), include.lowest = FALSE, label = lbl_add)) %>%
  group_by(day_bins, add_bins, ca) %>%
  summarize(CR_avg = mean(C), n = n())

p2 <- data_hm %>%
  ggplot(aes(x = day_bins, y = add_bins)) +
  geom_tile(aes(fill = CR_avg)) + 
  scale_fill_gradient2(low = "white", high = "steelblue", 
                       midpoint = 0.05, 
                       breaks = seq(0, 1, 0.2), 
                       limits = c(0, 1)) +
  labs(title="crownrot by temperature sum and transit time with and without CA",
       x ="transit time (days)", y = "temperature sum (degree.days)") +
  labs(fill = "Avg crown rot ratio") +
  geom_text(aes(label = n), size = 2) +
  facet_grid(. ~ ca)
p2
p2 





data_hm %>% filter(add_bins == c("901-950") & day_bins == c("36-40") & ca == "CA")


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
            
     
