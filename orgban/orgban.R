library(readxl)
library(tidyverse)
library(ggthemes)
orgban <- read_excel("orgban.xlsx")
p <- orgban %>% ggplot(aes(x= reorder(country, -perc), y = perc)) + geom_col() 
  
p + labs(title = "Organic banana production share EU27+UK 2021",
    caption = "Source: CIRAD", 
    x = "country", y = "percentage") +
  theme_economist(
    base_size = 10,
    base_family = "sans",
    horizontal = TRUE,
    dkpanel = FALSE
  )

prod <- read_excel("orgban.xlsx", sheet = "year")
p2 <- prod %>% mutate(ton = ec+dr+pe+af+co) %>%
  
  ggplot(aes(x= reorder(year, ton), y = ton)) + geom_col() 

p2 + labs(title = "Organic banana import EU27+UK 2013-2020",
         caption = "Source: CIRAD", 
         x = "year", y = "tonnes") +
  scale_y_continuous(labels = scales::comma) +
  theme_economist(
    base_size = 10,
    base_family = "sans",
    horizontal = TRUE,
    dkpanel = FALSE
  )
