library(readr)
library(tidyverse)
library(stringr)
library(lubridate)

lots <- read_delim("H:/Downloads/partijen_20.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)




lots$DateE <- as.Date(lots$ETD, "%d-%m-%Y")
lots$DateA <- as.Date(lots$ETA, "%d-%m-%Y")

as.date
lots %>% 
  mutate(DateE = as.Date(lots$ETD, "%d-%m-%Y"), as.Date(lots$ETA, "%d-%m-%Y"), week=isoweek(DateE))  %>%
  filter(str_detect(MRKACODE, '(?i)[a-z]+[RFA]')) #%>% # (?i) ignores case) %>%
select(Week, week, Supplier, qty, MRKACODE)

  
