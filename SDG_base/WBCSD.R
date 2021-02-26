library(stringr)
library(readtext)
#regex-uitdrukkingen testen: https://regexr.com/

setwd("~/projects/SDG_base")
url <- "https://www.wbcsd.org/Overview/Our-members/Members"
#b <- read_html(url)

b <- readtext('WBCSD.txt')
b <- tolower(b)

c7 <- str_extract_all(b, pattern = "(\\/[a-z]*.svg)")
c8 <- str_extract_all(b, pattern = "(\\/[a-z]*_i320.jpg)")
c8
c9 <- str_extract_all(b, pattern = "(\\/([a-z]*\\W)*[a-z]*_i320\\.jpg)") #dit herhaalt het patroon door tussen haakjes te zetten \\W is willekeurig @#$%^&*()
c10 <- str_extract_all(b, pattern = "(\\/([a-z]*\\W)*[a-z]*\\.svg)") #nu zijn ze er allemaal :-)


r1 <- unlist(c(c7, c8, c9, c10))
r1
r2 <- str_remove(r1, "\\/")
r3 <- str_remove(r2, "\\.svg")
r4 <- str_remove(r3, "_i320\\.jpg")
r5 <-as.data.frame(r4)
