library(measurements)
library(readr)
test <- read_csv("~/test.txt")
t_cm <- conv_unit(test$inch, "inch", "cm")
t_cm <- set_units(t_cm, cm)
plot(test$inch, t_cm)


test$inch
library(stringr)
library(units)
mat <- as.matrix(stringr::str_extract_all(test$inch, "\\d+", simplify = TRUE))

storage.mode(mat) <- "numeric" #wijzigt karakter in numeriek.
mat[is.na(mat)] <- 0
mat
length <- mat[,1]*2.54 + mat[,2]/8*2.54
length

inch <- function(a){
  mat <- as.matrix(stringr::str_extract_all(a, "\\d+", simplify = TRUE))
  storage.mode(mat) <- "numeric" #wijzigt karakter in numeriek.
  mat[is.na(mat)] <- 0
  length <- mat[,1]*2.54 + mat[,2]/8*2.54
  length <- set_units(length, cm)
  }
length <- inch(test$inch)
hist(length)

plot(length, t_cm)
df <- data.frame(x=length, y=t_cm)
library(tidyverse)
library(ggforce)
qplot(x=x, y=y, data=df) +
  scale_x_unit(unit = 'cm') +
  scale_y_unit(unit = 'cm')





