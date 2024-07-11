library(tidyverse)
library(ggplot2)
library(dplyr)
library(dslabs)

#url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
#h <- read_html(url)
#class(h)
#h

#tab <- h %>% html_nodes("table")
#tab <- tab[[2]]

#tab <- tab %>% html_table()

#class(tab)

#murders_raw <- tab %>% setNames(c("state", "deaths", "suicide", "homicide", "accidents", "law"))

#head(tab)

#class(tab$population)

#class(tab$total)

#s <- "5'10\""

#cat(s)

#commas <- function(x) any(str_detect(x, ","))
#murders_raw %>% summarize_all(commas)

#test_1 <- str_replace_all(murders_raw$deaths, ",", "")
#test_1 <- as.numeric(test_1)

#test_2 <- parse_number(murders_raw$deaths)
#identical(test_2, test_1)

#murders_new <- murders_raw %>% mutate_at(2:4, parse_number)
#murders_new %>% head

data("reported_heights")

#head(reported_heights)


#reported_heights %>% mutate(new_height = as.numeric(height)) %>% filter(is.na(new_height)) %>% head(n=10)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

problems <- reported_heights %>% filter(not_inches(height)) %>% .$height

problems

#length(problems)

#pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
#str_subset(problems, pattern) %>% head(n=10) %>% cat

#pattern2 <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)"
#str_subset(problems, pattern2) %>% head(n=10) %>% cat


#ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81))
#ind <- ind[!is.na(ind)]
#problems[ind] %>% head(n=10) %>% cat


#pattern3 <- ","

#str_detect(murders_raw$deaths, pattern3)

#str_subset(reported_heights$height, "cm")

#yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
#no <- c("A2B", "A21B")
#s <- c(yes, no)
#pattern3 <- "^[4-7]\\s*'\\s*\\d{1,2}$"
#pattern3_2 <- "^[4-7]'\\s*\\d{1,2}\"$"

#sum(str_detect(problems, pattern3))

#problems[c(2,10,11,12,15)] %>% str_view_all(pattern3)

#str_subset(problems, "''")

#problems %>% str_replace("feet|ft|foot", "'") %>%
#  str_replace("inches|in|''|\"", "") %>%
#  str_detect(pattern3) %>% sum

#data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
#           none_or_more = str_detect(yes, "A1*B"),
#           none_or_once = str_detect(yes, "A1?B"),
#           once_or_more = str_detect(yes, "A1+B"))

#pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <- "^([4-7])\\s*[,\\.\\s*]\\s*(\\d*)$"

#yes <- c("5,9", "5,11", "6,", "6,1")
#no <- c("5'9", ",", "2,8", "6.1.1")
#s <- c(yes, no)

#str_detect(s, pattern_without_groups)

#str_detect(s, pattern_with_groups)

#str_match(s, pattern_with_groups)

#str_extract(s, pattern_with_groups)

#str_replace(s, pattern_with_groups, "\\1'\\2")

#str_subset(problems, pattern_with_groups) %>% str_replace(pattern_with_groups, "\\1'\\2") %>% head


not_inches_or_cm <- function(x, smallest=50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

problems <- reported_heights %>%
  filter(not_inches_or_cm(height)) %>%
  .$height

length(problems)

converted <- problems %>%
  str_replace("feet|ft|foot", "'") %>%
  str_replace("inches|in|''|\"", "") %>%
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")


pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)

mean(index)


problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)



