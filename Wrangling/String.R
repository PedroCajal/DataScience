#library(tidyverse)
#library(ggplot2)
library(dplyr)
library(dslabs)
#library(purrr)
#library(lubridate)


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

#data("reported_heights")

#head(reported_heights)


#reported_heights %>% mutate(new_height = as.numeric(height)) %>% filter(is.na(new_height)) %>% head(n=10)

#not_inches <- function(x, smallest = 50, tallest = 84){
#  inches <- suppressWarnings(as.numeric(x))
#  ind <- is.na(inches) | inches < smallest | inches > tallest
#  ind
#}

#problems <- reported_heights %>% filter(not_inches(height)) %>% .$height

#problems

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
#pattern_with_groups <- "^([4-7])\\s*[,\\.\\s*]\\s*(\\d*)$"

#yes <- c("5,9", "5,11", "6,", "6,1")
#no <- c("5'9", ",", "2,8", "6.1.1")
#s <- c(yes, no)

#str_detect(s, pattern_without_groups)

#str_detect(s, pattern_with_groups)

#str_match(s, pattern_with_groups)

#str_extract(s, pattern_with_groups)

#str_replace(s, pattern_with_groups, "\\1'\\2")

#str_subset(problems, pattern_with_groups) %>% str_replace(pattern_with_groups, "\\1'\\2") %>% head


#not_inches_or_cm <- function(x, smallest=50, tallest = 84){
#  inches <- suppressWarnings(as.numeric(x))
#  ind <- !is.na(inches) &
#    ((inches >= smallest & inches <= tallest) |
#       (inches/2.54 >= smallest & inches/2.54 <= tallest))
#  !ind
#}

#problems <- reported_heights %>%
#  filter(not_inches_or_cm(height)) %>%
#  .$height

#length(problems)

#converted <- problems %>%
#  str_replace("feet|ft|foot", "'") %>%
#  str_replace("inches|in|''|\"", "") %>%
#  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")


#pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
#index <- str_detect(converted, pattern)

#mean(index)


#problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
#pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
#str_replace(problems, pattern_with_groups, "\\1'\\2")

#yes <- c("5 feet 7inches", "5 7")
#no <- c("5ft 9 inches", "5 ft 9 inches")
#s <- c(yes, no)

#converted <- s %>% 
#  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
#  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
#  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

#pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
#str_detect(converted, pattern)



#s <- c("5'10", "6'1\"", "5'8inches")

#tab <- data.frame(x = s)

#tab %>% separate(x, c("feet", "inches"), sep="'", fill = "right")

#tab %>% extract(x, c("feet", "inches"), regex="(\\d)'(\\d{1,2})")

#yes <- c("5", "6", "5")
#no <- c("5'", "5''", "5'4")
#s <- c(yes, no)
#str_replace(s, "^([56])'?$", "\\1'0")

#filename <- system.file("extdata/murders.csv", package = "dslabs")
#lines <- readLines(filename)

#x <- str_split(lines, ",")

#x %>% head()

#col_names <- x[[1]]

#x <- x[-1]

#map(x, 1) %>% head()


#dat <- data.frame(map_chr(x, 1),
#                  map_chr(x, 2),
#                  map_chr(x, 3),
#                  map_chr(x, 4),
#                  map_chr(x, 5)) %>% 
#  mutate_all(parse_guess) %>%
#  setNames(col_names)

#dat %>% head()

#dat <- x %>%
#  transpose() %>%
#  map( ~ parse_guess(unlist(.))) %>%
#  setNames(col_names) %>%
#  as.data.frame()

#dat %>% head()

#x <- str_split(lines, ",", simplify = TRUE)

#col_names <- x[1,]

#x <- x[-1,]

#x %>% as.data.frame() %>%
#  setNames(col_names) %>%
#  mutate_all(parse_guess)

#data("raw_data_research_funding_rates")

#head(raw_data_research_funding_rates)

#tab <- str_split(raw_data_research_funding_rates, "\n")

#tab <- tab[[1]]

#the_names_1 <- tab[3]
#the_names_2 <- tab[4]


#the_names_1 <- the_names_1 %>%
#  str_trim() %>%
#  str_replace_all(",\\s.", "") %>%
#  str_split("\\s{2,}", simplify = TRUE)
#the_names_1

#the_names_2 <- the_names_2 %>%
#  str_trim() %>%
#  str_split("\\s+", simplify = TRUE)
#the_names_2

#tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
#the_names <- c(the_names_2[1], tmp_names) %>%
#  str_to_lower() %>%
#  str_replace_all("\\s", "_")
#the_names

#new_research_funding_rates <- tab[6:14] %>%
#  str_trim %>%
#  str_split("\\s{2,}", simplify = TRUE) %>%
#  data.frame(stringsAsFactors = FALSE) %>%
#  setNames(the_names) %>%
#  mutate_at(-1, parse_number)
#new_research_funding_rates %>% head()

#identical(research_funding_rates, new_research_funding_rates)

#data("gapminder")

#gapminder %>% filter(region =="Caribbean") %>%
#  mutate(country = recode(country,
#                          'Antigua and Barbuda' = "Barbuda",
#                          'Dominican Republic' = "DR",
#                          'St. Vincent and the Grenadines' = "St. Vincent",
#                          'Trinidad and Tobago' = "Trinidad")) %>%
#  ggplot(aes(year, life_expectancy, color = country)) +
#  geom_line()

#day <- c("Monday", "Tuesday")
#staff <- c("Mandy, Chris and Laura", "Steve, Ruth and Frank")


#schedule <- data.frame(day = day, staff = staff)

#schedule

#tidy <- schedule %>% 
#  mutate(staff = str_split(staff, ", | and ")) %>% 
#  unnest()

#dat <- gapminder %>% filter(region == "Middle Africa") %>% 
#  mutate(country = recode(country, 
#                          "Central African Republic" = "CAR", 
#                          "Congo, Dem. Rep." = "DRC",
#                          "Equatorial Guinea" = "Eq. Guinea"))

#dat

#str_detect("19.5", "[1-9]*\\.5")

#library(rvest)
#library(tidyverse)
#library(stringr)
#url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
#tab <- read_html(url) %>% html_nodes("table")
#polls <- tab[[6]] %>% html_table(fill = TRUE)


#polls <- polls %>%
#  setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")) %>%
#  filter(str_detect(remain, "%"))

#1
#as.numeric(str_remove(polls$remain, "%"))
#2
#as.numeric(polls$remain)/100
#3
#parse_number(polls$remain)
#4
#str_remove(polls$remain, "%")/100
#5
#as.numeric(str_replace(polls$remain, "%", ""))/100
#6
#parse_number(polls$remain)/100

#str_replace(polls$undecided, "N/A", "0")

#polls_us_election_2016$startdate %>% head

#as.numeric(polls_us_election_2016$startdate) %>% head

#polls_us_election_2016 %>% filter(pollster == "Ipsos" & state == "U.S.") %>%
#  ggplot(aes(startdate, rawpoll_trump)) +
#  geom_line()

#set.seed(2)

#dates <- sample(polls_us_election_2016$startdate, 10) %>% sort

#data.frame(date = days(dates),
#           month = month(dates),
#           day = day(dates),
#           year = year(dates))

#month(dates, label = TRUE)

#x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
#       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")

#ymd(x)

#x <- "09/01/02"

#ydm(x)

#myd(x)

#dmy(x)

#dym(x)

#now() 

#now() %>% hour()
#now() %>% minute()
#now() %>% second()

#x <- c("12:34:56")

#hms(x)

#x <- "Nov/2/2012 12:34:56"

#mdy_hms(x)

library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
library(tidytext)
set.seed(1)

#data("trump_tweets")

#head(trump_tweets)

#names(trump_tweets)

#trump_tweets %>% select(text) %>% head

#trump_tweets %>% count(source) %>% arrange(desc(n))

#trump_tweets %>% 
#  extract(source, "source", "Twitter for (.*)") %>%
#  count(source) 

#campaign_tweets <- trump_tweets %>% 
#  extract(source, "source", "Twitter for (.*)") %>%
#  filter(source %in% c("Android", "iPhone") &
#           created_at >= ymd("2015-06-17") & 
#           created_at < ymd("2016-11-08")) %>%
#  filter(!is_retweet) %>%
#  arrange(created_at)

#ds_theme_set()
#campaign_tweets %>%
#  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
#  count(source, hour) %>%
#  group_by(source) %>%
#  mutate(percent = n / sum(n)) %>%
#  ungroup %>%
#  ggplot(aes(hour, percent, color = source)) +
#  geom_line() +
#  geom_point() +
#  scale_y_continuous(labels = percent_format()) +
#  labs(x = "Hour of day (EST)",
#       y = "% of tweets",
#       color = "")

#example <- data_frame(line = c(1, 2, 3, 4),
#                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
#example
#example %>% unnest_tokens(word, text)

#i <- 3008
#campaign_tweets$text[i]
#campaign_tweets[i,] %>% 
#  unnest_tokens(word, text) %>%
#  select(word)

#pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

#campaign_tweets[i,] %>% 
#  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
#  select(word)

#campaign_tweets[i,] %>% 
#  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
#  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
#  select(word)

#tweet_words <- campaign_tweets %>% 
#  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
#  unnest_tokens(word, text, token = "regex", pattern = pattern) 

#tweet_words %>% 
#  count(word) %>%
#  arrange(desc(n))

#tweet_words <- campaign_tweets %>% 
#  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
#  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
#  filter(!word %in% stop_words$word ) 

#tweet_words %>% 
#  count(word) %>%
#  top_n(10, n) %>%
#  mutate(word = reorder(word, n)) %>%
#  arrange(desc(n))

#tweet_words <- campaign_tweets %>% 
#  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
#  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
#  filter(!word %in% stop_words$word &
#           !str_detect(word, "^\\d+$")) %>%
#  mutate(word = str_replace(word, "^'", ""))

#android_iphone_or <- tweet_words %>%
#  count(word, source) %>%
#  spread(source, n, fill = 0) %>%
#  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
#           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
#android_iphone_or %>% arrange(desc(or))
#android_iphone_or %>% arrange(or)

#android_iphone_or %>% filter(Android+iPhone > 100) %>%
#  arrange(desc(or))

#android_iphone_or %>% filter(Android+iPhone > 100) %>%
#  arrange(or)

#nrc <- get_sentiments("nrc") %>%
#  select(word, sentiment)

#tweet_words %>% inner_join(nrc, by = "word", relationship = "many-to-many") %>% 
#  select(source, word, sentiment) %>% sample_n(10)

#sentiment_counts <- tweet_words %>%
#  left_join(nrc, by = "word", relationship = "many-to-many") %>%
#  count(source, sentiment) %>%
#  spread(source, n) %>%
#  mutate(sentiment = replace_na(sentiment, replace = "none"))
#sentiment_counts

#tweet_words %>% group_by(source) %>% summarize(n = n())

#sentiment_counts %>%
#  mutate(Android = Android / (sum(Android) - Android) , 
#         iPhone = iPhone / (sum(iPhone) - iPhone), 
#         or = Android/iPhone) %>%
#  arrange(desc(or))

#library(broom)
#log_or <- sentiment_counts %>%
#  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
#          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
#          conf.low = log_or - qnorm(0.975)*se,
#          conf.high = log_or + qnorm(0.975)*se) %>%
#  arrange(desc(log_or))

#log_or

#log_or %>%
#  mutate(sentiment = reorder(sentiment, log_or),) %>%
#  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
#  geom_errorbar() +
#  geom_point(aes(sentiment, log_or)) +
#  ylab("Log odds ratio for association between Android and sentiment") +
#  coord_flip() 

#android_iphone_or %>% inner_join(nrc) %>%
#  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
#  arrange(desc(or))

#android_iphone_or %>% inner_join(nrc, by = "word") %>%
#  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
#  mutate(log_or = log(or)) %>%
#  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
#  mutate(word = reorder(word, log_or)) %>%
#  ggplot(aes(word, log_or, fill = log_or < 0)) +
#  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
#  geom_bar(stat="identity", show.legend = FALSE) +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

options(digits = 3)

#brexit_polls %>% filter(month(startdate)==4)
  
#brexit_polls %>% filter(round_date(enddate, unit = "week")=="2016-06-12")

#brexit_polls %>% mutate(endday = weekdays(enddate))

#data(movielens)

#movielens <- movielens %>% mutate(timestamp = as_datetime(timestamp))

#movielens %>%
#  mutate(year = year(timestamp)) %>%
#  count(year) %>%
#  arrange(desc(n)) %>%
#  slice(1) %>%
#  pull(year)


#movielens %>%
#  mutate(hour = hour(timestamp)) %>%
#  count(hour) %>%
#  arrange(desc(n)) %>%
#  slice(1) %>%
#  pull(hour)

library(gutenbergr)

pattern <- "^Pa-z*"


#gutenberg_works(title == "Pride and Prejudice")

words <- gutenberg_download(1342, mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg/")

words <- words %>% unnest_tokens(word,text)

words <- words %>% filter(!word %in% stop_words$word)
  

words <- words %>% filter(!str_detect(word, "\\d"))

#words <- words %>% count(word) %>%
#  top_n(25, n) %>%
#  mutate(word = reorder(word, n)) %>%
#  arrange(desc(n)) %>% print(n = 25)

#afinn <- get_sentiments("afinn")

afinn_sentiments <- words %>% inner_join(afinn, by = "word") %>% select(word, value)
afinn_sentiments

afinn_sentiments %>% filter(value>0)

afinn_sentiments %>% filter(value==4)