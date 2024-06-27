library(tidyverse)
library(ggplot2)
library(dplyr)
library(dslabs)
library(Lahman)
library(rvest)

#path <- system.file("extdata", package = "dslabs")

#list.files(path)

#filename <- "murders.csv"
#fullpath <- file.path(path, filename)

#fullpath

#file.exists(filename)

#dat <- read_csv(fullpath)

#dat2 <- read.csv(fullpath)

#head(dat)

#head(dat2)

#url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"

#dat3 <- read_csv(url)

#dat3

#path <- system.file("extdata", package = "dslabs")

#filename <- file.path(path, "fertility-two-countries-example.csv")

#wide_data <- read_csv(filename)

#select(wide_data, country, '1960' : '1967')

#wide_data %>% pivot_longer(-country)

#new_tidy_cata <- wide_data %>% pivot_longer(-country, names_to = "year", values_to = "fertility",
#                                            names_transform = list(year=as.numeric))

#new_tidy_cata %>% ggplot(aes(year, fertility, colour = country)) +
#  geom_point()

#new_wide_data <- new_tidy_cata %>% pivot_wider(names_from = year, values_from = fertility)

#select(new_wide_data, country, '1960' : '1967')


#path <- system.file("extdata", package = "dslabs")

#filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")



#raw_dat <- read_csv(filename)

#select(raw_dat, 1:4)

#dat <- raw_dat %>% pivot_longer(-country)

#head(dat)

#dat %>% separate(name, c("year", "name"),extra = "merge", convert = TRUE) %>% pivot_wider()

#dat %>% separate(name, c("year", "name_1", "name_2"),fill = "right", convert = TRUE) %>% unite(variable_name, name_1, name_2, sep = "_")


#co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
#  setNames(1:12) %>%
#  mutate(year = as.character(1959:1997))

#co2_tidy <- pivot_longer(co2_wide, -year, names_to = "month", values_to = "co2")

#co2_tidy

#data(admissions)
#dat <- admissions %>% select(-applicants)

#tmp <- admissions %>%
#  pivot_longer(cols = c(admitted, applicants), names_to = "key", values_to = "value")
#tmp

#tmp2 <- unite(tmp, column_name, c(key, gender))

#data(murders)

#head(murders)

#data("polls_us_election_2016")

#head(results_us_election_2016)

#identical(results_us_election_2016$state, murders$state)

#tab <- left_join(murders, results_us_election_2016, by = "state")

#head(tab)

#tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
#  geom_point()  +
#  geom_text() +
# scale_x_continuous(trans = "log2") +
#  scale_y_continuous(trans = "log2") +
#  geom_smooth(method = "lm", se = FALSE)


#tab1 <- slice(murders, 1:6) %>% select(state, population)

#tab1

#tab2 <- slice(results_us_election_2016, c(22, 44, 14, 1, 27, 45)) %>% select(state, electoral_votes)

#tab2

#semi_join(tab1, tab2)

#bind_cols(a = c(1:3), b = c(4:6))

#head(tab)

#tab1 <- tab[, 1:3]
#tab2 <- tab[, 4:6]
#tab3 <- tab[, 7:9]

#new_tab <- bind_cols(tab1, tab2, tab3)

#head(new_tab)

#tab1 <- tab[1:2,]
#tab2 <- tab[3:4,]

#bind_rows(tab1, tab2)

#tab1 <- tab[1:5,]
#tab2 <- tab[3:7,]

#tab1
#tab2

#intersect(tab1, tab2)

#union(tab1, tab2)

#setdiff(tab1, tab2)

#setdiff(tab2, tab1)

#setequal(tab1, tab2)



#tab1 <- slice(murders, c(1:3, 8, 9)) %>% select(state, population)

#tab1

#tab2 <- slice(results_us_election_2016, c(22, 44, 14, 1, 23, 27)) %>% select(state, electoral_votes)

#tab2


#dim(tab1)

#dim(tab2)


#datata <- semi_join(tab1, tab2)

#datata


#top <- Batting %>% 
#  filter(yearID == 2016) %>%
#  arrange(desc(HR)) %>%    # arrange by descending HR count
#  slice(1:10)    # take entries 1-10
#top %>% as_tibble()

#top_names <- top %>% left_join(People)  %>%
#  select(playerID, nameFirst, nameLast, HR)

#top_salary <- Salaries %>% filter(yearID == 2016) %>%
#  right_join(top_names) %>% select(nameFirst, nameLast, teamID, HR, salary)

#AwardsPlayers %>% filter(yearID==2016) %>% inner_join(top)

#AwardsPlayers %>% filter(yearID==2016) %>% anti_join(top) %>% distinct(playerID, keep_all = TRUE)

#url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
#h <- read_html(url)
#class(h)
#h

#tab <- h %>% html_nodes("table")
#tab <- tab[[2]]

#tab <- tab %>% html_table()

#class(tab)

#tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))

#head(tab)

url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")

html_text(nodes[[8]])

html_table(nodes[[8]])

tab1 <- html_table(nodes[[10]])

tab2 <- html_table(nodes[[19]])

tab_1 <- tab1[-1, -1]
tab_2 <- tab2[-1, ]

colnames(tab_1) <- c("Team", "Payroll", "Average")
colnames(tab_2) <- c("Team", "Payroll", "Average")

joined_tab <- full_join(tab_1, tab_2, by = "Team")

num_rows <- nrow(joined_tab)

print(num_rows)

url2 <- "https://web.archive.org/web/20210606000626/https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

h2 <- read_html(url2)

nodes2 <- html_nodes(h2, "table")

html_table(nodes2)

