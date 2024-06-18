library(tidyverse)


path <- system.file("extdata", package = "dslabs")

list.files(path)

filename <- "murders.csv"
fullpath <- file.path(path, filename)

#fullpath

#file.exists(filename)

dat <- read_csv(fullpath)

dat2 <- read.csv(fullpath)

head(dat)

head(dat2)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"

dat3 <- read_csv(url)

dat3