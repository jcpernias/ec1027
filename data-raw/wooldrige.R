## code to prepare `wooldrige` dataset goes here

library(tidyverse)
library(glue)

hprice1_names <-
  c("price", "assess", "bdrms", "lotsize", "sqrft", "colonial",
    "lprice", "lassess", "llotsize", "lsqrft", "XXX")
hprice1_types <- "ddiiiiddddc"
hprice1 <-
  read_table2("./data-raw/wooldridge/HPRICE1.raw",
              col_names = hprice1_names, na = ".",
              col_types = hprice1_types) %>%
  select(1:6)
usethis::use_data(hprice1, overwrite = TRUE)


traffic2_names <-
  c("year", "totacc", "fatacc", "injacc", "pdoacc", "ntotacc", "nfatacc",
    "ninjacc", "npdoacc", "rtotacc", "rfatacc", "rinjacc", "rpdoacc", "ushigh",
    "cntyrds", "strtes", "t", "tsq", "unem", "spdlaw", "beltlaw", "wkends",
    "feb", "mar", "apr", "may", "jun", "jul" , "aug", "sep", "oct", "nov",
    "dec", "ltotacc",  "lfatacc",  "prcfat" ,   "prcrfat" ,  "lrtotacc",
    "lrfatacc", "lntotacc", "lnfatacc", "prcnfat",  "lushigh", "lcntyrds",
    "lstrtes",  "spdt", "beltt", "prcfat_1", "X49")

traffic2_types <-
  glue('{strrep("i", 18)}d{strrep("i", 14)}{strrep("d", 12)}iidc')

traffic2_orig <-
  read_table2("./data-raw/wooldridge/TRAFFIC2.raw",
             col_names = traffic2_names, na = ".",
             col_types = traffic2_types) %>%
  select(1:16, 19:22)

traffic2 <- traffic2_orig %>%
  mutate(month = rep(1:12, 9)) %>%
  relocate(month, .after = year)


usethis::use_data(traffic2, overwrite = TRUE)



