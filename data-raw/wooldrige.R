## code to prepare `wooldrige` dataset goes here

library(tidyverse)
library(glue)


traffic2_names <-
  c("year", "totacc", "fatacc", "injacc", "pdoacc", "ntotacc", "nfatacc", "ninjacc",
    "npdoacc", "rtotacc", "rfatacc", "rinjacc", "rpdoacc", "ushigh", "cntyrds", "strtes",
    "t", "tsq", "unem", "spdlaw", "beltlaw", "wkends", "feb", "mar",
    "apr", "may", "jun", "jul" , "aug", "sep", "oct", "nov",
    "dec", "ltotacc",  "lfatacc",  "prcfat" ,   "prcrfat" ,  "lrtotacc", "lrfatacc",
    "lntotacc", "lnfatacc", "prcnfat",  "lushigh", "lcntyrds", "lstrtes",  "spdt", "beltt",
    "prcfat_1", "X49")
traffic2_types = paste0(c(rep("i", 18), "d", rep("i", 14), rep("d", 12), "i", "i", "d", "c"), collapse = "")

traffic2 <-
  read_table2("./data-raw/wooldridge/TRAFFIC2.raw",
             col_names = traffic2_names, na = ".",
             col_types = traffic2_types) %>%
  select(1:16, 19:22)


usethis::use_data(traffic2, overwrite = TRUE)

