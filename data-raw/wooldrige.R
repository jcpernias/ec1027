## code to prepare `wooldrige` dataset goes here

library(tidyverse)
library(glue)

datasets <- list(
  bwght = list(
    file = "BWGHT",
    cols = cols(faminc = "d", cigtax = "d", cigprice = "d", bwght = "d",
                fatheduc = "d", motheduc = "d", parity = "d", male = "d",
                white = "d", cigs = "d", lbwght = "-", bwghtlbs = "-",
                packs = "-", lfaminc = "-", .rest = "c")
  ),
  earns = list(
    file = "EARNS",
    cols = cols(year = "i", wkearns = "d", wkhours = "d", outphr = "d",
                hrwage = "-", lhrwage = "-", loutphr = "-", t = "-",
                ghrwage = "-", goutphr = "-", ghrwge_1 = "-", goutph_1 = "-",
                goutph_2 = "-", lwkhours = "-", .rest = "c")
  ),
  gpa1 = list(
    file = "GPA1",
    cols = cols(age = "i", soph = "i", junior = "i", senior = "i",
                senior5 = "i", male = "i", campus = "i", business = "i",
                engineer = "i", colGPA = "d", hsGPA = "d", ACT = "i",
                job19 = "i", job20 = "i", drive = "i", bike = "i", walk = "i",
                voluntr = "i", PC = "i", greek = "i", car = "i", siblings = "i",
                bgfriend = "i", clubs = "i", skipped = "d", alcohol = "d",
                gradMI = "i", fathcoll = "i", mothcoll = "i", .rest = "c")
  ),
  hprice1 = list(
    file = "HPRICE1",
    cols = cols(price = "d", assess = "d", bdrms = "i",
                lotsize = "i", sqrft = "i", colonial = "i",
                lprice = "-", lassess = "-", llotsize = "-",
                lsqrft = "-", .rest = "c")
  ),
  hseinv = list(
    file = "HSEINV",
    cols = cols(year = "i", inv = "i", pop = "i", price = "d", linv = "-", lpop = "-",
                lprice = "-", t = "-", invpc = "-", linvpc = "-", lprice_1 = "-",
                linvpc_1 = "-", gprice = "-", ginvpc = "-", .rest = "c")
  ),
  intdef = list(
    file = "INTDEF",
    cols = cols(year = "i", i3 = "d", inf = "d", rec = "d", out = "d",
                def = "d", i3_1 = "-", inf_1 = "-", def_1 = "-", ci3 = "-",
                cinf = "-", cdef = "-", y77 = "-", .rest = "c")
  ),
  rdchem = list(
    file = "RDCHEM",
    cols = cols(rd = "d", sales = "d", profits = "d", rdintens = "-",
                profmarg = "-", salessq = "-", lsales = "-", lrd = "-",
                .rest = "c")
  ),
  traffic2 = list(
    file = "TRAFFIC2",
    cols = cols(year = "i", totacc = "i", fatacc = "i", injacc = "i",
                pdoacc = "i", ntotacc = "i", nfatacc = "i", ninjacc = "i",
                npdoacc = "i", rtotacc = "i", rfatacc = "i", rinjacc = "i",
                rpdoacc = "i", ushigh = "i", cntyrds = "i", strtes = "i",
                t = "-", tsq = "-", unem = "d", spdlaw = "i", beltlaw = "i",
                wkends = "i", feb = "-", mar = "-", apr = "-", may = "-",
                jun = "-", jul  = "-", aug = "-", sep = "-", oct = "-",
                nov = "-", dec = "-", ltotacc = "-", lfatacc = "-",
                prcfat  = "-",   prcrfat  = "-", lrtotacc = "-",
                lrfatacc = "-", lntotacc = "-", lnfatacc = "-",
                prcnfat = "-",  lushigh = "-", lcntyrds = "-", lstrtes = "-",
                spdt = "-", beltt = "-", prcfat_1 = "-", .rest = "c"),
    post = function(x) {
      x %>% mutate(month = rep(1:12, 9)) %>%
        relocate(month, .after = year)
    }
  ),
  wageprc = list(
    file = "WAGEPRC",
    cols = cols(price = "d", wage = "d", t = "-", lprice = "-", lwage = "-",
                gprice = "-", gwage = "-", gwage_1 = "-", gwage_2 = "-",
                gwage_3 = "-", gwage_4 = "-", gwage_5 = "-", gwage_6 = "-",
                gwage_7 = "-", gwage_8 = "-", gwage_9 = "-", gwage_10 = "-",
                gwage_11 = "-", gwage_12 = "-", gprice_1 = "-", .rest = "c"),
    post = function(x) {
      x %>% mutate(year = c(rep(1964:1986, each = 12), rep(1987, 10)),
                            month = c(rep(1:12, 23), 1:10)) %>%
        relocate(year, month)
    }
  )
)

make_dataset <- function(x, nm) {
  path <- glue("./data-raw/wooldridge/{x$file}.raw")
  ds <- read_table2(path, na = ".", col_types = x$cols,
                    col_names = names(x$cols$cols)) %>%
    select(-.rest)
  if (!is.null(x$post))
    ds <- x$post(ds)
  assign(nm, ds)
  do.call(usethis::use_data, list(as.name(nm), overwrite = TRUE))
}

purrr::iwalk(datasets, make_dataset)


