library(tidyverse)
dd <- read_file("data/day_04")

dd <- dd %>% 
  str_split("\n\n") %>% 
  unlist()

sum(str_count(dd, "byr:|iyr:|eyr:|hgt:|hcl:|ecl:|pid:") == 7) #first star!

dd <- dd[str_count(dd, "byr:|iyr:|eyr:|hgt:|hcl:|ecl:|pid:") == 7]

mdf <- function(x) {
  data.frame(byr = str_extract(x, "byr:[^ \n]*"),
             iyr = str_extract(x, "iyr:[^ \n]*"),
             eyr = str_extract(x, "eyr:[^ \n]*"),
             hgt = str_extract(x, "hgt:[^ \n]*"),
             ecl = str_extract(x, "ecl:[^ \n]*"),
             pid = str_extract(x, "pid:[^ \n]*"),
             hcl = str_extract(x, "hcl:[^ \n]*")
  )
}
ee <- mdf(dd) %>% 
  mutate_all(function(x) str_remove(x, "byr:|iyr:|eyr:|hgt:|hcl:|ecl:|pid:"))

is_good <- function(x) {
  (x[1] >= 1920 && x[1] <= 2002) &&
    (x[2] >= 2010 && x[2] <= 2020) &&
    (x[3] >= 2020 && x[3] <= 2030) &&
    is_height(x[4]) &&
    is_ecl(x[5]) &&
    str_detect(x[6], "^[0-9]{9}$") &&
    is_hcl(x[7])
}

is_height <- function(x) {
  if(!str_detect(x, "^[0-9]+cm$|^[0-9]+in$")) {
    return(FALSE)
  }
  if(str_detect(x, "cm")) {
    return(x >= 150 && x <= "193cm")
  } else {
    return(x >= 59 && x <= "76in")
  }
}

is_ecl <- function(x) {
  x %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
}

is_hcl <- function(x) {
  str_detect(x, "^\\#[a-f0-9]{6}$")
}

apply(ee, 1, is_good) %>% 
  sum #second star!


