library(tidyverse)
library(collections)

dd <- read_lines("data/day_18")

myadd <- function(str2) {
  while(str_detect(str2, "[+\\*]")) {
    a <- str_extract(str2, "[0-9]+.*?[+\\-\\*\\/].*?[0-9]+")
    b <- eval(parse(text = a))
    str2 <- str_replace(str2,  "[0-9]+.*?[+\\-\\*\\/].*?[0-9]+", as.character(b))
  }
  as.numeric(b)
}

arithmetic <- function(ss) {
  ss <- paste0("(", ss, ")")
  ee <- stack()
  repeat {
    char <- str_extract(ss,"[()\\+\\*]|[0-9]+")
    if(is.na(char)) {
      return(ee$pop())
    }
    if(char == ")") {
      val <- ""
      while(ee$peek() != "(" && ee$size() > 0) {
        val <- paste(ee$pop(), val)
      }
      ee$pop()
      ee$push(myadd(val))
    } else{
      ee$push(char)
    }
    ss <- str_remove(ss, "[()\\+\\*]|[0-9]+")
    rev(ee$as_list())
  }
}

sum(sapply(dd, arithmetic)) %>% format(scientific = FALSE) #first star!

myadd <- function(str2) {
  str <- str_remove_all(str2, " ")
  ee <- stack()
  char <- str_extract(str, "[0-9]+|[\\+\\*]")
  ee$push(char)
  repeat {
    str <- str_remove(str, "[0-9]+|[\\+\\*]")
    char <- str_extract(str, "[0-9]+|[\\+\\*]")
    if(is.na(char)) {
      break
    }
    if(ee$size() == 0) {
      ee$push(char)
    } else {
      if(ee$peek() == "+") {
        ee$push(eval(parse(text = paste(char, ee$pop(), ee$pop()))))
      } else {
        ee$push(char)
      }
    }
    rev(ee$as_list())
  }
  eval(parse(text = paste(ee$as_list() %>% unlist(), collapse = " ")))
}

sum(sapply(dd, arithmetic)) %>% format(scientific = FALSE) #second star!

