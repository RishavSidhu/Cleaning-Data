#Pacman includes all the required libraries 
if (!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, party, rio, tidyverse)

# 2.1
df <- read_csv("warpbreaks.csv")
sapply(df, class)
df$X1 <- as.integer(df$X1)
df$breaks <- as.integer(df$breaks)
df
sapply(df, class)
mean[1]
typeof(1)

#2.2
v <- factor(c("2", "3", "5", "7", "11"))

as.character(v)
v <- factor(c("5", "3", "5", "7", "11"))
as.numeric(v)
as.integer(as.character(v))

#2.3
example <- readLines("example.txt")
com_vec <- Example[grepl("^//", example)]
dat_vec <- Example[!grepl("^//", example)]
date <- strsplit(com_vec[1],":")[[1]][2]
gsub("^ ", "",date)
my_list <- strsplit(dat_vec, ";")
my_list
assignFields <- function(x){
  out <- character(3)
  i <- grepl("[[:alpha:]]",x)
  out[1] <- x[i]
  
  i <- which(as.integer(x) < 100)
  out[2] <- ifelse(length(i)>0, x[2], NA)
  
  i <- which(as.numeric(x) > 0)
  out[3] <- ifelse(length(i)>0, x[3], NA)
  out
}
standardFields <- lapply(my_list, assignFields)
standardFields
(my_mat <- matrix(
  unlist(standardFields)
  , nrow=length(standardFields)
  , byrow=TRUE))
