library(ggplot2)      #visualisation
library(ggthemes)     #visualisation
library(scales)       #visualisation
library(dplyr)        #data manipulation
library(mice)         #imputation
library(randomForest) #classification

train <- read.csv("train.csv", stringsAsFactors = F)
test  <- read.csv("test.csv", stringsAsFactors = F)
