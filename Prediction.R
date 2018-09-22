library(ggplot2)      #visualisation
library(ggthemes)     #visualisation
library(scales)       #visualisation
library(dplyr)        #data manipulation
library(mice)         #imputation
library(randomForest) #classification

train <- read.csv("train.csv", stringsAsFactors = F)
test  <- read.csv("test.csv", stringsAsFactors = F)
full  <- bind_rows(train, test)
str(full)

#Title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '',full$Name)

table(full$Sex, full$Title)

#Titles with low counts to be stored as rare_title
rare_title <- c('Capt','Col','Dona','Don','Dr','Jonkheer','Lady','Major','Mlle','Mme','Lady','Rev','Sir','the Countess')

#Correcting the titles
full$Title[full$Title == 'Mlle']        <- 'Miss'
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs'
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

table(full$Sex, full$Title)

#Surname from passenger names
full$Surname <- sapply(full$Name,
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

#A variable to hold the family size of each passenger including themselves
full$Fsize <- full$SibSp + full$Parch + 1

#A family variable to hold surname and family size
full$Family <- paste(full$Surname, full$Fsize, sep = '_')

#To find the relation between family size and survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) + 
  geom_bar(stat = 'count', position = 'dodge') +
  scale_x_continuous(breaks = c(1:11)) +
  labs(x = 'Family Size') + 
  theme_few()

#Discretise family size
full$FsizeD[full$Fsize == 1]                  <- 'Singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1]  <- 'Small'
full$FsizeD[full$Fsize > 4]                   <- 'Large'

