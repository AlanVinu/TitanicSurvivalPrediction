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

mosaicplot(table(full$FsizeD, full$Survived), main = 'Family Size by Survival', shade = TRUE)

#A variable for deck from the cabin variable of passengers
full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x,NULL)[[1]][1]))

#Passengers with ID's 62 and 830 are missing Embarked values
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() + 
  geom_hline(aes(yintercept = 80), colour = 'red', linetype = 'dashed', lwd = 2) +
  scale_y_continuous(labels = dollar_format()) +
  theme_few()

#Passengers[62 & 830] must've embarked from Charbourg('C')
full$Embarked[c(62,830)] <- 'C'

#Passenger 1044 has NA as Fare value so visualising based on the other info given
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], aes(x = Fare)) +
  scale_x_continuous(labels = dollar_format()) + 
  theme_few() + 
  geom_density(fill = 'lightblue', alpha = 0.4) +
  geom_vline(aes(xintercept = median(Fare, na.rm = T)),
             colour = 'red', linetype = 'dashed', lwd = 1)

#The median fare can be given to the passenger 1044
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = T)

sum(is.na(full$Age))

#imputation is required to fill in the missing ages of 263 passengers
factor_vars <- 
  c('PassengerId', 'Pclass', 'Sex', 'Embarked', 'Title', 'Surname', 'Family','FsizeD')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

set.seed(148)

#mice imputation excluding some variables which are not useful here
mice_mod <- 
  mice(full[, !names(full) %in% 
              c('PassengerId', 'Name', ' Ticket', 'Cabin', 'Family', 'Surname', 'Survived')],
       method = 'rf')

#saving output
mice_output <- complete(mice_mod)

#let's compare imputed data with original data
par(mfrow = c(1,2))
hist(full$Age, freq = F, main = "Age: Original Data", col = 'darkgreen', ylim = c(0,0.04))
hist(mice_output$Age, freq = F, main = "Age: MICE Data", col = 'darkblue', ylim = c(0,0.04))

full$Age <- mice_output$Age