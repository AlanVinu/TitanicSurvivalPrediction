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

#I'm going to use mode to impute the missing Embarked values
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode(full$Embarked)

#Passengers[62 & 830] must've embarked from Southampton('S')
full$Embarked[c(62,830)] <- 'S'

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

set.seed(142)

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

#Relationship between age and survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) +
  geom_histogram() +
  theme_few()

#Lets divide the graph into males and females
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) +
  geom_histogram() +
  theme_few() + 
  facet_grid(.~Sex) + 
  scale_fill_discrete(name = "Survival", breaks = c(0,1), 
                      labels = c("Didn't Survive", "Survived")) + 
  ggtitle("RMS Titanic") + 
  labs(x= "Age", y = "Count")

md.pattern(full)

#Prediction
train <- full[1:891, ]
test <- full[892:1309, ]

#Building the model using randomForest
set.seed(348)

rf_model <- 
  randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FsizeD, data = train)

#Checking the error rate
plot(rf_model, ylim = c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col = 1:3, fill = 1:3)

#to find the variable importance
importance <- importance(rf_model)
varImportance <- 
  data.frame(Variables = row.names(importance),
             Importance = round(importance[,'MeanDecreaseGini'],2))

#rank variable based on Importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance),
                           y = Importance, fill = Importance)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust = 0, vjust = 0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() +
  theme_few()

#Prediction
prediction <- predict(rf_model, test)

solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)