#install.packages("randomForest")
library(randomForest)
#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)
#instal.packages("ggplot2")
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("dplyr")
library(dplyr)
#install.packages("scales")
library(scales)
#install.packages("rpart")
library(rpart)
#install.packages("caTools")
library(caTools)
#install.packages("rpart.plot")
library(rpart.plot)

#Base de Treino
bdtrain = read.csv("train.csv")


#--------------------------CRIANDO VARIAVEL TITULO ----------------

#Garantir que nome seja string
bdtrain$Name <- as.character(bdtrain$Name)


#Criar variavel titulo
bdtrain$Title <- sapply(bdtrain$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
bdtrain$Title <- sub(' ', '', bdtrain$Title)

#Combinar os titulos mais raros juntos
bdtrain$Title[bdtrain$Title %in% c('Mme', 'Mlle')] <- 'Miss'
bdtrain$Title[bdtrain$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Rev'
bdtrain$Title[bdtrain$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

table(bdtrain$Title)

#summary(bdtrain)

bdtrain$Title <- factor(bdtrain$Title)

#summary(bdtrain)


#--------------------------CRIANDO VARIAVEL FamilyNum e familyNumD ----------------

bdtrain$familyNum <- bdtrain[,"SibSp"]  + bdtrain[,"Parch"]


ggplot(bdtrain, aes(x = familyNum, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

bdtrain$familyNumD[bdtrain$familyNum== 0] <- 'sem_familia'
bdtrain$familyNumD[bdtrain$familyNum < 4 & bdtrain$familyNum > 0] <- 'pequena_familia'
bdtrain$familyNumD[bdtrain$familyNum > 3] <- 'grande_familia'

summary(bdtrain)

bdtrain$familyNumD <- as.factor(bdtrain$familyNumD)
bdtrain$familyNum <- as.factor(bdtrain$familyNum)



#---------------------------Tratando variaveis invalidas -----------------------


#passageiros com id 62 e 830 nao possuem valor de Embarked
#podemos ver que eles pagaram $80 pelas passagens: 
bdtrain[c(62, 830), 'Fare'][[1]][1]

#E que estao na 1 classe
bdtrain[c(62, 830), 'Pclass'][[1]][1]

embark_fare <- bdtrain %>%
  filter(bdtrain$PassengerId != 62 & bdtrain$PassengerId != 830)

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()


#Como pode ser visto no grafico gerado, a media de $80 dolares pago na 1 classe, coincide perfeitamente com a zona de embarque C, logo:
bdtrain$Embarked[c(62, 830)] <- 'C'


#Tratando AGE

#Idade tem 177 NA's
table(is.na(bdtrain$Age)) #tabela com valores de Age e Na's

#A media da idade das mulheres eh 28 e dos homens, 31 (aproximado)
tapply(bdtrain$Age, bdtrain$Sex, mean, na.rm = TRUE)

#Substituir os valores NA conforme o sexo
bdtrain$Age = ifelse(is.na(bdtrain$Age),ifelse(bdtrain$Sex == 'female', 28, 31) , bdtrain$Age)



#-----------------------------------------------------------------------
summary(bdtrain)

#Mudar survived e PClass para factor
bdtrain$Survived <- as.factor(bdtrain$Survived)
bdtrain$Pclass <- as.factor(bdtrain$Pclass)

summary(bdtrain)
#------------------------Comecando a previsão ---------------------------

bdtrain$PassengerId = NULL
bdtrain$Name = NULL
bdtrain$Ticket = NULL
bdtrain$Cabin = NULL
bdtrain$SibSp = NULL
bdtrain$Parch = NULL
bdtrain$familyNumD = NULL
bdtrain$Embarked = NULL


classificador = rpart(formula = Survived~., data = bdtrain)

rpart.plot(classificador)


#dividir a base train
set.seed(476)
divisao = sample.split(bdtrain$Survived, SplitRatio = 0.85)
train = subset(bdtrain, divisao == TRUE)
test = subset(bdtrain, divisao == FALSE)

#Random Forest

summary(bdtrain)
set.seed(989)

train.rf = randomForest(Survived ~ ., data = train)

#Variaveis mais importantes
varImpPlot(train.rf)

previsao = predict(train.rf, newdata = test[-1], type = 'class')
matriz_confusao = table(test[,1],previsao)

print(matriz_confusao)

confusionMatrix(matriz_confusao)


############################ BASE TESTE   ###############################################


#Base de teste
bdtest = read.csv("test.csv")


#Ver resumo dos dados (NA's, Media, mediana ...)
summary(bdtest)

# 1 NA em fare
# 86 NA em age


#--------------------------CRIANDO VARIAVEL TITULO ----------------


#Garantir que nome seja string
bdtest$Name <- as.character(bdtest$Name)

#Criar variavel titulo
bdtest$Title <- sapply(bdtest$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
bdtest$Title <- sub(' ', '', bdtest$Title)

#Combinar os titulos mais raros juntos
bdtest$Title[bdtest$Title %in% c('Mme', 'Mlle')] <- 'Miss'
bdtest$Title[bdtest$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Rev'
bdtest$Title[bdtest$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

table(bdtest$Title)

#summary(bdtest)
bdtest$Title <- factor(bdtest$Title)
#summary(bdtest)


#------------------------CRIANDO VARIAVEL FamilyNum e familyNumD ----------------

bdtest$familyNum <- bdtest[,"SibSp"]  + bdtest[,"Parch"]

ggplot(bdtest, aes(x = familyNum, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

bdtest$familyNumD[bdtest$familyNum== 0] <- 'sem_familia'
bdtest$familyNumD[bdtest$familyNum < 4 & bdtest$familyNum > 0] <- 'pequena_familia'
bdtest$familyNumD[bdtest$familyNum > 3] <- 'grande_familia'

summary(bdtest)

bdtest$familyNumD <- as.factor(bdtest$familyNumD)
bdtest$familyNum <- as.factor(bdtest$familyNum)


#---------------------------Tratando variaveis invalidas -----------------------

#passageiro 153 possui fare NULL
bdtest[153, ]

#reset graphic devide
dev.off()


# Com esse grafico, eh possivel analisar que o preco medio de Fare para tripulantes da 3 classe, que embarcaram no portao S eh de 8.05

ggplot(bdtest[bdtest$Pclass == '3' & bdtest$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

#substituir o valor achado
bdtest$Fare[153] <- median(bdtest[bdtest$Pclass == '3' & bdtest$Embarked == 'S', ]$Fare, na.rm = TRUE)

bdtest[153, ]



#Idade tem 86 NA's
table(is.na(bdtest$Age)) 

#A media da idade das mulheres eh 28 e dos homens, 31 (aproximado)
tapply(bdtest$Age, bdtest$Sex, mean, na.rm = TRUE)

#Substituir os valores NA conforme o sexo
bdtest$Age = ifelse(is.na(bdtest$Age),ifelse(bdtest$Sex == 'female', 28, 31) , bdtest$Age)


#summary(bdtest)

#Mudar survived e PClass para factor
bdtest$Survived <- as.factor(bdtest$Survived)
bdtest$Pclass <- as.factor(bdtest$Pclass)
#bdtest$Age <- factor(bdtest$Age)
summary(bdtest)
#----------------Comecando a previsao

bdtest$PassengerId = NULL
bdtest$Name = NULL
bdtest$Ticket = NULL
bdtest$Cabin = NULL
bdtest$SibSp = NULL



previsao = predict(train.rf, bdtest, type = 'class')

summary(bdtrain$Title)
summary(bdtest$Title)
str(bdtest)
str(bdtrain)
table(previsao)
summary(previsao)



final = data.frame(previsao) 

print(previsao)


final$PassengerId <- 892 :1309
final$Survived <- final$previsao

final$previsao = NULL

write.csv(final, row.names=FALSE, file = "tabela_final.csv")


