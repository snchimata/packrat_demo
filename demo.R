lbs <- c("rpart", "rattle", "RColorBrewer")
sapply(lbs, function(x) require(x, character.only = TRUE) || {install.packages(x) ; library(x, character.only = TRUE)})



url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"
col.names <- c(
  'Status of existing checking account', 'Duration in month', 'Credit history'
  , 'Purpose', 'Credit amount', 'Savings account/bonds'
  , 'Employment years', 'Installment rate in percentage of disposable income'
  , 'Personal status and sex', 'Other debtors / guarantors', 'Present residence since'
  , 'Property', 'Age in years', 'Other installment plans', 'Housing', 'Number of existing credits at this bank'
  , 'Job', 'Number of people being liable to provide maintenance for', 'Telephone', 'Foreign worker', 'Status'
)
# Get the data
data <- read.csv(
  url
  , header=FALSE
  , sep=' '
  , col.names=col.names
)

library(rpart)
# Build a tree
# I already figured these significant variables from my first iteration (not shown in this code for simplicity)
decision.tree <- rpart(
  Status ~ Status.of.existing.checking.account + Duration.in.month + Credit.history + Savings.account.bonds
  , method="class"
  , data=data
)

#install.packages("rpart.plot")
library(rpart.plot)
library(rattle)
library(RColorBrewer)
# Visualize the tree
# 1 is good, 2 is bad
plot(decision.tree,margin=0.1, main="Classification Tree")
text(decision.tree, use.n=TRUE, all=TRUE, cex=.7,splits = TRUE)
fancyRpartPlot(decision.tree, uniform=TRUE,main="Classification Tree")
prp(
  decision.tree
  , extra=1
  , varlen=0
  , faclen=0
  , main="Decision Tree for German Credit Data"
)

new.data <- list(
  Status.of.existing.checking.account='A11'
  , Duration.in.month=20
  , Credit.history='A32'
  , Savings.account.bonds='A65'
)
predict(decision.tree, new.data)

save(decision.tree, file='decision_Tree_plumber.RData')


