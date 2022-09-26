library(readr)
library(dplyr)
library(caret)
library(tidyr)

setwd("E:/R/_cp2022/carelia")

Rec <- function(data, lev = NULL, model = NULL) {

  Rec <- MLmetrics::Recall (data$obs, data$pred, positive="c0")+MLmetrics::Recall (data$obs, data$pred,  positive="c1")
  if (is.na(Rec)) Rec<-1
  c(Rec = Rec/2)
}

set.seed(123)
ctrl <- trainControl(method = "cv", summaryFunction = Rec,
                     classProbs=TRUE, 
                     number = 10)

train0<-read.csv("train_dataset_train.csv")
test0<-read_csv("sample_solution.csv")

tp<-read.csv("type_contract.csv")
tp<-unique(tp)


named<-read_csv("named.csv")

#uni<-unique(c(train0$contract_id, test0$contract_id))
named<-named[named$contract_id%in%tp$contract_id,]
named$url[grep("sampo.ru", named$url)]<-"sampo.ru"
named$url[grep("rt.ru", named$url)]<-"rt.ru"

named.grp<-named%>%group_by(contract_id, url)%>%summarise(n=n())
named.wide<-named.grp%>%pivot_wider(names_from = url, values_from = n, values_fill = 0)

log<-read_csv("log.csv")
log.grp<-log%>%group_by(contract_id, event_type)%>%summarise(n=n())
log.wide<-log.grp%>%pivot_wider(names_from = event_type, values_from = n, values_fill = 0)



train<-train0%>%left_join(tp)
train<-train%>%left_join(named.wide)
train<-train%>%left_join(log.wide)
train<-train[,-1]
train[is.na(train)]<-0

train$blocked<-as.factor(paste0("c",train$blocked))




###select features #1
set.seed(123)
mod <- train(blocked~., data =train,
             method = "rf", metric="Rec",
            # verbosity=0,
          tuneGrid=expand.grid(.mtry=c(10)), 
                               
#          tuneGrid=expand.grid(.mtry=c(5) ,.splitrule=c("gini", "extratrees","hellinger"),.min.node.size=c(1)), 
          
             trControl = ctrl)
#mod

plot(varImp(mod))

imp<-varImp(mod)[[1]]
imp<-imp%>%arrange(desc(Overall))

tmp<-train%>%select(c(blocked,gsub("`","",rownames(imp)[1:50])))

##Select features #2
set.seed(123)
mod <- train(blocked~., data =tmp,
             method = "rf", metric="Rec",
             # verbosity=0,
             tuneGrid=expand.grid(.mtry=c(7)), 
             
             #          tuneGrid=expand.grid(.mtry=c(5) ,.splitrule=c("gini", "extratrees","hellinger"),.min.node.size=c(1)), 
             
             trControl = ctrl)
#mod

plot(varImp(mod))

imp<-varImp(mod)[[1]]
imp<-imp%>%arrange(desc(Overall))

tmp2<-tmp%>%select(c(blocked,gsub("`","",rownames(imp)[1:25])))

#Final Model
####
##Model
set.seed(123)
mod <- train(blocked~., data =tmp2,
             method = "ranger", metric="Rec",
             # verbosity=0,
             #tuneGrid=expand.grid(.mtry=c(5)), 
             
             tuneGrid=expand.grid(.mtry=c(5) ,.splitrule=c("gini", "extratrees","hellinger"),.min.node.size=c(1)), 
             
             trControl = ctrl)

##########
test0<-read_csv("sample_solution.csv")
test<-test0%>%left_join(tp)
test<-test%>%left_join(named.wide)
test<-test%>%left_join(log.wide)
test<-test[,-c(2)]
test[is.na(test)]<-0
tmp55<-predict(mod,test, type="prob")[,2]
test0$blocked<-ifelse(predict(mod,test, type="prob")[,2]>0.16,1,0)
#table(test0$blocked)

write_csv(test0,"2509.csv")
