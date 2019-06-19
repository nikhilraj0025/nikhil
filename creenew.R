Cree1<-read.csv("C:/Users/AKHIL/Desktop/New folder/CreditRisk.csv")
View(Cree1)
library(dplyr)
Cree<-na.omit(Cree1)
b<-table(Cree$Loan_Status1)

##############dummy variable or one hot encoding############################################################
Cree<-mutate(Cree,Gender1=ifelse(Gender=="Male",1,0))
Cree<-mutate(Cree,Married1=ifelse(Married=="Yes",1,0))
Cree<-mutate(Cree,Education1=ifelse(Education=="Graduate",1,0))
Cree<-mutate(Cree,Self_Employed1=ifelse(Self_Employed=="Yes",1,0))
Cree<-mutate(Cree,Loan_Status1=ifelse(Loan_Status=="Y",1,0))
Cree<-mutate(Cree,newarea1=ifelse(Property_Area=="Urban",2,ifelse(Property_Area=="Rural",0,1)))
Cree<-select(Cree,4,7,8,9,10,11,14,15,16,17,18,19)                    
View(Cree)                    


#####################factorising###########################################################

Cree$Gender1<-factor(Cree$Gender1)
Cree$Married1<-factor(Cree$Married1)
Cree$Education11<-factor(Cree$Education1)
Cree$Self_Employed1<-factor(Cree$Self_Employed1)
Cree$Loan_Status1<-factor(Cree$Loan_Status1)
Cree$Gender1<-factor(Cree$newarea1)


#####################Sampling##################################################################

set.seed(121)
crd<-sample(2,nrow(Cree),replace=TRUE,prob=c(0.8,0.2))
crd_Train<-Cree[crd==1,]
crd_Test<-Cree[crd==2,]


#####################Oversampling#################################################

cree_s<-filter(crd_Train,Loan_Status1==0)
cree_tr_s<-rbind(crd_Train,cree_s,cree_s,cree_s)
bb<-table(cree_tr_s$Loan_Status1)
bb


###############"Model on  Logistic Regression "Prediction on TEST"##########################################################


ml<-glm(Loan_Status1~.,family=binomial,data=cree_tr_s)
prd<-predict(ml,crd_Test,type="response")
prd_df<-data.frame(prd,crd_Test$Loan_Status1)
prd_df<-mutate(prd_df,prd=ifelse(prd>0.5,1,0))
colnames(prd_df)<-c("predict","actual")
ta<-table(prd_df$predict,prd_df$actual)
ta
acc_1<-sum(diag(ta))/sum(ta)
acc_1




################# Model on Decision Tree "Prediction on Test"#########################################################

library(party)
rm(prd_df1)
ml1<-ctree(Loan_Status1~.,data=cree_tr_s)
prd1<-predict(ml1,crd_Test,type="response")
prd_df1<-data.frame(prd1,crd_Test$Loan_Status1)
colnames(prd_df1)<-c("predict","actual")
t1<-table(prd_df1$predict,prd_df1$actual)
t1
acc_2<-sum(diag(t1))/sum(t1)
acc_2

################## Model on random Forest"Prediction on Test"#############################################

library(randomForest)
rm(prd_df2)
ml2<-randomForest(Loan_Status1~.,data=cree_tr_s)
prd2<-predict(ml2,crd_Test,type="response")
prd_df2<-data.frame(prd2,crd_Test$Loan_Status1)
View(prd_df2)
colnames(prd_df2)<-c("predict","actual")
t2<-table(prd_df2$predict,prd_df2$actual)
t2
acc_3<-sum(diag(t2))/sum(t2)
acc_3




######################## model on svm "Prediction on Test" ################################################
library(e1071)
rm(prd_df3)
ml3<-svm(Loan_Status1~.,data=cree_tr_s)
prd3<-predict(ml3,crd_Test,type="response")
prd_df3<-data.frame(prd3,crd_Test$Loan_Status1)
colnames(prd_df3)<-c("predict","actual")
t3<-table(prd_df3$predict,prd_df3$actual)
t3
acc_4<-sum(diag(t3))/sum(t3)
acc_4




############### model on naive Bayes on Test #######################################


library(e1071)
ml9<-naiveBayes(Loan_Status1~.,data=cree_tr_s)
prd9<-predict(ml9,crd_Test)
prd_df9<-data.frame(prd9,crd_Test$Loan_Status1)
colnames(prd_df9)<-c("predict","actual")
t9<-table(prd_df9$predict,prd_df9$actual)
t9
acc_9<-sum(diag(t9))/sum(t9)
acc_9





########   model on knn on test##############################################
library(class)
knn_ctg1<-knn(train=crd_Train,test=crd_Test,cl=crd_Train$Loan_Status1,k=5)
pre_act_ctg1<-data.frame(knn_ctg1,crd_Test$Loan_Status1)
colnames(pre_act_ctg1)<-c("predict","actual")
tab_new6<-table(pre_act_ctg)
ran_acc6<-sum(diag(tab_new6))/sum(tab_new6)*100
ran_acc6







#####################Model on Logistic Regression "Prediction on full data#####################
ml4<-glm(Loan_Status1~.,family=binomial,data=cree_tr_s)
prd4<-predict(ml4,Cree,type="response")
prd_df4<-data.frame(prd4,Cree$Loan_Status1)
prd_df4<-mutate(prd_df4,prd4=ifelse(prd4>0.5,1,0))
colnames(prd_df4)<-c("predict","actual")
t4<-table(prd_df4$predict,prd_df4$actual)
t4
acc_5<-sum(diag(t4))/sum(t4)
acc_5

###########################model on randomForest "Prediction on full data"########################


library(randomForest)
ml5<-randomForest(Loan_Status1~.,data=cree_tr_s)
prd5<-predict(ml5,Cree,type="response")
prd_df5<-data.frame(prd5,Cree$Loan_Status1)
colnames(prd_df5)<-c("predict","actual")
t5<-table(prd_df5$predict,prd_df5$actual)
t5
acc_6<-sum(diag(t5))/sum(t5)
acc_6





###############MOdel on decision tree "Prediction on full data"###################################

library(party)
rm(prd_df6)
rm(t6)
ml6<-ctree(Loan_Status1~.,data=cree_tr_s)
prd6<-predict(ml6,Cree,type="response")
prd_df6<-data.frame(prd6,Cree$Loan_Status1)
colnames(prd_df6)<-c("predict","actual")
t6<-table(prd_df6$predict,prd_df6$actual)
t6
acc_7<-sum(diag(t6))/sum(t6)
acc_7









######################## model on svm "Prediction on Full data" ################################################
library(e1071)
ml7<-svm(Loan_Status1~.,data=cree_tr_s)
prd7<-predict(ml7,Cree,type="response")
prd_df7<-data.frame(prd7,Cree$Loan_Status1)
colnames(prd_df7)<-c("predict","actual")
t7<-table(prd_df7$predict,prd_df7$actual)
t7
acc_8<-sum(diag(t3))/sum(t3)
acc_8



###########################model on naive Bayes on Full data########################

library(e1071)
ml10<-naiveBayes(Loan_Status1~.,data=cree_tr_s)
prd10<-predict(ml10,Cree)
prd_df10<-data.frame(prd10,Cree$Loan_Status1)
colnames(prd_df10)<-c("predict","actual")
t10<-table(prd_df10$predict,prd_df10$actual)
t10
acc_10<-sum(diag(t10))/sum(t10)
acc_10
