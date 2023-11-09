install.packages("DescTools")
library(DescTools)
CHD.data<-matrix(c(178, 1411, 79, 1486), nrow=2, ncol=2, byrow=T)
dimnames(CHD.data)<-list(Factor=c("Type-A", "Type-B"),
                         Event=c("CHD", "No CHD"))

print(CHD.data)
RelRisk(CHD.data, conf.level = 0.95)

Total<-rowSums(CHD.data)
CHD.df<-as.data.frame.table(CHD.data)
print(CHD.df)

CHD.df<-CHD.df[1:2,]
CHD.df$Total<-Total
CHD.df

coba<-CHD.df
CHD.df<-within(CHD.df, Factor <- relevel(Factor, ref = 2))


regpois<-glm(Freq~Factor,offset=log(Total),family=poisson(link="log"), data=CHD.df)
summary(regpois)

exp(regpois$coefficients[2])

exp(coef(regpois)[1])

exp(coef(regpois)[1]+coef(regpois)[2])

CHD.smoking<-read.csv("https://raw.githubusercontent.com/raoy/data/master/chd%20smoking.csv")

CHD.smoking$Smoking<-as.factor(CHD.smoking$Smoking)
CHD.smoking$Factor<-as.factor(CHD.smoking$Factor)

CHD.smoking <- within(CHD.smoking, Smoking <- relevel(Smoking, ref = 4))
CHD.smoking <- within(CHD.smoking, Factor <- relevel(Factor, ref = 2))


levels(CHD.smoking$Factor)

levels(CHD.smoking$Smoking)

CHD.smoking

regpois2<-glm(CHD~Factor+Smoking,offset=log(Total),family=poisson(link="log"), data=CHD.smoking)


summary(regpois2)

pred<-predict(regpois2, type="response")
df.pred<-data.frame(CHD=CHD.smoking$CHD, "CHD-pred"=pred)

df.pred

with(regpois2, cbind(res.deviance = deviance, df = df.residual,
                     p = pchisq(deviance, df.residual, lower.tail=FALSE)))

