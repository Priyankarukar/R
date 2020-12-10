getwd()

data=read.csv('SalesMarketing.csv')
data
summary(data)
str(data)

data<-read.csv('C:/Users/priyanka rukar/Documents/SalesMarketing.csv')
summary(data)
str(data)
head(data)



#Linear Model

boxplot(data$Gross.profit~data$Order.method.type)
boxplot(Revenue~Order.method.type,data=data)
boxplot(Revenue~Year,data=data)

library(lattice)
histogram(~Revenue,data=data,col="blue")
histogram(data$Gross.profit,col="Red")


library(car)
scatterplot(Gross.profit~Order.method.type,data=retail.df,main="ScatterPlot of GrossProfit with Year",ylab="Gross profit",xlab="Order method type",horizontal=TRUE)
scatterplotMatrix(~ Gross.profit + Revenue , data=retail.df, main="Gross profit")
scatterplotMatrix(~Gross.profit+Year+Product.type+Order.method.type , data=retail.df)
scatterplot(Gross.profit ~ Year ,data=retail.df, main="Scatterplot of Gross profit with year", ylab="Gross profit", xlab="Year", horizontal=TRUE)

library(Hmisc)
colretails <-c("Gross.profit","Product.cost","Quantity")
corMatrix <- rcorr(as.matrix(retail.df[,colretails]))
corMatrix

library(corrgram)
colretails <- c("Gross.profit","Product.cost","Quantity")
corrgram(retail.df[,colretails], order=TRUE,
         main="Gross profit vs others",
         lower.panel=panel.pts, 
         upper.panel=panel.pie,diag.panel=panel.minmax, text.panel=panel.txt)

#read the dataset
retail.df<- read.csv(paste("SalesMarketing.csv",sep=""))
attach(retail.df)

#Regression model  
m1 <- Gross.profit~ Year+ Product.line+ Product.type+ Order.method.type+ Revenue +Quantity + Unit.cost +Unit.sale.price
f1 <- lm(m1,data=retail.df)
summary(f1)

#One way contigency table
mytable<-with(retail.df,table(Order.method.type))
mytable

mytable2 <- with(retail.df,table(Retailer.country))
mytable2

#Two way contigency table
productlinebyOrdertype<- xtabs(~retail.df$Product.line+retail.df$Order.method.type,data=retail.df) 
productlinebyOrdertype
addmargins(productlinebyOrdertype)

producttypebyOrdertype<-xtabs(~retail.df$Product.type+retail.df$Order.method.type,data=retail.df)

producttypebyOrdertype
addmargins(producttypebyOrdertype)

avgprofit = aggregate(Gross.profit ~ Order.method.type , data = retail.df, mean)
avgprofit

#Pearson's Correlation test
cor.test(retail.df[,"Gross.profit"], retail.df[,"Quantity"])
cor.test(retail.df[,"Gross.profit"], retail.df[,"Revenue"])

#T-test
t.test(retail.df$Revenue,retail.df$Planned.revenue)

#Regression Model
M1 <- Gross.profit ~  Year + Product.line + Product.type + Order.method.type + Revenue + Quantity + Unit.cost + Unit.sale.price
f1 <- lm(M1, data = retail.df)
summary(f1)

m1=data.frame(Year=2004,Product.line="Camping Equipment",Product.type="Cooking Gear",Revenue=315044.3,Order.method.type="Telephone",Quantity=66385,Unit.cost=2.552857,Unit.sale.price=5.195714)
Gross.profit =predict(f1,m1)
Gross.profit

library(leaps)
leap1<-regsubsets(M1,data = retail.df,nbest=1)
plot(leap1,scale="adjr2")

#model 2
M2 <- Gross.profit~Revenue+Quantity+Unit.cost+Unit.sale.price
f2 <-lm(M2,data=retail.df)
summary(f2)

library(leaps)
leap2 <- regsubsets(M2,data=retail.df,nbest=1)
plot(leap2,scale="adjr2")
summary(f1)$adj.r.squared

summary(f2)$adj.r.squared

