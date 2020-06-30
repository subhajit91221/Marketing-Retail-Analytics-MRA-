## Set Working Directory

setwd("D:/BABI/Marketing & Retail Analytics (MRA)/Project/")
getwd()

library(psych)
library(DataExplorer)
library(corrplot)
library(ppcor)
library(rpart)
library(rpart.plot)
library(ROCR)
library(rattle)
library(car)
library(olsrr)
library(MASS)
library(class)
library(caret)
library(lattice)
library(ggplot2)
library(readxl)
library(dplyr)

#The data is loaded onto mydata object

mydata = read_excel("CafeCoffeeNight_FnB Only.xlsx")

View(mydata)


#Importing the excel file  the data and descriptive statistics

head(mydata)
dim(mydata)
str(mydata)
names(mydata)
describe(mydata)
summary(mydata)

## Check for missing value (NA)
anyNA(mydata)
plot_missing(mydata)


mktBasket=mydata

#The 'split' function takes the first parameter and groups them with respect to the invoice numbers
mktBasket.Agg=split(mktBasket$ItemDesc,mktBasket$BillNumber)
head(mktBasket.Agg)

#Find total amount per bill , export to a an excel for further analysis 
BillSummarization <- mktBasket %>%
  group_by(BillNumber) %>%
  summarise(TotalperBill = sum(Total))

write.csv(BillSummarization, file = "BillSummarization.csv", row.names = FALSE)

# Remove duplicates
mktBasket.Agg2=list()
for(i in 1:length(mktBasket.Agg)){
  mktBasket.Agg2[[i]]=unique(mktBasket.Agg[[i]])
}
head(mktBasket.Agg2)


library(arules) #'arules' has a specific data structure called Transactions
Txns=as(mktBasket.Agg2,"transactions")
summary(Txns)


inspect(Txns[1:10])#here we are looking at 10 transactions
 
freq=itemFrequency(Txns)#gives us the frequency of each items
freq=freq[order(-freq)]#gives the frequencies in descending order
freq
freq["CAPPUCCINO"]
barplot(freq[1:20])#first 20 items barplot
itemFrequencyPlot(Txns,support=.05)
itemFrequencyPlot(Txns,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Item Frequency Plot")

#Association rules

arules=apriori(data=Txns)
inspect(sort(arules,by='lift'))

#The 'apriori' function has a lot of default rules. Let us see what happens if we override them.

arules2=apriori(data=Txns,parameter=list(support=0.001,confidence=0.0025,minlen=2))
summary(arules2)

inspect(sort(arules2,by='lift'))


#Now let us plot arules2. We are plotting 'confidence' vs 'support' with 'lift' as a colour gradient.

library(RColorBrewer)#we are using this library to plot the colour as a gradient rather than a single colour
library(arulesViz)
plot(arules2,control=list(col=brewer.pal(11,"Spectral")))#the plot function now works on the basis of the arulesViz package
#ideally, here we are looking for something on the top right corner with a deep red
summary(arules2)

subrules2=head(sort(arules2,by="confidence"),20)
inspect(subrules2)
plot(subrules2,method="graph",engine = "htmlwidget")

saveAsGraph(head(subrules2, n = 100, by = "lift"), file = "rules.graphml")


rules_df=as(arules2,"data.frame")#here we are converting arules2 in to a data frame
#Rule: {A}=>{B}
#Probability(A)-LHS Support
rules_df$LHSSupport=rules_df$support/rules_df$confidence#gives us the probability of A on the LHS
#Probability(B)-RHS Support
rules_df$RHSSupport=rules_df$confidence/rules_df$lift#gives us the probability of B on the RHS
print(rules_df)#here we can finally use the print command rather than inspect as it has been converted in to a data frame
write.table(rules_df,file="MBA_output.csv",sep=",",append=FALSE,row.names = FALSE)
