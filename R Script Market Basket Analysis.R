#install and open the necessary libraries
install.packages("arulesViz")
install.packages("grid")
install.packages("arules")
install.packages("arulesViz", dependencies = TRUE)
install.packages("prabclus")
install.packages("DEoptimR")
install.packages("trimcluster")
install.packages("promises")
install.packages("treemap")
install.packages("mime")
install.packages("shiny")

library(treemap)
pacman::p_load(shiny,arules, arulesViz,grid, prabclus, DEoptimR, trimcluster, dplyr, ggplot2, pdftools,plyr,Matrix,tidyr,mgsub,treemap,RColorBrewer)

#set directory
setwd("C:/Users/Martin Albaek/Documents/coding academy/Ubiqum/Course/Module 2/Part 4/Market Basket Analysis/Datasets")
getwd()

#import csv file with the product types
TypeProduct <- read.csv("item2017.csv", header = TRUE, sep = ";")

#Upload dataset as a transaction documnent
Transactions <- read.transactions(file = "ElectronidexTransactions2017.csv",sep = ",",rm.duplicates = TRUE,format="basket")

#Inspection of data
VectorSize <- data.frame(size(Transactions))
ggplot(data=VectorSize, aes(VectorSize$size.Transactions.))+
  geom_histogram(col="yellow",aes(fill=..count..))+
  labs(x="Transaction size",y="Frequency")+labs(title="Histogram of transaction size")

#Apply the Apriori Algorythm
Rule1 <- apriori(Transactions, parameter = list(supp=0.01, conf= 0.54))
inspect(sort(Rule1,by="lift"))

#Check if there is any redundant rule
is.redundant(Rule1)

#Plot the rules in decreasing order for lift
plot(Rule1, method = "graph")
plot(utils::head(sort(Rule1,by="lift"), 6) ,method = "graph")

#Extract Transaction as a csv file
TranCSV <- read.csv(file = "ElectronidexTransactions2017.csv", header = FALSE)
TranCSV <- TranCSV[!apply(TranCSV == "", 1, all),]

#Create Lookup table and add the value 
Lookup <- unique(TypeProduct)

#Add Brand Name to Lookup
#add new column with brand created by having a column with only the first word of the Product Name
Lookup$ProductName<-as.character(Lookup$ProductName)
Lookup <- Lookup %>% separate(ProductName, c("ProductBrand"), remove = FALSE) 

#Substitute some of the words in the brand category that we know are not brands with their respective brand
SubstitutedNames <- c("iPad","iPhone","iMac","1TB","2TB","3","3TB","5TB","Large","Computer","Full",
                      "Gaming","Generic","Halter","HDMI","PC","Slim",
                      "Smart","Height","USB","VGA","Wireless","Cyber","Audio",
                      "Ethernet","Multi","Monster")

ReplacedNames <- c("Apple","Apple","Apple","N.N","N.N","N.N","N.N","N.N","N.N","N.N","N.N","N.N","N.N","N.N","N.N",
                   "N.N","N.N","N.N","N.N","N.N","N.N","N.N","N.N","N.N","N.N",
                   "N.N","Beats")

Lookup$ProductBrand <- mgsub(Lookup$ProductBrand,SubstitutedNames,ReplacedNames)
Lookup$ProductBrand[9] <- c("Alienware")
#Lookup$ProductBrand <- gsub("N.N",NA,Lookup$ProductBrand)

Lookup$ProductBrand <- gsub("N.N","",Lookup$ProductBrand)


#we want to create two transactions objects with instead of Product name we want the 
#corresponding Product type and Product Brand, so we substitute the labels of each
#transaction objects thank to our lookup table and mgsub

#Creation of a new label on the transaction dataset about Product Type
Transactions@itemInfo$ProductType <- Transactions@itemInfo$labels

#Substitute Product Names with the corresponding Product Type
Transactions@itemInfo$ProductType <- 
  mgsub(Transactions@itemInfo$ProductType,Lookup$ProductName,Lookup$ProductType)

#Creation of a new Transaction for product type
TranProductType <- aggregate(Transactions,itemInfo(Transactions)[["ProductType"]])

#Creation of a Rule for Product Type
Rule2 <- apriori(TranProductType, parameter = list(conf=0.50, supp= 0.01))

is.redundant(Rule2)

#to explore the rules in order to have useful insight
ruleExplorer(Rule2)

plot(utils::head(sort(Rule2,by="lift"), 10) ,method = "graph")
#Found that the most important rules on Product Type regards Desktop

#Creation of a new label on the transaction dataset about Product Brand
Transactions@itemInfo$ProductBrand <- Transactions@itemInfo$labels

#Substitute Product Names with the corresponding Product Brand
Transactions@itemInfo$ProductBrand <- 
  mgsub(Transactions@itemInfo$ProductBrand,Lookup$ProductName,Lookup$ProductBrand)

#Creation of a new Transaction set with Product Brand
TranProductBrand <- aggregate(Transactions,itemInfo(Transactions)[["ProductBrand"]])

#Creation of a Rule for Product Brand
Rule3 <- apriori(TranProductBrand, parameter = list(supp=0.01, conf=0.01))

#to explore the rule in order to have useful insight
ruleExplorer(Rule3)

#Creation of two datasets, one for Business to Customer transaction and the other one
#for Business to Business transaction

#transformation of the transaction object for product name into a matrix 
#(cannot use the product one), otherwise we could not be able to see different product type 
#per transaction, as the matrix, being logial, would only show whether the object is present
#or not, not its frequency
DTPName <- as.data.frame(as(Transactions,"matrix"))

#creating empty dataframe
DTPName1 <- c()

ncolumn <- c(1:ncol(DTPName))

#run loop to substitute each True value with its column name 
for (i in ncolumn) { 
  a <- ifelse(DTPName[,i]==TRUE,colnames(DTPName[i]),NA)
  DTPName1 <- cbind(DTPName1,a)
}

#substitute each item name with its Product Type
DTPName2 <- mgsub(DTPName1,Lookup$ProductName,Lookup$ProductType)

#create a loop with a if else function (condition is if items per transactions are more than 
#five or if there is more than more product type per transaction then it should be a BTB business,
#Otherwise it might be a BTC transaction)

BTBt <- c()
BTCt <- c()

#create vector that indicates which row is BTB and BTC transaction
nrows <- c(1:nrow(DTPName))

VectorBTB_BTC <- c()

for (j in nrows) {
  if(length(as.vector(na.omit(DTPName2[j,])))>4 |
     length(as.vector(na.omit(DTPName2[j,]))) != length(unique(as.vector(na.omit(DTPName2[j,])))))
  {
    VectorBTB_BTC <- rbind(VectorBTB_BTC,"BTB")
  } else {
    VectorBTB_BTC <- rbind(VectorBTB_BTC,"BTC")
  }
}

#Create Transaction object of BTB and BTC for Product type
BTB_PT <- TranProductType[VectorBTB_BTC=="BTB"]
BTC_PT <- TranProductType[VectorBTB_BTC=="BTC"]

#create rules for BTB_PT transactional object 
rule4 <- apriori(BTB_PT, parameter = list(conf=0.01, supp= 0.1))

inspect(sort(rule4,by="supp"))

is.redundant(rule4)

inspect(rule4)

ruleExplorer(rule4)


#Produce Graphs displaying the frequency of the Product categories for BTB and BTC activities 
arules::itemFrequencyPlot(BTB_PT,
                          topN= 15,
                          col=brewer.pal(8,'Pastel2'),
                          main= 'Relative PT Item Frequency Plot BTB',
                          type = "relative",
                          ylab = "Item Frequency(Relative)"
                          )
                          
arules::itemFrequencyPlot(BTC_PT,
                          topN= 15,
                          col=brewer.pal(8,'Pastel2'),
                          main= 'Relative Item Frequency Plot BTC',
                          type = "relative",
                          ylab = "Item Frequency(Relative)"
                          )

#Creating Treemap For BTB PT 
BTB_PT_IF <- as.data.frame(itemFrequency(BTB_PT))

BTB_PT_IF1 <- cbind(BTB_PT_IF,TranProductType@itemInfo$ProductType)

colnames(BTB_PT_IF1) <- c("ItemFreq","ProductType")

TMBTB_PT <- treemap(BTB_PT_IF1,index="ProductType",vSize ="ItemFreq",title = "Product Type Frequency BTB")

#Creating Treemap for BTC PT
BTC_PT_IF <- as.data.frame(itemFrequency(BTC_PT))

BTC_PT_IF1 <- cbind(BTC_PT_IF,TranProductType@itemInfo$ProductType)

colnames(BTC_PT_IF1) <- c("ItemFreq","ProductType")

TMBTC_PT <- treemap(BTC_PT_IF1,index="ProductType",vSize ="ItemFreq",title = "Product Type Frequency BTC")

#Creating Treemap for the whole DataSet 
Tran_PT_IF <- as.data.frame(itemFrequency(TranProductType))

Tran_PT_IF1 <- cbind(Tran_PT_IF,TranProductType@itemInfo$ProductType)

colnames(Tran_PT_IF1) <- c("ItemFreq","ProductType")

TMBTB_PT <- treemap(Tran_PT_IF1,index="ProductType",vSize ="ItemFreq",title = "Product Type Frequency")
