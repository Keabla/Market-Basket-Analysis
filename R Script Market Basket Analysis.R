#install and open the necessary libraries
install.packages("arules")
install.packages("arulesViz")
install.packages("grid")
install.packages("arulesViz", dependencies = TRUE)
install.packages("prabclus")
install.packages("DEoptimR")
install.packages("trimcluster")


pacman::p_load(arules, arulesViz, dplyr, grid, prabclus,DEoptimR,trimcluster, ggplot2, pdftools)

#set directory
setwd("C:/Users/Martin Albaek/Documents/coding academy/Ubiqum/Course/Module 2/Part 4/Market Basket Analysis")
getwd()

#Upload dataset as a transaction documnent
Transactions <- read.transactions(file = "ElectronidexTransactions2017.csv",sep = ",",rm.duplicates = TRUE,format="basket")
View(Transactions)
ncol(Transactions)

#Inspection of data
inspect(Transactions)
length(Transactions)
itemLabels(Transactions)
summary(Transactions)

VectorSize <- data.frame(size(Transactions))
ggplot(data=VectorSize, aes(VectorSize$size.Transactions.))+geom_histogram(col="yellow",aes(fill=..count..))+labs(x="Transaction size",y="Frequency")+labs(title="Histogram of transaction size")

itemFrequency(Transactions)

#Apply the Apriori Algorythm
Rule1 <- apriori(Transactions, parameter = list(supp=0.01, conf= 0.51, minlen=2))
inspect(sort(Rule1,by="lift"))

#Inspect Item Rule
ItemRules <- subset(Rule1
                    , items %in% "iMac")
inspect(ItemRules)

#Check if there is any redundant rule
is.redundant(Rule1)

#Plot the rules
plot(Rule1)
plot(Rule1[1:4],method = "graph", control=list(type="iMac"))

plot(Rule1[1:5],method = "graph")

#Extract data from pdf file
text <- pdf_text("ElectronidexItems2017.pdf")
View(text)

text %>% strsplit(split="\n")

text2 <- strsplit(text,"\n")
head(text2[[1]])


        

