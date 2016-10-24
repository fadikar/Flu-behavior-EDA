setwd('D:\\git\\Flu-behavior-EDA\\data')
library(XLConnect)
library(lattice)

cdata <- read.csv('classification_data.csv')
attach(cdata)

histogram( ~ Q13, col=c("red", "green"))
pdf("Q17.pdf", height = 6, width=8)
histogram( ~ Q13 | Q17, data=subset(cdata, Q17 != "#NULL!"), 
           xlab="Get vaccine", col="grey")
dev.off()
histogram( ~ Q13 | Q14, data=subset(cdata, Q14 != "#NULL!"))
pdf("Q15.pdf", height = 6, width=8)
histogram( ~ Q13 | Q15, data=subset(cdata, Q15 != "#NULL!"), 
           xlab="Get vaccine",col="grey")
dev.off()
histogram( ~ Q13 | Q16, data=subset(cdata, Q16 != "#NULL!"))
histogram( ~ Q13 | Q19, data=subset(cdata, Q19 != "#NULL!"))
histogram( ~ Q13 | Q20, data=subset(cdata, Q20 != "#NULL!"))
histogram( ~ Q13 | Q21, data=subset(cdata, Q21 != "#NULL!"))

###############################################

adata <- read.csv('association_data.csv')

q12ans <- vector()
q12desc <- vector()
q12freq <- vector()
for (i in 1:14){
  q12desc <- c(q12desc, rep(i, 3))
  q12ans <- c(q12ans, c("Sometimes", "Never", "Always"))
  q12freq <- c(q12freq, sum(adata[,24+i] == q12ans[1]), sum(adata[,24+i] == q12ans[2]), sum(adata[,24+i] == q12ans[3]))
}

barchart(q12freq ~ q12ans | as.character(q12desc), ylab = "Frequency")
