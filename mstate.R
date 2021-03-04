library(mstate)
data(ebmt3)
n<-nrow(ebmt3)
table(ebmt3$dissub)
round(100*table(ebmt3$dissub)/n)
round(100*table(ebmt3$age)/n)
round(100*table(ebmt3$drmatch)/n)
round(100*table(ebmt3$tcd)/n)


tmat<-transMat(x=list(c(2,3),c(3),c()),names=c("Tx","PR","RelDeath"))

paths(tmat)