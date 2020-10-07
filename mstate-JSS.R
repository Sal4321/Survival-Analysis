###################################################
### chunk number 1: options
###################################################
options(prompt = "R> ", continue = "+  ", width = 70,
    useFancyQuotes = FALSE)


###################################################
### chunk number 2: data
###################################################
library("mstate")
data("ebmt4")
ebmt <- ebmt4


###################################################
### chunk number 3: datashow
###################################################
head(ebmt)


###################################################
### chunk number 4: leveladjustment
###################################################
# better coding factors
ebmt$match <- factor(ebmt$match, levels=c("no gender mismatch", "gender mismatch"))
ebmt$proph <- factor(ebmt$proph, levels=c("no", "yes"))
ebmt$agecl <-factor(ebmt$agecl, levels=c("<=20","20-40",">40"))


###################################################
### chunk number 5: transition matrix
###################################################
tmat <- transMat(x = list( c(2, 3, 5, 6), 
                           c(4, 5, 6), 
                           c(4, 5, 6), 
                           c(5,6),
                           c(), 
                           c()),  # x[[i]] consists of a vector of state numbers reachable from state i
                 names = c("Tx","Rec","AE","Rec+AE","Rel","Death"))
tmat
paths(tmat) # all possible paths


###################################################
### chunk number 6: msprep
###################################################
msebmt <- msprep(data=ebmt,trans=tmat, time=c(NA,"rec","ae","recae","rel","srv"),
            status=c(NA,"rec.s","ae.s","recae.s","rel.s","srv.s"),
            keep=c("match","proph","year","agecl"))
msebmt[msebmt$id==1, ]
msebmt[msebmt$id==2, ]


###################################################
### chunk number 7: events: numbers of transitions
###################################################
events(msebmt)


###################################################
### chunk number 8: expand.covs
###################################################
covs <- c("match","proph","year","agecl")
msebmt <- expand.covs(msebmt, covs, longnames=FALSE)
msebmt[msebmt$id==1, ]


###################################################
### chunk number 9: expand.covs, convert time into years
###################################################
msebmt[, c("Tstart", "Tstop", "time")] <- msebmt[, c("Tstart", "Tstop", "time")]/365.25


###################################################
### chunk number 10: c0
###################################################
c0 <- coxph(Surv(Tstart,Tstop,status)~strata(trans), data=msebmt, method="breslow")


###################################################
### chunk number 11: msfitgreenwood
###################################################
msf0 <- msfit(object=c0, vartype="greenwood", trans=tmat)


###################################################
### chunk number 12: summarymsfit eval=FALSE
###################################################
## summary(msf0)


###################################################
### chunk number 13: summarymsfitshow
###################################################
head(msf0$Haz)
tail(msf0$Haz)
head(msf0$varHaz)
tail(msf0$varHaz)


###################################################
### chunk number 14: allcumhaza eval=FALSE
###################################################
## plot(msf0, las=1, lty=rep(1:2, c(8,4)), xlab="Years since transplantation")


###################################################
### chunk number 15: msfitaalen
###################################################
# msf0a <- msfit(object=c0, vartype="aalen", trans=tmat)
# head(msf0a$Haz)
# tail(msf0a$Haz)
# head(msf0a$varHaz)
# tail(msf0a$varHaz)


###################################################
### chunk number 16: probtransskipnonpar
###################################################
pt0 <- probtrans(msf0, predt=0, method="greenwood")
pt0a <- probtrans(msf0a, predt=0, method="aalen")


###################################################
### chunk number 17: probtransnonpar
###################################################
summary(pt0, from=1) # from state 1
summary(pt0a, from=1) 


###################################################
### chunk number 18: nonpar1plota eval=FALSE
###################################################
## library("colorspace")
## statecols <- heat_hcl(6, c = c(80, 30), l = c(30, 90), power = c(1/5, 2))[c(6,5,3,4,2,1)]
## ord <- c(2,4,3,5,6,1)
## plot(pt0, ord=ord, xlab="Years since transplantation", las=1, type="filled", col=statecols[ord])


###################################################
### chunk number 19: nonpar1plotb
###################################################
library("colorspace")
statecols <- heat_hcl(6, c = c(80, 30), l = c(30, 90), power = c(1/5, 2))[c(6,5,3,4,2,1)]
ord <- c(2,4,3,5,6,1)
plot(pt0, ord=ord, xlab="Years since transplantation", las=1, type="filled", col=statecols[ord])


###################################################
### chunk number 20: probtransskipnonpar100
###################################################
pt100 <- probtrans(msf0, predt=100/365.25, method="greenwood")
summary(pt100, from=1)

###################################################
### chunk number 21: nonpar2a eval=FALSE
###################################################
## plot(pt100,ord=c(2,4,3,5,6,1),xlab="Years since transplantation",main="Starting from transplant", xlim=c(0,10), las=1, type = "filled", col = statecols[ord])
## plot(pt100,from=3,ord=c(2,4,3,5,6,1),xlab="Years since transplantation",main="Starting from AE", xlim=c(0,10),las=1, type = "filled", col = statecols[ord])


###################################################
### chunk number 22: nonpar2b
###################################################
par(mfrow=c(1,2))
plot(pt100,ord=c(2,4,3,5,6,1),xlab="Years since transplantation",main="Starting from transplant", xlim=c(0,10),las=1, type = "filled", col = statecols[ord])
plot(pt100,from=3,ord=c(2,4,3,5,6,1),xlab="Years since transplantation",main="Starting from AE",xlim=c(0,10), las=1, type = "filled", col = statecols[ord])
par(mfrow=c(1,1)) # from state 3


###################################################
### chunk number 23: cfull
###################################################
cfull <- coxph(Surv(Tstart,Tstop,status) ~                 
    match.1+match.2+match.3+match.4+
    match.5+match.6+match.7+match.8+
    match.9+match.10+match.11+match.12+
    proph.1+ proph.2+proph.3+proph.4+
    proph.5+proph.6+proph.7+proph.8+             
    proph.9+proph.10+proph.11+proph.12+           
    year1.1+year1.2+year1.3+year1.4+     
    year1.5+year1.6+year1.7+year1.8+       
    year1.9+year1.10+year1.11+year1.12+ 
    year2.1+year2.2+year2.3+year2.4+    
    year2.5+year2.6+year2.7+year2.8+      
    year2.9+year2.10+year2.11+year2.12+  
    agecl1.1+agecl1.2+agecl1.3+agecl1.4+
    agecl1.5+agecl1.6+agecl1.7+agecl1.8+          
    agecl1.9+agecl1.10+agecl1.11+agecl1.12+        
    agecl2.1+agecl2.2+agecl2.3+agecl2.4+
    agecl2.5+agecl2.6+agecl2.7+agecl2.8+            
    agecl2.9+agecl2.10+agecl2.11+agecl2.12
        + strata(trans),
        data = msebmt, method="breslow")


###################################################
### chunk number 24: msfit2A
###################################################
whA <- which(msebmt$proph=="yes" & 
                 msebmt$match=="no gender mismatch" & 
                 msebmt$year=="1995-1998" & 
                 msebmt$agecl=="<=20")
patA <- msebmt[rep(whA[1],12),9:12]
patA$trans <- 1:12
attr(patA, "trans") <- tmat
patA <- expand.covs(patA, covs, longnames=FALSE)
patA$strata <- patA$trans
msfA <- msfit(cfull, patA, trans=tmat)


###################################################
### chunk number 25: msfit2B
###################################################
whB <- which(msebmt$proph=="no" & 
                 msebmt$match=="gender mismatch" & 
                 msebmt$year=="1985-1989" & 
                 msebmt$agecl==">40")
patB <- msebmt[rep(whB[1],12),9:12]
patB$trans <- 1:12
attr(patB, "trans") <- tmat
patB <- expand.covs(patB, covs, longnames=FALSE)
patB$strata <- patB$trans
msfB <- msfit(cfull, patB, trans=tmat)


###################################################
### chunk number 26: probtransskip2
###################################################
ptA <- probtrans(msfA,predt=0)


###################################################
### chunk number 27: probtrans2B
###################################################
ptB <- probtrans(msfB,predt=0)


###################################################
### chunk number 28: par1a eval=FALSE
###################################################
## plot(ptA,ord=c(2,4,3,5,6,1),main="Patient A", las=1, xlab="Years since transplantation",xlim=c(0,10), type = "filled", col = statecols[ord])


###################################################
### chunk number 29: par1b
###################################################
par(mfrow=c(1,2))
plot(ptA,ord=c(2,4,3,5,6,1),main="Patient A", las=1, xlab="Years since transplantation", xlim=c(0,10), type = "filled", col = statecols[ord])
plot(ptB,ord=c(2,4,3,5,6,1),main="Patient B", las=1, xlab="Years since transplantation", xlim=c(0,10), type = "filled", col = statecols[ord])
par(mfrow=c(1,1))


###################################################
### chunk number 30: probtranspar2a
###################################################
pt100A <- probtrans(msfA,predt=100/365.25)


###################################################
### chunk number 31: probtranspar2b
###################################################
pt100B <- probtrans(msfB,predt=100/365.25)


###################################################
### chunk number 32: par2a eval=FALSE
###################################################
## plot(pt100A,from=3,ord=c(2,4,3,5,6,1),main="Patient A", las=1, xlab="Years since transplantation", xlim=c(0,10), type = "filled", col = statecols[ord])


###################################################
### chunk number 33: par2ba
###################################################
par(mfrow=c(1,2))
plot(pt100A,from=3,ord=c(2,4,3,5,6,1),main="Patient A", las=1, xlab="Years since transplantation", xlim=c(0,10), type = "filled", col = statecols[ord])
plot(pt100B,from=3,ord=c(2,4,3,5,6,1),main="Patient B", las=1, xlab="Years since transplantation", xlim=c(0,10), type = "filled", col = statecols[ord])
par(mfrow=c(1,1))


###################################################
### chunk number 34: probtransskipfixhor
###################################################
ptA10yrs <- probtrans(msfA, predt=10, direction="fixedhorizon")


###################################################
### chunk number 35: fixhor
###################################################
head(ptA10yrs[[1]])


###################################################
### chunk number 36: fixhorfig
###################################################
pt1b <- ptA10yrs[[1]]
pt3b <- ptA10yrs[[3]]
pt4b <- ptA10yrs[[4]]
ind1 <- which(pt1b$time < 60/365.25)
ind2 <- which(pt1b$time >= 60/365.25 & pt1b$time < 80/365.25)
ind3 <- which(pt1b$time >= 80/365.25)
tttime <- pt1b$time
plot(tttime[ind1], 1-(pt1b$pstate5[ind1]+pt1b$pstate6[ind1]), type="s", lwd=2, xlim=c(0,2), ylim=c(0.55,1),
    xlab="Years since transplantation", ylab="Probability of 10-year relapse-free survival",
    las=1)
lines(tttime[-ind1], 1-(pt1b$pstate5[-ind1]+pt1b$pstate6[-ind1]), type="s",lwd=1, lty=2)
segments(tttime[max(ind1)], 1-(pt1b$pstate5[max(ind1)]+pt1b$pstate6[max(ind1)]),
        tttime[max(ind1)], 1-(pt3b$pstate5[max(ind1)]+pt3b$pstate6[max(ind1)]), col=2, lty=3)
lines(tttime[ind2], 1-(pt3b$pstate5[ind2]+pt3b$pstate6[ind2]), type="s", lwd=2, col=2)
segments(tttime[max(ind2)], 1-(pt3b$pstate5[max(ind2)]+pt3b$pstate6[max(ind2)]),
        tttime[max(ind2)], 1-(pt4b$pstate5[max(ind2)]+pt4b$pstate6[max(ind2)]), col=3, lty=3)
lines(tttime[ind3], 1-(pt3b$pstate5[ind3]+pt3b$pstate6[ind3]), type="s", lwd=1, col=2, lty=2)
lines(tttime[ind3], 1-(pt4b$pstate5[ind3]+pt4b$pstate6[ind3]), type="s", lwd=2, col=3)
legend("topleft",c("From Tx","From AE", "From AE + Rec"),lwd=c(2,2,2), col=c(1,2,3), bty="n")


###################################################
### chunk number 37: dataPH1
###################################################
msebmt$strata <- msebmt$trans
msebmt$strata[msebmt$trans %in% c(6,9,11)] <- 3
msebmt$strata[msebmt$trans %in% c(7,10,12)] <- 4
msebmt$strata[msebmt$trans==8] <- 6
msebmt$Z2 <- 0
msebmt$Z2[msebmt$trans %in% c(6,7)] <- 1
msebmt$Z3 <- 0
msebmt$Z3[msebmt$trans %in% c(9,10)] <- 1
msebmt$Z4 <- 0
msebmt$Z4[msebmt$trans %in% c(11,12)] <- 1
msebmt <- expand.covs(msebmt, covs=c("Z2","Z3","Z4"))


###################################################
### chunk number 38: dataPH2
###################################################
coxPH <- coxph(Surv(Tstart,Tstop,status) ~                 
match.1+match.2+match.3+match.4+
match.5+match.6+match.7+match.8+
match.9+match.10+match.11+match.12+
proph.1+ proph.2+proph.3+proph.4+
proph.5+proph.6+proph.7+proph.8+             
proph.9+proph.10+proph.11+proph.12+           
year1.1+year1.2+year1.3+year1.4+     
year1.5+year1.6+year1.7+year1.8+       
year1.9+year1.10+year1.11+year1.12+ 
year2.1+year2.2+year2.3+year2.4+    
year2.5+year2.6+year2.7+year2.8+      
year2.9+year2.10+year2.11+year2.12+  
agecl1.1+agecl1.2+agecl1.3+agecl1.4+
agecl1.5+agecl1.6+agecl1.7+agecl1.8+          
agecl1.9+agecl1.10+agecl1.11+agecl1.12+        
agecl2.1+agecl2.2+agecl2.3+agecl2.4+
agecl2.5+agecl2.6+agecl2.7+agecl2.8+            
agecl2.9+agecl2.10+agecl2.11+agecl2.12+
Z2.6+Z2.7+Z3.9+Z3.10+Z4.11+Z4.12      
        + strata(strata),
        data = msebmt, method="breslow")


###################################################
### chunk number 39: predAPH
###################################################
patAPH <- patA
patAPH$strata[6:12] <- c(3,4,6,3,4,3,4)  
patAPH$Z2 <- 0
patAPH$Z2[patAPH$trans %in% c(6,7)] <- 1
patAPH$Z3 <- 0
patAPH$Z3[patAPH$trans %in% c(9,10)] <- 1
patAPH$Z4 <- 0
patAPH$Z4[patAPH$trans %in% c(11,12)] <- 1
patAPH <- expand.covs(patAPH, covs=c("Z2","Z3","Z4"))
msfAPH <- msfit(coxPH, patAPH, trans=tmat)
ptAPH <- probtrans(msfAPH,predt=0)


###################################################
### chunk number 40: probtransskipcpses
###################################################
pt100APH <- probtrans(msfAPH, predt=100/365.25)
ptA3 <- pt100A[[3]]
ptAPH3 <- pt100APH[[3]]
plot(ptA3$time, ptA3$se6, xlim=c(0,2), type="s", lwd=2,
    xlab="Years since transplantation", ylab="Standard errors", las=1)
lines(ptAPH3$time, ptAPH3$se6, type="s", lwd=2, lty=2)
text(2,0.085,"Death",adj=1)
lines(ptA3$time, ptA3$se5, type="s", lwd=2, col=8)
lines(ptAPH3$time, ptAPH3$se5, type="s", lwd=2, lty=2, col=8)
text(2,0.0425,"Relapse",adj=1)
legend("topleft", c("non-PH","PH"), lwd=2, lty=c(1,2), bty="n")


###################################################
### chunk number 41: cpses
###################################################
ptA3 <- pt100A[[3]]
ptAPH3 <- pt100APH[[3]]
#plot(ptA3$time, ptA3$se5, type="s", xlim=c(0,1), lwd=2, col=1,
#    xlab="Years since transplantation", ylab="Standard errors", las=1)
plot(ptA3$time, ptA3$se6, xlim=c(0,2), type="s", lwd=2,
    xlab="Years since transplantation", ylab="Standard errors", las=1)
lines(ptAPH3$time, ptAPH3$se6, type="s", lwd=2, lty=2)
text(2,0.085,"Death",adj=1)
lines(ptA3$time, ptA3$se5, type="s", lwd=2, col=8)
lines(ptAPH3$time, ptAPH3$se5, type="s", lwd=2, lty=2, col=8)
text(2,0.0425,"Relapse",adj=1)
legend("topleft", c("non-PH","PH"), lwd=2, lty=c(1,2), bty="n")


###################################################
### chunk number 42: redrank
###################################################
rr1 <- redrank(Surv(Tstart,Tstop,status) ~ match+proph+year+agecl, data=msebmt, R=1, print.level=0)


###################################################
### chunk number 43: redrankshow1
###################################################
rr1$Alpha
rr1$Gamma


###################################################
### chunk number 44: redrankshow1beta
###################################################
rr1$Beta


###################################################
### chunk number 45: definetheta
###################################################
rr1beta <- function(data) {
    rr1 <- redrank(Surv(Tstart,Tstop,status) ~ match+proph+year+agecl, data=data, R=1, print.level=0)
    return(as.vector(rr1$Beta))
}
th <- rr1beta(msebmt)


###################################################
### chunk number 46: msboot eval=FALSE
###################################################
## set.seed(1234)
## msb <- msboot(theta=rr1beta,data=msebmt,id="id",B=500,verbose=0)


###################################################
### chunk number 47: msbootse eval=FALSE
###################################################
## sqrt(apply(msb,1,var))


###################################################
### chunk number 48: mssample
###################################################
set.seed(1234)
msfAsample <- mssample(Haz=msfA$Haz,trans=tmat,tvec=1:10,M=10000)


###################################################
### chunk number 49: showmssample
###################################################
msfAsample


