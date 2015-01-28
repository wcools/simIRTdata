#
# Item Response Theory when applied to proficiency for 1pl and 2pl items
#
# dictates that the probability of a successful response to a given problem
# depends on the ability of the subject (theta) and the difficulty of the item (beta)
# data are simulated based on a Bernouili distribution conditional on the difference (theta - beta)

# PROFICIENCY (subject abilities)
# set number of subjects and from uniform equidistant intervals generate z-scores to represent standard normal distribution
nrSubjects <- 256
S <- round(qnorm(seq(.001,(1-.001),length.out=nrSubjects),0,1),3)
names(S) <- paste0('s',sprintf(eval(paste0("%0",nchar(nrSubjects),"d")),1:length(S)))

# DIFFICULTY & DISCRIMINATION (item properties)
# set number of items and from uniform equidistant intervals generate scores to represent uniform distribution
nrItems <- 32									# must be dividable by 4 and not equal to 10, 100, 1000, ... for discrimination
# difficulties
I <- seq(-3,3,length.out=nrItems)
names(I) <- paste0('i',sprintf(eval(paste0("%0",nchar(nrItems),"d")),1:length(I)))
# discrimination
tmp <- seq(.75,1.25,length.out=nrItems/4)^4
D <- c(tmp,tmp[length(tmp):1],tmp[length(tmp):1],tmp)
names(D) <- names(I)

# SUCCESS PROBABILITIES
# assume that each subject responds to each item
# assume a success probability that is inversely related to the logit of S-I
# reset the discrimination D as rep(1,length(D)) to go from 2PL to 1PL
P2 <- round(plogis(sweep(outer(S,I,'-'),MARGIN=2,D,`*`)),3)
P1 <- round(plogis(sweep(outer(S,I,'-'),MARGIN=2,rep(1,length(D)),`*`)),3)
set.seed(312)
O1 <- matrix(unlist(Map(rbinom,prob=P1,size=1,n=1)),ncol=length(I))
O2 <- matrix(unlist(Map(rbinom,prob=P2,size=1,n=1)),ncol=length(I))
# alternative way to generate the 1 and 0's from a binomial
# O2 <- matrix(mapply(function(r,c) rbinom(1,1,P2[r,c]), row(P2), col(P2) ),nrow=nrow(P2),ncol=ncol(P2))
# O1 <- matrix(mapply(function(r,c) rbinom(1,1,P1[r,c]), row(P1), col(P1) ),nrow=nrow(P1),ncol=ncol(P1))
dimnames(P1) <- dimnames(P2) <- dimnames(O1) <- dimnames(O2) <- list(names(S),names(I))

# the data
str(O1)
str(O2)
str(P1)
str(P2)


# plot
tmpPar <- par()
par(bg="#202020",col="grey",col.axis="grey",col.lab="grey",col.main="grey",col.sub="grey")

par(mfrow=c(1,2))
plot(S,dnorm(S), xlab="subject proficiency", ylab="density", main="subject population values")
plot(I,D,xlab="item difficulty",ylab="item discrimination", main="item population values",yaxt='n',type='n')
axis(2,round(D,2),las=1,cex.axis=.6,col="grey")
text(I,D,labels=names(I),cex=.75)
par(mfrow=c(1,1))

par(tmpPar)
