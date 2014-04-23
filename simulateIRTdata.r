# Item Response Theory when applied to proficiency 
# dictates that the probability of a succesful response to a given problem 
# depends on the ability of the subject and the difficulty of the item

# data are simulated based on a bernouili distribution conditional on the difference
# between the subject ability and the item difficulty
 
# PROFICIENCY (subject abilities)
# assume 49 values on a standard normal distribution with probabilities from .025 to .975
S <- round(qnorm(seq(.01,(1-.01),by=.02),0,1),3)
names(S) <- paste0('s',1:length(S))

# DIFFICULTY & DISCRIMINATION (item properties)
# assume 65 values with probabilities from -2.4 to 2.4 on a uniform distribution
I <- seq(-2.4,2.4,by=.075)
names(I) <- paste0('i',1:length(I))
D <- c(seq(.05,3.15,.1),1,seq(3.15,.05,-.1))
tst <- unlist(lapply(1:4,function(.x) seq(.x,32,by=4)))
tst <- c(tst[32:1],33,tst)
D <- D[tst]
names(D) <- names(I)

# SUCCESS PROBABILITIES
# assume that each subject responds to each item
# assume a success probability that is inversly related to the logit of S-I
P2 <- round(plogis(sweep(outer(S,I,'-'),MARGIN=2,D,`*`)),3)
# reset the discrimation D as rep(1,length(D)) to go from 2PL to 1PL
P1 <- round(plogis(sweep(outer(S,I,'-'),MARGIN=2,rep(1,length(D)),`*`)),3)
dimnames(P1) <- dimnames(P2) <- list(names(S),names(I))

# OBSERVATIONS (Success / Failure)
# assume that the observations are distributed bernouli given the inverse logit of S-I
set.seed(312)
O2 <- matrix(mapply(function(r,c) rbinom(1,1,P2[r,c]), row(P2), col(P2)  ),nrow=nrow(P2),ncol=ncol(P2))
O1 <- matrix(mapply(function(r,c) rbinom(1,1,P1[r,c]), row(P1), col(P1)  ),nrow=nrow(P1),ncol=ncol(P1))
dimnames(O1) <- dimnames(O2) <- list(names(S),names(I))

# PROBABILITIES
par(mfrow=c(2,2))
	image(P1,main="inv.logit(1pl)",xlab="subjects",ylab="items",xaxt='n',yaxt='n')
	image(P2,main="inv.logit(2pl)",xlab="subjects",ylab="items",xaxt='n',yaxt='n')
# OBSERVATIONS
	image(O1.5,main="Obs(1pl)",xlab="subjects",ylab="items",xaxt='n',yaxt='n')
	image(O2.5,main="Obs(2pl)",xlab="subjects",ylab="items",xaxt='n',yaxt='n')
par(mfrow=c(1,1))

