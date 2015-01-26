#
# Item Response Theory when applied to proficiency for 1pl and 2pl items
#
# uses the R objects that are created in simulateIRTdata.r

tmpPar <- par()

# the probability distribution
# png(file="simProbIRT.png",width=400,height=350)
par(mfrow=c(1,2),bg="#202020", mgp=c(1.3,.5,0), oma = c(2,1,0,0) + 1, mar = c(0,0,1,1) + 3)
	image(P1,main="P(success) ~ inv.logit(1pl)",xlab="subjects (proficiency)",ylab="items (difficulty)",col.lab="white",xaxt='n',yaxt='n',col.main="white")
	axis(2,at=(-1+1:nrItems)/(nrItems-1),labels=dimnames(P1)[[2]],tick=TRUE,las=1,cex.axis=.5)
	image(P2,main="P(success) ~ inv.logit(2pl)",xlab="subjects (proficiency)",ylab="items (difficulty)",col.lab="white",xaxt='n',yaxt='n',col.main="white")
	axis(2,at=(-1+1:nrItems)/(nrItems-1),labels=dimnames(P2)[[2]],tick=TRUE,las=1,cex.axis=.5)
	axis(4,at=(-1+1:nrItems)/(nrItems-1),labels=paste0(D),tick=TRUE,las=1,cex.axis=.5)
par(mfrow=c(1,1))
savePlot("simProbIRT.png",type="png")
# dev.off()

# the probability distribution
# png(file="simObsIRT.png",width=400,height=350)
par(mfrow=c(1,2),bg="#202020", mgp=c(1.3,.5,0), oma = c(2,1,0,0) + 1, mar = c(0,0,1,1) + 3)
	image(O1,main="simulated values for P ~ inv.logit(1pl)",xlab="subjects (proficiency)",ylab="items (difficulty)",col.lab="white",xaxt='n',yaxt='n',col.main="white")
	axis(2,at=(-1+1:nrItems)/(nrItems-1),labels=dimnames(P1)[[2]],tick=TRUE,las=1,cex.axis=.5)
	image(O2,main="simulated values for P ~ inv.logit(2pl)",xlab="subjects (proficiency)",ylab="items (difficulty)",col.lab="white",xaxt='n',yaxt='n',col.main="white")
	axis(2,at=(-1+1:nrItems)/(nrItems-1),labels=dimnames(P2)[[2]],tick=TRUE,las=1,cex.axis=.5)
	axis(4,at=(-1+1:nrItems)/(nrItems-1),labels=paste0(D),tick=TRUE,las=1,cex.axis=.5)
par(mfrow=c(1,1))
savePlot("simObsIRT.png",type="png")
# dev.off()

par(tmpPar)

par(bg="#202020")
plot(colMeans(O2),cex=D*1.5,pch=19,ylim=c(0,1),xaxt='n',ylab="average performance",xlab="items (easy to hard)",main="simulated performance per item",col.lab="white",xaxt='n',yaxt='n',col.main="white",col=2)
points(colMeans(O1),col=7)
lines(1:length(I),1-plogis(I),col=7)
axis(1,dimnames(O2)[[2]],at=1:64,cex.axis=.5,col=9)
axis(2,at=seq(0,1,.1),cex.axis=.5,las=1)
savePlot("simMeanObsIRT.png",type="png")
par(tmpPar)

