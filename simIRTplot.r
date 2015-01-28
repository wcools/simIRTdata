#
# Item Response Theory when applied to proficiency for 1pl and 2pl items
#
# uses the R objects that are created in simulateIRTdata.r

getwd()
toPrint <- TRUE

# plot
tmpPar <- par()
par(bg="#202020",col="grey",col.axis="grey",col.lab="grey",col.main="grey",col.sub="grey")

# population values for subject proficiency (Normal) and item difficulty/discrimination
par(mfrow=c(1,2))
plot(S,dnorm(S), xlab="subject proficiency", ylab="density", main="subject population values")
plot(I,D,xlab="item difficulty",ylab="item discrimination", main="item population values",yaxt='n',type='n')
axis(2,round(D,2),las=1,cex.axis=.6,col="grey")
text(I,D,labels=names(I),cex=.75)
par(mfrow=c(1,1))
if(toPrint) savePlot("simPopValuesIRT.png",type="png")

# heatmap for 1PL and 2PL model population values for the probability of success
par(mfrow=c(1,2),bg="#202020", mgp=c(1.3,.5,0), oma = c(2,1,0,0) + 1, mar = c(0,0,1,1) + 3)
image(P1,main="P(success) ~ inv.logit(1PL model)",xlab="subjects (~ proficiency)",ylab="items (~ difficulty)",xaxt='n',yaxt='n')
axis(2,at=(-1+1:nrItems)/(nrItems-1),labels=dimnames(P1)[[2]],tick=TRUE,las=1,cex.axis=.5,col="grey")
image(P2,main="P(success) ~ inv.logit(2PL model)",xlab="subjects (~ proficiency)",ylab="items (~ difficulty)",xaxt='n',yaxt='n')
axis(2,at=(-1+1:nrItems)/(nrItems-1),labels=dimnames(P2)[[2]],tick=TRUE,las=1,cex.axis=.5,col="grey")
axis(4,at=(-1+1:nrItems)/(nrItems-1),labels=paste0(round(D,2)),tick=TRUE,las=1,cex.axis=.5,col="grey")
par(mfrow=c(1,1))
if(toPrint) savePlot("simProbIRT.png",type="png")

# heatmap for 1PL and 2PL model population values for the probability of success
par(mfrow=c(1,2),bg="#202020", mgp=c(1.3,.5,0), oma = c(2,1,0,0) + 1, mar = c(0,0,1,1) + 3)
image(O1,main="simulated item responses ~ inv.logit(1PL model)",xlab="subjects (~ proficiency)",ylab="items (~ difficulty)",xaxt='n',yaxt='n')
axis(2,at=(-1+1:nrItems)/(nrItems-1),labels=dimnames(P1)[[2]],tick=TRUE,las=1,cex.axis=.5,col="grey")
image(O2,main="simulated item responses ~ inv.logit(2PL model)",xlab="subjects (~ proficiency)",ylab="items (~ difficulty)",xaxt='n',yaxt='n')
axis(2,at=(-1+1:nrItems)/(nrItems-1),labels=dimnames(P2)[[2]],tick=TRUE,las=1,cex.axis=.5,col="grey")
axis(4,at=(-1+1:nrItems)/(nrItems-1),labels=paste0(round(D,2)),tick=TRUE,las=1,cex.axis=.5,col="grey")
par(mfrow=c(1,1))
if(toPrint) savePlot("simObsIRT.png",type="png")

# average performance per item with indication of discrimination
plot(I,colMeans(O2),cex=D*1.5,pch=19,ylim=c(0,1),xaxt='n',ylab="average performance",xlab="items (easy to hard)",main="simulated performance per item",xaxt='n',yaxt='n',col=2)
points(I,colMeans(O1),col=7)
lines(I,1-plogis(I),col="grey")
axis(1,names(I),at=I,cex.axis=.5,col="grey")
legend("topright",legend=c("2PL","1PL"),pch=c(19,1),col=c(2,7),bty='n')
if(toPrint) savePlot("simMeanObsIRT.png",type="png")

par(tmpPar)




