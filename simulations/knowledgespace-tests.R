



######################################################################

##set this to true to regenerate the model-point layout saved in lpoints
if(exists("resetpoints"))
{
  resetpoints <- resetpoints
}else{
  
  resetpoints <- TRUE
  
}
demo <- 1

if(demo==1)
{
  ## a is a thermostat.
  
  a <- matrix(c(1,1,0,0,0,
                0,1,1,0,0,
                0,0,1,1,0,
                0,1,0,1,1,
                0,0,0,0,1), nrow = 5, byrow = TRUE)
  inp <- c(T,F,F,F,F)
  out <- c(F,F,F,F,T)
  
  
  l <- matrix(c( -1,0,
                 0,0,
                 1,0,
                 .5,1,
                 2,1),ncol=2,byrow=T)
  
  
  
  
  #center manual points:
  l <- t(t(l)-colMeans(l))
  resetpoints <- F
}else if(demo==2) #flat set of predictors
{
  
  a <- matrix(c(1,0,0,0,1,
                0,1,0,0,1,
                0,0,1,0,1,
                0,0,0,1,1,
                0,0,0,0,1), nrow = 5, byrow = TRUE)
  inp <- c(T,T,T,T,F)
  out <- c(F,F,F,F,T)
  
  
  l <- matrix(c( -1,1.5,
                 -1,.5,
                 -1,-.5,
                 -1,-1.5,
                 1,0),ncol=2,byrow=T)
  
  
  
  
  #center manual points:
  l <- t(t(l)-colMeans(l))
  resetpoints <- F
  
  
}

sml <- matrix(0,nrow=nrow(a),ncol=ncol(a))
sml[1,1] <- 1
sml[5,5] <- 1



allpossibles <- F
#here, plot all possible submodels of the input/output nodes.
if(allpossibles)
{
  ##This is too too many to plot
  all <- matrix(T,nrow(a),ncol(a))
  all[,inp] <- 0
  all[out,] <- 0
  diag(all) <- 1  
  a <- all
}


#pdf("testsubsets-c.pdf",width=8,height=10)
par(mfrow=c(5,3),mar=c(0,0,0,0))
x <- allSubsets(a,smallest =sml,input=inp,output=out,plot=F)
#dev.off()



par(mfrow=c(6,3),mar=c(0,0,0,0))
basematrix <- a

subs1 <- x#allSubsets(basematrix,smallest=sml,plot=F)

##add a few submodels outside the veridical model.

bad <- a
bad[2,1] <- 1
bad[1,4] <- 1
bad[5,2] <- 0
diag(bad) <- 1
subs2 <- allSubsets(bad,smallest=sml,plot=F)

#subs <-unique(append(subs1,subs2))
subs <- subs1  
#subs[[length(subs)+1]] <- bad


##now, record whether each of subs is a proper submodel of mastermodel


submodels <- unlist(lapply(subs,function(x){ isSubModel(basematrix,x)}))
numstates <- length(subs)
kstates <- matrix(0,nrow=numstates,ncol=length(subs[[1]]))
precedence <- matrix(0,numstates,numstates)

for(i in 1:numstates)
{
  print(paste(i, "of " , numstates))
  
  kstates[i,] <- as.vector(subs[[i]])
  for(j in 1:numstates)
  {
    
    precedence[i,j] <- precedes(as.vector(subs[[i]]),
                                as.vector(subs[[j]]))
    
  }
}



diag(precedence) <- 0
pedit <- precedence

pedit <- editprec(precedence,0) ##this does not work when we edit out for superset

par(mfrow=c(1,2))
pts <-gplot(pedit)

pos <- cbind(pts[,2],rowSums(kstates));
#gplot(pedit,coord=pos,jitter=F)
#pos <- newpos(pos,pedit,10,byrow=T);gplot(pedit,coord=pos,jitter=F,edge.lty=1,edge.lwd=.1,vertex.cex=2.5,vertex.col="white")

if(0)
{
  vcol <- rgb(49/255,130/255,189/255) #"#3182BD"
  
  #pdf("lattice2.pdf",width=9,height=8)
  
  par(mfrow=c(1,1))
  par(mar=c(8,4,12,0))
  image(1:ncol(kstates),1:nrow(kstates),t(kstates),
        col=c("white",vcol),xaxt="n",yaxt="n",
        xlab="Feature",ylab="Knowledge state",main="Features in belief states")
  segments(0,0:dim(kstates)[1]+.5, 
           dim(kstates)[2]+.5,0:dim(kstates)[1]+.5,col="grey")
  segments(0:dim(kstates)[2]+.5,0,0:dim(kstates)[2]+.5,dim(kstates)[1]+.5,col="grey")
  axis(2,1:15,las=1,cex.axis=.75,col="grey80")
}

pos0 <- pos
pos <- newpos(pos0,pedit,max(rowSums(kstates))+1,byrow=T,reps=20)

#pdf("Masterlattice.pdf",width=8,height=10)
par(mfrow=c(1,1),mar=c(0,0,0,0))
gplot(pedit,coord=pos,jitter=F,edge.lty=1,edge.lwd=.1,vertex.cex=4,vertex.col="gold",
      suppress.axes=T,pad=0,
      main="",ylab="",xlim=range(pos[,1]))
title(main="Lattice of Knowledge States",line=0)
title(ylab="Number of positive beliefs",line=-.5)
axis(2,0:10,las=1,line=-2.5)
#text(pos[,1],pos[,2],1:numstates)
#dev.off()


##create points for plotting:
lpts <- gplot(pedit,coord=pos,jitter=F,edge.lty=1,edge.lwd=.1,vertex.cex=4,vertex.col="white",
              suppress.axes=T,pad=-5,ylim=c(-1,1+max(rowSums(kstates))),
              main="",ylab="",xlim=range(pos[,1]))


#dev.off() 

#plot base matrix to get points

par(bg = 'white')
k <- nrow(basematrix)
if(resetpoints)
{
  
  l <- gplot(basematrix, edge.lwd = 0.8, edge.col = "grey", label = 1:k, 
             label.pos = 5, label.col = "white", label.cex = 1.2,
             vertex.enclose = T, vertex.cex = 1.5, vertex.col = "grey20", vertex.border = "grey20", jitter = F)
  ##nomalize l so it is centered at 0
  
  l <- t(t(l)-colMeans(l))
  
  
}

#pdf("fullmodel.pdf",width=5,height=4)
par(mfrow=c(1,1),mar=c(0,0,0,0))
plotMM(basematrix,basematrix,coords=l,
       input=inp,output=out)
#dev.off()

#combinations <- lapply(0:(2^k - 1), function(networkMatrix) MyIntToBit(networkMatrix,k))

#pdf("lattice.pdf",width=16,height=16,pointsize=30)
par(mar=c(3,3,3,0))

yaxisscale <- 1

scalex <- .2
scaley<- .3

pos2<- pos
pos2[,2] <- pos[,2] * yaxisscale

xlim <- range(c(range(pos2[,1]) + range(l[,1]*scalex), 
                range(pos2[,1]) - range(l[,1]*scalex))) + c(-1,1)


ylim <- range(c(0,range(pos2[,2]) + range(l[,2]*scaley), 
                range(pos2[,2]) - range(l[,2]*scaley))) *1.1 


#plot(0,0,type="n",xlim=xlim,ylim=ylim)

lpts <- gplot(pedit,coord=pos2,jitter=F,edge.lty=1,edge.lwd=.1,
              vertex.cex=2.8,vertex.col="white",
              vertex.border = c("red","darkgreen")[submodels+1],
              vertex.lty = c(3,1)[submodels+1],
              arrowhead.cex=.5, vertices.last=T,
              suppress.axes=T,pad=3,ylim=ylim,new=T,
              main="",ylab="",xlim=xlim)

title(main="Lattice of Knowledge States",line=0)
title(ylab="Number of known relationships",line=-.5)
axis(2,0:max(pos[,2])*yaxisscale,labels=0:max(pos[,2]),las=1,line=-2.5)


#text(pos[,1],pos[,2],1:numstates)


for(i in 1:nrow(lpts))
{
  
  mat <- subs[[i]]
  
  
  #    if((connectedness(as.matrix(networkMatrix[combinations[[i]] == T, combinations[[i]] == T])) == 1) && sum(combinations[[i]]) != 1) {
  #      emptyMatrix[[i-1]] <- matrix(data = NA, nrow = nrow(networkMatrix), ncol = ncol(networkMatrix))
  #      emptyMatrix[[i-1]][combinations[[i]] == T, combinations[[i]] == T] <- networkMatrix[combinations[[i]] == T, combinations[[i]] == T]
  #      emptyMatrix[[i-1]][is.na(emptyMatrix[[i-1]])] <- 0
  
  
  #color <- ifelse(mat == 1, "red", "grey20")
  plotMM(basematrix,mat,l, new=F,color=c("red","darkgreen")[submodels[i]+1],
         centerx= lpts[i,1],centery=lpts[i,2],scalex=scalex,scaley=scaley,
         input=inp,output=out)
  #          par(bg = 'oldlace')
  #          gplot(networkMatrix, coord = l, edge.lwd = 0.8, edge.col = "grey", vertex.cex = 1.5, jitter = F)
  #          gplot(emptyMatrix[[i-1]], coord = l, edge.col= "red", edge.lwd = 3.5, 
  #                new = F, label = 1:nrow(networkMatrix), label.pos = 5, label.col = "white", label.cex = 1.2, 
  #                vertex.enclose = T, vertex.cex = 1.5, vertex.col = color, vertex.border = color, jitter = F)
  
  
  
  
  
}

# dev.off()




##now, can we simulate data from one of the models?


