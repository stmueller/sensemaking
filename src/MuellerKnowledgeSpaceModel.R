## Knowledge Spaces for Explanation
## (c) 2017-19 Shane T. Mueller (shanem@mtu.edu)
## Software code in the R statistical computing language
## for simulating opinion dynamics using a knowledge-space 
## model.
##

source("definitions.R")
##use sna library for visualization
library(sna)

setupKnowledgeLattice <- function (numstates,numfeatures,type="spread")
  {
    
    ##generate feature states along a spectrum.

    kl <- matrix(0,ncol=numfeatures,nrow=(numstates-2))

    for(i in 1:(numstates-2))
      {
        p <- getp(type)  
        kl[i,] <- runif(numfeatures)<p
      }
    kl <- unique( rbind(rep(0,numfeatures),kl,rep(1,numfeatures)))

    while(nrow(kl) < numstates)
      { 
        p <- getp(type)
        kl <- unique(rbind(kl,runif(numfeatures)<p))
      }

    ord <- order(rowSums(kl))
    kl[ord,]

  }


getp <- function(type="spread")
{
        if(type=="spread")
          {
            p <- runif(1)

          }else if(type=="polar"){
            p <- .5 + .4 * ((runif(1)<.5)*2-1)
          }else if(type == "centered")
           {
             p <- runif(1)*.2+.4
           }else { 
             p <- getp()
           }
 p
}


##returns T if l1 features are a subset of l2
precedes <- function(l1,l2)
  {
    sum(!(l1&!l2))==length(l1)
  }


##
editprec <- function(mat,crit=1)
  {
    tmp <- mat %*% mat
    mat[tmp > crit] <- 0
    mat
  }

##this takes a precedence graph and gives the delta, instead of just 0/1.
shortestprec <- function(mat)
{
  
  
}


## This makes a new layout of the knowledge lattice, based on adjacency and
## original position.
##
newpos <- function(pos,adj,scale=10,byrow=T,reps=1, respace=T)
  {

    ##make it the mean of nodes connected to it.
    newpos <- pos
    weights <- rep(0,nrow(adj))

    for( k in reps)
    {
    for(i in 1:nrow(adj))
      {
        ##get a list of all nodes that connect TO i
        to <- adj[,i]
        from <- adj[i,]
        tofrom <- to|from

        ##adjust x val to be the average of to and from positions.
#        print(tofrom)
#        print(sum(tofrom))
        if(sum(tofrom)>1)
          {
            newpos[i,1] <- mean(pos[tofrom,][,1])
          }
        weights[i] <- sum(tofrom)+1
      }
}

    ##

    
    
    if(byrow)
      {
        x <- data.frame(newpos,weights=weights)
        

        
        np.min <- aggregate(newpos[,1],list(newpos[,2]),min)$x
        np.max <- aggregate(newpos[,1],list(newpos[,2]),max)$x
#        np.mean <- aggregate(newpos[,1],list(newpos[,2]),mean)$x
        np.mean <- by(x,x$X2,function(g){weighted.mean(g$X1,g$weights)})
        ids <- 1:nrow(adj)
        ##
        np.range <- np.max-np.min
        np.range[np.range==0] <- 1  ##rescore 0 range to be 1;
        ##we divide by this and it shouldn't be undefined.

        ##this scales for each row
        ii <- 1
        for(i in levels(as.factor(pos[,2])))
          {
            tmp <- ids[as.character(pos[,2])==i]
            num <- length(tmp)
            ##scale to fit in -scale..scale
            range <- np.range[ii]
            newpos[,1][tmp] <- (newpos[,1][tmp] - np.mean[ii])/range * scale+np.mean[ii]
            ii <- ii+1
          }
      }else {
        if(1){

          min <- min(newpos[,1])
          max <- max(newpos[,1])
          mean <- mean(newpos[,1])
          range <- max-min
          
          newpos[,1] <- (newpos[,1]-mean)/range*scale
          
        }else{
          ##do nothing
        }
      }

    
    
    
    
    ##this respaces each row.
    if(respace)
    {
      ##this is the overall range of things.
      maxrange <- range(newpos[,1])
      ids <- 1:nrow(pos)
      
      for(i in levels(as.factor(pos[,2])))
      {
        tmp <- ids[as.character(pos[,2])==i]
        num <- length(tmp)
        ##scale to fit in -scale..scale
      
        xs <- newpos[tmp,1]  
        
        newxs <- ((rank(xs))/(num+1))*(maxrange[2]-maxrange[1]) + min(maxrange) 
        newpos[tmp,1]<- newxs
      }
      
      
      
    }
    
    
    
    newpos
  }


##
setup <- function(knowledge,n)
{
  ##we won't allow ineligible states, so just
  ##keep track of knowledge states

  #beliefs <<- sample(1:numstates,n,replace=T)

  ##set up two generally extreme groups
  num <- nrow(knowledge)
#  prob <- ((1:num)^2 + (num:1)^2)
#  prob <- prob/sum(prob)
#  print(prob)
  ##Sample uniformly across the states
 beliefs<<-sample(1:num,n,replace=T)
# beliefs<<-sample(c(1,num),n,replace=T,prob=prob)
#  beliefs<<-c(1:num,sample(c(1,num),n-num,replace=T))
#  beliefs<<-c(rep(1,50),rep(15,50))

}

##note that reps is 10 for each n.
##keep =T restarts simulation.

run <- function(n=1000,keep=F)
  {

    if(keep)
      {
        dat <- matrix(0,numagents,(n+1))
        dat[,1] <- beliefs
      }
    reps<- 10
    for(i in 1:n+1)
      {
        for(j in 1:reps)
          update(kl)

        if(keep)
          {
            dat[,i] <- beliefs
          }
      }
#    matplot(0:n,t(dat),type="p",pch=".",col=grey(thresh))
    if(!keep)
      {dat <- beliefs}
    dat
  }



update <- function(kl)
  {

    ##choose two
    pair <- sample.int(numagents,2)

    ##pick out their states in the knowledge lattice:
    kID <- beliefs[pair]


    ## get their belief vectors
    kpair <- kl[kID,]


    
    ## create candidate new belief vectors by
    ## changing each belief.
    changeab <- runif(numfeatures)<mu
    changeba <- runif(numfeatures)< mu
#    changeba <- changeab
    newa <- kpair[1,] * changeab + kpair[2,] * (1-changeab)
    newb <- kpair[1,] * (1-changeba) + kpair[2,] * (changeba)

    xa  <- isvalid(newa,kl)
    
    if(xa>0)
      {
        
        beliefs[pair[1]] <<- xa
      }

    xb  <- isvalid(newb,kl)
    if(xb>0)
      {

        beliefs[pair[2]] <<- xb
      }

  }

isvalid  <- function(belief, states)
  {
    as.numeric(apply(states,1,function(x){(sum(x==belief))==numfeatures})%*% 1:numstates)

  }


connected <- function(networkMatrix) {
  
  ## first, let's remove any values for which both rows and columns are blank.
  keep <- (rowSums(networkMatrix) >0) |(  colSums(networkMatrix)>0)
  networkMatrix <- networkMatrix[keep,keep]
  count <- 0

  if(length(networkMatrix)==0)
  {
    return(TRUE)  
  }

  return(connectedness(networkMatrix) == 1)
}



MyIntToBit <- function(x, dig) {
  i <- 0L
  string <- numeric(dig)
  while (x > 0) {
    string[dig - i] <- x %% 2L
    x <- x %/% 2L
    i <- i + 1L
  }
  string
}




allSubModels <- function(mastermodel)
{
   diag(mastermodel) <- 0
  ##consider all submodels based on only deletions.
  tmp <- as.vector(mastermodel)
  toremove <- (1:length(tmp))[tmp==1]  ##these are the bits that can be flipped.
  k <- length(toremove)
  sets <- lapply(0:(2^(k) - 1), function(x) MyIntToBit(x,k))
  
  
  models <- list()
  for(i in 1:length(sets))
  {
    
    modelbase <- tmp
    filter <-     toremove[sets[[i]]==1]
    if(length(filter)>0)
    {
      modelbase[filter] <- 0
    }
    models[[i]] <- matrix(modelbase,nrow(mastermodel))
    
    
  }
  models
}


## This tests whether matrix is a proper superset of submatrix,
## or alternately is submatrix is a proper subset model.

isSubModel <- function(mat,submat)
{
  diag(mat) <- ((rowSums(mat) + colSums(mat)) >0)+0
  diag(submat) <- ((rowSums(submat) + colSums(submat)) >0)+0
  
  precedes(as.vector(submat),
           as.vector(mat)) 

}


## This finds all submodels of networkmatrix.
##
allSubsets <- function(networkMatrix, smallest=NULL,
                       input=rep(F,nrow(networkMatrix)),
                        output=rep(F,nrow(networkMatrix)),  plot=T) {
  
  if(is.null(smallest))
  {
    smallest <- matrix(0,nrow=nrow(networkMatrix),ncol=ncol(networkMatrix))
  }
  
  k <- nrow(networkMatrix)

  
  
  ##this produces all proper node-subsets:
  ##we do subsets on nodes, not links, because it would take too long for all possible link-based submodels.
 # combinations <- lapply(0:(2^(k) - 1), function(f) MyIntToBit(f,k))
  ##this gives all submodels.
  allsubs <- allSubModels(networkMatrix)
  cat("Number of submodels identified:          ", length(allsubs) , "\n")
  
  #for(i in 1:length(allsubs)){plotMM(networkMatrix, allsubs[[i]],l)}
  ##now, filter out any combinations that are not supersets of smallest.
  propersuper <- lapply(allsubs,function(x){isSubModel(x,smallest)})
  cat("Number of proper supersets of smallest:  ",sum(unlist(propersuper)),"\n")  

  par(bg = 'white')
  l <- gplot(networkMatrix, edge.lwd = 0.8, edge.col = "grey", label = 1:k, 
            label.pos = 5, label.col = "white", label.cex = 1.2,
             vertex.enclose = T, vertex.cex = 1.5, vertex.col = "grey20", 
            vertex.border = "grey20", jitter = F)
  properSubSuper <- list()
  
  for(i in 1:length(allsubs)) {
    
    
    
       ##for each combination, check if it is connected.
      connected <- connected(allsubs[[i]])
      
      aa <- allsubs[[i]]
      ##now, see if any of the edges are bidirectional.
      differ  <-   abs(aa-t(aa)) >0
      present<- (abs(aa) +abs(t(aa)))>0
      nobidi <- !any((present & !differ)[upper.tri(present)]) ##this is T if there are no bidirectional
      
      
      ##now, see if all inputs are connected to all outputs.
      gd <- is.finite(geodist(allsubs[[i]])$gdist[input,output])
#     gd <- T
    if(connected && propersuper[[i]] && all(gd) && nobidi)
        {
        ##add this to the master list.
          properSubSuper[[i]] <- allsubs[[i]]
        
         if(plot == T) {
            plotMM(networkMatrix, allsubs[[i]],l,input=input,output=output)
         }
         
      }
    
  }
  
  
  cat("Connected subsupergraphs:                ",length(properSubSuper),"\n")  
  
  properSubSuper<- properSubSuper[-which(sapply(properSubSuper, is.null))]
  
  cat("Non-Null Connected subsupergraphs:       ",length(properSubSuper),"\n")  
  
  properSubSuper <- lapply(properSubSuper,function(xx){diag(xx)<-0;xx})
  un <- unique(properSubSuper)
  cat("Unique Connected subsupergraphs:       ",length(un),"\n")  
#  return(allsubs) 
  return(properSubSuper)
}





#################################################
## This is a smiple visualization set-up
#################################################


#set.seed(105)
numstates   <<- 15
numfeatures <<- 20
numagents <<-  10
mu      <<-  .3  ##convergence parameter


##set up knowledge lattice so it is distributed
kl <<- setupKnowledgeLattice(numstates,numfeatures)
##set it up so it is polarized
#kl <<- setupKnowledgeLattice(numstates,numfeatures,type="polar")

precedence <- matrix(0,numstates,numstates)

for(i in 1:numstates)
  for(j in 1:numstates)
  {
    precedence[i,j] <- precedes(kl[i,],kl[j,])
  }

pedit <- editprec(precedence)
pedit <- editprec(pedit)
par(mfrow=c(1,2))
pts <-gplot(precedence)
#gplot(pedit,coord=pts)
#par(mfrow=c(1,1))
pos <- cbind(pts[,2],rowSums(kl));gplot(pedit,coord=pos,jitter=F)
#pos <- newpos(pos,pedit,10,byrow=T);gplot(pedit,coord=pos,jitter=F,edge.lty=1,edge.lwd=.1,vertex.cex=2.5,vertex.col="white")

vcol <- rgb(49/255,130/255,189/255) #"#3182BD"

pdf("lattice1.pdf",width=9,height=8)

par(mfrow=c(1,2))
par(mar=c(8,4,12,0))
image(1:ncol(kl),1:nrow(kl),t(kl),col=c("white",vcol),xaxt="n",yaxt="n",
      xlab="Feature",ylab="Knowledge state",main="Features in belief states")
segments(0,0:15+.5,25.5,0:15+.5,col="grey")
segments(0:25+.5,0,0:25+.5,15.5,col="grey")
axis(2,1:15,las=1,cex.axis=.75,col="grey80")


par(mar=c(0,5,3,0))
pos <- newpos(pos,pedit,10,byrow=T,respace=T)

gplot(pedit,coord=pos,jitter=F,edge.lty=1,edge.lwd=.1,vertex.cex=3,vertex.col=vcol,
      suppress.axes=T,pad=-2,
      main="",ylab="")
title(main="Lattice of Knowledge States",line=0)
title(ylab="Number of positive beliefs",line=-.5)
axis(2,0:20,las=1,line=-2.5)
text(pos[,1],pos[,2],1:numstates)


dev.off() 



 
######################################################################

##set this to true to regenerate the model-point layout saved in lpoints
if(exists("resetpoints"))
{
  resetpoints <- resetpoints
}else{
  
  resetpoints <- TRUE
  
}
demo <- 2

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


pdf("testsubsets-c.pdf",width=8,height=10)
par(mfrow=c(5,3),mar=c(0,0,0,0))
x <- allSubsets(a,smallest =sml,input=inp,output=out,plot=F)
dev.off()



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

pdf("Masterlattice.pdf",width=8,height=10)
par(mfrow=c(1,1),mar=c(0,0,0,0))
gplot(pedit,coord=pos,jitter=F,edge.lty=1,edge.lwd=.1,vertex.cex=4,vertex.col="gold",
              suppress.axes=T,pad=0,
              main="",ylab="",xlim=range(pos[,1]))
title(main="Lattice of Knowledge States",line=0)
title(ylab="Number of positive beliefs",line=-.5)
axis(2,0:10,las=1,line=-2.5)
#text(pos[,1],pos[,2],1:numstates)
dev.off()


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

pdf("fullmodel.pdf",width=5,height=4)
par(mfrow=c(1,1),mar=c(0,0,0,0))
plotMM(basematrix,basematrix,coords=l,
       input=inp,output=out)
dev.off()

#combinations <- lapply(0:(2^k - 1), function(networkMatrix) MyIntToBit(networkMatrix,k))

pdf("lattice.pdf",width=16,height=16,pointsize=30)
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
  
 dev.off()
 
 
 
 
 ##now, can we simulate data from one of the models?
 
 
