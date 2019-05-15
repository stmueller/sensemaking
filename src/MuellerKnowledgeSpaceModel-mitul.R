## Knowledge Spaces for Explanation
## (c) 2017 Shane T. Mueller (shanem@mtu.edu)
## Software code in the R statistical computing language
## for simulating opinion dynamics using a knowledge-space 
## model.
##


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

editprec <- function(mat)
  {
    tmp <- mat %*% mat
    mat[tmp > 2] <- 0
    mat
  }



newpos <- function(pos,adj,scale=10,byrow=T)
  {

    ##make it the mean of nodes connected to it.
    newpos <- pos
    weights <- rep(0,nrow(adj))

    for(i in 1:nrow(adj))
      {
        ##get a list of all nodes that connect TO i
        to <- adj[,i]
        from <- adj[i,]
        tofrom <- to|from

        ##adjust x val to be the average of to and from positions.
        print(tofrom)
        print(sum(tofrom))
        if(sum(tofrom)>1)
          {
            newpos[i,1] <- mean(pos[tofrom,][,1])
          }
        weights[i] <- sum(tofrom)+1
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
          ##do nuttin.
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


#################################################
## This is a smiple visualization set-up
#################################################


set.seed(105)
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
pos <- newpos(pos,pedit,10,byrow=F)

gplot(pedit,coord=pos,jitter=F,edge.lty=1,edge.lwd=.1,vertex.cex=3,vertex.col=vcol,
      suppress.axes=T,pad=-2,
      main="",ylab="")
title(main="Lattice of Knowledge States",line=0)
title(ylab="Number of positive beliefs",line=-.5)
axis(2,0:20,las=1,line=-2.5)
text(pos[,1],pos[,2],1:numstates)


dev.off() 



connected <- function(networkMatrix) {
  count <- 0
  for(i in 1:nrow(networkMatrix)) {
    if((1 %in% networkMatrix[i,] | 1 %in% networkMatrix[,i]) == FALSE) {
      count <- count + 1
    } else {
      count <- count
    }
  }
  
  if(count == 0) {
    return(1) 
  } else {
    return(0)
  }
}




a <- matrix(c(0,0,0,0,0,
              0,0,1,0,0,
              1,0,0,1,0,
              0,1,0,0,1,
              0,1,0,0,0), nrow = 5, byrow = TRUE)


b <- matrix(c(0,0,0,0,0,0,0,
              1,0,0,0,0,0,1,
              0,1,0,0,1,0,0,
              0,0,1,0,0,1,0,
              0,0,0,1,0,0,0,
              0,0,0,0,1,0,0,
              0,0,0,0,0,0,0), nrow = 7, byrow = TRUE)

c <- matrix(c(0,0,0,0,0,0,0,1,
              1,0,0,0,0,0,1,0,
              0,1,0,0,1,0,0,0,
              0,0,1,0,0,1,0,1,
              0,0,0,1,0,0,0,0,
              0,0,0,0,1,0,0,0,
              0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0), nrow = 8, byrow = TRUE)




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



allSubsets <- function(networkMatrix, plot=T) {
  
  k <- nrow(networkMatrix)
  emptyMatrix <- list()
  combinations <- lapply(0:(2^k - 1), function(networkMatrix) MyIntToBit(networkMatrix,k))
  
  par(bg = 'oldlace')
  l <- gplot(networkMatrix, edge.lwd = 0.8, edge.col = "grey", label = 1:k, 
             label.pos = 5, label.col = "white", label.cex = 1.2,
             vertex.enclose = T, vertex.cex = 1.5, vertex.col = "grey20", vertex.border = "grey20", jitter = F)
  
  
  
  for(i in 2:length(combinations)) {
    
      if((connectedness(as.matrix(networkMatrix[combinations[[i]] == T, combinations[[i]] == T])) == 1) && sum(combinations[[i]]) != 1) {
        emptyMatrix[[i-1]] <- matrix(data = NA, nrow = nrow(networkMatrix), ncol = ncol(networkMatrix))
        emptyMatrix[[i-1]][combinations[[i]] == T, combinations[[i]] == T] <- networkMatrix[combinations[[i]] == T, combinations[[i]] == T]
        emptyMatrix[[i-1]][is.na(emptyMatrix[[i-1]])] <- 0
        color <- ifelse(combinations[[i]] == 1, "red", "grey20")
        
        if(plot == T) {
          
          par(bg = 'oldlace')
          gplot(networkMatrix, coord = l, edge.lwd = 0.8, edge.col = "grey", vertex.cex = 1.5, jitter = F)
          gplot(emptyMatrix[[i-1]], coord = l, edge.col= "red", edge.lwd = 3.5, 
                new = F, label = 1:nrow(networkMatrix), label.pos = 5, label.col = "white", label.cex = 1.2, 
                vertex.enclose = T, vertex.cex = 1.5, vertex.col = color, vertex.border = color, jitter = F)
          
        }
         
        
      }
    
  }
  emptyMatrix <- emptyMatrix[-which(sapply(emptyMatrix, is.null))]
  return(emptyMatrix)

}

