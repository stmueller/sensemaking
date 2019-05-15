## Knowledge Spaces for Explanation
## (c) 2017-19 Shane T. Mueller (shanem@mtu.edu)
## Software code in the R statistical computing language
## for simulating opinion dynamics using a knowledge-space 
## model.
##

##This must be loaded somewhere before using.
#require("./definitions.R")

##use sna library for visualization
require(sna)

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



#This will return a set of sub-models, without restriction.  But it will only return models that differ by less than distance.
#if distance=nrow(mastermodel) the default, it will return all models.
getSubModels <- function(mastermodel,distance=nrow(mastermodel))
{
   diag(mastermodel) <- 0
  ##consider all submodels based on only deletions.
  tmp <- as.vector(mastermodel)
  toremove <- (1:length(tmp))[tmp==1]  ##these are the bits that can be flipped.
  k <- length(toremove)
  
  sets <- lapply(0:(2^(k) - 1), function(x) MyIntToBit(x,k))
  
  count <- 1
  models <- list()
  for(i in 1:length(sets))
  {
    
    modelbase <- tmp
    filter <-     toremove[sets[[i]]==1]
    if(length(filter)>0)
    {
      modelbase[filter] <- 0
    }
    
    if((sum(tmp) - sum(modelbase))<=distance)
    {
       models[[count]] <- matrix(modelbase,nrow(mastermodel))
       count <- count + 1
    }
    
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
## This relies on allSubModels, but does additional checking,
## and permits a lower-bound submodel.
## It will only return connected submodels that are supersets of smallest.
## It will not return bidirectional link models either (assuming networkMatrix has some)
## In addition,  it will only return submodels
## in which all input nodes reach an output. 
## This prevents pruning of input nodes, so if pruneInputs = T,
## it will permit any sub/superset model that has at least one input node.

allNeighborSubsets <- function(networkMatrix, smallest=NULL,
                       input=rep(F,nrow(networkMatrix)),
                       output=rep(F,nrow(networkMatrix)),  
                       pruneInputs = F,
                       useConnected = F,
                       neighborhood= 1,
                       plot=T) {
  
  if(is.null(smallest))
  {
    smallest <- matrix(0,nrow=nrow(networkMatrix),ncol=ncol(networkMatrix))
  }
  
  k <- nrow(networkMatrix)

  
  
  ##this produces all proper node-subsets:
  ##we do subsets on nodes, not links, because it would take too long for all possible link-based submodels.
 # combinations <- lapply(0:(2^(k) - 1), function(f) MyIntToBit(f,k))
  ##this gives all submodels.
  allsubs <- getSubModels(networkMatrix,distance=neighborhood)
  cat("Number of submodels identified:          ", length(allsubs) , "\n")
  
  #for(i in 1:length(allsubs)){plotMM(networkMatrix, allsubs[[i]],l)}
  ##now, filter out any combinations that are not supersets of smallest.
  propersuper <- lapply(allsubs,function(x){isSubModel(x,smallest)})
  cat("Number of proper supersets of smallest:  ",sum(unlist(propersuper)),"\n")  

 
  
   ##now, iterate through all subsets, and keep only those that are
  ## 1. connected (option), 2. with all inputs leading to outputs, 3. with only mono-directional links.
  counter <- 1
  properSubSuper <- list()
  for(i in 1:length(allsubs)) {

    
      aa <- allsubs[[i]]  

      ##for each combination, check if it is connected.  This will prevent bifurcating a mental model, so that, 
      ##for example, one input leads to one output, and a second input leads to a second output.
     if(useConnected)
          isconnected <- connected(aa)
      else
        isconnected <- TRUE


      
      ##now, see if any of the edges are bidirectional. Remove any of these because their meaning is ambiguous.
      differ  <-   abs(aa-t(aa)) >0
      present<- (abs(aa) +abs(t(aa)))>0
      nobidi <- !any((present & !differ)[upper.tri(present)]) ##this is T if there are no bidirectional
      gd <- is.finite(geodist(allsubs[[i]])$gdist[input,output])
      
      if(pruneInputs)
      {
      ##now, see if all inputs are connected to all outputs.
          sallgood <-all(apply(gd,2,any))  ##each output must be reached by at least one input.
                                           ##Inputs can be abandoned though.
      }else{
          sallgood <- all(gd)  #every output must reach every input
      }
      
      
    if(isconnected && propersuper[[i]] && sallgood && nobidi)
        {
#      print(i)
        ##add this to the master list.
          properSubSuper[[counter]] <- allsubs[[i]]
          counter <- counter + 1
         if(plot == T) {
            #plotMM(networkMatrix, allsubs[[i]],l,input=input,output=output)
            plotMM(networkMatrix, aa,l,input=input,output=output)
           
            }
         
      }
    
  }
  
  
  cat("Connected subsupergraphs:                ",length(properSubSuper),"\n")  
  
  ## Null graphs should have been removed above using the counter trick.
#  properSubSuperx<- properSubSuper[-which(sapply(properSubSuper, is.null))]
#
#  cat("Non-Null Connected subsupergraphs:       ",length(properSubSuper),"\n")  
  
  properSubSuper <- lapply(properSubSuper,function(xx){diag(xx)<-0;xx})
  un <- unique(properSubSuper)
  cat("Unique Connected subsupergraphs:       ",length(un),"\n")  
#  return(allsubs) 
  return(un)
}



