

##This is a simulation exploration of the NILM sensemaking model.
library(ggplot2)
library(sna)
library(RColorBrewer)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



## Each row represeents a linear model
##
logit <- function(x){1/(1+exp(-x))}
logodds <- function(p){log(p/(1-p))}


##this simulates a static evaluation/prediction process.
## eweights is the weight from a particular exemplar model
## vals is the set of data, with the to-be-predicted node NA'ed
## iter is the number of steps to simulate; this is for dynamic systems.
##visibleNodes is specified because we want to use the given values, 
## not those predicted by internal model (if the model makes predictions about input)
## noise is the amount of noise to add to each value.
simulateStatic <- function(eweights, vals, iter=1,
                           visibleNodes, noise=rep(0,length(vals))
                           )
{

  n <- length(vals)
  out <- matrix(vals,nrow=iter,ncol=n,byrow=T)
  tmpvals <- out[1,]
  tmpvals[is.na(tmpvals)] <- 0
  if(iter>=0)
  {
    for(i in 1:iter)
     {
    
      tmpvals <- unlist(tmpvals) ##this may end up being a list or some reason.

      tmpvals <- (as.vector(t(eweights) %*% tmpvals) + rnorm(n) * noise)
      tmpvals[visibleNodes] <- vals[visibleNodes] #reset the input values to use  the visible values
      out[i,] <-  tmpvals
    }
  }
  return(out[iter,,drop=F])
}

##This simulates the results of the network given input values and
##weights.  It will use vals to compute the outcome as a starting condition, so if
##you use 1 iteration, that will produce the best estimate to truth if actual parameters
##are known.
simulateEnsemble <- function(weights, vals, 
                             iter=length(vals)*2, #be sure a long chain could quiesce
                             inputnodes,
                             noise=rep(0,length(vals)) )
{
  exemplars <- dim(weights)[3]
  
  ##each time ,use a different [,,i] version of weights.
  out <- matrix(0,ncol=exemplars,nrow=length(vals))
  
  for(i in 1:exemplars)
  {

    vals2 <- runif(length(vals)) #start out with random non-null values.
    vals3 <- vals
    
    vals3[is.na(vals)] <- vals2[is.na(vals)]  #copy in random values when originals were missing 
     
 
    ##simulatestatic requires one 2-d matrix
      retval <- simulateStatic(weights[,,i],
                              vals3,iter,
                              visibleNodes=inputnodes,
                              noise)
    


      out[,i] <- unlist(retval)  ##this is needed when you have multiple output nodes???
  }
  
  ##here, each row of out is a value, each column is a model exemplar.
  out
}


## This uses delta-rule learning to adjust weights to make a better prediction about
## the output.
learnWeights <- function(weights, vals, input,output, observed)
{
  ##first, we make a prediction about the output nodes given the model assumptions.
   prediction <- simulateEnsemble(weights,vals,iter=1,inputnodes=input)
   
}




## THis will create a connected network with unidirectional
## connections, where every input leads to an output and every output
## can be reached from an input.
## outputs.
##

generateRandomMentalModel <-function(size,input,output,plot=F)
{  
    base <- matrix(0,size,size)
    ##include base nodes:
    
    diag(base) <- input | output
    
    ##This is the largest possible network, and is not valid because 
    ##it is doubly-connected:
    
    
    all <- matrix(T,size,size)
    all[,input] <- 0
    all[output,] <- 0
    diag(all) <- 1  
    
    coords <- gplot.layout.fruchtermanreingold(all,layout.par=list()) 
    
    tries <- 1  
    good <- F
    while(!good)
    {
      test <- base #make a test matrix to manipulate.
      ##pick a new edge
      newedge <- sample(1:size,size=2)  ##sample a random set.
      i <- newedge[1]
      j <- newedge[2] 
      if(!(test[i,j]==1 ))   ##turn off
         {
           test[i,j] <- 0
           test[j,i] <- all[j,i]  #turn on the other way if possible.
         } else 
         ##only add if we have no elements of the symmetry.
        {
         test[i,j] <- all[i,j] ## set the new link to whatever it is in the master.
        }
#      test[i,i] <- all[i,i]
#      test[j,j] <- all[j,j]
      

      if(plot)
        plotMM(test,test,input,output,coords)
      #for each combination, check if it is connected.
      conn <- connected(test)
         
      
      
      good <- conn & inOutConnected(test,input,output)[1]

      
      tries <- tries + 1
      if(tries > 100)
      {
        #return( GenerateRandomNetwork(size,input,output))
        cat("Failed. Could not generate valid network.")
        return(base)
      }
      base <- test
    }
    
    return (base)
}

inOutConnected <- function(net,inp,out)
{
  gd <- geodist(net,inf.replace=F)$gdist
  
  ##test if each input connects to an output
  goodInpAll <- T
  for(i in  (1:length(inp))[inp])
    {
        goodinp <-F
        for(j in  (1:length(out))[out])
        {
          goodinp <- goodinp | gd[i,j]
        }
        goodInpAll <- goodInpAll & goodinp
  }  

  goodOutAll <- T
  for(j in  (1:length(out))[out])
  
    {
    goodout <-F

    for(i in  (1:length(inp))[inp])
        {
      goodout <- goodout | gd[i,j]
    }
    goodOutAll <- goodOutAll & goodout
  }  
  return(c(goodInpAll & goodOutAll,goodInpAll,goodOutAll))
}


##this takes a bitvector and return indices
##possibly multiple) of the T values in the vector.
mysamp <- function(values,n=length(values))
{
   vals <-(1:length(values))[values]
   n <- min(n,length(vals))
  newvals <- vals[order(runif(length(vals)))]
  newvals[1:n]
}



## When an ensemble has made a set of predictions about a data set output node, 
## this can calculate the delta.
getPredictionDelta <- function(data,predicteddata,outputnode)
{
  
  numpreds <- dim(predicteddata)[3]
  truths <- matrix(data[,outputnode],ncol=numpreds,nrow=nrow(data))
  
  predictions <- predicteddata[,outputnode,]
  
  delta <- predictions-truths
  
  ##now, we have the prediction error for each examplar model on each data case.
  ##return this, and you might find grand sd, or mean of individual sds or something.
  return(delta)
}


## This performs N learning steps for an output node on a given data set.

## On one hand, it serves as the inner learning loop.
## This can be used 'realtime', or can be used to test hypotheticals, 
## important here is the learnweights variable, which restricts delta-rule learning to
## just those connections.  allows you to 'freeze' your current model and 
## test what would happen if ,ou added another node, quickly learning its weight based
## on stm.


#Returns:  This returns a list with named objects:
## hweights:           the updated weights
## stm:                updated STM matrix
## learningerror:      record ofhe how far off each model was on each step
## predictions:        The model's prediction of the non-input values.  That is, for each 
##                     data row, it places in predictions its predicted value,  iteratively replacing anything it predicts as
##                     it backtracks.  Non-predicted values remain NULL
## data:               The actual data observed during the learning sequence.


##steps is an optional argument. If missing, it will use nrow(data), which means it will
##use every row of data, in order, once. If longer, it will recycle.

pureLearn <- function(iweights,frame,alpha,
                      exemplarsToUpdate, numExemplars,
                      outputnodes, inputnodes,
                      
                      steps=nrow(data),
                      data,
                      stm,
                 
                      maxback=nrow(iweights),
                      sample=F,
                      printme = FALSE,
                      fixed=NULL)
{
  ##the fixed variable specifies links that don't get updated at all. They will use their initial values.
  if(is.null(fixed))
  {
    fixed <- matrix(F,nrow=nrow(frame),ncol=ncol(frame))
  }
  
  numnodes <- length(inputnodes)
  stmSize <- nrow(stm)
  numExemplars <- dim(iweights)[[3]]
  observed <- stm[1,] ##placeholder; records the 'true' data observed on a cycle.
  observed[outputnodes] <- NA
  
  output.lerror <- array(NA, dim=list(numExemplars,numnodes,steps))
  output.pdata <- array(NA,dim=list(numExemplars,numnodes,steps))
 
  ##This either samples the data block randomly or repeats it until it is big enough to match the steps.
  ##However, data may have NAs. This could happen if stm is used to forecast data but the stepsize is small.
  ##So, we should only use the rows that are NOT NA
  
  notNA <- which(!is.na(rowSums(data)))
  
  if(sample)
  { ##randomly sample the data:
    sampling <- sample(notNA,steps,replace=T)
  
   }else{
    ##repeat and reuse the data:
     sampling <- rep(notNA,length.out=steps)
   }
  
  
  output.data <- data[sampling,]
  for(i in 1:steps)##go through this many examples of data.
  {
    exemplars <- sample(numExemplars)[1:exemplarsToUpdate]  ##pick, e.g., five at a time:


    truth <- as.matrix(data[sampling[i],])  #what is really the case
    stm[(i-1)%%stmSize+1,] <- truth[,1]  ##populate a short-term memory history. 

    
    observed <- truth  ##what the observed truth is.
    observed[outputnodes]<- NA  ##zero out the output nodes, so that we are not using that.
    
    ##this is predicting based ONLY on the input nodes.
    
    iter <- 1
    ##pick which node to use (of the output node(s)). Select only one per step.
    node <- mysamp(outputnodes,1)
    
    predictions <- matrix(NA,nrow=length(observed),ncol=numExemplars)
    ##now, we have made a set of predictions for 
    ##pick an output node and back-propogate error.
    while(iter < maxback)
    {
      
     if(printme) cat("Node:",node ,"| ")
     filter <-  !((1:length(outputnodes)) == node ) &!outputnodes & frame[,node]
     ##only go through all this if the frame has available nodes to add.
     inp <- frame[,node]
     
     if(sum(abs(inp))>0)
     {
       
     ##make a prediction of each missing value, for each model.
     ## we will make a prediction for ALL elements of the ensemble, so we can 
     ##more easily track error across the ensemble.  But we only update the ensemble. (wts filters this)
  
     curpred <- simulateEnsemble(weights = iweights,
                                 
                                  vals=observed,iter=1,
                                  inputnodes=filter, ##input
                                  noise=rep(0.000,length(filter)))  

    
    ##this updates predictions for just the current node
    predictions[node,] <- curpred[node,]
    output.pdata[,,i] <- t(predictions)
    

     
    delta <- (as.matrix(truth)[,rep(1,ncol(predictions))])  - predictions
  #  print(cat("Max: " , max(delta)))

        ##for each model, get the error for this node's prediction:
    loss <- t(delta[node,keep=T])
    ##^^^^^this is the amount we were off, for each model.
  
    ##Calculate the input weights of the exemplar models
    wts <- t(iweights[,node,exemplars])   ##filter weights by frame.
    #^^^^  these are the original weights we need to update.
    
    
    
    ## truth may have NAs in it--for nodes we know nothing about or those
    ##that are placeholders.  These NAs propagate to change, wts, and iweights
    ##and could screw things up.
    na.filt <- !is.na(truth)
    
    ##change is how much each weight needs to be adjusted.
    
    
    change <- t(alpha * (truth) %*% (loss[1,exemplars,drop=F]) * t(abs(sign(wts))))
    change[is.na(change)] <- 0  ##don't adjust any NA values.
    
    change[fixed[,node]] <- 0         ##Don't adjust any fixed weights either.
    wts[,na.filt] <- wts[,na.filt] + change[,na.filt]
    
    
    iweights[,node,exemplars] <- t(wts)

    output.lerror[,node,i] <- (loss)
    }else{
      
      if(printme) cat ("Maximum previous node\n")
      iter <- maxback
    }  
   
     ##now, move backward to one of the incoming nodes
     node <- mysamp(inp>0,1)
     iter <- iter + 1
    
    }

    
      
  }

  return (list(hweights=iweights,stm=stm,
               learningerror=output.lerror,
               predictions=output.pdata,
               data=output.data))
}

## this examines the 'output' values, predicted by the 'input' values, 
## for a given fuzzy mental model.  It examines the ensemble of weights
## against a specific data set. This data might not be the entire training set, 
## but rather a recent subset (e.g., short-term memory).

evaluateMentalModel <- function(data,hweights,meanerror,
                                observed,
                                input,output,
                                thresh = .025,  ##threshold for suggesting removal.
                                iter=1,noise=rep(0.001,nrow(hweights[,,1])),
                                plot=T
                              )
{
  #now, let's make a prediction about the data from the model, using only inputs.
  
  numExemplars <- dim(hweights)[[3]]
  sets <- dim(hweights)[3]
  keep <- matrix(NA, nrow=nrow(data),ncol=sum(output))
  consistent <- rep(NA,nrow(data))
  
  pdata <- array(0,dim=list(nrow(data),ncol(data),sets))
  for(i in 1:nrow(data)) 
  {
    
    observed <- data[i,]
    truth <- observed
    observed[!input] <- NA

    ##make a prediction of each missing value
    pdata[i,,] <- simulateEnsemble(hweights,
                                      observed,iter=iter,
                                      inputnodes=input,
                                      noise=noise  )
    
    
   
    inside <-  truth[output] >= min((pdata[i,,]-t(meanerror))[output,] ) &
          truth[output] <=    max((pdata[i,,]+t(meanerror))[output,] ) 
    

    ##for each observation, keep will be true if the true value is inside the range of simulated ensemble values.
    ## keep is recorded for each observation in the data set.
    ##
    keep[i,] <- inside
    
    consistent[i] <- (max(pdata[i,output,])-min(pdata[i,output,])) < ( 4*mean(meanerror[,output]))
    
    
  }
  
  
  ##let's compute the relative consistency of predictions.
  convergencecoefficient <- mean(apply(pdata[,output,],1,sd))/sd(data[,output]) ##average sd of the estimates / total sd 
  
  
  ##we can see if any links should be removed by examining the z-score of the variables.
  ##We can't really tell by z-score alone if values have converged--this needs to be examined
  ##in the prediction space, but z-score gives us a reasonable order to examine the variables in:
  
  means <- matrix(0,dim(hweights)[1],dim(hweights)[2])
  sds <-   matrix(0,dim(hweights)[1],dim(hweights)[2])
                     
  for(i in 1:dim(hweights)[1])
   {
    means[i,] <- apply(hweights[i,,],1,mean)
    sds[i,] <- apply(hweights[i,,],1,sd)
   }
  
  
  ##this remove stat is how we determine whether to cull a predictor from the model.
  
  remove <- keepPredictor(hweights,data,output,thresh=thresh)
  removeMat = matrix(F,nrow=nrow(hweights),
                       ncol=ncol(hweights))
  removeMat[remove,output] <- TRUE

  #   if(0)
#   {
#   
#   ##use this to judge whether a node might be removed.
#   filt <- sds>0  #pick out just the features with non-zero variance (the ones under consideration)
# #  cross.sd <-  sd(means[filt])  ##this is the variance _across_ parameters.
#   testorder <- order((abs(means/sds)[,output]))
#   remove <- rep(F,length(output))
#   deltas <- getPredictionDelta(data,pdata,output)
#   ##now, we want to test the features in order of the ones most likely to be removed.
# 
#   for(feature in testorder)
#    {
#     pdata2 <- array(0,dim=list(nrow(data),ncol(data),sets))
#     
#      ##create a new hypothetical weighting set, zeroing out one feature
#      hweights2 <- hweights
#      hweights2[feature,output,] <- 0
#      
#      for(i in 1:nrow(data)) 
#      {
#        
#        observed <- data[i,]
#        truth <- observed
#        observed[!input] <- NA
#        
#        pdata2[i,,] <- simulateEnsemble(hweights2,
#                                     observed,iter=iter,
#                                     inputnodes=input,
#                                     noise=noise  )
#      
#      }
#      deltas2 <- getPredictionDelta(data,pdata2,output)
#      print(paste("feature:",feature, sd(deltas), sd(deltas2) ))
#      print(paste("feature:",feature, mean(apply(deltas,1,sd)), mean(apply(deltas2,1,sd))))
#      
#     if(sd(deltas2)/sd(deltas)<.98)
#      {
#        remove[feature] <- TRUE
#      }
#   }
#   }
  if(plot)
  {
    data1 <- data.frame(values=rep(as.vector(data),dim(pdata)[3]),
                        simvalues = as.vector(pdata),
                        nodes=factor(rep(rep(1:(ncol(data)),each=nrow(data)),dim(pdata)[3])),
                        exemplar=rep(1:dim(pdata)[3],each=(dim(pdata)[2] * dim(pdata)[1]))
    )
    cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    cbPalette[(1:length(output))[output]] <- "gold"
    
    x <- ggplot(data1,aes(x=values,y=simvalues, colour=nodes)) + 
      geom_abline(slope=1,intercept=0,colour="grey",size=.5)+
      geom_point(size=.2) + 
      scale_colour_manual(values=cbPalette)+
    facet_grid(nodes~exemplar,scales="free")  + theme_bw()
    print(x)
    
  }
  return(list(pdata=pdata,
              keep = keep,
              convergence=convergencecoefficient,
              consistent=consistent,
              removeVar=removeMat,
              mean=means,
              sd =sds ))
}



## This function just looks at the parameter estimates of the mental model
## and determines any relative outliers.  It does not look at predictions,
## Just thetail of the mental model.
evaluateEnsemble <- function(hweights,
                                input,output)
{
  
  
  variances <- apply(hweights[input,output,],2,var)
  sses <-  variances/sum(variances)
  
  
  #now, let's make a prediction about the data from the model, using only inputs.
  
  numExemplars <- dim(hweights)[[3]]
  
  
  ##we can see if any links should be removed by examining the z-score of the variables.
  ##We can't really tell by z-score alone if values have converged--this needs to be examined
  ##in the prediction space, but z-score gives us a reasonable order to examine the variables in:
  
  means <- matrix(0,dim(hweights)[1],dim(hweights)[2])
  sds <-   matrix(0,dim(hweights)[1],dim(hweights)[2])
  
  for(i in 1:dim(hweights)[1])
  {
    means[i,] <- apply(hweights[i,,],1,mean)
    sds[i,] <- apply(hweights[i,,],1,sd)
  }
  
  
  ##This calculates a z-score for each member of the ensemble.  It should be able to identify the 
  ##worst model.
  ensembleZ <- abs((hweights - array(means,c(dim(hweights)[1],dim(hweights)[2],numExemplars))))/
                     array(sds,c(dim(hweights)[1],dim(hweights)[2],numExemplars))
  
  meanZ <- colMeans(ensembleZ[input,output,])
  
  
    return(list(mean=means,
              sd =sds,
              sse=sses,
              meanEnsembleZ = meanZ ))
}


##this evaluates a model against data and judges whether
## coefficients should be removed on the basis of the 
## specified 'output' cells. Those output may be 
## intermediate during backpropogation.

keepPredictor <- function(hweights,data,output,thresh=.025)
{
  
  outcomesd <- sd(data[,output],na.rm=T)
  predictorsd <- apply(data,2,function(x){sd(x,na.rm=T)})
  
  ##this gives standardized coefficients for the ensemble.
  standardized <-hweights[,output,]*predictorsd/outcomesd
  
  ## matplot(standardized)
  #suggest a variable for removal if its maximum standardized value is less than .025 
  maxcoef <- apply(abs(standardized),1,max)
  remove <- maxcoef < thresh
#  print(maxcoef)
  remove
}


## This plots a mental model.
## base:  considers T/F or 0/1 for presence of connections. Converts to zero/non-zero only.
## emat: edges; the network actually being plotted.

plotMM <- function(base,
                   emat,
                   input=rep(F,nrow(coords)),
                   output=rep(F,nrow(coords)),
                   coords=NULL, 
                   color="navy",
                   centerx=0, centery=0, scalex=1, scaley=1,
                   font.cex=1,
                   
                   new=TRUE)
{
  
  basepos <- abs(base) > .0000001
  ematpos <- abs(emat) > 0
  
  base <- sign(base)
  emat <- sign(emat) * basepos
  
  if(is.null(coords))
  {
    coords <- gplot.layout.fruchtermanreingold(basepos,layout.par=list())
  }
  coords <- t((c(scalex,scaley) * t(coords) + c(centerx,centery)))
  
  
  #this is F/T for if a node is valid on either dimension.
  nodes <- (rowSums(ematpos)>0) | (colSums(ematpos)>0)
  #diag(base)<-0    #Don't use self-self links--treat these as node presence
  #diag(emat) <- 0
  
  
  
  
  cols <- c("gold","grey25","darkgreen" )[1+input + 2*output]
  cols[!nodes] <-  NA #grey out missing nodes
  border <- rep("black",length(nodes))
  border[!nodes] <- NA
  ##Do the true world
  textcol <- c("black","white","white")[1+input + 2*output]
  textcol[!nodes] <- NA
  
  
  edgecol <- matrix( c("red","blue",color)[t(sign(emat)+2)],nrow(base),ncol(base),byrow=T)
  
  #diag(base) <- 
  ##this should plot be the background only in light grey.
  gplot(base, coord = coords, edge.lwd = 2.8, edge.col = "grey",
        label.cex = .9*sqrt(scalex*scaley)*font.cex, 
        vertex.cex = 1.5*font.cex, jitter = F,
        vertex.col="grey", new=new,diag=F)
  
  #nodes <- diag(emat)+1

  gplot(emat,coord=coords,diag=F,
        edge.col=edgecol,
        edge.lty.neg=1,
        label=1:length(nodes),label.col=textcol,
        label.cex = .9*sqrt(scalex*scaley)*font.cex, 
        label.pos=5,
        vertex.col=cols,
        vertex.enclose=T,
        vertex.cex=1.5*font.cex,
        vertex.border=border,new=F,
        jitter=F)
  
  
}


plotWeights <- function(weights,frame, output,main="",cex=1,...)
{
  nexem <- dim(weights)[3]
  for(exemplar in 1:nexem)
  {
    weights[,,exemplar]  <- weights[,,exemplar]* frame
    
  }
  
  
  wts <- weights[,,]
  
  for(node in 1:length(output))
  {
    
    if(output[node])
    {
     matplot(wts[,node,],type="p",pch=1,col="black",main=paste(main,"\nOutput node:",node),cex=cex,
             ylab="Parameter value",xlab="Parameter",...)
     for(exemplar in 1:nexem)
      {
        points(1:nrow(wts), wts[,node,exemplar],col=c("grey","black")[1+frame[,output]],pch=16,cex=cex,type="o",...)
     }
      matplot(wts[,node,],type="p",pch=1,col="black",add=T,cex=cex,...)
  }
 }
}

generateData <- function(reps=1000,aweights,avals,iter,input,noise)
{
  #reps <- 1000 ##should have more reps than nodes!
  numnodes <- length(avals)
  
  
  samplingnoise <- rep(1,numnodes)
  noise <- rep(.05,numnodes)  ##set this higher and the whole thing falls apart.
  
  avals <- matrix(runif(nrow(aweights)*reps) * samplingnoise,
                  nrow=reps,ncol=nrow(aweights),byrow=T)
  data <- matrix(0,nrow=reps,ncol=nrow(aweights))
  avals[,!input] <- NA
  
  ##Now, a data set of just the input nodes has been created.
  ##each row represents an independent case.
  
  noise <- rep( .2,numnodes)
  ##thes are 'real' behavior of the system. All random fluctuation comes from noise; 
  ##avals is the same each time. Here, since this is the 'real' one, we simulate just one per condition.
  for(i in 1:reps)
  {
    data[i,]<- simulateEnsemble(aweights,avals[i,],iter=10,inputnodes=input,noise=noise)
  }
  
  return(data)
}

##returns the max vote, or randomly chooses amongst the best votes.
vote <- function(votes)
{
  tally <- table(votes)
  maxvote <-max(tally)
  tally[tally<maxvote] <- 0
  print(tally)
  sample(names(tally),size=1,p=tally)
  
}

