## These models run models 1..7. Simulation 9/10+ are in .Rmd files
##
##

source("definitions.R")


if(exists("seed"))
{
    seed <- seed + 1
} else {
    seed <- 100
}

#seed <- 136
cat("Using random seed: ",seed,"\n")
set.seed(seed)


##for computational workshop, tests 2 and 6 were used.
## Key to tests:
##  1:  5 nodes, two paralles pathways to 4, then 5
##  2:  creativity paper sim 1
##Test 2: examine what happens when the mental model is correct.

##  3:  6 nodes, 3 edge nodes, one outcome node, 2 middle nodes
##  4: 6 nodes; 6 direct input to one final node
##  5: 
##  6: creativity paper sim 2
##test 6: same as #2, but looking at an incorrect model as a comparison.

##  7/8: exploring contrast in learning

test <- 8

https://www.youtube.com/watch?v=rqYUCV4FHI4

##to set up an environment, you need:
## 1. to specify nodes
## 2. To specify connections between nodes (aweights)
## 3. To specify values of nodes (avals)
## 4. to specify input, output, and 'visible' nodes.
## 5. Specify initial hypothesis space (hvals/hweights/hframe,houtput)



if(test==1)
{
numnodes <- 5

##here is the 'true' system model. Generate a bunch of example data to
##learn from.

avals <- runif(numnodes)
##each row represents the outgoing connections from each node
aweights <-array( 
   matrix(c(0,1,0,-.5,0,
            0,0,1.5,0,0,
            0,0,0,-.8,0,
            0,0,0,0,2.3,
            0,0,0,0,0), nrow = numnodes, byrow = TRUE),
         dim=list(numnodes,numnodes,1))
input <- c(T,F,F,F,F)
output <- c(F,F,F,F,T)
visible <- c(F,T,T,T,F) ##should include input and output nodes.





hvals <- avals
hframe <- aweights[,,1]!=0 ##pick out the correct mental model:
diag(hframe) <- 0

##make it deliberately wrwong:

hframe[3,5] <- 1
hframe[4,5] <- 0
houtput <- output
houtput[4] <- T

} else if (test==2 | test==6)
{
  
  ##Test 2: examine what happens when the mental model is correct.
  ##test 6: same deal but looking at an incorrect model as a comparison.
  numnodes<- 5
  
  ##here is the 'true' system model. Generate a bunch of example data to
  ##learn from.
  
  avals <- runif(numnodes)
  ##each row represents the outgoing connections from each node
  aweights <-array( 
    matrix(c(0,0,0,0,-2,
             0,0,0,0,+2,
             0,0,0,0,.2,
             0,0,0,0,-.2,
             0,0,0,0,0), nrow = numnodes, byrow = TRUE),
    dim=list(numnodes,numnodes,1))
  input <- c(T,T,T,T,F)
  output <- !rowSums(aweights!=0)
  #c(F,F,F,T,F,T)
  visible <- c(T,T,T,T,T) ##should include input and output nodes.
  
  
  
  
  
  hvals <- avals
  
  #hframe <- generateRandomMentalModel(5,input,output)#
  hframe <- aweights[,,1]!=0 ##pick out the correct mental model:
  diag(hframe) <- 0

  houtput <- !rowSums(hframe!=0)
  
  
}else if(test==3)
  
{
  
  
  
  ##Test 3: examine what happens when the mental model has an extra node/connection.
  numnodes<- 6
  
  ##here is the 'true' system model. Generate a bunch of example data to
  ##learn from.
  
  avals <- runif(numnodes)
  ##each row represents the outgoing connections from each node
  aweights <-array( 
    matrix(c(0,0,0, -.5, 0, -.2,
             0,0,0,1.5,0,-2,
             0,0,0,-.8,0,0,
             0,0,0,0,2.3,0,
             0,0,0,0,0,1,
             0,0,0,0,0,0), nrow = numnodes, byrow = TRUE),
    dim=list(numnodes,numnodes,1))
  input <- c(T,T,T,F,F,F)
  output <- !rowSums(aweights!=0)
  #c(F,F,F,T,F,T)
  visible <- c(T,F,F,F,F,T) ##should include input and output nodes.
  
  
  
  
  
  hvals <- avals
  
  #hframe <- generateRandomMentalModel(5,input,output)#
  hframe <- aweights[,,1]!=0 ##pick out the correct mental model:
  diag(hframe) <- 0
  
  ##make it deliberately wrwong:
  if(0)
  {
    hframe[3,5] <- 1
    hframe[4,5] <- 0
    hframe[1,6] <- 0
    
    
  }
  houtput <- !rowSums(hframe!=0)
} else if(test==4)
{
  
  
  ##Test 4: examine what happens when the mental model has a missing link.
  
  
  numnodes<- 6
  
  ##learn from.
  
  avals <- runif(numnodes)
  ##each row represents the outgoing connections from each node
  aweights <-array( 
    matrix(c(0,0,0, 0, 0, -5,
             0,0,0,0,0,-2,
             0,0,0,0,0,+2,
             0,0,0,0,0,.2,
             0,0,0,0,0,-.2,
             0,0,0,0,0,0), nrow = numnodes, byrow = TRUE),
    dim=list(numnodes,numnodes,1))
  input <- c(T,T,T,T,T,F)
  output <- !rowSums(aweights!=0)
  #c(F,F,F,T,F,T)
  visible <- c(T,T,T,T,T,T) ##should include input and output nodes.
  
  
  
  
  
  hvals <- avals
  
  #hframe <- generateRandomMentalModel(5,input,output)#
  hframe <- aweights[,,1]!=0 ##pick out the correct mental model:
  diag(hframe) <- 0
  
  ##make it deliberately wrwong:
  if(1)
  {
    hframe[1,6] <- 0
    
  }
  houtput <- !rowSums(hframe!=0)
} else if(test==7 | test==8)
{ 
  ##input nodes and 1 output node.
  ## there are only 6 relevant nodes, the rest are garbage.
  ##test 8 is the random contrast condition for the same set-up
  ##test 9 is a crafted constrast condition.
  
  numnodes<- 15
  
  ##learn from.
  
  avals <- runif(numnodes)
  ##each row represents the outgoing connections from each node
  aweights <-array( matrix(rep(0,numnodes^2),numnodes),
    dim=list(numnodes,numnodes,1))
  aweights[,numnodes,1] <- c(-10,-10,-10,rep(0,numnodes-7),10,10,10,0)
  input <- c(rep(T,numnodes-1),F)
  output <- !input
  visible <- rep(T,numnodes)
  
  
  
  
  hvals <- avals
  ##start at complete random weights.
  hframe <- matrix(rep(0,numnodes^2),numnodes)
  hframe[1:(numnodes-1),numnodes] <- rnorm(numnodes-1)
  #hframe <- generateRandomMentalModel(5,input,output)#
 # hframe <- aweights[,,1]!=0 ##pick out the correct mental model:
  diag(hframe) <- 0

  houtput <- output#!rowSums(hframe!=0)
  
  
}

##############################
###############################
## Setup complete.


if(0)
{
  par(mfrow=c(1,2),mar=c(0,0,0,0))
  ##Do the true world
#  textcol <- c("black","white","white")[1+input + 2*output]
  coord <- plotMM(aweights[,,1],aweights[,,1],input,output)
  title("Veridical Causal Model Structure",line=-1)
  plotMM(aweights[,,1],hframe,input,houtput,coord=coord)
  title("Mental model structure",line=-1)
}

#####################################################
#Now, generate the data:

data <- generateData(reps=1000,aweights=aweights,avals=avals,iter=10,input=input,noise=noise)


##also, generate test data under other circumstances.
##we could, for example, test the model on a different
##true world model and see how it does.

noise <- rep(.05,numnodes)  ##set this higher and the whole thing falls apart.
##Here, create a second set of aweights and data.

avals2 <- avals
avals2[!input] <- NA

##Now, a data set of just the input nodes has been created.
##each row represents an independent case.

aweights2 <- aweights
if(test==6)
{
  aweights2[1,5,1] <-   .2
  aweights2[2,5,1] <-  -.2
  aweights2[3,5,1] <-   -2
  aweights2[4,5,1] <-   2
  
}
data2 <- generateData(reps=1000,aweights=aweights2,avals=avals2,iter=10,input=input,noise=noise)
  


##############################################################
## now, do the main simulation/learning phase.
#############################################################

#set.seed(as.numeric(Sys.time()))
numexemplars <- 10      ##how many exemplars
exemplarsToUpdate <- 5
simreps <- 1500
trial <- 1


##implement learning here.
alpha <- .25 #learning rate.
decay <- .9995
maxback <- 10


if(F)
{
  par(mfrow=c(1,2))
  coords <- plotMM(sign(aweights[,,1]),sign(aweights[,,1]),input,output)
  plotMM(hframe,hframe,input,houtput,coords=coords)
}


##now, set up the ensemble of hypothesezs about the frame.
hweights <- array(rep(hframe,numexemplars)* (1-2*runif(numnodes^2*numexemplars)),dim=list(numnodes,numnodes,numexemplars))

##estimate mean error of model throughout, for each node.
meanerror <- matrix(0,nrow=numexemplars, ncol=nrow(hframe))

ord <- sample(nrow(data))
##this repeats a random order  enough times to fill in simreps. Same order each time.
simtrials <- rep(ord,ceiling(simreps/length(ord)))[sample(1:simreps)]
learningerror <- array(NA,
                       dim=list(exemplarsToUpdate,numnodes,simreps))
trials <- ord
counter <- 1

prevdata <- data[1,]
truth <- prevdata
observed <- prevdata
observed[houtput] <- NA
for(i in simtrials)##go through this many examples of data.
{
  if(test==8)
  {
    prevtruth <- truth
    prevobserved <- observed
  }
  exemplars <- sample(numexemplars)[1:exemplarsToUpdate]  ##pick, e.g., five at a time:
  truth <- data[i,]  #what is really the case
  observed <- truth  ##what the observed truth is.
  observed[houtput]<- NA

  
##this is predicting based ONLY on the input nodes.

iter <- 1
##pick which node to use (of the output nodes)
node <- mysamp(houtput,length(houtput))


##now, we have made a set of predictions for 
##pick an output node and back-propogate error.

while(iter < maxback)
{
  cat("Node:",node ,"| ")

    filter <-  !((1:length(output)) == node ) &!output


  ##make a prediction of each missing value, for each model.
  predictions <- simulateEnsemble(hweights[,,exemplars,drop=FALSE],
                                  observed,iter=10,
                                  inputnodes=filter, ##input
                                  noise=rep(0.0001,length(input)))  

if(test==8){
  prevpredictions <- simulateEnsemble(hweights[,,exemplars,drop=FALSE],
                                  prevobserved,iter=10,
                                  inputnodes=filter, ##input
                                  noise=rep(0.0001,length(input)))  
}
  
  ##only go through all this if we have a real problem:
    inp <- hframe[,node]
  if(sum(abs(inp))>0)
  {
    ##for each model, get the error for this node's prediction:
    delta2 <- t(truth -(predictions))[,node] #here is an error signal, comparing to all known true values.
    ##^^^^^this is the amount we were off, for each model.
    
    ##Calculate the input weights of the exemplar models
    weights <- t(hweights[,node,exemplars]) 
    #^^^^  these are the original weights we need to update.

    
    ##change is how much each weight needs to be adjusted.
  #  change <-  t(alpha * (delta2) * truth * t(abs(sign(weights)))) ##abs(sign(weights)) forces output to 0 and preserves exemplars.
    change <- t(alpha * truth %*% t(delta2) * t(abs(sign(weights))))
    
       # change[,node] <- 0
    if(test==8)
    {
      ##test 8 uses contrast. Here, we update weights to move toward the double-delta we 
      #just saw:
      
      #delta2.old <-  t(prevtruth -(prevpredictions))[,node]
      #change.old <-   alpha * (delta2.old) * prevtruth * abs(sign(weights))
      
      #ddelta <- delta2.old-delta2
      #dchange <- change.old-change
      #dweight <- - dchange/ddelta
      dtruth <- (prevtruth-truth)
      ##normalize so the change in output node is 1.0:
      
      ##this weights these things:
      #sfdtruth <- dtruth2^(gamma)/sum(dtruth2^gamma,na.rm=T)
      #sfdtruth <- sfdtruth / mean(sfdtruth,na.rm=T) #make mean 1.0, so that 7=8 if gamma=0
      #sfdtruth[is.na(sfdtruth)] <- 0
      
      dtruth[node] <- 0
      worst <- order(abs(dtruth),decreasing=T)[6:length(dtruth)]
      
      change[,worst] <- 0 
      
      ##find the subset of nodes that have the largest difference in input values
      
      
      
      
      ##scale toward that
      #newweights <- weights * (1-alpha) + dtruth * alpha
      #newweights[node] <- 0
      #hweights2 <- hweights
      #hweights2[,node,exemplars] <- t(newweights)
      #newpred <-  simulateEnsemble(hweights2[,,exemplars,drop=FALSE],
      #                             prevobserved,iter=10,
      #                             inputnodes=filter, ##input
      #                             noise=rep(0.0001,length(input)))  
      weights <- weights + change
      
    }else{
      weights <- weights + change
    }


  hweights[,node,exemplars] <- t(weights)
  

   ##now, move backward no one of the incoming nodes
   node <- mysamp(inp>0,1)
#   iter <- maxback
   learningerror[,,counter] <- (delta2)
   counter <- counter + 1
  } else{
    cat ("Maximum backprop\n")
    iter <- maxback
  }

   iter <- iter + 1  
}
  alpha <- alpha * decay
 trial <- trial + 1
 
 prevdata <- truth##update memory for prior trial.

}

if(test==7|test==8)
{
par(mfrow=c(1,3),mar=c(3,4,2,0))
  image(as.matrix(hweights[,numnodes,]),col=brewer.pal(11,"RdYlGn"))
  matplot(hweights[,numnodes,],type="b",pch=16,lty=3,col="black")
 lerr <- apply(abs(learningerror),3, mean)
  plot(lerr,col="grey30",main="Error over time",ylab="Mean absolute error",xlab="Learning round")
  points(lowess(lerr,f=.2),lty=1,type='l',lwd=4,col="red")
}


if(0)
{
  par(mfrow=c(1,1))

  ##this examines the 'output' values, predicted by the 'input' values, 
  ##for a given fuzzy mental model.
  ev <- evaluateMentalModel(data,hweights,meanerror,
                      observed,
                      input,houtput,
                      iter=10,noise=rep(0.01,numnodes),plot=T)
  print(ev$remove)

  ev2 <- evaluateMentalModel(data2,hweights,meanerror,
                              observed,
                              input,houtput,
                              iter=10,noise=rep(0.01,numnodes),plot=T)
  
cat("Percent of observed data that are consistent with model:", (mean(ev$keep)),"\n")
cat("percent consistent:",mean(ev$consistent),"\n")
cat("Percent of observed data that are consistent with converged model:", (mean(!ev$consistent | (ev$consistent&!ev$keep))),"\n")


cat("Percent of observed data that are consistent with model:", (mean(ev2$keep)),"\n")
cat("percent consistent:",mean(ev2$consistent),"\n")

cat("Percent of observed data that are consistent with converged model:", (mean(!ev2$consistent | (ev2$consistent&!ev2$keep))),"\n")

cols <- colorRampPalette(c("gold","black"))(10)
par(mfrow=c(1,2),mar=c(5,3,1,1))
matplot(data[,5],ev$pdata[,5,],pch=16,xlab="Observed data",ylab="Model predictions",main="True data",col=cols)
abline(0,1,lwd=3)
abline(2*mean(meanerror[,5]),1)
abline(-2*mean(meanerror[,5]),1)

matplot(data2[,5],ev2$pdata[,5,],pch=16,xlab="Observed data",ylab="Model predictions",main="Improper data",col=cols)
abline(0,1,lwd=3)
abline(2*mean(meanerror[,5]),1)
abline(-2*mean(meanerror[,5]),1)

if(0)
{
  
  sim2dat <- 
read.table(text="  100   .82     .82                .18    .18
  200   .86     .81                  .14  .19
  400   .92     .78                  .08	.22
  600   .95     .71                .17    .4
  800   .95     .74                 .25    .44
  1000  .93     .72                  .28    .45
  1200   .88    .49                  .19    .52
  1400    .95   .34                   .05    .66
  1600   .87     .33                  .13    .67
  1800   .9      .33                .1      .67
  2000   .87    .32            .13   .68")
  pdf("sim2-consistent.pdf",width=8,height=4)
par(mfrow=c(1,1),las=1)
  matplot(sim2dat$V1,sim2dat[,2:3],type="o",col=c("black","gold"),
          lty=c(1,3,1,3),pch=c(16,16,2,2),cex=1.2,bty="L",
          ylim=c(0,1),
          lwd=2,xlab='Learning cycles',ylab="Percent",xlim=c(0,2000))  
legend(100,.3,c("True data","Improper data"),
       bty="n",pch=c(16,16,2,2),
       lty=c(1,3),col=c("black","gold"))
dev.off()
  
  }
if(test==2)
  {
    ##look at the 
 #   par(mfrow=c(1,1),mar=c(3,4,1,1))
#     boxplot(t(hweights[,5,]))
#    matplot(1:5,(hweights[,5,]),add=T,pch=16,col="grey30")
#    abline(0,0)
    
  #  apply(hweights[,5,],1,sd)*apply(data,2,sd)
    
    vals <- data.frame(Parameter=paste("p",1:5,sep=""),Weights=as.vector(hweights[,5,]))
    params <- data.frame(w=aweights[,5,1],x=1:5)
    
    png(paste("weights-",simreps,".png",sep=""), width=1000,height=400)
    p <- ggplot(vals,mapping=aes(x=Parameter,y=Weights) )+ theme_bw()+
      geom_violin(fill="gold",scale="count",adjust=3) +
      geom_point(colour="grey30") +  
      geom_point(data=params,mapping=aes(x=x,y=w),color="black",size=5,shape=3)+
      ylim(-3,3)+
      xlab("Parameter estimate") + ylab("Estimated weight") + ggtitle(paste("Weight estimates after",simreps,"cycles"))
    print(p)
    dev.off()
  }
#plot(data[1,],pch=16,cex=3,ylim=range(data))
#matplot(pdata[1,,],add=T)
#SimulateStatic(aweights,avals,iter=10,inputnodes=input,noise=noise,outputnodes=output)



#### Can we have a more complex model that involves a transition?
}
