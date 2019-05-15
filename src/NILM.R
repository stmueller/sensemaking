## This code is used to define a class related to a'network of improper linear models'.
## An ILM links together nodes to permit inference/prediction about a future state
## A NILM is an assembly of interacting ILMS, that allows simulation arguments to be specified
## (static versus dynamic) and variables to be monitored for posterior distributions


.InitializeNILM <-  function(name="dummyModel",
                          ILMs,
                          inputcoefficients,
                          numcoef,
                          type="static"  ##could be 'static' (same starting coefficients used on each simulation);
                                          ## 'dynamic' (for complex interactions)
                                          ## 'iterated'
)
{
    .self$name <<- name
    .self$ILMs <<- ILMs
    .self$inputcoefficients = inputcoefficients
    .self$currentcoefficients = inputcoefficients
    .self$numcoef<<-  length(inputcoefficients)    
    .self$type <<- type
}

##rounds refers to the number of time steps within each simulation.  It should 
## not matter if type=static.



.PlotNILM <- function()
{
    ##make edge-list network
    starts <- c()
    ends <-  c()
    
    ## First, collect all relevant nodes.   
    for(i in ILMs)
    {
        ilm <- ILMS[[i]]
        end <- ilm$dependentnode
        for(start in ilm$inputnodes)
        {
            starts <- c(starts,start)
            dn <- ilm$dependentnode
            ends <- c(ends,dn$name)
            
        }

            
    }
    
    
    net <- network( cbind(starts,ends))
#    valmat <- matrix(0,numcoef+2,numcoef+2)
#    valmat[,1] <- c(0,intercept,coefficients)
#    colnames(valmat) <-c(name,"intercept",unlist(inputnodes))
#    rownames(valmat) <-c(name,"intercept",unlist(inputnodes))
    
 #   net <- network(valmat,
 #                  names.eval=edge.names)
 #  set.edge.value(net, "value",valmat)
    
    gnet <- ggnetwork(net,layout="kamadakawai")
    
    #random:
#    gnet <- ggnetwork(net,layout=matrix(runif(2*length(starts)),ncol=2))
    
    #names <- c(name,"intercept",inputnodes)
    #net$names <- names[net$vertex.names]
    #                                   ggplot(ggnetwork(mat)) + geom_point()
   myplot <- ggplot(gnet, aes(x = x, y = y, xend = xend, yend = yend)) +
        geom_edges(color = "gold",
                   size=1.5,
                   curvature=.04,
                   arrow = arrow(length = unit(6, "pt"), 
                                 type = "closed")) + 
        geom_nodes(color = "yellow4", size = 5) +
        geom_nodelabel_repel(aes(label = gnet$vertex.names),
                             label.size=.5,
                             box.padding = unit(1, "lines")) +
#        geom_edgetext(aes(label=value))  +
        theme_minimal() +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              panel.background = element_rect(fill = "grey25"),
              panel.grid = element_blank()) +
        ggtitle("Mental Model",subtitle=name)
    
   
    return(myplot)
}


##base conditions are held in inputcoefficients when NILM is 
## defined.  If you want to specify a specific set of node values
## over time, use a data.frame with rounds() rows and the values
## to use (presumably controlled by environment.)

.runNILM <- function(rounds=1,
                     environmentvalues=NULL,
                     reset=FALSE)
{
 
    if(reset)
    {
        currentcoefficients <<- inputcoefficients
    }
    evs <- FALSE
    if(!is.null(environmentvalues))
    {
        if(nrow(environmentvalues)!=rounds)
        {
            stop("In runNILM, prespecified environment values must have same length and number of rounds.")
        }
        evs <- TRUE
    }

    ##Start by setting up the data stream, using inputcoef for starting values.
    data <- (matrix(rep(currentcoefficients,rounds+1),
                    ncol=numcoef,nrow=rounds+1,byrow=T))
    colnames(data) <- names(currentcoefficients)
    
    ##substitute environment values into data matrix when appropriate.
    if(evs)
    {
        for(val in names(environmentvalues))
        {
     
            flter <- match(val,colnames(data))
        
            if(any(flter))
            {
                data[2:(rounds+1),flter] <-environmentvalues[[val]]
           }
       }
        
    }
    
    round <- 2
    ##At the beginning of the round, the data values are populated with starting values.
    sc <- data[round-1,]
    
#    data[round-1,1:numcoef] <- sc

    ##now, fill in any NULL values with base-rate simulated values.
    
    for(par in 1:numcoef)
    {
        if(is.na(data[round,par]) |
           is.null(data[round,par]))
        {
            tmp <- NODES[[colnames(data)[par]]]$simulate()
            data[round-1,par] <-tmp
        }
    }
    copyfilter <- rep(F,numcoef)
    for(round in 2:(rounds+1))
    {
        ##We now need to estimate new values.  start with the previous round values.
        newvals <- data[round-1,]
        
        ##Now, loop through each ILM in order and estimate values for dependent nodes based on current
        ## values.  On each round, since we have already copied the previous values to the current,
        ##we use/update current values until each of the ILMs has been run in order.
        ##note that this means the order of the ILM could matter.
        for(iname in ILMs)
        {
            ilm <- ILMS[[iname]]
            out <- ilm$predict(newvals)
            dn <- ilm$dependentnode
            tmpfilter <- (dn$name==names(newvals))

            ##add the output node to newvals
            newvals[[dn$name]] <- out
            copyfilter <- copyfilter|tmpfilter
        }
        
        data[round,copyfilter] <- newvals[copyfilter]
    }
    
    currentcoefficients <<- data[round,]
    data <- data.frame(step=1:nrow(data),data)
    
return(data)
}



## This observes the system running while the mental model is running in synchrony.
## Every checkcycles cycles, it will assess the accuracy of the model versus reality,
## readjust parameters to fit and determining error.  
## evalnode is the node to monitor.
.RunInSync <-    function(rounds=1,
                          checkCycles = 10,
                          environmentvalues=NULL,
                          evalnode)
    {
        
    

    }

##This will attempt to compute a likelihood value for a given 
##set of values, given the current 
.ComputeLikelihood <- function(data)
{
    
}

NILM <- setRefClass("NILM",
                   
                   fields=list(name="character",
                               ILMs = "character",
                               inputcoefficients = "numeric",
                               currentcoefficients = "numeric",
                               numcoef = "integer",
                               type="character"

                   ),
                   
                   methods=list(initialize = .InitializeNILM,
                                run= .runNILM,
                                plot = .PlotNILM,
                                likelihood = .ComputeLikelihood)
                   
)


