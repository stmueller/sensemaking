require(ggnetwork)
require(ggplot2)
require(network)

##This specifies an ILM--an assembly of nodes and links.
##this will create the links used in the ILM; in theory 
##another ILM could use the same links with different association values.

##This creates a decision/prediction model from nodes,  links,
## and combination rules.



#                          initialize = .InitializeILM,
#                     .print=function(){print(inputnames);print(coefficients)},
#                     plot = .PlotILM ,
#                     predict = .Predict
#                   )


.InitializeILM <- function(name="dummy",
                           dependentnode,
                           inputnodes,
                           coefficients=list(),
                           numcoef,
                           
                           model = "linear",
                           intercept = 0,
                           multiplier = 1,
                           sigma = 0
                           )
{
  .self$name <<- name
  .self$dependentnode <<- dependentnode
  .self$inputnodes <<- inputnodes

  if(length(coefficients) != length(inputnodes))
  {
    warning("input nodes to ILM must equal coefficients")
  }
  
    cc <- coefficients
    names(cc) <- .self$inputnodes
   .self$coefficients <<- cc
   .self$numcoef <<- length(coefficients)
   
   
#  names(coefficients) <<- inputnodes
#  names(.self$coefficients) <<- inputnodes

  ##Check that each node exists.

  for(nodename in inputnodes)
  { 
  
    if(is.null(NODES[[nodename]]))
    {
      warning(paste("node: [" ,nodename, "]not defined.  Create node before it is used in model."))
    }
  }
  
    .self$intercept <<- intercept
    .self$multiplier <<- multiplier
    .self$model <<- model
    .self$sigma <<- sigma

    ILMS[[name]] <- .self
}


##Plot a nodes here:

.PlotILM <- function()
{
  
  ##make edge-list network
  starts <- c("intercept",inputnodes)
  ends <- rep(name,numcoef+1)

  net <- network( cbind(starts,ends))
  
  ##create matrix related to network coefficients
  ##1 is the output node
  ##2 is intercept
  valmat <- matrix(0,numcoef+2,numcoef+2)
  valmat[,1] <- c(0,intercept,coefficients)
  colnames(valmat) <-c(name,"intercept",unlist(inputnodes))
  rownames(valmat) <-c(name,"intercept",unlist(inputnodes))
  
  net <- network(valmat,
                 names.eval=edge.names)
  set.edge.value(net, "value",valmat)
  
  gnet <- ggnetwork(net,
                    layout = layout.layers(net,
                                           c(1,rep(3,numcoef+1))))
  
    #names <- c(name,"intercept",inputnodes)
  #net$names <- names[net$vertex.names]
  #                                   ggplot(ggnetwork(mat)) + geom_point()
  ggplot(gnet, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "gold",
               size=1.5,
               curvature=.14,
               arrow = arrow(length = unit(6, "pt"), 
                             type = "closed")) + 
    geom_nodes(color = "yellow4", size = 5) +
    geom_nodelabel_repel(aes(label = gnet$vertex.names),
                         label.size=.5,
                         box.padding = unit(1, "lines")) +
    geom_edgetext(aes(label=value))  +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.background = element_rect(fill = "grey25"),
          panel.grid = element_blank())
  
}

##If valuelist is a data.frame, it should be equal in rows to reps
##and we should use that instead of repeating.
.PredictILM <- function(valuelist,reps=1)
{
  
  ##valuelist should contain estimated values for all known input values.
  ##How missing data is handled is up to the model.
  ##Typically, you would use prior informations to 'fill in' 
  ## the blanks--guess the most typical value.
  ##We need an input for every named coefficient
  base <- matrix(0,ncol=length(coefficients),nrow=reps)
  nms <- names(coefficients)
   for(i in (1:length(nms)))
  {
    
    tmpname <- nms[i]

    
    if(!is.null(valuelist[[tmpname]]))
    {
      
      if(length(valuelist[[tmpname]])==reps)
        base[,i] <- valuelist[[tmpname]]
      else
        base[,i] <-rep(valuelist[[tmpname]][1],reps)  ##given value
      
    }else{
      print(paste("argument",tmpname, "does not exist simulating from baserate distribution"))
      ###Simulate from priors
      base[,i] <- NODES[[tmpname]]$simulate(reps)
    }
  }
  
  if(model=="linear")
  {
    
    ##Linear model will compute weighted sum of 
        ##
    

    return(multiplier * (intercept + base%*%coefficients + rnorm(reps,mean=0,sd=sigma)))
    
    
  }else if (model=="logistic")
    
  {
    tmp <- intercept + base%*%coefficients + rnorm(reps,mean=0,sd=sigma)
    return (multiplier/(1+exp(-tmp)))
  }else if(model=="AND")
  {
      return(multiplier * all(base!=0))
    
    
  }else if(model=="OR")
  {
      
      return (any(coefficients!=0))
  }
  else if(model=="product")
  { 
     ##computes product via exponentials
     return( exp(sum(log(base*coefficients))))
  }
  else if (model == "threshold") 
  {
      ##threshold node: returns true if value is greater than coefficients;
      ##if coef[2] is negative, does less-than.
      if(multiplier>0)
          return(base[1]>coefficients[1])
      else
          return (base[1] < coefficients[1])
  }
  else if(model == "BRDM")
    {
     
    
      return (1.0)
             
    }else if(model == "WHICHMAX"){
      return(which.max(base))    
    }else 
    {
        warning(paste("Unknown model type",model))
        
    }
  
}




layout.layers <- function(d,layers)
{
  
  d <- as.edgelist.sna(d)
  if (is.list(d)) 
    d <- d[[1]]
  n <- attr(d, "n")
  
  xrange <- c(-5,5)
  tmp <-  cbind(rep(0,n),
                layers)
  xs <- rep(0,length(layers))
  lays <- as.factor(layers)
  for(i in levels(lays))
  {
    filter <- lays==i
    num <- sum(filter)

    ##divide range into n+1 bins
    skip <- diff(xrange)/(num+1)
    xvals <-min(xrange) + (1:num)*skip
    xs[filter] <- xvals
  }
  
  
  tmp <- as.matrix(cbind(xs,layers))
  
 return(tmp)  
}





ILM <- setRefClass("ILM",
                   
                   fields=list(name="character",
                               dependentnode="Node",
                               inputnodes="character",
                               coefficients="numeric",
                               numcoef = "numeric",
                               intercept = "numeric",
                               multiplier="numeric",
                               model = "character",
                               sigma = "numeric"
                               
                   ),
                   
                   methods=list(initialize = .InitializeILM,
                                plot = .PlotILM,
                                predict = .PredictILM)
                   
)
