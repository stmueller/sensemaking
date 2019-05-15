
## This specifies a "Node" in the ILM Network.
##
##


##This will simulate (n) a node value based on only the prior.
.SimulateNode <- function(n=1,...)
{
  print(priorDist)
  if(priorDist=="identity")
  {
    return (rep(coefficients[[ 1 ]],n))
    
  }else if (priorDist == "Bernoulli")
  {
    return (as.numeric(runif(n)< coefficients[["p"]]))
    
  }else if(priorDist == "normal")
  {
    return (rnorm(n,mean=coefficients[["mean"]],
                  sd=coefficients[["sd"]]))
    
  }else if(priorDist == "identity")
  {
      return(coefficients[1])
  }
}

.InitializeNode <- function(name="dummy",
                            priorDist="identity",
                            coefficients=list(),
                            numcoef=length(coefficients)
                            )
{
#    callSuper(...)
    .self$name <<- name
    .self$priorDist <<- priorDist
    .self$coefficients <<- coefficients
    .self$numcoef <<- length(coefficients)
    
        #.self$truename <<- paste0("NODE-",name)
     NODES[[name]] <- .self
}


.PrintNode <- function()
{
    cat(name,"--",numcoef,'\n')
}

Node <- setRefClass("Node",
                    fields = list(name="character",
                                  # truename = "character",
                                  priorDist = "character",
                                  coefficients = "list",
                                  numcoef = "integer"
                    ),
                    
                    methods=list(initialize = .InitializeNode,
                                 simulate   = .SimulateNode,
                                 shortprint = .PrintNode)
                    
)
