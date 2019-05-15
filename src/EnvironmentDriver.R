##  This is the baseline environment model.  It has two main parts that need to be implemented for 
## any model:
##  The environment() model.  These are not influenced by the control system; static or changing elements of 
##  the world.
##  The controller() model. This is the automation or intelligent system that interacts with the environment.
##  reset(). which wipes the world new.

## When the 'step' function is run, is updates the environment and the controller. This permits a 
## higher-order loop to run the environment and the NILM mental model in synchrony.


.InitializeEnvironment <- function(name="dummy",
                                 dt=1)
{
        .self$name <<- name
        .self$dt <<- dt
}

.StepEnvironmentDriver <- function()
{
    .self$stepEnvironment()
    .self$stepController()
}
.StepEnvironment <- function(){}
.StepController <- function(){}

EnvironmentDriver <- setRefClass("EnvironmentDriver",
                   
                   fields=list(name="character",
                               dt = "numeric"  ##time step size

                   ),
                   
                   methods=list(initialize = .InitializeEnvironment,
                                stepEnvironment=.StepEnvironment,
                                stepController=.StepController,
                                step=.StepEnvironmentDriver)
                   
)
