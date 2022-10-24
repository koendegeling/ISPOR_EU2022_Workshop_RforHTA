# This script details several discrete event simulation (DES) implementations in R as presented
# at during the workshop "Accelerating Adoption and Standardization of R in Health Technology 
# Assessment: the Case of Discrete Event Simulation", which was presented at the 2022 European 
# meeting of the Professional Society for Health Economics and Outcomes Research (ISPOR) in 
# Vienna, Austria.
#
# This workshop was presented as a combined effort by a group of export who are jointly working
# on a book chapter on implementing DES in R for Health Technology Assessment:
# - Mark Clements, Karolinska Institutet, Sweden
# - Mohsen Sadatsafavi, University of British Colombia, Canada
# - James O'Mahony, Trinity College Dublin, Ireland
# - Hendrik Koffijbeg, University of Twente, Netherlands
# - Petros Pechlivanoglou, University of Toronto, Canada
# - David Rios, Ontario Health, Canada
# - Koen Degeling, Lumen Value & Access, United States/Netherlands
#
# Four different options for implementing DES in R are presented:
# - Base R - Vanilla
# - Base R - Vectorized
# - Base R - Closures
# - Simmer package
# 
# A short explanation of each implementation options is provided in the corresponding sections.
# This list of implementations is in no way exhaustive, but was deliberately chosen to showcase
# different characteristics.
#
# The model that is implemented is fairly simple and describe the following process:
# - Waking up at 6.30am
# - Having breakfast 15 to 30 minutes after waking up
# - Having coffee between 45 minutes to 1 hour after having breakfast
# - On 2 to out of 3 days, stretching 30 minutes after having coffee
#
# Note that time is chosen to be defined in hours across all implementation examples. Also note
# that this code was not developed to give an exhaustive explanation of how to implement DES in
# R or of the different implementation options - its main purpose is to give an idea about how
# the options differ.
#
# The script was written en tested for R version 4.1.2 and requires the following packages:
#   - dplyr         version 1.0.8
#   - simmer        version 4.4.4
#   - simmer.plot   version 0.1.17
#
# Note that all code needs to be run from top to bottom, otherwise errors may occur.




### INITIALIZATION ----

# Clear the Global Environment
rm(list = ls()); gc();

# Install packages if they are not already installed
if(!require(dplyr))       install.packages('dplyr')
if(!require(simmer))      install.packages('simmer')
if(!require(simmer.plot)) install.packages('simmer.plot')

# Load the required packages
library(dplyr)        # for alternative implementation of the vectorized approach
library(simmer)       # for implementing the DES using the simmer package
library(simmer.plot)  # visualize the simmer model

# Define the number of individuals to be simulated
n_sim <- 1000




### BASE R - VANILLA ----

# The "vanilla" implementation in base R is the potentially purest implementation and a great
# way to understand how DES works. It walks through the process as described step-by-step and
# along the way records the time at which events happen. To simulate multiple individuals, this
# process is repeated.

# Create a matrix to store the event times
m_events <- matrix(nrow = n_sim, ncol = 5, 
                   dimnames = list(NULL, 
                                   c('ID', 'WakeUp', 'Breakfast', 'Coffee', 'Stretch')))

# Set the random number seed for reproducibility
set.seed(123)

# Loop to the number of simulated individuals
for(i_sim in 1:n_sim) {
  
  # Record the individual's ID
  m_events[i_sim, 'ID'] <- i_sim
  
  # WakeUp at 6.30am
  m_events[i_sim, 'WakeUp'] <- 6.5
  
  # Have breakfast between 15 to 30 minutes after waking up
  m_events[i_sim, 'Breakfast'] <- m_events[i_sim, 'WakeUp'] + 
    runif(n = 1, min = 0.25, max = 0.5)
  
  # Have coffee between 45 minutes to 1 hour after breakfast
  m_events[i_sim, 'Coffee'] <- m_events[i_sim, 'Breakfast'] + 
    runif(n = 1, min = 0.75, max = 1)
  
  # On 2 to out of 3 days, stretch 30 minutes after coffee
  if(runif(n = 1) < 2/3) {
    m_events[i_sim, 'Stretch'] <- m_events[i_sim, 'Coffee'] + 0.5
  } else {
    m_events[i_sim, 'Stretch'] <- NA
  }
  
}

# Example of output
head(m_events)




### BASE R - VECTORIZED ----

# The "vectorized" implementation in base R is similar to the "vanilla" implementation, as 
# it walks through the process as described step-by-step and along the way records the time at 
# which events happen. However, this is done for all simulated individuals at once, so it is 
# much faster if a substantial number of individuals is to be simulated.

# Set the random number seed for reproducibility
set.seed(123)

# Initialize a data.frame with the individuals' ID
df_events <- data.frame(ID = 1:n_sim)

# WakeUp at 6.30am
df_events$WakeUp <- 6.5

# Have breakfast between 15 to 30 minutes after waking up
df_events$Breakfast <- df_events$WakeUp + 
  runif(n = n_sim, min = 0.25, max = 0.5)

# Have coffee between 45 minutes to 1 hour after breakfast
df_events$Coffee <- df_events$Breakfast + 
  runif(n = n_sim, min = 0.75, max = 1)

# On 2 to out of 3 days, stretch 30 minutes after coffee
df_events$Stretch <- ifelse(runif(n = n_sim) < 2/3, 
                            df_events$Coffee + 0.5,
                            NA)

# Example of output
head(df_events)

# The Tidyverse equivalent using 'dplyr' would be:
library(dplyr)
set.seed(123)
df_events_tidy <- data.frame(ID = 1:n_sim) %>% 
  mutate(
    WakeUp = 6.5,
    Breakfast = WakeUp + runif(n = n(), min = 0.25, max = 0.5),
    Coffee = Breakfast + runif(n = n(), min = 0.75, max = 1),
    Stretch = ifelse(runif(n = n()) < 2/3, Breakfast + 0.5, NA)
  )




### BASE R - CLOSURES ----

# Whereas the other implementation approach are process-based, i.e. their structure follows
# that of the process being modelled, the "closures" implementation in base R uses an event-
# based approach. The code represents what is typically considered object-oriented code and
# does not really define the process, but the actions that should be taken when a certain
# event happens. This may be much more complex conceptually, but provides a great flexibility.

# Create a matrix to store the event times
m_events2 <- matrix(nrow = n_sim, ncol = 5, 
                    dimnames = list(NULL, c('ID', 'WakeUp', 'Breakfast', 'Coffee', 'Stretch')))

DES <- function(){
  
  # Functions supporting the DES
  push = function(time,event) {
    insert.ord <- findInterval(time,times) 
    times <<- append(times,time,insert.ord)
    events <<- append(events,event,insert.ord)
  }
  pop = function(){
    head <- structure(events[[1]], time=times[1])
    times <<- times[-1]
    events <<- events[-1]
    return(head)
  }
  empty = function() {
    length(times) == 0
  }
  clear = function() {
    times <<- numeric()
    events <<- list()
  }
  remove = function(predicate, ...) {
    i <- sapply(events, predicate, ...)
    stopifnot(is.logical(i))
    i[is.na(i)] <- TRUE
    times <<- times[!i]
    events <<- events[!i]
  }
  scheduleAt = function(time, event) {
    attr(event,"time") <- time
    attr(event,"sendingTime") <- currentTime
    # push described earlier 
    push(time, event)
  }
  now = function(){
    # returns currentTime
    return(currentTime)
  }        
  
  # Define what happens at the start
  init <- function(){
    
    m_events2[i_sim, 'ID'] <<- i_sim
    
    scheduleAt(time = 6.5, 
               event = "WakeUp")
  }
  
  # Handle future events
  handleMessage <- function(event){
    
    if(event == "WakeUp"){ 
      
      m_events2[i_sim, 'WakeUp'] <<- now()
      
      scheduleAt(time = now() + runif(n = 1, min = 0.25, max = 0.5),
                 event = 'Breakfast')
    }
    
    if(event == "Breakfast"){
      
      m_events2[i_sim, 'Breakfast'] <<- now()
      
      scheduleAt(time = now() + runif(n = 1, min = 0.75, max = 1),
                 event = 'Coffee')
    }
    
    if(event == "Coffee"){
      
      m_events2[i_sim, 'Coffee'] <<- now()
      
      if(runif(n = 1) < 2/3) {
        scheduleAt(time = now() + 0.5,
                   event = 'Stretch')
      }
    }
    
    if(event == 'Stretch') {
      
      m_events2[i_sim, 'Stretch'] <<- now()
      
    }
    
  }
  
  times = c()
  events = c()
  currentTime = 0
  previousEventTime = 0
  
  # initialize our DES         
  init()
  
  while(!empty()) { # while there is an event to do
    # run the DES by first, getting the next event using pop()
    event <- pop()
    # setting the currentTime when the event occured
    currentTime <- attr(event, "time")
    # then for each event, doing the appropriate set of actions
    handleMessage(event)
    # then re-updating the event.
    previousEventTime <<- currentTime
  }
  
}

# Run the simulation
set.seed(123)
for(i_sim in 1:n_sim) {
  
  DES()
  
}




### SIMMER PACKAGE ----

# The "simmer" package provides a convenient process-based approach to implementing DES in R
# with the possibility of considering resources and capacity constraints. It required the user
# to define the process that is being modelled as a trajectory. To run the simulation,
# inidividuals are simulated through the trajectory. The backbone of the "simmer" package is in
# C++, which means simulations are relatively fast despite the potential complexity.

# Define the pathway
traj <- trajectory() %>%
  
  # WakeUp at 6.30am
  timeout(task = 6.5) %>%
  set_attribute(keys = 'WakeUp', values = function() now(sim)) %>% 
  
  # Have breakfast between 15 to 30 minutes after waking up
  timeout(task = function() runif(n = 1, min = 0.25, max = 0.5)) %>% 
  set_attribute(keys = 'Breakfast', values = function() now(sim)) %>% 
  
  # Have coffee between 45 minutes to 1 hour after breakfast
  timeout(task = function() runif(n = 1, min = 0.75, max = 1)) %>% 
  set_attribute(keys = 'Coffee', values = function() now(sim)) %>% 
  
  # On 2 to out of 3 days, stretch 30 minutes after coffee
  branch(option = function() if(runif(n = 1) < 2/3) {1} else {0},
         continue = c(TRUE),
         
         # Stretch
         trajectory() %>% 
           timeout(task = 0.5) %>% 
           set_attribute(keys = 'Stretch', values = function() now(sim))
  
  )


# Visualize the defined pathway
plot(traj)

# Define the simulation environment
sim <- simmer() %>% 
  add_generator(name_prefix = 'ID', trajectory = traj, distribution = at(rep(0, n_sim)), mon = 2)

# Run the simulation
set.seed(123)
sim %>% reset() %>% run()

# Extract output
df_simmer <- get_mon_attributes(sim)

# Example of output
head(df_simmer)




### COMPARE OUTCOMES ----

# This section of codes compares the results for the different implementations. If sufficient
# individuals are simulated (i.e., stochastic / first-order uncertainty is simulated out), the
# results should be very similar as the different implementations represent the same model
# structure and parameters.

# Mean time of waking up
c('Base R - Vanilla'    = mean(m_events[, 'WakeUp']),
  'Base R - Vectorized' = mean(df_events$WakeUp),
  'Base R - Closures'   = mean(m_events2[, 'WakeUp']),
  'Simmer package'      = mean(df_simmer$time[df_simmer$key == 'WakeUp'])
)

# Mean time of having breakfast
c('Base R - Vanilla'    = mean(m_events[, 'Breakfast']),
  'Base R - Vectorized' = mean(df_events$Breakfast),
  'Base R - Closures'   = mean(m_events2[, 'Breakfast']),
  'Simmer package'      = mean(df_simmer$time[df_simmer$key == 'Breakfast'])
)

# Mean time of having coffee
c('Base R - Vanilla'    = mean(m_events[, 'Coffee']),
  'Base R - Vectorized' = mean(df_events$Coffee),
  'Base R - Closures'   = mean(m_events2[, 'Coffee']),
  'Simmer package'      = mean(df_simmer$time[df_simmer$key == 'Coffee'])
)

# Probability of stretching
c('Base R - Vanilla'    = mean(is.na(m_events[, 'Stretch']) == FALSE),
  'Base R - Vectorized' = mean(is.na(df_events$Stretch) == FALSE),
  'Base R - Closures'   = mean(is.na(m_events2[, 'Stretch']) == FALSE),
  'Simmer package'      = sum(df_simmer$key == 'Stretch')/n_sim
)

# Mean time of stretching, if stretching
c('Base R - Vanilla'    = mean(m_events[, 'Stretch'], na.rm = TRUE),
  'Base R - Vectorized' = mean(df_events$Stretch, na.rm = TRUE),
  'Base R - Closures'   = mean(m_events2[, 'Stretch'], na.rm = TRUE),
  'Simmer package'      = mean(df_simmer$time[df_simmer$key == 'Stretch'])
)

