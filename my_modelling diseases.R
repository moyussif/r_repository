rm(list=ls())
gc(reset = TRUE)
#------------------------------------modelling---------------------
library(readxl)
library(readr)
library(rio)
library(tidyverse)
library(skimr)
library(deSolve)
library(reshape2)
#Part one: SIR Mode-----------(Example one)_________________________

sir<-function(time,state,parameters)
{with(as.list(c(state,parameters)),
      {dS<--beta*S*I
      dI<-beta*S*I-gamma*I
      dR<-gamma*I
      return(list(c(dS,dI,dR)))})}

# state/compartment =(SIR),  +   parameters =(beta, gamma)_________

#Provide the initial values and some parameters as below:
init<-c(S=1-1e-6,I=1e-6,0.0)
parameters<-c(beta=1.4247,gamma=0.14286)
times<-seq(0,70,by=1)

#Put the results in dataframe    Ordinary differential equation (ode)
ode<-as.data.frame(ode(y=init,times=times,func=sir,parms=parameters))
summary(ode)

out<-as.data.frame(ode)
out$time<-NULL

#Ploting the results
matplot(times,out,type="l",xlab="Time",ylab="Susceptible and Recovered",main = "SIR Model",
        lwd = 1, lty = 1, bty = "l", col = 2:4)
legend(40,0.7,c("Susceptible","Infected","Recovered"),pch=1,col=2:4)




infections <- out$I
peak <- max(infections)
match(peak, infections) #this will show the day where peak is high

#..................................................................

# Step 1: writing the differential equations with() function below:

sir_2 <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <- beta * I * S - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}

#with() works on lists only, not on vectors.........

#Step 2: defining some values for the parameters in a named vector:

parameters_values <- c(
  beta = 0.004, # infectious contact rate (/person/day)
  gamma = 0.5 # recovery rate (/day)
)
#------------------------------------------------------------------
#Step 3: defining initial values for the variables
#The initial values of the variables need to be defined in a named vector:
initial_values <- c(
    S = 999, # number of susceptible at time = 0
    I = 1, # number of infectious at time = 0
    R = 0 # number of recovered (and immune) at time = 0
  )
#Step 4: the points in time where we need to calculate variables values
#We want to know the values of our SIR model variables at these time points:
time_values <- seq(0, 10) # days

#Step 5: numerically solving the SIR model We have defined all the needed ingredients:
  ls()

  ## [1] "infections" "init" "initial_values"
  ## [4] "ode" "out" "parameters"
  ## [7] "parameters_values" "peak" "sir"
  ## [10] "sir_2" "time_values" "times"

  [1] “initial_values” “parameters_values” “sir_equations”
  [4] “time_values”
  
  sir_2
  ## function(time, variables, parameters) {
  ## with(as.list(c(variables, parameters)), {
  ## dS <- -beta * I * S
  ## dI <- beta * I * S - gamma * I
  ## dR <- gamma * I
  ## return(list(c(dS, dI, dR)))
  ## })
  ## }
  parameters_values
  ## beta gamma
  ## 0.004 0.500
  initial_values
  ## S I R
  ## 999 1 0
  time_values
  ## [1] 0 1 2 3 4 5 6 7 8 9 10
  
  #Clearly, everything looks fine so now we can use the ode() function of the deSolve package to numericallysolve our model:
    
  sir_values_1 <- ode(
      y = initial_values,
      times = time_values,
      func = sir_2,
      parms = parameters_values
    )
  sir_values_1
  #you can use these values for further analytical steps,  
  sir_values_1 <- as.data.frame(sir_values_1)  
  sir_values_1
  
  with(sir_values_1, {
    # plotting the time series of susceptibles:
    plot(time, S, type = "l", col = "blue",
         xlab = "time (days)", ylab = "number of people")
    # adding the time series of infectious:
    lines(time, I, col = "red")
    # adding the time series of recovered:
    lines(time, R, col = "green")
  })
# adding a legend:
  legend("right", c("susceptibles", "infectious", "recovered"),
         col = c("blue", "red", "green"), lty = 1, bty = "n")
  
 -----------------------------------------------------
#The value of the R0 is
  (999 + 1) * parameters_values["beta"] / parameters_values["gamma"]
# Or __________________assigning N <- (999 + 1)
  N <- (999 + 1)
  N * parameters_values["beta"] / parameters_values["gamma"]
# beta ------#8
  
  sir_try <- function(beta, gamma, S0, I0, R0, times) {
    require(deSolve) # for the "ode" function
    # the differential equations:
    sir_equations <- function(time, variables, parameters) {
      with(as.list(c(variables, parameters)), {
        dS <- -beta * I * S
        dI <- beta * I * S - gamma * I
        dR <- gamma * I
        return(list(c(dS, dI, dR)))
        9
      })
    }
    # the parameters values:
    parameters_values <- c(beta = beta, gamma = gamma)
    # the initial values of variables:
    initial_values <- c(S = S0, I = I0, R = R0)
    # solving
    out <- ode(initial_values, times, sir_equations, parameters_values)
    # returning the output:
    as.data.frame(out)
  }
  sir_try(beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = seq(0, 10))
  ## time S I R
  ## 1 0 999.0000000 1.00000 0.000000
  ## 2 1 963.7055761 31.79830 4.496125
  ## 3 2 461.5687749 441.91575 96.515480
  ## 4 3 46.1563480 569.50418 384.339476
  ## 5 4 7.0358807 373.49831 619.465807
  ## 6 5 2.1489407 230.12934 767.721720
  ## 7 6 1.0390927 140.41085 858.550058
  ## 8 7 0.6674074 85.44479 913.887801
  ## 9 8 0.5098627 51.94498 947.545162
  ## 10 9 0.4328913 31.56515 968.001960
  ## 11 10 0.3919173 19.17668 980.431400
  
 # Comparing a model’s predictions with data-----------------------  
  
  ##flu <- read.table("https://bit.ly/2vDqAYN", header = TRUE)
  # The above link may be broken in the future so is a good practice to save
  #the data on your computer after first download from the internet
  load("flu.RData")
  flu
  ## day cases
  ## 1 0 1
  ## 2 1 6
  ## 3 2 26
  ## 4 3 73
  ## 5 4 222
  ## 6 5 293
  ## 7 6 258
  ## 8 7 236
  ## 9 8 191
  ## 10 9 124
  ## 11 10 69
  ## 12 11 26
  ## 13 12 11
  ## 14 13 4
  ##Plot the points of the flu data set and use the sir_try() function to visually compare the model’s predictions and the data points:
  
  with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
  predictions <- sir_try(beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = flu$day)
  with(predictions, lines(time, I, col = "red"))
  
  
  #The above model did not fit the observed data well so we need to train the model on the data by changing
  #beta and gamma parameters. In this case, we will change only the beta parameter:
  
    with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
  predictions <- sir_try(beta = 0.0025, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = flu$day)
  with(predictions, lines(time, I, col = "red"))
  
 ------------------------------------------------------------------------- 
  
  ### The above model is better than the first model but not good yet
  Write a function that takes parameters values as inputs and draws the figure as an output. Play with that
  function to see how changing the values of parameters can bring the model’s predictions closer to the data
  points.
  model_fit <- function(beta, gamma, data, N = 763, ...) {
    I0 <- data$cases[1] # initial number of infected (from data)
    times <- data$day # time points (from data)
    # model's predictions:
    predictions <- sir_try(beta = beta, gamma = gamma, # parameters
                           S0 = N - I0, I0 = I0, R0 = 0, # variables' intial values
                           times = times) # time points
    # plotting the observed prevalences:
    with(data, plot(day, cases, ...))
    # adding the model-predicted prevalence:
    with(predictions, lines(time, I, col = "red"))
  } 
  
  model_fit(beta = 0.004, gamma = 0.5, flu, pch = 19, col = "red", ylim = c(0, 600))
  
  -----------------------------------------------------------------------------
    
    
    The above model did not fit the data well so let us change the beta parameter to train the data
  model_fit(beta = 0.0025, gamma = 0.5, flu, pch = 19, col = "red", ylim = c(0, 600))  
  
 
  The above model reasonably fits the data well.
  Let us get some model predictions based on the above model:
    beta=0.0025
  gamma=0.5
  s0=762
  I0=1
  R0=0
  time=flu$day
  predictions <- sir_try(beta = beta, gamma =gamma, S0 = s0, I0 = I0, R0 = R0, times = time)
  predictions
  ## time S I R
  ## 1 0 762.00000 1.000000 0.000000
  ## 2 1 757.84809 4.059187 1.092719
  ## 3 2 741.40936 16.111906 5.478730
  ## 4 3 682.07965 58.760314 22.160032
  ## 5 4 522.92669 164.772360 75.300946
  ## 6 5 296.50106 277.719531 188.779410
  ## 7 6 143.33630 285.513298 334.150405
  ## 8 7 75.29039 224.790636 462.918979
  ## 9 8 46.75452 158.037921 558.207561
  ## 10 9 33.73731 105.793282 623.469410
  ## 11 10 27.18922 69.184382 666.626394
  14
  ## 12 11 23.63171 44.695620 694.672672
  ## 13 12 21.59168 28.679392 712.728932
  ## 14 13 20.37877 18.329558 724.291668
  And we want to compare these model’s predictions with real prevalence data:
    One simple way to do so is to compute the “sum of squares” as below:
    sum((predictions$I - flu$cases)ˆ2)
  
  ## [1] 6980.877
  Which is the squared sum of the lengths of vertical black segments of the figure below:
    #the observed prevalences:
    with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
  # the model-predicted prevalences:
  with(predictions, lines(time, I, col = "red", type = "o"))
  # the "errors":
  segments(flu$day, flu$cases, predictions$time, predictions$I)
  
  And we want to predict beyond the observed time period (i.e., forecast) based on the best parameters from
  the fitted model above
  newtime=seq(0,20)
  model_forecast <- sir_try(beta = beta, gamma =gamma, S0 = s0, I0 = I0, R0 = R0, times = newtime)
  summary(model_forecast)
  ## time S I R
  ## Min. : 0 Min. : 18.49 Min. : 0.7717 Min. : 0.0
  ## 1st Qu.: 5 1st Qu.: 19.18 1st Qu.: 4.0592 1st Qu.:188.8
  ## Median :10 Median : 27.19 Median : 18.3296 Median :666.6
  ## Mean :10 Mean :204.13 Mean : 70.8674 Mean :488.0
  ## 3rd Qu.:15 3rd Qu.:296.50 3rd Qu.:105.7933 3rd Qu.:736.4
  ## Max. :20 Max. :762.00 Max. :285.5133 Max. :743.7
  matplot(model_forecast, type="l", lty=1, main="SIRS model", xlab="Time",ylab="Number of people")
  legend <- colnames(model_forecast)[2:4]
  legend(15000,900000, legend=legend, col=2:4, lty = 1)
  
  
  What are the effects of increasing or decreasing the values of the transmission contact rate (β) and the
  recovery rate (γ) on the shape of the epi curve?

  #Estimating model’s parameters
  Sums of squares
  This is our model’s predictions:
    predictions <- sir_try(beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = flu$day)
  predictions
  ## time S I R
  ## 1 0 999.0000000 1.000000 0.000000
  ## 2 1 963.7055761 31.798299 4.496125
  ## 3 2 461.5687749 441.915745 96.515480
  ## 4 3 46.1563480 569.504176 384.339476
  ## 5 4 7.0358807 373.498313 619.465807
  ## 6 5 2.1489407 230.129339 767.721720
  ## 7 6 1.0390927 140.410850 858.550058
  ## 8 7 0.6674074 85.444792 913.887801
  ## 9 8 0.5098627 51.944975 947.545162
  ## 10 9 0.4328913 31.565149 968.001960
  ## 11 10 0.3919173 19.176683 980.431400
  ## 12 11 0.3689440 11.648910 987.982146
  ## 13 12 0.3556517 7.075651 992.568698
  ## 14 13 0.3478130 4.297635 995.354552
  And we want to compare these model’s predictions with real prevalence data:
    flu
  ## day cases
  ## 1 0 1
  ## 2 1 6
  ## 3 2 26
  ## 4 3 73
  ## 5 4 222
  ## 6 5 293
  ## 7 6 258
  ## 8 7 236
  ## 9 8 191
  ## 10 9 124
  ## 11 10 69
  ## 12 11 26
  ## 13 12 11
  ## 14 13 4
  One simple way to do so is to compute the “sum of squares” as below:
    sum((predictions$I - flu$cases)ˆ2)
  ## [1] 514150.7
  Which is the squared sum of the lengths of vertical black segments of the figure below
  
  #the observed prevalences:
  with(flu, plot(day, cases, pch = 19, col = "red", ylim = c(0, 600)))
  # the model-predicted prevalences:
  with(predictions, lines(time, I, col = "red", type = "o"))
  # the "errors":
  segments(flu$day, flu$cases, predictions$time, predictions$I)
  
  #=================-----------==========================================
  
  Working with programmable decision: Shiny
  User-defined models Users can also specify their own models using the neweqns argument. neweqns takes a
  function containing the equations for the new model, with syntax as outlined in the example below. Note
  the syntax follows that used by the popular ODE solver deSolve.
  require(shinySIR)
  ## Loading required package: shinySIR
  ## Loading required package: dplyr
  ##
  ## Attaching package: ’dplyr’
  ## The following objects are masked from ’package:stats’:
  ##
  ## filter, lag
  ## The following objects are masked from ’package:base’:
  ##
  ## intersect, setdiff, setequal, union
  ## Loading required package: tidyr
  ## Loading required package: ggplot2
  ## Loading required package: shiny
  run_shiny(model = "SIR")
  ## Warning in run_shiny(model = "SIR"): The length of the manual colour scale
  ## vector (’values’) must equal the number of model variables. Using default
  ## ggplot colours instead.
  ##
  ## Listening on http://127.0.0.1:3912
  ## Warning: ‘tbl_df()‘ was deprecated in dplyr 1.0.0.
  ## i Please use ‘tibble::as_tibble()‘ instead.
  ## i The deprecated feature was likely used in the shinySIR package.
  ## Please report the issue to the authors.
  ## This warning is displayed once every 8 hours.
  ## Call ‘lifecycle::last_lifecycle_warnings()‘ to see where this warning was
  ## generated.
  
  mySIRS <- function(t, y, parms) {
    with(as.list(c(y, parms)),{
      # Change in Susceptibles
      dS <- - beta * S * I + delta * R
      # Change in Infecteds
      dI <- beta * S * I - gamma * I
      # Change in Recovereds
      dR <- gamma * I - delta * R
      return(list(c(dS, dI, dR)))
    })
  }
  #The interactive plot can then be created by calling this function with neweqns, specifying initial conditions
 # for all model variables (ics), and specifying vectors for the parameter attributes, including parameter starting
 # values (parm0), names to be displayed in the interactive menu (parm_names), and minimum and maximum
 # values for the interactive menu (parm_min and parm_max, respectivel
  
  run_shiny(model = "SIRS (w/out demography)",
            neweqns = mySIRS,
            ics = c(S = 9999, I = 1, R = 0),
            parm0 = c(beta = 5e-5, gamma = 1/7, delta = 0.1),
            parm_names = c("Transmission rate", "Recovery rate", "Loss of immunity"),
            parm_min = c(beta = 1e-5, gamma = 1/21, delta = 1/365),
            parm_max = c(beta = 9e-5, gamma = 1 , delta = 1))
  ## Warning in run_shiny(model = "SIRS (w/out demography)", neweqns = mySIRS, : The
  ## length of the manual colour scale vector (’values’) must equal the number of
  ## model variables. Using default ggplot colours instead.
  ##
  ## Listening on http://127.0.0.1:3931
  
  ==============================================================================
  ------------------------------------------------------------------------------
 =============================================================================== 
  Part two: SIRS models in R (the focus of this session)
  Modelling Waning Immunity
  It is possible that people gain immunity after recovering from the infection but the immunity doesn’t last
  forever. So, these individuals become susceptible again. Here, σ is the waning rate.
  The SIRS model is an extension of SIR model. For SIRS, additional compartment called “waning immunity”
  is added to the SIR model. Thus, The basic SIRS model has three compartments with three parameters 
  
  ∂S
  ∂t
  = -(β x S) + (σ x R) . . . . . . . . . . . . (6)
  ∂I
  ∂t
  = (β x S) - (γ x I) . . . . (7)
  ∂R
  ∂t
  = (γ x I) - (σ x R) . . . . . . . . . . . . . . . . . . (8)
  Waning immunity
  Assume σ =0.2 per day,β =0.4 per day and waning rate,*σ =1/10 per year if average immunity period is
  taken as 10 years. The model is run for a time period of 50 years in daily intervals.
  require(ggplot2)
  require(deSolve)
  require(reshape2)
  ## Loading required package: reshape2
  ##
  ## Attaching package: ’reshape2’
  ## The following object is masked from ’package:tidyr’:
  ##
  ## smiths
  # Model input
  initial_values=c(S=999999,I=1,R=0)
  parameters=c(gamma=0.2*365,beta=0.4*365,sigma=1/(10))
  # Time points
  time=seq(from=1,to=50,by=1/365)
  #SIR model function
  sirs_model <- function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      N=S+I+R
      lambda=beta*(I/N)
      dS=-lambda*S+sigma*R
      dI=lambda*S-gamma*I
      dR=gamma*I-sigma*R
      return(list(c(dS,dI,dR)))
    })
  }
  # Solving the differential equations:
  model_sirs<-as.data.frame(ode(y=initial_values,func = sirs_model,parms=parameters,times = time))
  names(model_sirs)
  ## [1] "time" "S" "I" "R"
  matplot(model_sirs, type="l", lty=1, main="SIRS model", xlab="Time",ylab="Number of people")
  legend <- colnames(model_sirs)[2:4]
  legend(15000,900000, legend=legend, col=2:4, lty = 1)
  
  
  # Note that We can plot the prevalence instead of the number of people by
  # dividing the values by 1,000,000
  #Alternatively, plot the prevalence with ggplot2
  model_sirs_long=melt(model_sirs,id="time")
  names(model_sirs_long)
  ## [1] "time" "variable" "value"
  #Prevalence plot
  ggplot(data = model_sirs_long,
         aes(x = time, y = value/1000000, colour = variable, group = variable)) +
    geom_line() +
    xlab("Time (years)")+
    ylab("Prevalence") +scale_color_discrete(name="State")
  
  ==============================================================================
  ------------------------------------------------------------------------------
  ==============================================================================
  Part three: SEIR model (the focus of this session)
  SEIR model fitting
  The SEIR model is an extension of SIR model. For SEIR, additional compartment called “exposed” is added
  to the SIR model. Thus, The basic SEIR model has four compartments represented as:
  
  1. S - “Susceptible” – individuals who have not been exposed to the virus
  2. E - “Exposed” – individuals exposed to the virus, but not yet infectious
  3. I - “Infectious” – exposed individuals who go on to become infectious
  4. R - “Recovered” – infectious individuals who recover and become immune to the virus
  5. Population size N is the sum of the individuals in the 4 compartments.
  
  
  
  Parameters of the SEIR model
  The flow of individuals between compartments is characterised by a number of parameters.
  β (beta): is the transmission coefficient. Think of this as the average number of infectious contacts an
  infectious individual in the population makes at each time period. A high value of β means the virus has
  more opportunity to spread.
  σ (sigma): is the rate at which exposed individuals become infectious. Think of it as the reciprocal of the
  average time it takes to become infectious. That is, if an individual becomes infectious after 4 days on
  average, σ will be 1/4 (or 0.25).
  γ (gamma): is the rate at which infectious individuals recover. As before, think of it as the reciprocal of the
  average time it takes to recover. That is, if it takes 10 days on average to recover, γ will be 1/10 (or 0.1).
  μ (mu): is an optional parameter to describe the mortality rate of infectious individuals. The higher μ is,
  the more deadly the virus.
  From these parameters, you can construct a set of differential equations. These describe the rate at which
  each compartment changes size.
  Setting-up SEIR Equations
  Equation (9) - Susceptible
  The first thing to see from the model is that there is no way S can increase over time. There are no flows
  back into the compartment. Equation (6) must be negative, as S can only ever decrease.
  In what ways can an individual leave compartment S?
    Well, they can become infected by an infectious individual in the population.
  At any stage, the proportion of infectious individuals in the population = I/N.
  And the proportion of susceptible individuals will be S/N.
  Under the assumption of perfect mixing (that is, individuals are equally likely to come into contact with any
                                          other in the population), the probability of any given contact being between an infectious and susceptible
  individual is (I / N) * (S / N).
  This is multiplied by the number of contacts in the population. This is found by multiplying the transmission
  coefficient β, by the population size N.
  Combining that all together and simplifying gives equation (9):
    ∂S
  ∂t
  = - (β x S x I) / N . . . . . . . . . . (9)
  Equation (10) - Exposed
  Next, let’s consider the “exposed” compartment, E. Individuals can flow into and out of this compartment.
  The flow into E will be matched by the flow out of S. So the first part of the next equation will simply be
  the opposite of the previous term.
  Individuals can leave E by moving into the infectious compartment. This happens at a rate determined by
  two variables – the rate σ and the current number of individuals in E.
  So overall equation (10) is:
  
  
    ∂E
  ∂t
  = (β x S x I) - (σ x E) . . . . (10)
  Equation (11) - Infectious
  The next compartment to consider is the “infectious” compartment, I.
  There is one way into this compartment, which is from the “exposed” compartment.
  There are two ways an individual can leave the “infectious” compartment.
  Some will move to “recovered”. This happens at a rate γ.
  Others will not survive the infection. They can be modeled using the mortality rate μ.
  So equation (11) looks like:
    ∂I
  ∂t
  = (σ x E) - (γ x I) - (μ x I) . . . . (11)
  Equation (12) - Recovered
  Now let’s look at the “recovered” compartment, R.
  This time, individuals can flow into the compartment (determined by the rate γ).
  And no individuals can flow out of the compartment (although in some models, it is assumed possible to
                                                      move back into the “susceptible” compartment - especially infectious diseases where re-infection is possible
                                                      - COVID-19?).
  So the overall equation (12) looks like this:
    ∂R
  ∂t
  = γ x I . . . . . . . . . . . . . . . . . . (12)
  Equation (13) - Mortality (optional)
  Using similar reasoning, you could also construct equation (13) for the change in mortality. You might
  consider this a fifth compartment in the model.
  ∂M
  ∂t
  = μ x I . . . . . . . . . . . . . . . . . . (13)
  You may set μ to zero (0) to exclude this compartment from the model.
  Thus, we have given the full set of differential equations (9-13)
  Solving equations 9-13 (SEIR model) in R
  require(deSolve)
  SEIR <- function(time, current_state, params){
    with(as.list(c(current_state, params)),{
      N <- S+E+I+R
      dS <- -(beta*S*I)/N
      dI <- sigma*E - gamma*I - mu*I
      dR <- gamma*I
      dM <- mu*I
      return(list(c(dS, dE, dI, dR, dM)))
    })
  }
  
  
  The above function describes and provides 3 arguments:
    (a) The current time step.
  (b) A list of the current states of the system (that is, the estimates for each of S, E, I and R at the current
                                                  time step).
  (c) A list of parameters used in the equations (recall these are β, σ, γ, and μ).
  Inside the function body, you define the system of differential equations as described above. These are
  evaluated for the given time step and are returned as a list. The order in which they are returned must
  match the order in which you provide the current states.
  Now take a look at the code below:
    params <- c(beta=0.5, sigma=0.25, gamma=0.2, mu=0.001)
  initial_state <- c(S=999999, E=1, I=0, R=0, M=0)
  times <- 0:365
  The above codes initialises the parameters and initial state (starting conditions) for the model.
  It also generates a vector of times from zero to 365 days.
  Now, create the model:
    model <- ode(initial_state, times, SEIR, params)
  This uses deSolve’s ode() function to solve the equations with respect to time.
  See here for the documentation.
  
  The arguments required are:
  1. The initial state for each of the compartments
  2. The vector of times (this example solves for up to 365 days)
  3. The SEIR() function, which defines the system of equations
  4. A vector of parameters to pass to the SEIR() function
  
  Running the command below will give the summary statistics of the model.
  
  summary(model)
  ## S E I R M
  ## Min. 108263.6 3.616607e-07 0.000000e+00 0.00 0.0000
  ## 1st Qu. 108263.7 5.957435e-03 1.414971e-02 63894.43 319.4721
  ## Median 108395.7 8.470071e+00 1.273726e+01 886814.36 4434.0718
  ## Mean 362798.6 9.745754e+03 1.212158e+04 612272.74 3061.3637
  ## 3rd Qu. 852375.5 1.734331e+03 2.533956e+03 887299.83 4436.4991
  ## Max. 999999.0 1.092967e+05 1.265161e+05 887299.86 4436.4993
  ## N 366.0 3.660000e+02 3.660000e+02 366.00 366.0000
  ## sd 381257.2 2.475783e+04 2.969234e+04 387333.47 1936.6673
  
  Already, you will find some interesting insights.
  1. Out of a million individuals, 108,264 did not become infected.
  2. At the peak of the epidemic, 126,516 individuals were infectious simultaneously.
  3. 887,300 individuals recovered by the end of the model.
  4. A total of 4436 individuals died during the epidemic.
  You can also visualise the evolution of the pandemic using the matplot() function.
  colnames(model)
  ## [1] "time" "S" "E" "I" "R" "M"
  matplot(model, type="l", lty=1, main="SEIR model", xlab="Time")
  legend <- colnames(model)[2:6]
  legend("right", legend=legend, col=2:6, lty = 1)
  
  
  #Assign both x and y axis labels
  matplot(model, type="l", lty=1, main="SEIR model", xlab="Time",ylab="Number of people")
  legend <- colnames(model)[2:6]
  legend("right", legend=legend, col=2:6, lty = 1)
  #Add the peak time line vertically
  matplot(model, type="l", lty=1, main="SEIR model", xlab="Time",ylab="Number of people")
  abline(v=112,col="blue")
  legend <- colnames(model)[2:6]
  legend("right", legend=legend, col=2:6, lty = 1) 
  
  #Add the peak time line vertically
  matplot(model, type="l", lty=1, main="SEIR model", xlab="Time",ylab="Number of people")
  abline(v=112,col="blue")
  text(112,920000,"Peak day: 112",cex = 0.7,pos=3)
  legend <- colnames(model)[2:6]
  legend("right", legend=legend, col=2:6, lty = 1)
  
  #The associated plot is shown above:
  #You can also coerce the model output to a dataframe type. Then, you can analyse the model further.

  infections <- as.data.frame(model)$I
  peak <- max(infections)
  match(peak, infections)
  ## [1] 112
 ===============================================================================
    -------------------------------------------------------------------------
    SEIR model with intervention methods
  The SEIR model is an interesting example of how an epidemic develops without any changes in the population’s
  behaviour.
  You can build more sophisticated models by taking the SEIR model as a starting point and adding extra
  features.
  This lets you model changes in behaviour (either voluntary or as a result of government intervention).
  Many (but not all) countries around the world entered some form of “lockdown” during the coronavirus
  pandemic of 2019.
  Ultimately, the intention of locking down is to alter the course of the epidemic by reducing the transmission
  coefficient, β.
  The code below defines a model which changes the value of β between the start and end of a period of
  lockdown.
  All the numbers used are purely illustrative. You could make an entire research career (several times over)
  trying to figure out the most realistic values.
  SEIR_lockdown <- function(time, current_state, params){
    with(as.list(c(current_state, params)),{
      beta = ifelse(
        (time <= start_lockdown || time >= end_lockdown),
        0.5, 0.1
      )
      N <- S+E+I+R
      dS <- -(beta*S*I)/N
      dE <- (beta*S*I)/N - sigma*E
      dI <- sigma*E - gamma*I - mu*I
      dR <- gamma*I
      dM <- mu*I
      return(list(c(dS, dE, dI, dR, dM)))
    })
  }
  The only change is the extra ifelse() statement to adjust the value of β to 0.1 during lockdown and β to 0.5
  before and after the lockdown.Thus, if before or after the lockdown, give beta value to be 0.5 (i.e., increased
                                                                                                  transmission rate) but if within the lockdown periods, assign beta value to be 0.1 (i.e., reduced transmission
                                                                                                                                                                                      rate)
  You need to pass two new parameters to the model. These are the start and end times of the lockdown
  periods.
  Here, the lockdown begins on day 90, and ends on day 150.
  params <- c(
    sigma=0.25,
    gamma=0.2,
    mu=0.001,
    start_lockdown=90,
  )
  initial_state <- c(S=999999, E=1, I=0, R=0, M=0)
  times <- 0:365
  model2 <- ode(initial_state, times, SEIR_lockdown, params)
  Now you can view the summary and graphs associated with this model.
  summary(model2)
  ## S E I R M
  ## Min. 156885.7 7.699207e-01 0.00000 0.00 0.0000
  ## 1st Qu. 160478.2 6.929205e+01 97.71405 63668.75 318.3438
  ## Median 789214.4 1.246389e+03 1735.66330 194379.16 971.8958
  ## Mean 589558.9 9.216918e+03 11460.62036 387824.44 1939.1222
  ## 3rd Qu. 867639.6 1.030043e+04 13780.17591 829898.56 4149.4928
  ## Max. 999999.0 6.083432e+04 72443.97892 838916.89 4194.5845
  ## N 366.0 3.660000e+02 366.00000 366.00 366.0000
  ## sd 350719.3 1.570278e+04 18893.31145 346542.57 1732.7128
  This will reveal:
    You can see:
    1. Out of a million individuals, 156,886 did not become infected.
  2. At the peak of the epidemic, 72,444 individuals were infectious simultaneously. 838,917 individuals
  recovered by the end of the model.
  3. A total of 4195 individuals died during the epidemic.
  4. Plotting the model using matplot() reveals a strong “second wave” effect (as was seen across many countries in Europe towards the end of 2020).
  matplot(
    model2,
    type="l",
    lty=1,
    main="SEIR model (with intervention)",
    xlab="Time"
  )
  legend <- colnames(model2)[2:6]
  legend("right", legend=legend, col=2:6, lty = 1)
  
  
  Put the start and end of the lockdown periods for better visualization
  matplot(
    model2,
    type="l",
    lty=1,
    main="SEIR model (with intervention)",
    xlab="Time",
    ylab="Number of people"
  )
  abline(v=90, col="blue")
  abline(v=150, col="blue")
  text(112,600000,"start & end- lockdown: day 90 & 150",cex = 0.7,pos=3)
  legend <- colnames(model2)[2:6]
  legend("right", legend=legend, col=2:6, lty = 1)
  
  
  
  Finally, you can coerce the model to a dataframe and carry out more detailed analysis from there.
  infections <- as.data.frame(model2)$I
  peak <- max(infections)
  match(peak, infections)
  ## [1] 223
  1. In this scenario, the number of infections peaked on day 223.
  2. In other scenarios, you could model the effect of vaccination. Or, you could build in seasonal differences
  in the transmission rate.
  
  Exercise for participants (duration - one hour)
  Using the last model above, fit a similar model for the scenarios below separately, and interpret your results
  for each scenario:
    
  (1) vary the value of β between the start and end of a period of lockdown as 0.5 and 0.2 respectively.
  (2) vary the value of β between the start and end of a period of lockdown as 0.6 and 0.2 respectively.
  (3) vary the value of γ to 0.4 and β between the start and end of a period of lockdown as 0.5 and 0.1
  respectively.
      
  
  
   