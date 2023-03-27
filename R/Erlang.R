
#' Calculate Intensity
#' 
#' This function calculates  intensity of the incoming volume (workload) in a given interval
#' 
#' Inputs: (volume:int,AHT:float,interval:int)
#' 
#' @param volume - the number of expected contacts in the interval
#' @param AHT - the average handle time per contact
#' @param interval - the number of time units  where the volume is expected

#' @export
intensity <- function(volume, AHT, interval = 1) {
  (volume / interval) * AHT
}

#' Calculate Erlangs using Erlang C formula
#' 
#' This function calculates the number of Erlangs in a given interval
#' 
#' Inputs: (agents: int, volume:int,AHT:float,interval:int)
#' 
#' @param agents - the number agents available in that interval
#' @param volume - the number of expected contacts in the interval
#' @param AHT - the average handle time per contact
#' @param interval - the number of time units  where the volume is expected

#' @export
erlang_c <- function(agents, volume, AHT, interval = 1) {
  int <- intensity(volume, AHT, interval)
  erlang_b_inv <- 1
  for (i in 1:agents) {
    erlang_b_inv <- 1 + erlang_b_inv * i / int
  }
  erlang_b <- 1 / erlang_b_inv
  agents * erlang_b / (agents - int * (1 - erlang_b))
}

#' Calculate Erlang_CL
#' 
#' This function calculates Erlangs using Erlang C while considering the available number of lines formula
#' 
#' Inputs: (agents: int, volume:int,AHT:float,interval:int, lines: int)
#' 
#' @param agents - the number agents available in that interval
#' @param volume - the number of expected contacts in the interval
#' @param AHT - the average handle time per contact
#' @param interval - the number of time units  where the volume is expected
#' @param lines - total number of customers that can be in the system at the same time 

#' @export
erlang_cl <- function(agents, volume, AHT, interval = 1,lines) {
  volume = min(lines,volume) # only lines number of contacts can be accommodated at a time. 
  int <- intensity(volume, AHT, interval)
  erlang_b_inv <- 1
  for (i in 1:agents) {
    erlang_b_inv <- 1 + erlang_b_inv * i / int
  }
  erlang_b <- 1 / erlang_b_inv
  agents * erlang_b / (agents - int * (1 - erlang_b))
}

#' Calculate Erlang_X
#' 
#' This function calculates Erlangs using Erlang X 
#' 
#' Inputs: (agents: int, volume:int,AHT:float,interval:int, lines: int, patience:int, retrials: int)
#' 
#' @param agents - the number agents available in that interval
#' @param volume - the number of expected contacts in the interval
#' @param AHT - the average handle time per contact
#' @param interval - the number of time units  where the volume is expected
#' @param lines - total number of customers that can be in the system at the same time 
#' @param patience - The average time a customer is willing to wait in the queue
#' @param retrials - The probability that a customer who abandons, redials. (0 ≤ Retrials ≤ 1)

#' @export
erlang_x <- function(agents, volume, AHT, interval = 1,lines, patience,retrials) {
  volume = min(lines+retrials*(patience>interval)*volume,volume) # only lines number of contacts can be accommodated at a time. 
  int <- intensity(volume, AHT, interval)
  erlang_b_inv <- 1
  for (i in 1:agents) {
    erlang_b_inv <- 1 + erlang_b_inv * i / int
  }
  erlang_b <- 1 / erlang_b_inv
  agents * erlang_b / (agents - int * (1 - erlang_b))
}

#' Calculate Erlang_XD
#' 
#' This function calculates Erlangs using Erlang X considering the definition
#' 
#' Inputs: (agents: int, volume:int,AHT:float,interval:int, lines: int, patience:int, retrials: int, definition: boolean)
#' 
#' @param agents - the number agents available in that interval
#' @param volume - the number of expected contacts in the interval
#' @param AHT - the average handle time per contact
#' @param interval - the number of time units  where the volume is expected
#' @param lines - total number of customers that can be in the system at the same time 
#' @param patience - The average time a customer is willing to wait in the queue
#' @param retrials - The probability that a customer who abandons, redials. (0 ≤ Retrials ≤ 1)
#' @param definition - modes of calculating the service level (0-virtual SLA, 1 - answered SLA, 2 - offered sla)

#' @export
erlang_xd <- function(agents, volume, AHT, interval = 1,lines, patience,retrials,definition=2) {
  volume = min(lines+retrials*(patience>interval)*volume,volume) # only lines number of contacts can be accommodated at a time.
  int <- intensity(volume, AHT, interval)
  erlang_b_inv <- 1
  for (i in 1:agents) {
    erlang_b_inv <- 1 + erlang_b_inv * i / int
  }
  erlang_b <- 1 / erlang_b_inv
  agents * erlang_b / (agents - int * (1 - erlang_b))
}

#' Calculate Erlang_XV
#' 
#' This function calculates Erlangs using Erlang X considering the variance of the volume
#' 
#' Inputs: (agents: int, volume:int,AHT:float,interval:int, lines: int, patience:int, retrials: int, variance: float)
#' 
#' @param agents - the number agents available in that interval
#' @param volume - the number of expected contacts in the interval
#' @param AHT - the average handle time per contact
#' @param interval - the number of time units  where the volume is expected
#' @param lines - total number of customers that can be in the system at the same time 
#' @param patience - The average time a customer is willing to wait in the queue
#' @param retrials - The probability that a customer who abandons, redials. (0 ≤ Retrials ≤ 1)
#' @param variance - the variance of the forecasted volume. To be used if the distribution is not poisson.

#' @export
erlang_xv <- function(agents, volume, AHT, interval = 1,lines, patience,retrials,variance) {
  volume = min(lines+retrials*(patience>interval)*volume,volume) # only lines number of contacts can be accommodated at a time.
  int <- intensity(volume, AHT, interval)
  erlang_b_inv <- 1
  for (i in 1:agents) {
    erlang_b_inv <- 1 + erlang_b_inv * i / int
  }
  erlang_b <- 1 / erlang_b_inv
  agents * erlang_b / (agents - int * (1 - erlang_b))
}

#' Calculate Erlang_XDV
#' 
#' This function calculates Erlangs using Erlang X considering the patience, retrials and variance of the volume
#' 
#' Inputs: (agents: int, volume:int,AHT:float,interval:int, lines: int, patience:int, retrials: int, variance: float, definition:boolean)
#' 
#' @param agents - the number agents available in that interval
#' @param volume - the number of expected contacts in the interval
#' @param AHT - the average handle time per contact
#' @param interval - the number of time units  where the volume is expected
#' @param lines - total number of customers that can be in the system at the same time 
#' @param patience - The average time a customer is willing to wait in the queue
#' @param retrials - The probability that a customer who abandons, redials. (0 ≤ Retrials ≤ 1)
#' @param variance - the variance of the forecasted volume. To be used if the distribution is not poisson.
#' @param definition - modes of calculating the service level (0-virtual SLA, 1 - answered SLA, 2 - offered sla)

#' @export
erlang_xdv <- function(agents, volume, AHT, interval = 1,lines, patience,retrials,variance,definition) {
  volume = min(lines+retrials*(patience>interval)*volume,volume) # only lines number of contacts can be accommodated at a time.
  int <- intensity(volume, AHT, interval)
  erlang_b_inv <- 1
  for (i in 1:agents) {
    erlang_b_inv <- 1 + erlang_b_inv * i / int
  }
  erlang_b <- 1 / erlang_b_inv
  agents * erlang_b / (agents - int * (1 - erlang_b))
}

#' Calculate Service Level
#' 
#' This function calculates the expected service level as percentage. 
#' 
#' Inputs: (agents: int, volume:int,AHT:float,interval:int, lines: int, patience:int, retrials: int, variance: float, definition:boolean)
#' 
#' @param agents - the number agents available in that interval
#' @param volume - the number of expected contacts in the interval
#' @param AHT - the average handle time per contact
#' @param interval - the number of time units  where the volume is expected
#' @param lines - total number of customers that can be in the system at the same time 
#' @param patience - The average time a customer is willing to wait in the queue
#' @param retrials - The probability that a customer who abandons, redials. (0 ≤ Retrials ≤ 1)
#' @param variance - the variance of the forecasted volume. To be used if the distribution is not poisson.
#' @param definition - modes of calculating the service level (0-virtual SLA, 1 - answered SLA, 2 - offered sla)

#' @export
SL <- function(agents, volume, AHT, slaTarget, interval = 1,shrink = 0,lines = NULL, patience = NULL,retrials = NULL,variance = NULL,definition = NULL) {
  
  # agents = floor(agents*(1-shrink)) # factor for shrinkage
  
  if(is.null(lines)) pw <- erlang_c(agents, volume, AHT, interval)
  else if((is.null(patience)) & (is.null(retrials)) & (is.null(variance)) & (is.null(definition)) ) pw <- erlang_cl(agents, volume, AHT, interval,lines)
  else if(!(is.null(patience)) & !(is.null(retrials)) & (is.null(variance)) & (is.null(definition)) ) pw <- erlang_x(agents, volume, AHT, interval,lines,patience,retrials)
  else if(!(is.null(patience)) & !(is.null(retrials)) & (is.null(variance)) &!(is.null(definition)) ) pw <- erlang_xd(agents, volume, AHT, interval,lines,patience,retrials,definition)
  else if(!(is.null(patience)) & !(is.null(retrials)) &!(is.null(variance)) &(is.null(definition)) ) pw <- erlang_xv(agents, volume, AHT, interval,lines,patience,retrials,variance)
  else if(!(is.null(patience)) & !(is.null(retrials)) &!(is.null(variance)) &!(is.null(definition)) ) pw <- erlang_xdv(agents, volume, AHT, interval,lines,patience,retrials,variance,definition)
  
  
  int <- intensity(volume, AHT, interval)
  1 - (pw * exp(-(agents - int) * (slaTarget / AHT)))
}

#' Calculate Required FTEs
#' 
#' This function calculates the required FTEs in a certain interval
#' 
#' Inputs: (volume:int,AHT:float, slaTarget:float, slaTime:int,interval:int,shrink:float, lines: int, patience:int, retrials: int, variance: float, definition:boolean)
#'
#' @param volume - the number of expected contacts in the interval
#' @param AHT - the average handle time per contact
#' @param slaTarget - the service level target in percentage
#' @param slaTime - the maximum allowed waiting time in service level definition
#' @param interval - the number of time units  where the volume is expected
#' @param shrink description
#' @param lines - total number of customers that can be in the system at the same time 
#' @param patience - The average time a customer is willing to wait in the queue
#' @param retrials - The probability that a customer who abandons, redials. (0 ≤ Retrials ≤ 1)
#' @param variance - the variance of the forecasted volume. To be used if the distribution is not poisson.
#' @param definition - modes of calculating the service level (0-virtual SLA, 1 - answered SLA, 2 - offered sla)

#' @export
ReqFTE <- function(volume, AHT, slaTarget, slaTime, interval = 1,shrink=0,lines = NULL, patience = NULL,retrials = NULL,variance = NULL,definition = NULL) {
  agents <- round(intensity(volume, AHT, interval) + 1)
  gos <- SL(agents, volume, AHT, slaTarget, interval,lines,patience,retrials,variance,definition)
  while (gos < slaTarget) { #slaTime * (slaTime > 1) / 100
    agents <- agents + 1
    gos <- SL(agents, volume, AHT, slaTarget, interval,lines,patience,retrials,variance,definition)
  }
  return(agents/(1-shrink))
}

#' Calculate ASA
#' 
#' This function calculates the expected average speed of answering
#' 
#' Inputs: (agents: int, volume:int,AHT:float,interval:int, lines: int, patience:int, retrials: int, variance: float, definition:boolean)
#'
#' @param agents - the number agents available in that interval
#' @param volume - the number of expected contacts in the interval
#' @param AHT - the average handle time per contact
#' @param interval - the number of time units  where the volume is expected
#' @param shrink description
#' @param lines - total number of customers that can be in the system at the same time 
#' @param patience - The average time a customer is willing to wait in the queue
#' @param retrials - The probability that a customer who abandons, redials. (0 ≤ Retrials ≤ 1)
#' @param variance - the variance of the forecasted volume. To be used if the distribution is not poisson.
#' @param definition - modes of calculating the service level (0-virtual SLA, 1 - answered SLA, 2 - offered sla)

#' @export
ASA <- function(agents, volume, AHT, interval=1,lines = NULL, patience = NULL,retrials = NULL,variance = NULL,definition = NULL)
{
  if(is.null(lines)) pw <- erlang_c(agents, volume, AHT, interval)
  else if((is.null(patience)) & (is.null(retrials)) & (is.null(variance)) & (is.null(definition)) ) pw <- erlang_cl(agents, volume, AHT, interval,lines)
  else if(!(is.null(patience)) & !(is.null(retrials)) & (is.null(variance)) & (is.null(definition)) ) pw <- erlang_x(agents, volume, AHT, interval,lines,patience,retrials)
  else if(!(is.null(patience)) & !(is.null(retrials)) & (is.null(variance)) &!(is.null(definition)) ) pw <- erlang_xd(agents, volume, AHT, interval,lines,patience,retrials,definition)
  else if(!(is.null(patience)) & !(is.null(retrials)) &!(is.null(variance)) &(is.null(definition)) ) pw <- erlang_xv(agents, volume, AHT, interval,lines,patience,retrials,variance)
  else if(!(is.null(patience)) & !(is.null(retrials)) &!(is.null(variance)) &!(is.null(definition)) ) pw <- erlang_xdv(agents, volume, AHT, interval,lines,patience,retrials,variance,definition)
  
  Erlangs = intensity(volume, AHT, interval)
  
  pw*AHT/(agents-Erlangs)
}

#' Calculate CAI
#' 
#' This function calculates the expected percentage of calls/chats to be answered immediately
#' 
#' Inputs: (agents: int, volume:int,AHT:float,interval:int, lines: int, patience:int, retrials: int, variance: float, definition:boolean)
#'
#' @param agents - the number agents available in that interval
#' @param volume - the number of expected contacts in the interval
#' @param AHT - the average handle time per contact
#' @param interval - the number of time units  where the volume is expected
#' @param shrink description
#' @param lines - total number of customers that can be in the system at the same time 
#' @param patience - The average time a customer is willing to wait in the queue
#' @param retrials - The probability that a customer who abandons, redials. (0 ≤ Retrials ≤ 1)
#' @param variance - the variance of the forecasted volume. To be used if the distribution is not poisson.
#' @param definition - modes of calculating the service level (0-virtual SLA, 1 - answered SLA, 2 - offered sla)

#' @export
CAI <- function(agents, volume, AHT, interval=1,lines = NULL, patience = NULL,retrials = NULL,variance = NULL,definition = NULL) # calls answered immediately
{
  if(is.null(lines)) pw <- erlang_c(agents, volume, AHT, interval)
  else if((is.null(patience)) & (is.null(retrials)) & (is.null(variance)) & (is.null(definition)) ) pw <- erlang_cl(agents, volume, AHT, interval,lines)
  else if(!(is.null(patience)) & !(is.null(retrials)) & (is.null(variance)) & (is.null(definition)) ) pw <- erlang_x(agents, volume, AHT, interval,lines,patience,retrials)
  else if(!(is.null(patience)) & !(is.null(retrials)) & (is.null(variance)) &!(is.null(definition)) ) pw <- erlang_xd(agents, volume, AHT, interval,lines,patience,retrials,definition)
  else if(!(is.null(patience)) & !(is.null(retrials)) &!(is.null(variance)) &(is.null(definition)) ) pw <- erlang_xv(agents, volume, AHT, interval,lines,patience,retrials,variance)
  else if(!(is.null(patience)) & !(is.null(retrials)) &!(is.null(variance)) &!(is.null(definition)) ) pw <- erlang_xdv(agents, volume, AHT, interval,lines,patience,retrials,variance,definition)
  
  (1-pw)*100
}

#' Calculate Occupancy
#' 
#' This function calculates the expected level of occupancy
#' 
#' Inputs: (volume:int,AHT:float,interval:int,agents: int)
#'
#' @param volume - the number of expected contacts in the interval
#' @param AHT - the average handle time per contact
#' @param interval - the number of time units  where the volume is expected 
#' @param agents - the number agents available in that interval

#' @export
Occupancy <- function(volume, AHT, interval,agents) # maximum occupancy
  {
  erlangs = intensity(volume, AHT, interval)
  erlangs/agents
}


