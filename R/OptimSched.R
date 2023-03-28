
#' Calculate Optimum Schedule
#' 
#' This function generates the optimum schedule shells for contract based workforce
#' 
#' Inputs: (cap_plan:dataframe,shiftLength:float,weeklyWorkingDays:int,minEfficiency:float, intervals: int,partTime:boolean,minShiftLength:int) 
#' 
#' @param cap_plan - the per-interval headcount requirement
#' @param shiftLength - How long a single shift is interms of the chosen intervals. Default value is 8 for 8hrs considering hourly capacity plan
#' @param weeklyWorkingDays - Number of contract shifts in a single week
#' @param minEfficiency - Minimum schedule efficiency to be generated
#' @param intervals - the number of intervals within the day (24 if hourly data)
#' @param partTime - T - if part-time shifts are allowed, F, otherwise.  
#' @param minShiftLength - the minimum shift length. Not needed if PartTime = F
#' @export
OptimumSchedule <- function(cap_plan, shiftLength = 8,weeklyWorkingDays = 5,minEfficiency = 0.1,intervals = 24,partTime = F,minShiftLength = 0) # input = dataframe with 1- hr interval FTE requirement for 1 week
{
  
  `%pipe%` <- dplyr::`%>%`
  nDays = ncol(cap_plan)
  
  # initializing variables
  ScheduleShells <- data.frame(AgentNumber = numeric(), days = numeric(),shift = numeric(),shiftLength = numeric())
  TotalCoverage <- data.frame(matrix(0, ncol = nDays, nrow = intervals))
  colnames(TotalCoverage)=colnames(cap_plan) = columnNames <- lapply(1:nDays, paste0,"_Date")
  
  n = 0 # agent number
  totalEff <- 0 # total efficinecy
  DaysCombo <- combn(1:nDays, weeklyWorkingDays) # all possible days combinations
  
  # iterate to find the best combinations
  while(totalEff < minEfficiency){ # loop until totalEff >= minEfficiency
    n = n+1 # next agent
    
    effi = list()
    covrge = data.frame(Interval = 0:(intervals-1))
    
    for (startTime in 0:(intervals-1))
    {
      # define days
      
      if(!partTime)
      {
        
        for (i in 1:ncol(DaysCombo))
        {
          dayStart = dayEnd <- DaysCombo[,i]
          endTime <- (startTime+shiftLength)%%intervals
          
          # schedule definition
          sched <- data.frame(DStart = dayStart,
                              DEnd = dayEnd,
                              Tstart = startTime,
                              TEnd = endTime)
          # add to existing coverage
          index <- paste(startTime,i,sep = "_")
          
          # update the newTotalCov
          newTotalCov <- TotalCoverage + coverage(sched,nDays,intervals)
          colnames(newTotalCov) <- lapply(columnNames, paste0,index)
          covrge <- cbind(covrge,newTotalCov)
          
          # check efficiency
          effi[index] <- accuracy_F(newTotalCov,cap_plan)
        }
      }
      
      else if(partTime)
      {
        for(i in 1:ncol(DaysCombo))
        {
          dayStart = dayEnd <- DaysCombo[,i]
          
          for( shiftL in minShiftLength:shiftLength)          {
            
            endTime <- startTime+shiftL
            
            # schedule definition
            sched <- data.frame(DStart = dayStart,
                                DEnd = dayEnd,
                                Tstart = startTime,
                                TEnd = endTime)
            # add to existing coverage
            index <- paste(startTime,i,sep = "_")
            index <- paste(index,shiftL,sep = "*")
            
            # update the newTotalCov
            newTotalCov <- TotalCoverage + coverage(sched,nDays,intervals)
            colnames(newTotalCov) <- lapply(columnNames, paste0,index)
            
            covrge <- cbind(covrge,newTotalCov)
            
            # check efficiency
            effi[index] <- accuracy_F(newTotalCov,cap_plan)
          }
        }
      }
      
      
      
      
    }
    maxEff <- effi[which.max(effi)] # maximum efficiency
    
    if(as.numeric(maxEff)>=as.numeric(totalEff))
    {
      totalEff <- as.numeric(maxEff) # update the totalEff
      
      bestcombo <- names(which.max(effi))
      TotalCoverage <- covrge %pipe% dplyr::select(ends_with(paste0("e",bestcombo)))
      
      # add to the schedule shells
      if(!partTime)
      {
        ScheduleShells <- ScheduleShells %pipe% tibble::add_row(AgentNumber = n, 
                                                     days = DaysCombo[,as.numeric(substring(bestcombo,which(strsplit(bestcombo, "")[[1]]=="_")+1,nchar(bestcombo)))],
                                                     shift = as.numeric(substring(bestcombo,1,which(strsplit(bestcombo, "")[[1]]=="_")-1)),
                                                     shiftLength = shiftLength)
      }
      
      if(partTime)
      {
        ScheduleShells <- ScheduleShells %pipe% tibble::add_row(AgentNumber = n, 
                                                     days = DaysCombo[,as.numeric(substring(bestcombo,which(strsplit(bestcombo, "")[[1]]=="_")+1,
                                                                                            which(strsplit(bestcombo, "")[[1]]=="*")-1))],
                                                     shift = as.numeric(substring(bestcombo,1,which(strsplit(bestcombo, "")[[1]]=="_")-1)),
                                                     shiftLength = as.numeric(substring(bestcombo,which(strsplit(bestcombo, "")[[1]]=="*")+1,nchar(bestcombo))) )
      }
      
      
    }
    
    else { # break if the efficiency is not improving
      break
    }
    
  }
  
  # Function to cut parts of the header names.
  find_e <- function(text){
    short <- substring(text,1,which(strsplit(text, "")[[1]]=="e"))
    return(short)
  }
  
  colnames(TotalCoverage) <- lapply(colnames(TotalCoverage), find_e) # rename columns
  
  
  TotalCoverage$Interval<- 0:(intervals-1) # add a column
  cap_plan['Interval'] <- 0:(intervals-1)
  
  coverage_melt<- tidyr::pivot_longer(TotalCoverage, -c(Interval), values_to = "StaffedHeads", names_to = "Date") # melt to long format
  capPlan_melt <- tidyr::pivot_longer(cap_plan, -c(Interval), values_to = "RequiredHeads", names_to = "Date") # melt to long format
  
  Requ_cover<- dplyr::full_join(capPlan_melt,coverage_melt, by=c("Date","Interval")) # join the two tables.
  
  
  ## Plot
  p = ggplot2::ggplot(Requ_cover, ggplot2::aes(x = Interval)) +
    ggplot2::geom_col(ggplot2::aes(y = StaffedHeads),fill="white",color="darkblue")+
    ggplot2::geom_line(ggplot2::aes(y = RequiredHeads),colour="red", linewidth = 1.5)+
    ggplot2::facet_wrap(~Date,ncol = 2)+
    ggplot2::scale_x_continuous(breaks = seq(0,(intervals-1),2))
  
  results <- list()
  results$agents <- n
  results$coverage <-Requ_cover
  results$plot <-p
  results$schedule <-ScheduleShells
  results$efficiency <-totalEff
  return(results) 
  
  
}


#' Generate gig-economy type schedule
#' 
#' This function generates the optimum schedule shells for gig economy workforce. 
#' 
#' Inputs: (cap_plan:dataframe,shiftLength:float,weeklyWorkingDays:int,minEfficiency:float, intervals: int) 
#' 
#' @param cap_plan - the per-interval headcount requirement
#' @param shiftLength - How long a single shift is interms of the chosen intervals. Default value is 8 for 8hrs considering hourly capacity plan
#' @param weeklyWorkingDays - Number of contract shifts in a single week
#' @param minEfficiency - Minimum schedule efficiency to be generated
#' @param intervals - the number of intervals within the day (24 if hourly data)
#' @export
gigSchedule <- function(cap_plan, shiftLength = 8,minEfficiency = 0.1,intervals = 24,minShiftLength = 7) # input = dataframe with 1- hr interval FTE requirement for 1 week
{
  
  `%pipe%` <- dplyr::`%>%`
  
  nDays = ncol(cap_plan)
  
  # initializing variables
  ScheduleShells <- data.frame(day = numeric(),shift = numeric(),shiftLength = numeric())
  TotalCoverage <- data.frame(matrix(0, ncol = nDays, nrow = intervals))
  colnames(TotalCoverage)=colnames(cap_plan) = columnNames <- lapply(1:nDays, paste0,"_Date")
  
  n = 0 # agent number
  totalEff <- 0 # total efficinecy
  
  # iterate to find the best combinations
  
  covrge = data.frame(Interval = 0:(intervals-1))
  # define days
  for(i in 1:nDays)
  {
    while(totalEff < minEfficiency)
    {
      n = n+1 # next agent 
      effi = list()
      for (startTime in 0:(intervals-1))
      {
        
        for( shiftL in minShiftLength:shiftLength)
        {
          
          endTime <- (startTime+shiftL)%%intervals
          
          # schedule definition
          sched <- data.frame(DStart = i,
                              DEnd = i,
                              Tstart = startTime,
                              TEnd = endTime)
          # add to existing coverage
          index <- paste(startTime,i,sep = "_")
          index <- paste(index,shiftL,sep = "*")
          index <- paste(index,n,sep = "#")
          
          # update the newTotalCov
          newTotalCov <- TotalCoverage + coverage(sched,nDays,intervals)
          colnames(newTotalCov) <- lapply(columnNames, paste0,index)
          covrge <- cbind(covrge,newTotalCov)
          
          # check efficiency
          effi[index] <- accuracy_F(newTotalCov,cap_plan)
        }
      }
      
      maxEff <- effi[which.max(effi)] # maximum efficiency
      
      if(as.numeric(maxEff)>=as.numeric(totalEff))
      {
        totalEff <- as.numeric(maxEff) # update the totalEff
        
        bestcombo <- names(which.max(effi))
        TotalCoverage <- covrge %pipe% dplyr::select(dplyr::ends_with(paste0("e",bestcombo)))
        
        # add to the schedule shells
        ScheduleShells <- ScheduleShells %pipe% tibble::add_row(day = as.numeric(substring(bestcombo,which(strsplit(bestcombo, "")[[1]]=="_")+1,
                                                                                which(strsplit(bestcombo, "")[[1]]=="*")-1)),
                                                     shift = as.numeric(substring(bestcombo,1,which(strsplit(bestcombo, "")[[1]]=="_")-1)),
                                                     shiftLength = as.numeric(substring(bestcombo,which(strsplit(bestcombo, "")[[1]]=="*")+1,nchar(sub("#.*", "", bestcombo)))) )
        
        
        
      }
      
      else { # break if the efficiency is not improving gsub(".*\* (.+) \#.*", "\\1", bestcombo)
        break
      }
    }
    
    
  }
  
  
  
  # Function to cut parts of the header names.
  find_e <- function(text){
    short <- substring(text,1,which(strsplit(text, "")[[1]]=="e"))
    return(short)
  }
  
  colnames(TotalCoverage) <- lapply(colnames(TotalCoverage), find_e) # rename columns

  
  TotalCoverage$Interval<- 0:(intervals-1) # add a column
  cap_plan['Interval'] <- 0:(intervals-1)
  
  coverage_melt<- tidyr::pivot_longer(TotalCoverage, -c(Interval), values_to = "StaffedHeads", names_to = "Date") # melt to long format
  capPlan_melt <- tidyr::pivot_longer(cap_plan, -c(Interval), values_to = "RequiredHeads", names_to = "Date") # melt to long format
  
  Requ_cover<- dplyr::full_join(capPlan_melt,coverage_melt, by=c("Date","Interval")) # join the two tables. 
  
  ## Plot
  p = ggplot2::ggplot(Requ_cover, ggplot2::aes(x = Interval)) +
    ggplot2::geom_col(ggplot2::aes(y = StaffedHeads),fill="white",color="darkblue")+
    ggplot2::geom_line(ggplot2::aes(y = RequiredHeads),colour="red", linewidth = 1.5)+
    ggplot2::facet_wrap(~Date,ncol = 2)+
    ggplot2::scale_x_continuous(breaks = seq(0,(intervals-1),2))
  
  results <- list()
  results$coverage <-Requ_cover
  results$plot <-p
  results$schedule <-ScheduleShells
  results$efficiency <-totalEff
  return(results)
}