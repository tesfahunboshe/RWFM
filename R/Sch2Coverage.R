#' Calculate Coverage
#' 
#' This function loads a dataframe with schedule and calculates the per - interval per day coverage
#' 
#' Inputs: (schedule:dataframe,nDays:int,intervals:int) 
#' 
#' @param schedule - a dataframe with fields startDay, endDay, startTime and endTime all in integers
#' @param nDays - the number of days considered
#' @param intervals - the number of intervals within the day (24 if hourly data) 
#' @export
coverage <- function(schedule,nDays = 7,intervals=24)
{

  `%pipe%` <- dplyr::`%>%`
  
  # create two copies
  schedule1<- schedule
  schedule2<- schedule
  
  # if end is less than start, end = end + intervals. 
  schedule1$TEnd <- ifelse(schedule1$Tstart > schedule1$TEnd, schedule1$TEnd + intervals, schedule1$TEnd) 
  
  # 1 if working in that interval, 0 otherwise. 
  for (i in c(0:0:(intervals-1)))
  {
    schedule1[as.character(i)] <- ifelse(schedule1$Tstart <= i & schedule1$TEnd > i, 1,0)
  }
  
  
  # schedule start is midnight. 
  # take only those shifts that overflow. end = start,end = 0 otherwise
  schedule2$TEnd <- ifelse(schedule2$Tstart > schedule2$TEnd, schedule2$TEnd, 0)
  schedule2$Tstart <- 0
  
  # the next day
  schedule2$DStart <- schedule2$DStart + 1
  schedule2$DEnd <- schedule2$DEnd + 1
  
  # 1 if working in that interval, 0 otherwise. 
  for (i in c(0:0:(intervals-1)))
  {
    schedule2[as.character(i)] <- ifelse(schedule2$Tstart <= i & schedule2$TEnd > i, 1,0)
  }
  
  
  schedule_12 <- rbind(schedule1, schedule2) # bind the two parts
  
  covrge <- data.frame(matrix(ncol = nDays, nrow = intervals)) # column interval
  colnames(covrge) <- lapply(1:nDays, paste0,"_Date")
  
  for (datee in 1:nDays)
  {
    covrge[paste0(datee,"_Date")] <- schedule_12 %pipe% dplyr::filter(DStart==datee) %pipe% dplyr::select(!(DStart:TEnd)) %pipe% colSums(na.rm = T)
    
  }
  
  return(covrge)
  
}