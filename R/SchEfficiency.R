#' Calculate Accuracy
#' 
#' This function loads two dataframes and calculates the percentage simmilarity between their
#' corresponding elements. It is important that the dataframes are of the same size and column ids
#' 
#' Inputs: (coverage:dataframe,capacityPlan:dataframe) 
#' 
#' @param coverage - the per interval number of agents scheduled
#' @param capacityPlan - the per-interval headcount requirement

#' @export
accuracy_F <- function(coverage,capacityPlan)
{
  accuracy <- 1 - (sum(abs(coverage-capacityPlan),na.rm = T)/sum(capacityPlan,na.rm = T))
  return(accuracy)
  
}