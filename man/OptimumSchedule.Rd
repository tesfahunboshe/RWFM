% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OptimSched.R
\name{OptimumSchedule}
\alias{OptimumSchedule}
\title{Calculate Optimum Schedule}
\usage{
OptimumSchedule(
  cap_plan,
  shiftLength = 8,
  weeklyWorkingDays = 5,
  minEfficiency = 0.1,
  intervals = 24,
  partTime = F,
  minShiftLength = 0
)
}
\arguments{
\item{cap_plan}{\itemize{
\item the per-interval headcount requirement
}}

\item{shiftLength}{\itemize{
\item How long a single shift is interms of the chosen intervals. Default value is 8 for 8hrs considering hourly capacity plan
}}

\item{weeklyWorkingDays}{\itemize{
\item Number of contract shifts in a single week
}}

\item{minEfficiency}{\itemize{
\item Minimum schedule efficiency to be generated
}}

\item{intervals}{\itemize{
\item the number of intervals within the day (24 if hourly data)
}}

\item{partTime}{\itemize{
\item T - if part-time shifts are allowed, F, otherwise.
}}

\item{minShiftLength}{\itemize{
\item the minimum shift length. Not needed if PartTime = F
}}
}
\description{
This function generates the optimum schedule shells for contract based workforce
}
\details{
Inputs: (cap_plan:dataframe,shiftLength:float,weeklyWorkingDays:int,minEfficiency:float, intervals: int,partTime:boolean,minShiftLength:int)
}
