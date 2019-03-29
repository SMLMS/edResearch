source("source/dataSet.R")

Analysis <- setRefClass(
  "Analysis",
  fields = list(
    rawData = "DataSet",
    q = "data.frame"
  )
)

Analysis$methods(init = function()
{
  rawData$chooseFileName()
  rawData$importExcel()
}
)

Analysis$methods(printStatistics = function()
{
  print(q)
}  
)

Analysis$methods(createColumnName = function(n = 1, m = 1)
{
  s <- "frage_1."
  s <- paste0(s, n)
  s <- paste0(s, ".")
  s <- paste0(s, m)
  return(s)
}  
)
