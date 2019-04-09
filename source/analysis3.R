source("source/analysis.R")

Analysis3 <- setRefClass(
  "Analysis3",
  contains = "Analysis"
)

Analysis3$methods(expandRawData = function()
{
  rawData$filterData(column = 'none')
  dss <- rawData$getFilteredData()
  q <<- data.frame(students = rownames(dss))
  for (i in 3:7){
    start <- createColumnName(n=i, m=1)
    stop <- createColumnName(n=i, m=4)
    tempdss <- dss %>% select(start:stop)
    q <<- cbind(q, p=(rowSums(tempdss, na.rm = TRUE) > 0L))
  }
  names(q) <<- c("Probennummer", "Hypothese_1.3", "Hypothese_1.4", "Hypothese_1.5", "Hypothese_1.6", "Hypothese_1.7")
  rawData$setRawData(merge(dss, q))
  rawData$filterData(column = 'none')
}
)

Analysis3$methods(statistics = function()
{ 
  for (i in 3:7){
    d <- paste0("Hypothese_1.", i)
    rawData$setHypothesis("Hypothese_1.8")
    rawData$setDatum(d)
    print(cat("Ursache ", d))
    rawData$bayesianStatistics()
  }
}  
)


Analysis3$methods(graphics = function()
{
  rawData$plotBar(columns = c('Hypothese_1.2.1', 'Hypothese_1.2.2', 'Hypothese_1.2.3', 'Hypothese_1.2.4'), condition = 'none')
}
)