source("source/analysis.R")

Analysis4 <- setRefClass(
  "Analysis4",
  contains = "Analysis",
  fields = list(
    posterior = "data.frame"
  )
)

Analysis4$methods(expandRawData = function()
{
  rawData$filterData(column = 'none')
  dss <- rawData$getFilteredData()
  q <<- data.frame(Probennummer = c(1:11))
  for (i in 1:4){
    tempdss <- dss %>% select(
      createColumnName(n=3, m=i),
      createColumnName(n=4, m=i),
      createColumnName(n=5, m=i),
      createColumnName(n=6, m=i),
      createColumnName(n=7, m=i))
    tmpVec <- vector() 
    for (j in 1:11){
      tmpVec <- c(tmpVec, any(tempdss[j,] == TRUE, na.rm = TRUE))
    }
    q <<- cbind(q, tmpVec)
  }
  names(q) <<- c("Probennummer", "Verstaendnis", "Vorbereitung", "Angst_Gruppe", "Angst_Dozent")
  rawData$setRawData(merge(dss, q))
  rawData$filterData(column = 'none')
}
)

Analysis4$methods(statistics = function()
{
  posterior <<- data.frame(datum = c("Verstaendnis", "Vorbereitung", "Angst_Gruppe", "Angst_Dozent"),
                           p = c(0,0,0,0))
  for (i in 1:4){
    print(i)
    rawData$setHypothesis("Hypothese_1.8")
    rawData$setDatum(as.character(posterior$datum[i]))
    print(cat("Ursache ", as.character(posterior$datum[i], "\n")))
    posterior$p[i] <<- rawData$bayesianStatistics()
  }
}  
)

Analysis4$methods(graphics = function()
{
  fig <- ggplot(data=posterior) +
    geom_bar(
      mapping = aes(x=datum, y=p),
      stat = 'identity'
    )+
    xlab("D")+
    ylab("P(H=TRUE | D=TRUE)")+
    theme(text = element_text(size=20),axis.text.x = element_text(angle=30, hjust=1))
  plot(fig)
}
)