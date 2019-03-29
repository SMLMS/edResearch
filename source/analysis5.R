source("source/analysis.R")

Analysis5 <- setRefClass(
  "Analysis5",
  contains = "Analysis"
)

Analysis5$methods(statistics = function(frage = 1)
{
  q <<- data.frame(fragen = c(createColumnName(n=3, m=frage),
                              createColumnName(n=4, m=frage),
                              createColumnName(n=5, m=frage),
                              createColumnName(n=6, m=frage),
                              createColumnName(n=7, m=frage))
  )
  q <<- cbind(q, P=c(
    rawData$conProb(createColumnName(n=3, m=frage)),
    rawData$conProb(createColumnName(n=4, m=frage)),
    rawData$conProb(createColumnName(n=5, m=frage)),
    rawData$conProb(createColumnName(n=6, m=frage)),
    rawData$conProb(createColumnName(n=7, m=frage))
  ))
}
)

Analysis5$methods(graphics = function(frage = 1)
{
  rawData$plotBar(columns = c(createColumnName(n=3, m=frage),
                              createColumnName(n=4, m=frage),
                              createColumnName(n=5, m=frage),
                              createColumnName(n=6, m=frage),
                              createColumnName(n=7, m=frage))
                  , condition = 'none')
}
)