source("source/analysis.R")

Analysis1 <- setRefClass(
  "Analysis1",
  contains = "Analysis"
)

Analysis1$methods(statistics = function()
{
  q <<- data.frame(fragen = c("P(1.1)", "P(1.8)", "P(1.8|1.1)"))
  q <<- cbind(q, P=c(
    rawData$conProb(column = 'frage_1.1'),
    rawData$conProb(column = 'frage_1.8'),
    rawData$conProb(column = 'frage_1.8', condition = 'frage_1.1')
  ))
}
)

Analysis1$methods(graphics = function()
{
  rawData$plotColumn(column = 'frage_1.1')
  rawData$plotColumn(column = 'frage_1.8')
  rawData$plotColumn(column = 'frage_1.8', condition = 'frage_1.1')
}
)