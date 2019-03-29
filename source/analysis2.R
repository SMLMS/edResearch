source("source/analysis.R")

Analysis2 <- setRefClass(
  "Analysis2",
  contains = "Analysis"
)

Analysis2$methods(statistics = function()
{
  q <<- data.frame(fragen = c("P(1.2.1)", "P(1.2.2)", "P(1.2.3)", "P(1.2.4)"))
  q <<- cbind(q, P=c(
    rawData$conProb(column = 'frage_1.2.1'),
    rawData$conProb(column = 'frage_1.2.2'),
    rawData$conProb(column = 'frage_1.2.3'),
    rawData$conProb(column = 'frage_1.2.4')
  ))
}
)

Analysis2$methods(graphics = function()
{
  rawData$plotBar(columns = c('frage_1.2.1', 'frage_1.2.2', 'frage_1.2.3', 'frage_1.2.4'), condition = 'none')
}
)