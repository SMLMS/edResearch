source("source/analysis1.R")
source("source/analysis2.R")
source("source/analysis3.R")
source("source/analysis4.R")
source("source/analysis5.R")

main <- function(question = "1")
{
  switch (question,
    "1" = {
      q1 <- Analysis1$new()
      q1$init()
      q1$statistics()
      q1$graphics()
      q1$printStatistics()
    },
    "2" = {
      q1 <- Analysis2$new()
      q1$init()
      q1$statistics()
      q1$graphics()
      q1$printStatistics()
    },
    "3" = {
      q1 <- Analysis3$new()
      q1$init()
      q1$expandRawData()
      q1$statistics()
    },
    "4" = {
      q1 <- Analysis4$new()
      q1$init()
      q1$expandRawData()
      q1$statistics()
      q1$graphics()
    },
    "5" = {
      q1 <- Analysis5$new()
      q1$init()
      for (i in 1:4){
        q1$statistics(frage=i)
        q1$graphics(frage=i)
        q1$printStatistics()
      }
    }
  )
}

main(question = "4")


