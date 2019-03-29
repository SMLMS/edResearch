library(tcltk2)
library(readxl)
library(pracma)
library(tidyverse)
library(ggplot2)


DataSet <- setRefClass(
  "DataSet",
  fields = list(
    fileName = "character",
    folderName = "character",
    baseName = "character",
    rawData = "data.frame",
    hypothesis = "character",
    datum = "character",
    filteredData= "data.frame"
  )
)

DataSet$accessors("rawData")
DataSet$accessors("filteredData")
DataSet$accessors("hypothesis")
DataSet$accessors("datum")

# print functions
DataSet$methods(printFileName = function()
{
  cat("\nfileName = ",fileName,"\n")
}
)

DataSet$methods(printFolderName = function()
{
  cat("\nfolderName = ",folderName,"\n")
}  
)

DataSet$methods(printBaseName = function()
{
  cat("\nbaseName = ",baseName,"\n")
}  
)

DataSet$methods(printDataSet = function()
{
  printFileName()
  printFolderName()
  printBaseName()
}  
)

# special functions
DataSet$methods(chooseFileName = function()
{
  fileName <<- tclvalue(
    tkgetOpenFile(
      filetypes = "{ {excel Files} {.xlsx} } { {All Files} * }"
      )
    )
  if (!nchar(fileName)) {
    tkmessageBox(message = "No file was selected!")
    return(FALSE)
  }
  else {
    getFolderName()
    getBaseName()
    return(TRUE)
  }
}
)

DataSet$methods(getBaseName = function()
{
  baseName <<- tools::file_path_sans_ext(basename(fileName))
  baseName <<- paste(format(Sys.time(), "%Y%m%d_%H%M%S_"),baseName, sep="")
}  
)

DataSet$methods(getFolderName = function()
{
  folderName <<- dirname(fileName)  
}
)

DataSet$methods(importExcel = function()
{
  if (nchar(fileName))
  {
    x <- read_excel(
      fileName,
      sheet="Lehrforschung_Results_Malkusch",
      col_types = rep("text", 27)
      )
    x <- as.data.frame(x)
    for (i in colnames(x)){
      if (strcmp(i, "Probennummer"))
      {
        x[,i] <- as.numeric(x[,i])
      }
      else
      {
        x[,i] <- as.logical(x[,i])
      }
      rawData <<- x
    }
    
    return(TRUE)
  }
  else
  {
    cat("choose fileName first!\n")
    return(FALSE)
  }
} 
)

DataSet$methods(filterData = function(column = 'none', value=TRUE)
{
  if (strcmp(column, 'none'))
  {
    filteredData <<- rawData
  }
  else
  {
    filteredData <<- filter(rawData, rawData[,column]==value)
  }
}
)

DataSet$methods(plotColumn = function(column='none', condition ='none')
{
  filterData(column = condition)
  theme_set(theme_classic())
  pie <- ggplot(filteredData, aes(x = "", fill = filteredData[,column])) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    theme(axis.line = element_blank(), 
          plot.title = element_text(hjust=0.5)) + 
    labs(fill=column, 
         x=NULL, 
         y=NULL, 
         title='Kochendiagramm', 
         caption= paste('Bedingung:', condition, sep=' '))
  plot(pie + coord_polar(theta = "y", start=0))
} 
)

DataSet$methods(plotBar = function(columns = c('frage_1.1', 'frage_1.8'), condition = 'none')
{
  filterData(column = condition)
  dss <- subset(filteredData, select=columns)
  causes <- data.frame(
    fragen = columns,
    P=as.numeric(colSums(dss, na.rm = TRUE))/nrow(dss)
    )

  fig <- ggplot(data=causes) +
    geom_bar(
      mapping = aes(x=fragen, y=P),
      stat = 'identity'
    )
  plot(fig)
}
)

DataSet$methods(conProb = function(column='none', condition = 'none', value='TRUE')
{
  filterData(column = condition)
  return(table(filteredData[,column])[value]/length(filteredData[,column]))
}
)


DataSet$methods(bayesianStatistics = function()
{
  prior <- conProb(column = hypothesis)
  normalizor <- conProb(column = datum)
  likelihood <- conProb(column = datum, condition = hypothesis)
  posterior <- prior*likelihood/normalizor
  print(
    cat(
      "\nP(",hypothesis,") = ",prior,
      "\nP(",datum,") = ",normalizor,
      "\nP(",datum,"|",hypothesis,") = ",likelihood,
      "\nP(",hypothesis,"|",datum,") = ",posterior,
      "\n"
    )
  )
  return(posterior)
}  
)
