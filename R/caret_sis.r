#'  Find Optimal Subset of Predictors to pass along to the Modeling Stage.  Based on CaretPackage RFE
#'
#' @param dataframe  
#' @param digits how many decimal places you would like your numeric number to contain after its been truncated
#' @export

caret_sis <- function(dataframe) { 
  library(caret) 
  
  # for debugging pruposes 
  #  process = run('SISdir/1.1', to = 'data/same value') 
  #  dataframe=process$after$data
  
  # a caret control Object that dictates the behavior or RFE
  # http://caret.r-forge.r-project.org/featureselection.html
  control <- rfeControl(functions = rfFuncs , method = "repeatedcv", repeats = 3, verbose = TRUE,
                      returnResamp = "final", number = 5)

  sizeBuckets <-seq(1,length(names(dataframe)),by = round(ncol(dataframe)/15))
 
  #to utilize ROC as a metric 
  rfFuncs$summary <-twoClassSummary 
  caretFuncs$summary <-twoClassSummary 

  profile.test <- rfe(dataframe[,names(dataframe)!='dep_var'], factor(dataframe$dep_var), sizes = sizeBuckets , rfeControl = control,metric='ROC') # ,method='gbm')  #if one wants to utilize gbm

  eval(substitute( dataframe <- dataframe[,predictors(profile.test)] ), envir = parent.frame() ) 
}


