#'  Find Optimal Subset of Predictors to pass along to the Modeling Stage.  Based on CaretPackage RFE
#'
#' @param dataframe from which to run feature selection upon 
#' @export

caret_sis <- function(dataframe) { 
  library(caret) 
  
  if (!'toKeepColumns' %in% names(inputs)) {
    
    # a caret control Object that dictates the behavior or RFE
    # http://caret.r-forge.r-project.org/featureselection.html
    control <- rfeControl(functions = rfFuncs , method = "repeatedcv", repeats = 3, verbose = TRUE,
                        returnResamp = "final", number = 5)

    # Number of variables to fit in each sub-model. Sizes of 15 were arbitrarily chosen
    sizeBuckets <-seq(1,length(names(dataframe)),by = round(ncol(dataframe)/15))
   
    #to utilize ROC as a metric 
    rfFuncs$summary <-twoClassSummary 
    caretFuncs$summary <-twoClassSummary 
    

    profile.test <- rfe(dataframe[,names(dataframe)!='dep_var'], factor(dataframe$dep_var), sizes = sizeBuckets , rfeControl = control,metric='ROC') # ,method='gbm')  #if one wants to utilize gbm
    inputs$toKeepColumns <<- c('dep_var', predictors(profile.test))  
    eval(substitute( dataframe <- dataframe[,c('dep_var',predictors(profile.test)) ] ), envir = parent.frame() ) 
    
  }
  else { 
     #within the predict stage. Passing on columns filtered out by caret_sis 
      
     #done in case extra columns were removed relative to Training Stage  
     passedOnColumns <- intersect(inputs$toKeepColumns , names(dataframe))
     
     eval(substitute( dataframe <- dataframe[,passedOnColumns ] ), envir = parent.frame() ) 
  }

  

}


