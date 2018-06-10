#' Confusion Matrix Calculation Function
#'
#' This function calculates a confusion matrix for the provided predicted (in rows) and true (in columns) values. It is also obligatory to provide the actual number of classes. If some of the classes are missed, the function adds the necessary number of zero rows (in case of predictions) or zero columns (in case of true classes) to the classification matrix.
#' @param Ypred a vector of predicted values.
#' @param Ytrue a vector of actual values.
#' @param nclasses the number of actual classes.
#' @param CM.print a logical argument; whether to print the confusion matrix on the console or not.
#' @param sample.name is an optional character string specifying the name of the sample.
#' @keywords classification (confusion) matrix
#' @export
#' @examples
#' evCM(Ypred, Ytrue)

# TODO:


evCM <- function(Ypred, Ytrue, nclasses, CM.print=T, sample.name=""){

  ## check whether all categories were predicted
  v.r.incheck <- which(!(c(0:(nclasses-1)) %in% unique(Ypred)))-1
  v.c.incheck <- which(!(c(0:(nclasses-1)) %in% unique(Ytrue)))-1

  ## composing the confusion matrix
  tab.CM <- table(Ypred, Ytrue)
  
  ## checking the CM for missed Ypred classes
  if(length(v.r.incheck)!=0){
    tab.r0add <- as.table(matrix(0, nrow=length(v.r.incheck), ncol=ncol(tab.CM)))
    rownames(tab.r0add) <- v.r.incheck
    tab.CM <- rbind(tab.CM, tab.r0add)
    tab.CM <- tab.CM[order(as.numeric(rownames(tab.CM))),]
  } # end if in-check
  
  ## checking the CM for missed Ytrue classes
  if(length(v.c.incheck)!=0){
    tab.c0add <- as.table(matrix(0, ncol=length(v.c.incheck), nrow=nrow(tab.CM)))
    colnames(tab.c0add) <- v.c.incheck
    tab.CM <- cbind(tab.CM, tab.c0add)
    tab.CM <- tab.CM[ , order(as.numeric(colnames(tab.CM)))]
  } # end if in-check
  
  
  ##################################
  ## printing out the CM
  if(CM.print){
    print(paste0("*** The CM for sample ***", sample.name))
    print(tab.CM)
  }
  
  ## returning the result
  return(tab.CM)

} # end of funcCM
