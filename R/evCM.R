#' Confusion Matrix Calculation Function
#'
#' This function calculates a confusion matrix for the provided predicted (in rows) and true (in columns) values. It is also obligatory to provide the actual number of classes. If some of the classes are missed in predictions, the function adds a row of zeroes to the classification matrix.
#' The function returns a data.frame containing (in the percentage format) overall R^2 count, average sensitivity over categories, and individual sensitivities for each of the classes (by columns of CM).
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
  v.incheck <- which(!(c(0:(nclasses-1)) %in% unique(Ypred)))-1
  
  ## composing the confusion matrix
  tab.CM <- table(Ypred, Ytrue)
  
  ## checking the CM
  if(length(v.incheck)!=0){
    print("... !!! length of v.incheck != 0")
    tab.0add <- as.table(matrix(0, nrow=length(v.incheck), ncol=ncol(tab.CM)))
    rownames(tab.0add) <- v.incheck
    tab.CM <- rbind(tab.CM, tab.0add)
    tab.CM <- tab.CM[order(as.numeric(row.names(tab.CM))),]
  } # end if in-check
  
  ## printing out the CM
  if(CM.print){
    print(paste0("*** The CM for sample ***", sample.name))
    print(tab.CM)
  }
  
  ## get the diagonal and the sums of columns of CM
  v.diagCM <- vector(mode="integer", length=0)
  
  for(j.CMcol in c(1:ncol(tab.CM))){
    v.diagCM <- c(v.diagCM, tab.CM[j.CMcol,j.CMcol])
  } # end for j.CMcol
  
  ## get the sums of columns of CM
  v.sumsCM <- apply(tab.CM, 2, FUN=sum)
  v.senses <- round(100*v.diagCM/v.sumsCM, 2)
  c.r2count <- round(sum(v.diagCM)*100/sum(tab.CM),2)
  
  ## composing the output table of sensitivities
  tab.senses <- as.data.frame(t(c(c.r2count, mean(v.senses), v.senses)))
  colnames(tab.senses) <- c("R2c,%", "AvgSens,%", c(0:(nclasses-1)))
  
  ## returning the result
  return(tab.senses)

} # end of funcCM
