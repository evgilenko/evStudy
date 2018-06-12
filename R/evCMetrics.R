#' Function for CM metrics calculation
#'
#' This function calculates a number of specified metrics (precision, recall, and f1-score) for a given confusion matrix. R^2 count is also calculated.
#' 
#' Important! The confusion matrix should have no missing rows or columns and should be square!
#' 
#' @param CM a confusion matrix.
#' @param cat.names a vector of category names (optional).
#' @param s.expname character string giving the name of experiment (optional).
#' @keywords classification metrics
#' @export
#' @examples
#' evCMetrics(myCM)

evCMetrics <- function(CM, cat.names=vector(mode="character", length=0), s.expname=""){

  ## input checks
  if(ncol(CM)!=nrow(CM)){stop("CM is not square!")}
  
  v.metrics <- c("Precision", "Recall", "F1-score")
  # also check cats by row with cats by columns (use %in%)
  
  ## go through CM
  v.colsums <- apply(CM, 2, FUN=sum)
  v.rowsums <- apply(CM, 1, FUN=sum)
  
  v.diagCM <- vector(mode="integer", length=0)
  for(j.CMcol in c(1:ncol(CM))){
    v.diagCM <- c(v.diagCM, CM[j.CMcol,j.CMcol])
  } # end for j.CMcol
  
  ## R^2 count
  c.r2count <- round(sum(v.diagCM)*100/sum(CM),2)
  
  ## calculate precisions
  v.precs <- round(100*v.diagCM/v.rowsums, 2)
  v.precs[is.nan(v.precs)] <- 0
  
  ## calculate recalls
  v.recalls <- round(100*v.diagCM/v.colsums, 2)
  v.recalls[is.nan(v.recalls)] <- 0

  ## calculate f1-scores
  v.f1s <- 2*v.precs*v.recalls/(v.precs+v.recalls)
  v.f1s[is.nan(v.f1s)] <- 0
  
  ## composing the output table of sensitivities
  tab.precs <- as.data.frame(t(c(c.r2count, round(mean(v.precs),2), v.precs)))
  tab.recalls <- as.data.frame(t(c(c.r2count, round(mean(v.recalls),2), v.recalls)))
  tab.f1s <- as.data.frame(t(c(c.r2count, round(mean(v.f1s),2), v.f1s)))
  
  tab.res <- round(rbind(tab.precs, tab.recalls, tab.f1s), 2)
  tab.res <- cbind(rep(s.expname, 3), v.metrics, tab.res)
  colnames(tab.res) <- c("Experiment", "Metric", "R2c", "Average", c(0:(ncol(CM)-1)))
  rownames(tab.res) <- 
  
  return(tab.res)
  
}
