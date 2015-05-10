#' An Event Study Analysis Function
#'
#' This function runs event study calculations using the standard market model.
#' @param df.events The data frame containing the list of events.
#' @keywords event study analysis
#' @export
#' @examples
#' evStudy()


evStudy <- function(df.events, df.stocks, event.window = 2, estim.window = 100){ 

df.ars <- data.frame(event.day = c(-seq(from=event.window, to = 1),0,seq(1:event.window),"sigma2e"))

## calculate market returns
markPrice <- df.stocks[, 2]
markRet <- c(NA, diff(markPrice) / markPrice[-length(markPrice)])


for(evId in 1:nrow(df.events)){
    
    # calculate security returns
    firmPrice <- df.stocks[, df.events$firm_id[evId]]
    firmRet <- c(NA, diff(firmPrice) / firmPrice[-length(firmPrice)])
    
    # create estimation and prediction subsamples
    evDate <- which(df.stocks$Date == df.events$eventdate[evId])
    est.sample <- seq(from=evDate-event.window-estim.window, to=evDate-event.window-1)
    pred.sample <- seq(from=evDate-event.window, to=evDate+event.window)
    
    # estimation of market model
    m.model <- lm(firmRet[est.sample]~markRet[est.sample])
    df.ars$ret <- c(firmRet[pred.sample]-(m.model$coef[1]+m.model$coef[2]*markRet[pred.sample]), summary(m.model)$sigma^2)
    
    names(df.ars)[ncol(df.ars)]<-paste0(df.events$firm_id[evId],"_ev",df.events$event_id[evId])
    
    
}#--- end for evId

# calculate average AR by firms
Nfirms <- length(2:ncol(df.ars))
df.ars$AvAR<-apply(df.ars[,2:ncol(df.ars)],1,mean)
df.ars$AvAR[nrow(df.ars)]<-df.ars$AvAR[nrow(df.ars)]/Nfirms

# calculate CARs
df.ars$CAR <- df.ars$AvAR[1]

for(i in 3:nrow(df.ars)){
    df.ars$CAR[i-1]<-df.ars$CAR[i-2]+df.ars$AvAR[i-1]
}


df.ars$CAR[nrow(df.ars)]<-(2*event.window+1)*df.ars$AvAR[nrow(df.ars)]
plot.ts(df.ars$CAR[-nrow(df.ars)], ylim =c(-0.1,0.1), ylab="CAR")

theta.stat <- df.ars$CAR[nrow(df.ars)-1]/sqrt(df.ars$CAR[nrow(df.ars)])
print(paste0("Theta statistic: ", round(theta.stat,3)))
return(df.ars)
}