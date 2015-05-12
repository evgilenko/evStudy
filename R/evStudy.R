#' An Event Study Analysis Function
#'
#' This function runs event study calculations using the standard market model.
#' @param df.events The data frame containing the list of events. The first column of the data frame should be 'event_id'(type Integer) numbering the events from 1 to n. The second column shoud be 'event_date' (type Date). The third column should contain 'company_id' (type Chr).
#' @param df.stocks The data frame contains the list of trading dates. The first column should contain the dates of trading days (type Date). The second column should contain the values of the market index (type Numeric). The following columns should contain the prices of the companies' securities.
#' @param event.window The first half of the event window (default is 2, thus the whole event window length is 5).
#' @param estim.window The length of the estimation window (default is 100). It is adjacent to the beginning of the event window.
#' @keywords event study analysis
#' @export
#' @examples
#' evStudy(events, stocks)


evStudy <- function(df.events, df.stocks, event.window = 2, estim.window = 100){ 

df.ars <- data.frame(event.day = c(-seq(from=event.window, to = 1),0,seq(1:event.window),"sigma2e", "CARind", "Theta"))

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
    ar.pred <- firmRet[pred.sample]-(m.model$coef[1]+m.model$coef[2]*markRet[pred.sample])
    df.ars$ret <- c(ar.pred, summary(m.model)$sigma^2, sum(ar.pred), 
                    (sum(ar.pred)/sd(ar.pred))/sqrt(2*event.window+1) )
    
    names(df.ars)[ncol(df.ars)]<-paste0(df.events$firm_id[evId],"_ev",df.events$event_id[evId])
    
    
    
}#--- end for evId

# calculate average AR by firms
Nfirms <- length(2:ncol(df.ars))
ev.length <- 2*event.window + 1

df.ars$Avg<-c(apply(df.ars[1:(ev.length+2),2:ncol(df.ars)],1,mean),NA)
df.ars$Avg[ev.length+1]<-df.ars$Avg[ev.length+1]/Nfirms

# calculate CARs
df.ars$CAR <- NA
df.ars$CAR[1] <- df.ars$Avg[1]

for(i in 2:ev.length){
    df.ars$CAR[i]<-df.ars$CAR[i-1]+df.ars$Avg[i]
}

plot(df.ars$CAR[1:ev.length], type="b", ylab="CAR", xaxt="n", xlab="Event window")
axis(1, at=c(1:5), labels=df.ars[1:ev.length,1])

# theta-statistic calculation
df.ars$CAR[ev.length+1]<-ev.length*df.ars$Avg[ev.length+1]
df.ars$CAR[nrow(df.ars)] <- df.ars$CAR[ev.length]/sqrt(df.ars$CAR[ev.length+1])
print(paste0("Theta statistic: ", round(df.ars$CAR[nrow(df.ars)],3)))
print("Values of Theta greater than 1.96 in absolute value are statistically significant.")
return(df.ars)
}