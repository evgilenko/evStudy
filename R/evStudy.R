#' An Event Study Analysis Function
#'
#' This function runs event study calculations using the standard market model.
#' @param df.events The data frame containing the list of events. The first column of the data frame should be 'event_id' (type Integer) numbering the events from 1 to n. The second column shoud be 'event_date' (type Date). The third column should contain 'company_id' (type Chr).
#' @param df.stocks The data frame contains the list of trading dates. The first column should contain the dates of trading days (type Date). The second column should contain the values of the market index (type Numeric). The following columns should contain the prices of the companies' securities.
#' @param left.event.window The first half of the event window, before and NOT including the moment of the event (default is 5).
#' @param right.event.window The second half of the event window, after and NOT including the moment of the event (default is 2).
#' @param estim.window The length of the estimation window (default is 100). It is adjacent to the beginning of the event window.
#' @keywords event study analysis
#' @export
#' @examples
#' evStudy(events, stocks)

# TODO: 1) A separate function for graphs. 2) Check NAs in data befor the estimation window. 3) Check estimation window.

evStudy <- function(df.events, df.stocks, left.event.window=5, right.event.window=2, estim.window=100){ 

df.ars <- data.frame(event.day = c(-seq(from=left.event.window, to = 1),0,seq(1:right.event.window),"sigma2e", "CARind", "ThetaInd"), stringsAsFactors = F)

## calculate market returns
markPrice <- df.stocks[, 2]
markRet <- c(NA, diff(markPrice) / markPrice[-length(markPrice)])

# clear the console
cat("\014")
print("***** EVENT STUDY ANALYSIS *****")


for(evId in 1:nrow(df.events)){
    
    #print(paste0("Processing company: ",df.events$firm_id[evId]))
    
    # create estimation and prediction subsamples
    evDate <- which(df.stocks$Date == df.events$eventdate[evId])
    est.sample <- seq(from=evDate-left.event.window-estim.window, to=evDate-left.event.window-1)
    pred.sample <- seq(from=evDate-left.event.window, to=evDate+right.event.window)
    
    # calculate security returns
    firmPrice <- df.stocks[, df.events$firm_id[evId]]
    firmRet <- c(NA, diff(firmPrice) / firmPrice[-length(firmPrice)])
    
    if(var(firmPrice[est.sample])!=0 & sum(!is.finite(firmRet[est.sample]))==0){
                    
        # estimation of the market model
        m.model <- lm(firmRet[est.sample]~markRet[est.sample])
        ar.pred <- firmRet[pred.sample]-(m.model$coef[1]+m.model$coef[2]*markRet[pred.sample])
        df.ars$ret <- c(ar.pred, summary(m.model)$sigma^2, sum(ar.pred), 
                    (sum(ar.pred)/sd(ar.pred))/sqrt(left.event.window+right.event.window+1))
        names(df.ars)[ncol(df.ars)]<-paste0(df.events$firm_id[evId],"_ev",df.events$event_id[evId])
    } else{
    print(paste0("Warning! Prices of company ",df.events$firm_id[evId]," have either zero variance or NaNs over the estimation window and are excluded!"))
    }#--- end of else
        
}#--- end for evId

df.ars<-rbind(df.ars,c(NA,as.numeric(abs(df.ars[nrow(df.ars),2:ncol(df.ars)])>1.96)))
df.ars[nrow(df.ars),1]<-"IndTestResults"

# calculate average AR by firms
Nfirms <- length(2:ncol(df.ars))
ev.length <- left.event.window + right.event.window + 1

if(Nfirms>1){
    df.ars$Avg<-c(apply(df.ars[,2:ncol(df.ars)],1,mean))
    df.ars$Avg[ev.length+1]<-df.ars$Avg[ev.length+1]/Nfirms
    df.ars$ThetaAvgAR<-c(df.ars$Avg[1:ev.length]/sqrt(df.ars$Avg[ev.length+1]),NA,NA,NA,NA)

# calculate CARs
df.ars$CAR <- NA
df.ars$CAR[1] <- df.ars$Avg[1]

for(i in 2:ev.length){
    df.ars$CAR[i]<-df.ars$CAR[i-1]+df.ars$Avg[i]
}

# theta-statistic calculation
df.ars$CAR[ev.length+1]<-ev.length*df.ars$Avg[ev.length+1]
df.ars$CAR[nrow(df.ars)-1] <- df.ars$CAR[ev.length]/sqrt(df.ars$CAR[ev.length+1])

print("********************************")
print(paste0("CAAR theta statistic: ", round(df.ars$CAR[nrow(df.ars)-1],3)))
print("The values of CAAR theta greater than 1.96 in absolute value are statistically significant.")
print("********************************")
prop.theta <- df.ars$Avg[nrow(df.ars)]
print(paste0("The proportion of significant AAR thetas: ", round(prop.theta,4)))
print(paste0("The number of significant AAR thetas: ", round(prop.theta*Nfirms,4)))
print(paste0("The number of companies-events: ", Nfirms))



# plot of ARs
yARlim <- c(-max(abs(df.ars$Avg[1:ev.length]*100)+1),max(abs(df.ars$Avg[1:ev.length]*100)+1))
plot(df.ars$Avg[1:ev.length]*100, type="l", lwd=2, pch=19, main="AAR, %", xaxt="n", yaxt="n", ylab="", xlab="", ylim=yARlim, bty="n")
axis(1, at=c(1:ev.length), labels=df.ars[1:ev.length,1], pos=0)
axis(2, pos=left.event.window+1, las=1)
abline(v=left.event.window+1)
abline(h=0)

# plot of Thetas for event days
yThetalim <- c(-max(abs(df.ars$ThetaAvgAR[1:ev.length])+1),max(abs(df.ars$ThetaAvgAR[1:ev.length])+1))
plot(df.ars$ThetaAvgAR[1:ev.length], type="l", lwd=2, pch=19, main="Theta statistics", xaxt="n", yaxt="n", ylab="", xlab="", ylim=yThetalim, bty="n")
axis(1, at=c(1:ev.length), labels=df.ars[1:ev.length,1], pos=0)
axis(2, pos=left.event.window+1, las=1)
abline(v=left.event.window+1)
abline(h=0)
abline(h=c(-1.96,1.96), lty=5)
abline(h=c(-1.64,1.64), lty=2)

# plot of CARs
yCARlim <- c(-max(abs(df.ars$CAR[1:ev.length]*100)+1),max(abs(df.ars$CAR[1:ev.length]*100)+1))
plot(df.ars$CAR[1:ev.length]*100, type="l", lwd=2, pch=19, main="CAAR, %", xaxt="n", yaxt="n", xlab="", ylab="", xaxs="i", yaxs="i", bty="n", ylim=yCARlim)
axis(1, at=c(1:ev.length), labels=df.ars[1:ev.length,1], pos=0)
axis(2, pos=left.event.window+1, las=1)
abline(v=left.event.window+1)
abline(h=0)

}#--- end if Nfirms>1

# resulting data frame
return(df.ars)
}