#--- CONVERGENCE PLOTS ---#
# VERSION NOV 25 2015
# CHRISTIAN DANNE 
# DANNEC@TCD.IE 

plottrace.BVARM <- function(x,type=1,save=FALSE,height=13,width=13,...){
  .plottracebvarm(x,type,save,height,width)
}

plottrace.BVARS <- function(x,type=1,save=FALSE,plotSigma=TRUE, height=13,width=13,...){
  .plottracebvars(x,type,plotSigma,save,height,width)
}

plottrace.BVARW <- function(x,type=1,save=FALSE,plotSigma=TRUE, height=13,width=13,...){
  .plottracebvarw(x,type,plotSigma,save,height,width)
}

.plottracebvarm <- function(obj,type=1,save=FALSE,height=13,width=13){
  Betas <- obj$BDraws
  #
  mydata <- obj$data
  constant <- obj$constant
  p <- floor(dim(Betas)[1]/dim(Betas)[2])
  M <- as.numeric(dim(Betas)[2])
  K <- as.numeric(dim(Betas)[1])
  keep <- as.numeric(dim(Betas)[3])
  keep0 <- as.numeric(dim(Betas)[3]/2)
  keep1 <- as.numeric(dim(Betas)[3]/2+1)
  #
BetaPerm <- aperm(Betas,c(3,1,2))
ITER <-1:dim(obj$BDraws)[3]
#
  CoefLabels<-character(p*M)
  #
  jj <- 1
  for(i in 1:p){
    for(k in 1:M){
      CoefLabels[jj]<-paste("L",i,".",k,sep="")
      jj = jj + 1
    }
  }
  #
  if(class(dev.list()) != "NULL"){dev.off()}
  #
  vplayout <- function(x,y){viewport(layout.pos.row=x, layout.pos.col=y)}
  #
  CoefCount <- 1
  #
  if(constant==TRUE){
    for(i in 1:1){
      if(save==TRUE){cairo_ps(filename="Constant_trace.eps",height=(floor(height/M)),width=width)}
      pushViewport(viewport(layout=grid.layout(1,M)))
      #
      for(j in 1:M){
        VarName <- colnames(mydata)[j]
#
        TRACE <- data.frame(cbind(BetaPerm[,i,j],ITER))
        colnames(TRACE) <- c("TRACE", "ITER")
        TRACE1 <- data.frame(BetaPerm[,i,j])
        colnames(TRACE1) <- "TRACE"
        #
        TRACE2 <- data.frame(cbind(BetaPerm[1:keep0,i,j], BetaPerm[keep1:keep,i,j]))
        myls <- list(TRACE1, TRACE2)
        #
        #maximum number of rows
        max.rows <- max(nrow(TRACE1), nrow(TRACE2))
        #insert the needed `NA`s to each dataframe
        new_myls <- lapply(myls, function(x) { x[1:max.rows,] })
        TRACE3 <- data.frame(new_myls)
        colnames(TRACE3) <- c("TRACE", "FIRST", "SECOND")
        #
        if(j==1){
          if(type==1){
            print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab("Constant") + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(1,j))
          }else if(type==2){
            ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
            ACF <- data.frame(cbind(ACF$lag, ACF$acf))
             print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab("Constant") + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(1,j))
          }else if(type==3){
            print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE), na.rm=TRUE, color="darkblue" ,fill="blue", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) + geom_density(aes(x=SECOND),  na.rm=TRUE, color="darkgreen", fill="green", alpha=0.15) + geom_density(aes(x=FIRST) , na.rm=TRUE, color="darkred" ,fill="red", alpha=0.15)   ,vp = vplayout(1,j))
          }
        }else{
          if(type==1){
            print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) +  xlab(NULL) + ylab(NULL) + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(1,j))
          }else if(type==2){
             ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
             ACF <- data.frame(cbind(ACF$lag, ACF$acf))
             print(ggplot(ACF,aes(x=X1, y= X2)) +   xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(1,j))
          }else if(type==3){
             print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL)+ labs(title=paste(VarName)),vp = vplayout(1,j))
          }
        }
        Sys.sleep(0.3)
      }
      if(save==TRUE){dev.off()}
    }
    for(i in 1:p){
      SaveLag <- paste("CoefLag",as.character(i),"_trace.eps",sep="")
      #
      if(save==TRUE){cairo_ps(filename=SaveLag,height=height,width=width)}
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(M,M)))
      #
      for(j in 1:M){
        for(l in 1:M){
          VarName <- colnames(mydata)[l]
#
          TRACE <- data.frame(cbind(BetaPerm[,((i-1)*M+j+1),l], ITER))
          colnames(TRACE) <- c("TRACE", "ITER")
          TRACE1 <- data.frame(BetaPerm[,((i-1)*M+j+1),l])
        colnames(TRACE1) <- "TRACE"
        #
        TRACE2 <- data.frame(cbind(BetaPerm[1:keep0,((i-1)*M+j+1),l], BetaPerm[keep1:keep,((i-1)*M+j+1),l]))
        myls <- list(TRACE1, TRACE2)
        #
        #maximum number of rows
        max.rows <- max(nrow(TRACE1), nrow(TRACE2))
        #insert the needed `NA`s to each dataframe
        new_myls <- lapply(myls, function(x) { x[1:max.rows,] })
        TRACE3 <- data.frame(new_myls)
        colnames(TRACE3) <- c("TRACE", "FIRST", "SECOND")
          #
          if(j==1){
            if(l==1){
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE))+  scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                  CoefCount <- CoefCount + 1
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                 print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),  vp = vplayout(j,l))
                  CoefCount <- CoefCount + 1
               }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount]))+ labs(title=paste(VarName)), vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }
            }else{
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) +  xlab(NULL) + ylab(NULL) + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                 print(ggplot(ACF,aes(x=X1, y= X2)) +  xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')), vp = vplayout(j,l))
              }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) , vp = vplayout(j,l))
              }
            }
          }else{
            if(l==1){
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + geom_line(colour="red")   + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                  CoefCount <- CoefCount + 1
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                 print(ggplot(ACF,aes(x=X1, y= X2)) +xlab(NULL) + ylab(paste(CoefLabels[CoefCount]))   + geom_line(colour="red", size=1)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                  CoefCount <- CoefCount + 1
              }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89'))+ xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) , vp = vplayout(j,l))
                 CoefCount <- CoefCount + 1
              }
            }else{
              if(type==1){
               print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) +  xlab(NULL) + ylab(NULL) + geom_line(colour="red")  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                 print(ggplot(ACF,aes(x=X1, y= X2)) +  xlab(NULL) + ylab(NULL)  + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL)  , vp = vplayout(j,l))
              }
            }
          }
          Sys.sleep(0.3)
        }
      }
      if(save==TRUE){dev.off()}
    }
    #NO CONSTANT
  }else{
    for(i in 1:p){
      SaveLag <- paste("CoefLag",as.character(i),"_trace.eps",sep="")
      #
      if(save==TRUE){cairo_ps(filename=SaveLag,height=height,width=width)}
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(M,M)))
      #
      for(j in 1:M){
        for(l in 1:M){
          VarName <- colnames(mydata)[l]
#
          TRACE <- data.frame(cbind(BetaPerm[,((i-1)*M+j),l], ITER))
          colnames(TRACE) <- c("TRACE", "ITER")
                    TRACE1 <- data.frame(BetaPerm[,((i-1)*M+j),l])
        colnames(TRACE1) <- "TRACE"
        #
        TRACE2 <- data.frame(cbind(BetaPerm[1:keep0,((i-1)*M+j),l], BetaPerm[keep1:keep,((i-1)*M+j),l]))
        myls <- list(TRACE1, TRACE2)
        #
        #maximum number of rows
        max.rows <- max(nrow(TRACE1), nrow(TRACE2))
        #insert the needed `NA`s to each dataframe
        new_myls <- lapply(myls, function(x) { x[1:max.rows,] })
        TRACE3 <- data.frame(new_myls)
        colnames(TRACE3) <- c("TRACE", "FIRST", "SECOND")
          #
          if(j==1){
            if(l==1){
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE))+  scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                  CoefCount <- CoefCount + 1
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                 print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),  vp = vplayout(j,l))
                  CoefCount <- CoefCount + 1
               }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount]))+ labs(title=paste(VarName)), vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }
            }else{
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) +  xlab(NULL) + ylab(NULL) + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                 print(ggplot(ACF,aes(x=X1, y= X2)) +  xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')), vp = vplayout(j,l))
              }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) , vp = vplayout(j,l))
              }
            }
          }else{
            if(l==1){
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + geom_line(colour="red")   + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                  CoefCount <- CoefCount + 1
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                 print(ggplot(ACF,aes(x=X1, y= X2)) +xlab(NULL) + ylab(paste(CoefLabels[CoefCount]))   + geom_line(colour="red", size=1)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                  CoefCount <- CoefCount + 1
              }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89'))+ xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) , vp = vplayout(j,l))
                 CoefCount <- CoefCount + 1
              }
            }else{
              if(type==1){
               print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) +  xlab(NULL) + ylab(NULL) + geom_line(colour="red")  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                 print(ggplot(ACF,aes(x=X1, y= X2)) +  xlab(NULL) + ylab(NULL)  + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL)  , vp = vplayout(j,l))
              }
            }
          }
          Sys.sleep(0.3)
        }
      }
      if(save==TRUE){dev.off()}
    }
  }
}

.plottracebvars <- function(obj,type=1,plotSigma=TRUE,save=FALSE,height=13,width=13){
  Psis <- obj$PDraws
  Betas <- obj$BDraws
  #
  mydata <- obj$data
  p <- floor(dim(Betas)[1]/dim(Betas)[2])
  M <- as.numeric(dim(Betas)[2])
  K <- as.numeric(dim(Betas)[1])
  keep<-as.numeric(dim(Betas)[3])
  keep <- as.numeric(dim(Betas)[3])
  keep0 <- as.numeric(dim(Betas)[3]/2)
  keep1 <- as.numeric(dim(Betas)[3]/2+1)
  #
  BetaPerm <- aperm(Betas,c(3,1,2))
  PsiPerm <- aperm(Psis,c(3,1,2))
  ITER <-1:dim(obj$BDraws)[3]
  #
  CoefLabels<-character(p*M)
  jj <- 1
  for(i in 1:p){
    for(k in 1:M){
      CoefLabels[jj]<-paste("L",i,".",k,sep="")
      jj = jj + 1
    }
  }
  #
  if(class(dev.list()) != "NULL"){dev.off()}
  #
  vplayout <- function(x,y){viewport(layout.pos.row=x, layout.pos.col=y)}
  #
  CoefCount <- 1
  #
  for(i in 1:1){
    if(save==TRUE){cairo_ps(filename="Psi_trace.eps",height=(floor(height/M)),width=width)}
    pushViewport(viewport(layout=grid.layout(1,M)))
    #
    for(j in 1:M){
      VarName <- colnames(mydata)[j]
#
        TRACE <- data.frame(cbind(BetaPerm[,i,j],ITER))
        colnames(TRACE) <- c("TRACE", "ITER")
        TRACE1 <- data.frame(BetaPerm[,i,j])
        colnames(TRACE1) <- "TRACE"
        #
        TRACE2 <- data.frame(cbind(BetaPerm[1:keep0,i,j], BetaPerm[keep1:keep,i,j]))
        myls <- list(TRACE1, TRACE2)
        #
        #maximum number of rows
        max.rows <- max(nrow(TRACE1), nrow(TRACE2))
        #insert the needed `NA`s to each dataframe
        new_myls <- lapply(myls, function(x) { x[1:max.rows,] })
        TRACE3 <- data.frame(new_myls)
        colnames(TRACE3) <- c("TRACE", "FIRST", "SECOND")
        #
      if(j==1){
          if(type==1){
            print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab("Constant") + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(1,j))
          }else if(type==2){
            ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
            ACF <- data.frame(cbind(ACF$lag, ACF$acf))
             print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab("Constant") + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(1,j))
          }else if(type==3){
            print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE), na.rm=TRUE, color="darkblue" ,fill="blue", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) + geom_density(aes(x=SECOND),  na.rm=TRUE, color="darkgreen", fill="green", alpha=0.15) + geom_density(aes(x=FIRST) , na.rm=TRUE, color="darkred" ,fill="red", alpha=0.15)   ,vp = vplayout(1,j))
          }
      }else{
          if(type==1){
            print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) +  xlab(NULL) + ylab(NULL) + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(1,j))
          }else if(type==2){
             ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
             ACF <- data.frame(cbind(ACF$lag, ACF$acf))
             print(ggplot(ACF,aes(x=X1, y= X2)) +   xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(1,j))
          }else if(type==3){
             print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL)+ labs(title=paste(VarName)),vp = vplayout(1,j))
          }
      }
      Sys.sleep(0.3)
    }
    if(save==TRUE){dev.off()}
  }
  #
  for(i in 1:p){
    SaveLag <- paste("CoefLag_trace",as.character(i),".eps",sep="")
    #
    if(save==TRUE){cairo_ps(filename=SaveLag,height=height,width=width)}
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(M,M)))
    #
    for(j in 1:M){
      for(l in 1:M){
        VarName <- colnames(mydata)[l]
#
          TRACE <- data.frame(cbind(BetaPerm[,((i-1)*M+j),l], ITER))
          colnames(TRACE) <- c("TRACE", "ITER")
          TRACE1 <- data.frame(BetaPerm[,((i-1)*M+j),l])
        colnames(TRACE1) <- "TRACE"
        #
        TRACE2 <- data.frame(cbind(BetaPerm[1:keep0,((i-1)*M+j),l], BetaPerm[keep1:keep,((i-1)*M+j),l]))
        myls <- list(TRACE1, TRACE2)
        #
        #maximum number of rows
        max.rows <- max(nrow(TRACE1), nrow(TRACE2))
        #insert the needed `NA`s to each dataframe
        new_myls <- lapply(myls, function(x) { x[1:max.rows,] })
        TRACE3 <- data.frame(new_myls)
        colnames(TRACE3) <- c("TRACE", "FIRST", "SECOND")
          #
        if(j==1){
          if(l==1){
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE))+  scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                  CoefCount <- CoefCount + 1
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                 print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),  vp = vplayout(j,l))
                  CoefCount <- CoefCount + 1
               }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount]))+ labs(title=paste(VarName)), vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }
          }else{
            if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) +  xlab(NULL) + ylab(NULL) + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                 print(ggplot(ACF,aes(x=X1, y= X2)) +  xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')), vp = vplayout(j,l))
              }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) , vp = vplayout(j,l))
              }
          }
        }else{
          if(l==1){
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + geom_line(colour="red")   + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                  CoefCount <- CoefCount + 1
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                 print(ggplot(ACF,aes(x=X1, y= X2)) +xlab(NULL) + ylab(paste(CoefLabels[CoefCount]))   + geom_line(colour="red", size=1)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                  CoefCount <- CoefCount + 1
              }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89'))+ xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) , vp = vplayout(j,l))
                 CoefCount <- CoefCount + 1
              }
          }else{
              if(type==1){
               print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) +  xlab(NULL) + ylab(NULL) + geom_line(colour="red")  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                 print(ggplot(ACF,aes(x=X1, y= X2)) +  xlab(NULL) + ylab(NULL)  + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL)  , vp = vplayout(j,l))
              }
          }
        }
        Sys.sleep(0.3)
      }
    }
    if(save==TRUE){dev.off()}
  }
  #
  if(plotSigma==TRUE){
    Sigmas <- obj$SDraws
    SigmaPerm <- aperm(Sigmas,c(3,1,2))
    ITER <-1:dim(obj$SDraws)[3]
    #
    SaveSig <- paste("Sigma_trace.eps",sep="")
    if(save==TRUE){cairo_ps(filename=SaveSig,height=height,width=width)}
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(M,M)))
    #
    for(j in 1:M){
      for(l in 1:M){
        VarNameY <- colnames(mydata)[j]
        VarNameX <- colnames(mydata)[l]
        #
  TRACE <- data.frame(cbind(SigmaPerm[,j,l], ITER))
        colnames(TRACE) <- c("TRACE" ,"ITER")
        TRACE1 <- data.frame(SigmaPerm[,j,l])
          colnames(TRACE1) <- "TRACE"
          #
          TRACE2 <- data.frame(cbind(SigmaPerm[1:keep0,j,l], SigmaPerm[keep1:keep,j,l]))
          myls <- list(TRACE1, TRACE2)
          #
          #maximum number of rows
          max.rows <- max(nrow(TRACE1), nrow(TRACE2))
          #insert the needed `NA`s to each dataframe
          new_myls <- lapply(myls, function(x) { x[1:max.rows,] })
          TRACE3 <- data.frame(new_myls)
          colnames(TRACE3) <- c("TRACE", "FIRST", "SECOND")
           TRACE4 <- TRACE3
          colnames(TRACE4) <- c("TRACE", "FIRST", "SECOND")
        #
        #j==1 is for the variable title;l==1 is the coefficient label on y-axis
        if(j==1){
          if(l==1){
            if(type==1){
          print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(paste(VarNameY)) + labs(title=paste(VarNameX)) + geom_line(colour="red")  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==2){
              ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
              ACF <- data.frame(cbind(ACF$lag, ACF$acf))
              print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab(paste(VarNameY)) + labs(title=paste(VarNameX))  + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==3){
              print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89'))  + xlab(NULL) + ylab(paste(VarNameY)) + labs(title=paste(VarNameX))    , vp = vplayout(j,l))
            }
          }else{
            if(type==1){
          print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(NULL) + labs(title=paste(VarNameX)) + geom_line(colour="red")  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==2){
              ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
              ACF <- data.frame(cbind(ACF$lag, ACF$acf))
              print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab(NULL) + labs(title=paste(VarNameX))  + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==3){
              print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89'))  + xlab(NULL) + ylab(NULL) + labs(title=paste(VarNameX))    , vp = vplayout(j,l))
            }
          }
        }else{
          if(l==1){
            if(type==1){
          print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(paste(VarNameY)) + geom_line(colour="red")  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==2){
              ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
              ACF <- data.frame(cbind(ACF$lag, ACF$acf))
              print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab(paste(VarNameY))  + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==3){
              print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(paste(VarNameY))  , vp = vplayout(j,l))
            }
          }else{
             if(type==1){
          print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(NULL) + geom_line(colour="red")  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==2){
              ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
              ACF <- data.frame(cbind(ACF$lag, ACF$acf))
              print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab(NULL)   + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==3){
              print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL)   , vp = vplayout(j,l))
            }
          }
        }
        Sys.sleep(0.3)
      }
    }
    if(save==TRUE){dev.off()}
  }
  #
}

.plottracebvarw <-   function(obj,type=1,plotSigma=TRUE,save=FALSE,height=13,width=13){
  Betas <- obj$BDraws
  #
  mydata <- obj$data
  constant <- obj$constant
  p <- floor(dim(Betas)[1]/dim(Betas)[2])
  M <- as.numeric(dim(Betas)[2])
  K <- as.numeric(dim(Betas)[1])
  keep <- as.numeric(dim(Betas)[3])
  keep0 <- as.numeric(dim(Betas)[3]/2)
  keep1 <- as.numeric(dim(Betas)[3]/2+1)
  #
  BetaPerm <- aperm(Betas,c(3,1,2))
  ITER <-1:dim(obj$BDraws)[3]
#
      Sigmas <- obj$SDraws
    SigmaPerm <- aperm(Sigmas,c(3,1,2))
#
  CoefLabels<-character(p*M)
  #
  jj <- 1
  for(i in 1:p){
    for(k in 1:M){
      CoefLabels[jj]<-paste("L",i,".",k,sep="")
      jj = jj + 1
    }
  }
  #
  if(class(dev.list()) != "NULL"){dev.off()}
  #
  vplayout <- function(x,y){viewport(layout.pos.row=x, layout.pos.col=y)}
  #
  CoefCount <- 1
  #
  if(constant==TRUE){
    for(i in 1:1){
      if(save==TRUE){cairo_ps(filename="Constant_trace.eps",height=(floor(height/M)),width=width)}
      pushViewport(viewport(layout=grid.layout(1,M)))
      #
      for(j in 1:M){
        VarName <- colnames(mydata)[j]
        #
        TRACE <- data.frame(cbind(BetaPerm[,i,j],ITER))
        colnames(TRACE) <- c("TRACE", "ITER")
        TRACE1 <- data.frame(BetaPerm[,i,j])
        colnames(TRACE1) <- "TRACE"
        #
        TRACE2 <- data.frame(cbind(BetaPerm[1:keep0,i,j], BetaPerm[keep1:keep,i,j]))
        myls <- list(TRACE1, TRACE2)
        #
        #maximum number of rows
        max.rows <- max(nrow(TRACE1), nrow(TRACE2))
        #insert the needed `NA`s to each dataframe
        new_myls <- lapply(myls, function(x) { x[1:max.rows,] })
        TRACE3 <- data.frame(new_myls)
        colnames(TRACE3) <- c("TRACE", "FIRST", "SECOND")
        #
        if(j==1){
          if(type==1){
            print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab("Constant") + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(1,j))
          }else if(type==2){
            ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
            ACF <- data.frame(cbind(ACF$lag, ACF$acf))
            print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab("Constant") + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(1,j))
          }else if(type==3){
            print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE), na.rm=TRUE, color="darkblue" ,fill="blue", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) + geom_density(aes(x=SECOND),  na.rm=TRUE, color="darkgreen", fill="green", alpha=0.15) + geom_density(aes(x=FIRST) , na.rm=TRUE, color="darkred" ,fill="red", alpha=0.15)   ,vp = vplayout(1,j))
          }
        }else{
          if(type==1){
            print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) +  xlab(NULL) + ylab(NULL) + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(1,j))
          }else if(type==2){
            ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
            ACF <- data.frame(cbind(ACF$lag, ACF$acf))
            print(ggplot(ACF,aes(x=X1, y= X2)) +   xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(1,j))
          }else if(type==3){
            print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL)+ labs(title=paste(VarName)),vp = vplayout(1,j))
          }
        }
        Sys.sleep(0.3)
      }
      if(save==TRUE){dev.off()}
    }
    for(i in 1:p){
      SaveLag <- paste("CoefLag",as.character(i),"_trace.eps",sep="")
      #
      if(save==TRUE){cairo_ps(filename=SaveLag,height=height,width=width)}
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(M,M)))
      #
      for(j in 1:M){
        for(l in 1:M){
          VarName <- colnames(mydata)[l]
          #
          TRACE <- data.frame(cbind(BetaPerm[,((i-1)*M+j+1),l], ITER))
          colnames(TRACE) <- c("TRACE", "ITER")
          TRACE1 <- data.frame(BetaPerm[,((i-1)*M+j+1),l])
          colnames(TRACE1) <- "TRACE"
          #
          TRACE2 <- data.frame(cbind(BetaPerm[1:keep0,((i-1)*M+j+1),l], BetaPerm[keep1:keep,((i-1)*M+j+1),l]))
          myls <- list(TRACE1, TRACE2)
          #
          #maximum number of rows
          max.rows <- max(nrow(TRACE1), nrow(TRACE2))
          #insert the needed `NA`s to each dataframe
          new_myls <- lapply(myls, function(x) { x[1:max.rows,] })
          TRACE3 <- data.frame(new_myls)
          colnames(TRACE3) <- c("TRACE", "FIRST", "SECOND")
          #
          if(j==1){
            if(l==1){
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE))+  scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),  vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount]))+ labs(title=paste(VarName)), vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }
            }else{
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) +  xlab(NULL) + ylab(NULL) + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                print(ggplot(ACF,aes(x=X1, y= X2)) +  xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')), vp = vplayout(j,l))
              }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) , vp = vplayout(j,l))
              }
            }
          }else{
            if(l==1){
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + geom_line(colour="red")   + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                print(ggplot(ACF,aes(x=X1, y= X2)) +xlab(NULL) + ylab(paste(CoefLabels[CoefCount]))   + geom_line(colour="red", size=1)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89'))+ xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) , vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }
            }else{
              if(type==1){
               print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) +  xlab(NULL) + ylab(NULL) + geom_line(colour="red")  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                 print(ggplot(ACF,aes(x=X1, y= X2)) +  xlab(NULL) + ylab(NULL)  + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==3){
                print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL)  , vp = vplayout(j,l))
              }
            }
          }
          Sys.sleep(0.3)
        }
      }
      if(save==TRUE){dev.off()}
    }
    #NO CONSTANT
  }else{
    for(i in 1:p){
      SaveLag <- paste("CoefLag",as.character(i),"_trace.eps",sep="")
      #
      if(save==TRUE){cairo_ps(filename=SaveLag,height=height,width=width)}
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(M,M)))
      #
      for(j in 1:M){
        for(l in 1:M){
          VarName <- colnames(mydata)[l]
          #
          TRACE <- data.frame(cbind(BetaPerm[,((i-1)*M+j),l], ITER))
          colnames(TRACE) <- c("TRACE", "ITER")
          TRACE1 <- data.frame(BetaPerm[,((i-1)*M+j),l])
          colnames(TRACE1) <- "TRACE"
          #
          TRACE2 <- data.frame(cbind(BetaPerm[1:keep0,((i-1)*M+j),l], BetaPerm[keep1:keep,((i-1)*M+j),l]))
          myls <- list(TRACE1, TRACE2)
          #
          #maximum number of rows
          max.rows <- max(nrow(TRACE1), nrow(TRACE2))
          #insert the needed `NA`s to each dataframe
          new_myls <- lapply(myls, function(x) { x[1:max.rows,] })
          TRACE3 <- data.frame(new_myls)
          colnames(TRACE3) <- c("TRACE", "FIRST", "SECOND")
          #
          if(j==1){
            if(l==1){
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE))+  scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),  vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }else if(type==3){
                print(ggplot(data=TRACE) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=TRACE[5001:10000]),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=TRACE[1:5000]) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount]))+ labs(title=paste(VarName)), vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }
            }else{
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) +  xlab(NULL) + ylab(NULL) + geom_line(colour="red") + labs(title=paste(VarName)) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                print(ggplot(ACF,aes(x=X1, y= X2)) +  xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')), vp = vplayout(j,l))
              }else if(type==3){
                print(ggplot(data=TRACE) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=TRACE[5001:10000]),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=TRACE[1:5000]) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL) + labs(title=paste(VarName)) , vp = vplayout(j,l))
              }
            }
          }else{
            if(l==1){
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) + xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) + geom_line(colour="red")   + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                print(ggplot(ACF,aes(x=X1, y= X2)) +xlab(NULL) + ylab(paste(CoefLabels[CoefCount]))   + geom_line(colour="red", size=1)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }else if(type==3){
                print(ggplot(data=TRACE) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=TRACE[5001:10000]),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=TRACE[1:5000]) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89'))+ xlab(NULL) + ylab(paste(CoefLabels[CoefCount])) , vp = vplayout(j,l))
                CoefCount <- CoefCount + 1
              }
            }else{
              if(type==1){
                print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))))) +  xlab(NULL) + ylab(NULL) + geom_line(colour="red")  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==2){
                ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
                ACF <- data.frame(cbind(ACF$lag, ACF$acf))
                print(ggplot(ACF,aes(x=X1, y= X2)) +  xlab(NULL) + ylab(NULL)  + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
              }else if(type==3){
                print(ggplot(data=TRACE) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=TRACE[5001:10000]),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=TRACE[1:5000]) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL)  , vp = vplayout(j,l))
              }
            }
          }
          Sys.sleep(0.3)
        }
      }
      if(save==TRUE){dev.off()}
    }
  }
  if(plotSigma==TRUE){
    Sigmas <- obj$SDraws
    SigmaPerm <- aperm(Sigmas,c(3,1,2))
    ITER <-1:dim(obj$SDraws)[3]
    #
    SaveSig <- paste("Sigma_trace.eps",sep="")
    if(save==TRUE){cairo_ps(filename=SaveSig,height=height,width=width)}
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(M,M)))
    #
    for(j in 1:M){
      for(l in 1:M){
        VarNameY <- colnames(mydata)[j]
        VarNameX <- colnames(mydata)[l]
        #
        TRACE <- data.frame(cbind(SigmaPerm[,j,l], ITER))
        colnames(TRACE) <- c("TRACE" ,"ITER")
        TRACE1 <- data.frame(SigmaPerm[,j,l])
          colnames(TRACE1) <- "TRACE"
          #
          TRACE2 <- data.frame(cbind(SigmaPerm[1:keep0,j,l], SigmaPerm[keep1:keep,j,l]))
          myls <- list(TRACE1, TRACE2)
          #
          #maximum number of rows
          max.rows <- max(nrow(TRACE1), nrow(TRACE2))
          #insert the needed `NA`s to each dataframe
          new_myls <- lapply(myls, function(x) { x[1:max.rows,] })
          TRACE3 <- data.frame(new_myls)
          colnames(TRACE3) <- c("TRACE", "FIRST", "SECOND")
           TRACE4 <- TRACE3
          colnames(TRACE4) <- c("TRACE", "FIRST", "SECOND")
          print(dim(TRACE4))
        #j==1 is for the variable title;l==1 is the coefficient label on y-axis
        if(j==1){
          if(l==1){
            if(type==1){
          print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE))))))+ xlab(NULL) + ylab(paste(VarNameY)) + labs(title=paste(VarNameX)) + geom_line(colour="red")  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==2){
              ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
              ACF <- data.frame(cbind(ACF$lag, ACF$acf))
              print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab(paste(VarNameY)) + labs(title=paste(VarNameX))  + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==3){
              print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89'))  + xlab(NULL) + ylab(paste(VarNameY)) + labs(title=paste(VarNameX))    , vp = vplayout(j,l))
            }
          }else{
            if(type==1){
          print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE))))))+ xlab(NULL) + ylab(NULL) + labs(title=paste(VarNameX)) + geom_line(colour="red")  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==2){
              ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
              ACF <- data.frame(cbind(ACF$lag, ACF$acf))
              print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab(NULL) + labs(title=paste(VarNameX))  + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==3){
              print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89'))  + xlab(NULL) + ylab(NULL) + labs(title=paste(VarNameX))    , vp = vplayout(j,l))
            }
          }
        }else{
          if(l==1){
            if(type==1){
          print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE))))))+ xlab(NULL) + ylab(paste(VarNameY)) + geom_line(colour="red")  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==2){
              ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
              ACF <- data.frame(cbind(ACF$lag, ACF$acf))
              print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab(paste(VarNameY))  + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==3){
              print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(paste(VarNameY))  , vp = vplayout(j,l))
            }
          }else{
             if(type==1){
          print(ggplot(TRACE,aes(x=ITER, y=TRACE)) + scale_y_continuous(limit = c(-1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE)))),  1.5*max(c(abs(min(TRACE$TRACE)), abs(max(TRACE$TRACE))))))+ xlab(NULL) + ylab(NULL) + geom_line(colour="red")  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==2){
              ACF <- acf(TRACE$TRACE, lag.max = 50, type = c("correlation"), plot = F, na.action = na.fail, demean = T)
              ACF <- data.frame(cbind(ACF$lag, ACF$acf))
              print(ggplot(ACF,aes(x=X1, y= X2)) + xlab(NULL) + ylab(NULL)   + geom_line(colour="red", size=1) + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')),vp = vplayout(j,l))
            }else if(type==3){
              print(ggplot(data=TRACE3) + geom_density(aes(x=TRACE),  color="darkblue" ,fill="blue", alpha=0.15) + geom_density(aes(x=SECOND),  color="darkgreen", fill="green", alpha=0.15)  + geom_density(aes(x=FIRST) , color="darkred" ,fill="red", alpha=0.15)  + theme(panel.background = element_rect(fill='white', colour='grey5')) + theme(panel.grid.major = element_line(colour = 'grey89')) + xlab(NULL) + ylab(NULL)   , vp = vplayout(j,l))
            }
          }
        }
        Sys.sleep(0.3)
      }
    }
    if(save==TRUE){dev.off()}
  }
#
}

#--- END OF SCRIPT ---#
