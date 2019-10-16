# setup
## clear variables
rm(list= ls())

## libraries
library(ecp)
library(ggplot2)
library(forecast)

## setwd
setwd("c:/users/Joao Pedro/Documents/Educação/Outros/DrivenData/DevGoals/")

# input
## read train data csv
trainData <- read.csv2("TrainingSet.csv", sep = ",",stringsAsFactors = F)
t <- apply(trainData[, c(2:37)], 1, as.numeric)
trainData[, c(2:37)] <- as.data.frame(t(t))
rm(t)
colnames(trainData) <- c(colnames(trainData[1]),
                         substr(colnames(trainData[,c(2:37)]), 2, 5),
                         colnames(trainData[, c(38:40)]))

## read submission data
subData <- read.csv2("SubmissionRows.csv", sep = ",",stringsAsFactors = F)
subData <- subData[,1]

# process
## subset data
subSetData <- function(i, x, y){
  interval <- e.divisive(matrix(diff(y), ncol = 1), min.size = 30)
  interval <- interval$estimates[(length(interval$estimates)-1)]:
    interval$estimates[length(interval$estimates)]
  if (length(interval) <= 4){
    interval <- 1:length(x)
  }
  return(interval)
}
lossCalc <- function(y, fit, type){
  loss <- c()
  if (type == "rmse"){
    loss <- sqrt(sum((fit[!is.na(fit)] - y)^2)/length(y))
  }
  return(loss)
}
## model and prediction functions
modelFitAndPred <- function(modelType, x, y, xnew, xreg = NULL){
  out <- tryCatch(
    {
      fitPred <- list()
      if (modelType == "lm"){
        model <- lm(y ~ x)
        yhat <- predict(model, data.frame(x = xnew), interval = "prediction")[,c("fit")]
        fitPred[[1]] <- model$fitted.values
        fitPred[[2]] <- yhat
        fitPred[[3]] <- lossCalc(y, model$fitted.values, "rmse")
      } else if (modelType == "ets"){
        df1 <- data.frame(x = x, y = y)
        df2 <- data.frame(x = seq(x[1], x[length(x)]))
        df3 <- merge(df1, df2, all = T)
        tSeries <- ts(df3$y, start = df3$x[1])
        model <- ets(na.interp(tSeries), opt.crit = "mse")
        yhat <- forecast(tSeries, model = model, h=5)$mean
        fitPred[[1]] <- model$fitted[which(!is.na(df3$y))]
        fitPred[[2]] <- yhat
        fitPred[[3]] <- lossCalc(y, model$fitted[which(!is.na(df3$y))], "rmse")
      } else if (modelType == "auto.arima"){
        df1 <- data.frame(x = x, y = y)
        df2 <- data.frame(x = seq(x[1], x[length(x)]))
        df3 <- merge(df1, df2, all = T)
        tSeries <- ts(df3$y, start = df3$x[1])
        model <- auto.arima(na.interp(tSeries), stepwise = F, parallel = T)
        yhat <- forecast(tSeries, model = model, h=5)$mean
        fitPred[[1]] <- model$fitted[which(!is.na(df3$y))]
        fitPred[[2]] <- yhat
        fitPred[[3]] <- lossCalc(y, model$fitted[which(!is.na(df3$y))], "rmse")
      } else if (modelType == "nnetar"){
        df1 <- data.frame(x = x, y = y)
        df2 <- data.frame(x = seq(x[1], x[length(x)]))
        df3 <- merge(df1, df2, all = T)
        tSeries <- ts(df3$y, start = df3$x[1])
        model <- nnetar(na.interp(tSeries), MaxNWts = 10000, maxit = 10000, abstol = 1e-8)
        yhat <- forecast(tSeries, model = model, h=5)$mean
        fitPred[[1]] <- model$fitted[which(!is.na(df3$y))]
        fitPred[[2]] <- yhat
        fitPred[[3]] <- lossCalc(y, model$fitted[which(!is.na(df3$y))], "rmse")
      } else if (modelType == "baggedArima"){
        df1 <- data.frame(x = x, y = y)
        df2 <- data.frame(x = seq(x[1], x[length(x)]))
        df3 <- merge(df1, df2, all = T)
        tSeries <- ts(df3$y, start = df3$x[1])
        model <- baggedModel(na.interp(tSeries), 
                             fn = c("auto.arima"), stepwise = F, parallel = T)
        yhat <- forecast(model, h=5)$mean
        fitPred[[1]] <- model$fitted[which(!is.na(df3$y))]
        fitPred[[2]] <- yhat
        fitPred[[3]] <- lossCalc(y, model$fitted[which(!is.na(df3$y))], "rmse")
      } else if (modelType == "baggedEts"){
        df1 <- data.frame(x = x, y = y)
        df2 <- data.frame(x = seq(x[1], x[length(x)]))
        df3 <- merge(df1, df2, all = T)
        tSeries <- ts(df3$y, start = df3$x[1])
        model <- baggedModel(na.interp(tSeries))
        yhat <- forecast(model, h=5)$mean
        fitPred[[1]] <- model$fitted[which(!is.na(df3$y))]
        fitPred[[2]] <- yhat
        fitPred[[3]] <- lossCalc(y, model$fitted[which(!is.na(df3$y))], "rmse")
      } else if (modelType == "bats"){
        df1 <- data.frame(x = x, y = y)
        df2 <- data.frame(x = seq(x[1], x[length(x)]))
        df3 <- merge(df1, df2, all = T)
        tSeries <- ts(df3$y, start = df3$x[1])
        model <- bats(na.interp(tSeries))
        yhat <- forecast(tSeries, model = model, h=5)$mean
        fitPred[[1]] <- model$fitted[which(!is.na(df3$y))]
        fitPred[[2]] <- yhat
        fitPred[[3]] <- lossCalc(y, model$fitted[which(!is.na(df3$y))], "rmse")
      }
    
    fitPred
    },
    error = function(cond){
      fitPred <- list()
      model <- lm(y ~ x)
      yhat <- predict(model, data.frame(x = xnew), interval = "prediction")[,c("fit")]
      fitPred[[1]] <- model$fitted.values
      fitPred[[2]] <- yhat
      fitPred[[3]] <- lossCalc(y, model$fitted.values, "rmse")
      print(fitPred)
      message("Error")
      return(fitPred)
    }
  )
  return(out)
}
# plot predictions
plotPredictions <- function(x, y, fit, xnew, yhat, plotTitle){
  data1 <- data.frame(x = x, y = y)
  data2 <- data.frame(xnew = xnew, yhat = yhat)
  data3 <- data.frame(x = x, y = fit)
  p <- ggplot(data1, aes(x , y)) + geom_line() + 
    geom_point(data = data2, aes(xnew, yhat, color = "red")) + 
    geom_line(data = data2, aes(x = xnew, y = yhat, color = "red")) +
    geom_point(data = data3, aes(x = x, y = fit, color = "red")) +
    geom_line(data = data3, aes(x = x, y = fit, color = "red")) + 
    labs(title = plotTitle)
  print(p)  
}
## TO-DO: prediction functions
results <- data.frame(y1 = double(), y2 = double())
errors <- data.frame(rmse = double(), idx = numeric())
for (i in 1:length(subData)){
#for (i in 1:30){
  predIdx <- subData[i]
  cond <- !is.na(trainData[trainData$X == predIdx,c(2:37)])
  y <- trainData[trainData$X == predIdx, c(2:37)][cond]
  x <- as.numeric(colnames(trainData[,c(2:37)])[cond])
  plotTitle <- paste(trainData[trainData$X == predIdx,
                    c("Country.Name", "Series.Name")], collapse = " ||| ")
  ## function to subset the data
  interval <- subSetData(i,x,y)
  y <- y[interval]
  x <- x[interval]
  # function to fit the model and predict
  xnew <- 2008:2012
  print(i)
  foreMethod <- "auto.arima"
  if (foreMethod == "multRegr"){
    # obter pais
    country <- trainData[trainData$X == predIdx, "Country.Name"]
    # obter variaveis pais
    varCtry <- trainData[trainData$Country.Name == country, ]
    # obter correlacao
    corCtry <- cor(t(varCtry[,c(2:37)]), use = "pairwise.complete.obs")
    # obter variaveis mais correlatas
    # TO-DO: rever indice
    idxVar <- which(abs(corCtry[which(varCtry$X == predIdx),]) > 0.98 & 
    abs(corCtry[which(varCtry$X == predIdx),]) != 1)
    # dataframe construction
    df <- t(varCtry[varCtry$X == predIdx, c(2:37)])
    df <- cbind(t(varCtry[varCtry$X == predIdx,c(2:37)]), t(varCtry[idxVar,c(2:37)]))
    df <- df[which(!is.na(df[, 1])),]
    if (!is.null(ncol(df))){
      # column selection
      df <- df[ , colSums(is.na(df)) == 0]
    }
    if (is.null(ncol(df))){
      #fitPred <- modelFitAndPred("nnetar", x, y, xnew)
      fitPred <- modelFitAndPred("auto.arima", x, y, xnew)
      fit <- fitPred[[1]]
      yhat <- fitPred[[2]]
      idxNotNa <- which(!is.na(fit))
      plotPredictions(x[idxNotNa], y[idxNotNa], 
                      fit[idxNotNa], xnew, yhat, plotTitle)
      #invisible(readline(prompt="Press [enter] to continue"))
      # results
      results[i,] <- yhat[c(1,5)]
      rmse <- fitPred[[3]]
      errors[i, ] <- c(rmse, i)
    } else {
      # time-series with all correlated regressors and y
      tS <- ts(df, start = as.numeric(rownames(df))[1])
      # tS <- apply(tS, 2, na.interp)
      # NN model
      model <- nnetar(tS[, 1], xreg = tS[, 2:ncol(tS)], MaxNWts = 10000)
      # predicao variaveis mais correlatas
      xreg <- data.frame("2008" = c(), "2012" = c())
      usefulVar <- as.numeric(colnames(df))
      usefulVar <- usefulVar[2:length(usefulVar)]
      for (j in 1:length(usefulVar)){
        y2 <- df[, (j + 1)]
        x2 <- as.numeric(rownames(df))
        fitPred <- modelFitAndPred("nnetar", x2, y2, xnew)
        xreg <- rbind(xreg, fitPred[[2]])
      }
      colnames(xreg) <- c("2008", "2009", "2010", "2011","2012")
      xregT <- t(xreg)
      colnames(xregT) <- usefulVar
      # time-series with new regressors
      tS2 <- ts(xregT, start = as.numeric(rownames(xregT))[1])
      # predicao da variavel em questao
      yhat <- forecast(model, xreg = tS2)$mean
      fitPred[[1]] <- NA
      fitPred[[2]] <- yhat
      fitPred[[3]] <- NA
      results[i,] <- yhat[c(1,5)]
      rmse <- fitPred[[3]]
      errors[i, ] <- c(rmse, i)
      print("multiRegressors prediction")
    }
  } else {
    fitPred <- modelFitAndPred(foreMethod, x, y, xnew)
    fit <- fitPred[[1]]
    yhat <- fitPred[[2]]
    yhat[yhat > 1] <- 1
    yhat[yhat < 0] <- 0
    idxNotNa <- which(!is.na(fit))
    plotPredictions(x[idxNotNa], y[idxNotNa], 
                    fit[idxNotNa], xnew, yhat, plotTitle)
    #invisible(readline(prompt="Press [enter] to continue"))
    # results
    results[i,] <- yhat[c(1,5)]
    rmse <- fitPred[[3]]
    errors[i, ] <- c(rmse, i)
  }
}

# output
colnames(results) <- c("2008 [YR2008]", "2012 [YR2012]")
rownames(results) <- subData
write.csv(results, "submission.csv", sep = ",", quote = F)

# TO-DO: avaliar PCA
# TO-DO: avaliar VAR
