# LDA PREDICTION COMPLETE BACKTEST FUNCTION
# Arguments: SYMBOL, train.range, test.range, threshold;
# # SYMBOL (char) = select your ticker
# # train.range (char) = set date range for your training set
# # test.range (char) = set date range for your test set
# # threshold (num) = set the posterior probability threshold 
# #         * LONG if post prob down <= threshhold, else SHORT *
ticker = 'WMT';
train.range = "2007::2013";
test.range = "2014::";
thresh = 0.6;
tradingLDA = function (SYMBOL, train.range, test.range, threshold) {
  require(quantmod)
  require(PerformanceAnalytics)
  # Function to compute performance with transaction fees
  performance.transactions = function (SYMBOL, strat.position, cost = .001, cum.return) {
    require(xts)
    tick = as.xts(SYMBOL);
    ret = dailyReturn(tick);
    trade = as.numeric((strat.position != Lag(strat.position)));
    trade[1] = 1;
    trade = trade*cost;
    ret.sans.transactions = cum.return - trade;
    # Plot daily returns, positions (Cum Return, Daily Return, Drawdown)
    charts.PerformanceSummary(cbind(ret,cum.return,ret.sans.transactions))
    perform.out= rbind(SharpeRatio.annualized(ret.sans.transactions[,1],scale=252), 
                       Return.annualized(ret.sans.transactions[,1],scale=252),
                       Return.cumulative(ret.sans.transactions[,1], geometric=T))
    return(perform.out)
  }
  # Function to plot LDA density histograms
  ggplotLDAPrep <- function(x){
    require(ggplot2)
    if (!is.null(Terms <- x$terms)) {
      data <- model.frame(x)
      X <- model.matrix(delete.response(Terms), data)
      g <- model.response(data)
      xint <- match("(Intercept)", colnames(X), nomatch = 0L)
      if (xint > 0L) 
        X <- X[, -xint, drop = FALSE]
    }
    means <- colMeans(x$means)
    X <- scale(X, center = means, scale = FALSE) %*% x$scaling
    rtrn <- as.data.frame(cbind(X,labels=as.character(g)))
    rtrn <- data.frame(X,labels=as.character(g))
    return(rtrn)
  }
  
  SYMBOL1 = getSymbols(SYMBOL,auto.assign = F)
  train = SYMBOL1[train.range]
  test = SYMBOL1[test.range]
  chartSeries(SYMBOL1,subset='2016',theme=chartTheme('white'),TA='addVo();addDPO();')
  # Build and clean the SYMBOL data.frames (trouble using MASS functions w/ xts)
  train.volume = Vo(train)
  test.volume = Vo(test)
  volume = Vo(SYMBOL1)
  # Categorical direction based on returns
  train.direction = ifelse((dailyReturn(train) > 0),'Up','Down')
  test.direction = ifelse((dailyReturn(test) > 0),'Up','Down')
  direction = ifelse((dailyReturn(SYMBOL1) > 0),'Up','Down')
  # 10 lagged returns
  train.lags = Lag(dailyReturn(train),k=1:10);
  test.lags = Lag(dailyReturn(test),k=1:10);
  lags=Lag(dailyReturn(SYMBOL1),k=1:10);
  # Bind data by column
  train = cbind.data.frame(train.volume,train.lags,train.direction);
  test = cbind.data.frame(test.volume,test.lags,test.direction);
  SYMBOL1.df=cbind.data.frame(volume,lags,direction);
  # Rename categorical column
  colnames(train)[12] = 'Direction';
  colnames(test)[12] = 'Direction';
  colnames(SYMBOL1.df)[12] = "Direction";
  # Remove NA observations
  train = na.omit(train); test = na.omit(test); SYMBOL1.df = na.omit(SYMBOL1.df);
  
  # Linear Discriminant Analysis
  require(MASS)
  set.seed(222)
  lda.fit = lda(Direction ~ ., data=train)
  lda.fit
  fitted.plot = ggplotLDAPrep(lda.fit)
  title.gg = paste(SYMBOL,"LDA")
  print(ggplot(fitted.plot, aes(LD1,..density..,fill=labels))+geom_histogram() + ggtitle(title.gg))
  lda.pred = predict(lda.fit,test)
  lda.class=lda.pred$class 
  lda.table=table(lda.class,test$Direction)
  lda.table
  lda.error=mean(lda.class!=test$Direction)
  lda.error
  
  # Use full data for prediction-based trading system using LDA posteriors
  set.seed(222)
  lda.mod = lda(Direction ~ ., data=SYMBOL1.df)
  lda.mod
  # Predict market direction (in class element)
  lda.fullpreds = predict(lda.mod,SYMBOL1.df)
  labels(lda.fullpreds)
  # Pull posterior probabilities to use for trade rules using post prob threshold
  post.down = lda.fullpreds$posterior[,'Down']
  # Long: posterior down <= threshold
  # Short: posterior down > threshold (predicted to go up )
  # Use table to build a position multiplier :: PM = (% Up) / (% Down)
  lda.fullclass = lda.fullpreds$class
  perc.up = sum(lda.fullclass == 'Up') / sum(SYMBOL1.df$Direction == 'Up')
  perc.up
  perc.Down = sum(lda.fullclass == 'Down') / sum(SYMBOL1.df$Direction == 'Down')
  perc.Down
  multiplier = perc.up / perc.Down
  # Align SYMBOL and SYMBOL.df nrow
  SYMBOL1 = getSymbols(SYMBOL,auto.assign = F)
  omit = nrow(SYMBOL1) - nrow(SYMBOL1.df)
  SYMBOL1 = SYMBOL1[-c(1:omit), ]
  SYMBOL1 <- merge(SYMBOL1, Position=as.xts(ifelse(post.down <= threshold, 1, -1),
                                  dateFormat="Date"))
  SYMBOL1 <- merge(SYMBOL1, MultiLong=as.xts(ifelse(post.down <= threshold, 1*multiplier, -1),
                                          dateFormat="Date"))
  SYMBOL1 <- merge(SYMBOL1, MultiShort=as.xts(ifelse(post.down <= threshold, 1, -1*multiplier),
                                          dateFormat="Date"))
  SYMBOL1 <- merge(SYMBOL1, DualMulti=as.xts(ifelse(post.down <= threshold, 1*multiplier, -1*multiplier),
                                          dateFormat="Date"))
  SYMBOL1=na.omit(SYMBOL1)
  # Build return vector and plot returns
  my.ret <- lag(SYMBOL1$Position) * dailyReturn(SYMBOL1)
  ml.ret = lag(SYMBOL1$MultiLong) * dailyReturn(SYMBOL1)
  ms.ret = lag(SYMBOL1$MultiShort) * dailyReturn(SYMBOL1)
  dm.ret = lag(SYMBOL1$DualMulti) * dailyReturn(SYMBOL1)
  # Performance Statistics
  rbind(SharpeRatio.annualized(my.ret$Position, scale = 252), 
        Return.annualized(my.ret$Position,scale=252))
  # Risk Return Plot
  chart.RiskReturnScatter(my.ret)

  # TAKE TRANSACTION COSTS INTO ACCOUNT TO SEE IF STRATEGY IS VIABLE
  performance.output = list(performance.transactions(SYMBOL1, SYMBOL1$Position, cost = .001, my.ret),
                              performance.transactions(SYMBOL1, SYMBOL1$MultiLong, cost = .001, ml.ret),
                              performance.transactions(SYMBOL1, SYMBOL1$MultiShort, cost = .001, ms.ret),
                              performance.transactions(SYMBOL1, SYMBOL1$DualMulti, cost = .001, dm.ret))
  return (performance.output)
}
tradingLDA(SYMBOL = ticker, train.range = train.range, test.range = test.range, threshold = thresh)
