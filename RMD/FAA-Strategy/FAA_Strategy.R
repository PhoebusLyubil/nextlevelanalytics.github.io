FAA_Strategy<-function(){
  load.packages('quantmod')
  
  
  tickers = '
US.STOCKS = VTI + VTSMX
FOREIGN.STOCKS = VEU + FDIVX
EMERGING>MARKETS=EEM + VEIEX
US.10YR.GOV.BOND = IEF + VFITX
REAL.ESTATE = VNQ + VGSIX
COMMODITIES = DBC + CRB
CASH = BND + VBMFX
'
  
  load('./data/data.proxy.raw.Rdata')
  # Load saved Proxies Raw Data, data.proxy.raw
  
  data <- new.env()
  getSymbols.extra(tickers, src = 'yahoo', from = '1996-01-01', env = data, 
                   raw.data = data.proxy.raw, auto.assign = T,  set.symbolnames = T)
  # Get data at appropriate date
  
  for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
  bt.prep(data, align='remove.na', dates='::')
  
  data$universe = data$prices > 0
  # Sets to TRUE(or 1) all data prices that make sense
  
  data$universe$CASH = NA 
  # Set weighting of cash to NA
  
  prices = data$prices * data$universe
  # Filter out bad data that had negative prices
  n = ncol(prices)
  # Number of instruments
  
  period.ends = endpoints(prices, 'months')
  
  # Set period end to monthly endpoint
  period.ends <- period.ends[-1]
  if(period.ends[1]==1){ # If data started at end of month
    period.ends<-period.ends[-1]
  }
  
  period.ends<-period.ends-1
  # Signal generated one day before month end
  
  models = list()
  # Create an empty list
  
  commission = list(cps = 0.01, fixed = 10.0, percentage = 0.0)
  # Commission
  
  ret = diff(log(prices))
  # Get logged returns as a value
  
  n.top = 3
  # Number of assets in top category
  
  mom.lookback = 80
  vol.lookback = 80
  cor.lookback = 80
  # Variables for time lookback
  
  weight=c(1, 0.5, 0.5)
  # Weight for variables
  
  hist.vol = sqrt(252) * bt.apply.matrix(ret, runSD, n = vol.lookback)
  # Get the historical volatility in returns
  
  mom = (prices / mlag(prices, mom.lookback) - 1)[period.ends,]
  # Momentum calculation
  
  avg.cor = data$weight * NA
  # Set avg.cor too all NA
  
  for(i in period.ends[period.ends > cor.lookback]){
    hist = ret[(i - cor.lookback):i,]
    include.index = !is.na(colSums(hist))
    correlation = cor(hist[,include.index], use='complete.obs',method='pearson')
    avg.correlation = rowSums(correlation, na.rm=T)
    
    avg.cor[i,include.index] = avg.correlation
  }
  
  # Get correlations for each period
  mom.rank = br.rank(mom)
  cor.rank = br.rank(-(avg.cor[period.ends,]))
  vol.rank = br.rank(-hist.vol[period.ends,])
  
  # Ranking
  avg.rank = weight[1]*mom.rank + weight[2]*vol.rank + weight[3]*cor.rank
  meta.rank = br.rank(-avg.rank)
  
  #absolute momentum filter 
  weight = (meta.rank <= n.top)/rowSums(meta.rank <= n.top, na.rm=T) * (mom > 0)
  
  # cash logic
  weight$CASH = 1 - rowSums(weight,na.rm=T)
  data$weight[period.ends+1,] = weight[1:length(period.ends),] 
  # Set weight at period end based off signal from previous day
  
  models$strategy = bt.run.share(data, clean.signal=F,  trade.summary=T, silent=T, commission=commission)
  # Run strategy 
  
  models$strategy$period.weight = weight[1:length(period.ends),] 
  # To make the signal report work
  
  
  plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)        
  mtext('Cumulative Performance', side = 2, line = 1)
  
  plotbt.strategy.sidebyside(models, make.plot=F, return.table=T, perfromance.fn = engineering.returns.kpi)
  
  m = names(models)[1]
  
  plotbt.transition.map(models[[m]]$weight, name=m)
  legend('topright', legend = m, bty = 'n')
  
  plotbt.monthly.table(models[[m]]$equity, make.plot = F)
   
   plotbt(models, plottype = '12M', LeftMargin = 3)        
   mtext('12 Month Rolling', side = 2, line = 1)
   
   plotbt(models, xfun = function(x) { 100 * compute.drawdown(x$equity) }, LeftMargin = 3)
   mtext('Drawdown', side = 2, line = 1)
   
   signals = last.signals(models[m], make.plot=F, return.table=T, n=20)
   trades = last.trades(models[m], make.plot=F, return.table=T, n=20)
   
   bt.detail.summary
}