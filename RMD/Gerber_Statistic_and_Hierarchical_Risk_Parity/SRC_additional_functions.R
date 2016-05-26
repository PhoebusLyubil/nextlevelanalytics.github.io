repmat <- 
  function (v, n, m) 
  {
    kronecker(matrix(1, n, m), v)
  }
plota.theme <- function
(
  col.border = 'black',
  col.up = 'green',
  col.dn = 'red',
  col.x.highlight = 'orange',
  col.y.highlight = 'orange',
  alpha=NA
)
{
  col = c(col.border, col.up, col.dn, col.x.highlight, col.y.highlight)
  if(!is.na(alpha)) col = col.add.alpha(col, alpha)
  plota.control$col.border = col[1]
  plota.control$col.up = col[2]
  plota.control$col.dn = col[3]
  plota.control$col.x.highlight = col[4]
  plota.control$col.y.highlight = col[5]
}

plota.theme.green.orange <- function(alpha=NA)
{
  plota.theme(
    col.border = rgb(68,68,68, maxColorValue=255),
    col.up = rgb(0,204,0, maxColorValue=255),
    col.dn = rgb(255,119,0, maxColorValue=255),
    alpha = alpha
  )
}

plota.control = new.env()
plota.control$col.border = 'black'
plota.control$col.up = 'green'
plota.control$col.dn = 'red'
plota.control$col.x.highlight = 'orange'
plota.control$col.y.highlight = 'orange'
plota.control$xaxis.ticks = c()
plota.theme.green.orange()

beta.degree <- 
function (beta) 
{
    atan(beta) * 360/(2 * pi)
}

bt.apply <- 
function (b, xfun = Cl, ...) 
{
    out = b$weight
    out[] = NA
    symbolnames = b$symbolnames
    nsymbols = length(symbolnames)
    xfun = match.fun(xfun)
    for (i in 1:nsymbols) {
        msg = try(xfun(coredata(b[[symbolnames[i]]]), ...), silent = TRUE)
        if (class(msg)[1] == "try-error") 
            warning(i, msg, "\n")
        else out[, i] = msg
    }
    return(out)
}

bt.apply.matrix <- 
function (b, xfun = Cl, ...) 
{
    out = b
    out[] = NA
    nsymbols = ncol(b)
    xfun = match.fun(xfun)
    for (i in 1:nsymbols) {
        msg = try(xfun(coredata(b[, i]), ...), silent = TRUE)
        if (class(msg)[1] == "try-error") 
            warning(i, msg, "\n")
        else out[, i] = msg
    }
    return(out)
}

bt.calendar.strategy.fed.days.test <- 
function () 
{
    load.packages("quantmod")
    tickers = spl("SPY")
    data <- new.env()
    getSymbols.extra(tickers, src = "yahoo", from = "1980-01-01", 
        env = data, set.symbolnames = T, auto.assign = T)
    for (i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], 
        use.Adjusted = T)
    bt.prep(data, align = "keep.all", fill.gaps = T)
    prices = data$prices
    n = ncol(prices)
    dates = data$dates
    models = list()
    universe = prices > 0
    universe = universe & prices > SMA(prices, 100)
    info = get.FOMC.dates(F)
    key.date.index = na.omit(match(info$day, dates))
    key.date = NA * prices
    key.date[key.date.index, ] = T
    signals = list(T0 = 0)
    for (i in 1:15) signals[[paste0("N", i)]] = 0:i
    signals = calendar.signal(key.date, signals)
    models = calendar.strategy(data, signals, universe = universe)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    strategy.performance.snapshoot(models, T, sort.performance = F)
    dev.off()
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    out = sapply(models, function(x) list(CAGR = 100 * compute.cagr(x$equity), 
        MD = 100 * compute.max.drawdown(x$equity), Win = x$trade.summary$stats["win.prob", 
            "All"], Profit = x$trade.summary$stats["profitfactor", 
            "All"]))
    performance.barchart.helper(out, sort.performance = F)
    dev.off()
    png(filename = "plot3.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    strategy.performance.snapshoot(models$N15, control = list(main = T))
    dev.off()
    png(filename = "plot4.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    last.trades(models$N15)
    dev.off()
    png(filename = "plot5.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    trades = models$N15$trade.summary$trades
    trades = make.xts(parse.number(trades[, "return"]), as.Date(trades[, 
        "entry.date"]))
    layout(1:2)
    par(mar = c(4, 3, 3, 1), cex = 0.8)
    barplot(trades, main = "N15 Trades", las = 1)
    plot(cumprod(1 + trades/100), type = "b", main = "N15 Trades", 
        las = 1)
    dev.off()
}

bt.cluster.optimal.number.historical.test <- 
function () 
{
    load.packages("quantmod")
    tickers = spl("GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT")
    dates = "2007:03::"
    tickers = dow.jones.components()
    dates = "1970::"
    tickers = sp500.components()$tickers
    dates = "1994::"
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "1900-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    bt.prep(data, align = "keep.all", dates = dates)
    portfolio.allocation.custom.stats.clusters <- function(x, 
        ia) {
        return(list(ncluster.90 = max(cluster.group.kmeans.90(ia)), 
            ncluster.elbow = max(cluster.group.kmeans.elbow(ia)), 
            ncluster.hclust = max(cluster.group.hclust(ia))))
    }
    periodicity = "weeks"
    lookback.len = 250
    obj = portfolio.allocation.helper(data$prices, periodicity = periodicity, 
        lookback.len = lookback.len, min.risk.fns = list(EW = equal.weight.portfolio), 
        custom.stats.fn = portfolio.allocation.custom.stats.clusters)
    temp = list(ncluster.90 = "Kmeans 90% variance", ncluster.elbow = "Kmeans Elbow", 
        ncluster.hclust = "Hierarchical clustering at 1/3 height")
    for (i in 1:len(temp)) {
        hist.cluster = obj[[names(temp)[i]]]
        title = temp[[i]]
        png(filename = paste("plot", i, ".png", sep = ""), width = 600, 
            height = 500, units = "px", pointsize = 12, bg = "white")
        plota(hist.cluster, type = "l", col = "gray", main = title)
        plota.lines(SMA(hist.cluster, 10), type = "l", col = "red", 
            lwd = 5)
        plota.legend("Number of Clusters,10 period moving average", 
            "gray,red", x = "bottomleft")
        dev.off()
    }
}

bt.cluster.risk.parity.dow.30 <- 
function () 
{
    load.packages("quantmod")
    dates = "1995::"
    name = "Dow Jones 30"
    data = load.dow.jones(align = "keep.all", dates = dates)
    sectors = data$sectors
    tickers = data$symbolnames
    periodicity = "weeks"
    lookback.len = 250
    cluster.group <<- cluster.group.kmeans.90
    obj = portfolio.allocation.helper(data$prices, periodicity = periodicity, 
        lookback.len = lookback.len, min.risk.fns = list(EW = equal.weight.portfolio, 
            RP = risk.parity.portfolio(), ERC = equal.risk.contribution.portfolio, 
            Static.EW = distribute.weights(equal.weight.portfolio, 
                static.group(as.numeric(sectors))), Static.RP = distribute.weights(risk.parity.portfolio(), 
                static.group(sectors)), Static.ERC = distribute.weights(equal.risk.contribution.portfolio, 
                static.group(sectors)), Dynamic.EW = distribute.weights(equal.weight.portfolio, 
                cluster.group), Dynamic.RP = distribute.weights(risk.parity.portfolio(), 
                cluster.group), Dynamic.ERC = distribute.weights(equal.risk.contribution.portfolio, 
                cluster.group)), adjust2positive.definite = F, 
        custom.stats.fn = portfolio.allocation.custom.stats)
    models = create.strategies(obj, data, dates = (lookback.len):nrow(data$prices))$models
    title = paste(name, "(", periodicity, ",", lookback.len, 
        "days )")
    stats = bt.summary.report(models, title, data, obj, control = list(plot.weight.transition.maps = F, 
        plot.risk.contribution.transition.maps = F))
}

bt.extend.DBC.update.test <- 
function () 
{
    load.packages("quantmod")
    tickers = spl("GSG,DBC")
    data = new.env()
    getSymbols(tickers, src = "yahoo", from = "1970-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    temp = extract.table.from.webpage(join(readLines("TRJ_CRB")), 
        "EODValue")
    temp = join(apply(temp, 1, join, ","), "\n")
    data$CRB_1 = make.stock.xts(read.xts(temp, format = "%m/%d/%y"))
    data$CRB_2 = make.stock.xts(read.xts("prfmdata.csv", format = "%m/%d/%Y"))
    bt.prep(data, align = "remove.na")
    png(filename = "plot1.png", width = 500, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plota.matplot(scale.one(data$prices))
    dev.off()
}

bt.fa.one.month.test <- 
function () 
{
    load.packages("quantmod")
    info = sp500.components()
    tickers = info$tickers
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "1970-01-01", env = data, 
        auto.assign = T)
    rm.index = which(sapply(ls(data), function(x) nrow(data[[x]])) < 
        1000)
    rm(list = names(rm.index), envir = data)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    bt.prep(data, align = "keep.all", dates = "1994::")
    tickers = data$symbolnames
    sector = info$sector[match(tickers, info$tickers)]
    data.spy <- new.env()
    getSymbols("SPY", src = "yahoo", from = "1970-01-01", env = data.spy, 
        auto.assign = T)
    bt.prep(data.spy, align = "keep.all", dates = "1994::")
    prices = data$prices
    n = ncol(prices)
    periodicity = "months"
    period.ends = endpoints(data$prices, periodicity)
    period.ends = period.ends[period.ends > 0]
    prices = prices[period.ends, ]
    models = list()
    n.skip = 36
    data.spy$weight[] = NA
    data.spy$weight[] = 1
    data.spy$weight[1:period.ends[n.skip], ] = NA
    models$spy = bt.run(data.spy)
    data$weight[] = NA
    data$weight[period.ends, ] = ntop(prices, n)
    data$weight[1:period.ends[n.skip], ] = NA
    models$equal.weight = bt.run(data)
    factors = get.fama.french.data("F-F_Research_Data_Factors", 
        periodicity = periodicity, download = F, clean = F)
    map = match(format(index(factors$data), "%Y%m"), format(index(prices), 
        "%Y%m"))
    dates = index(factors$data)
    dates[!is.na(map)] = index(prices)[na.omit(map)]
    index(factors$data) = as.Date(dates)
    data.fa <- new.env()
    for (i in tickers) data.fa[[i]] = data[[i]][period.ends, 
        ]
    data.fa$factors = factors$data/100
    bt.prep(data.fa, align = "remove.na")
    index = match(index(data.fa$prices), index(data$prices))
    prices = data$prices[index, ]
    temp = NA * prices
    factors = list()
    factors$last.e = temp
    factors$last.e_s = temp
    for (i in tickers) {
        cat(i, "\n")
        obj = factor.rolling.regression(data.fa, i, 36, silent = T, 
            factor.rolling.regression.custom.stats)
        for (j in 1:len(factors)) factors[[j]][, i] = obj$fl$custom[, 
            j]
    }
    factors$one.month = coredata(prices/mlag(prices))
    load(file = "data.ff.factors.Rdata")
    quantiles = list()
    for (name in names(factors)) {
        cat(name, "\n")
        quantiles[[name]] = bt.make.quintiles(factors[[name]], 
            data, index, start.t = 1 + 36, prefix = paste(name, 
                "_", sep = ""))
    }
    png(filename = "plot1.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(quantiles$one.month$spread, quantiles$last.e$spread, 
        quantiles$last.e_s$spread)
    dev.off()
    png(filename = "plot2.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.strategy.sidebyside(quantiles$one.month$spread, quantiles$last.e$spread, 
        quantiles$last.e_s$spread)
    dev.off()
    png(filename = "plot3.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(quantiles$last.e)
    dev.off()
    png(filename = "plot4.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(quantiles$last.e_s)
    dev.off()
}

bt.fa.sector.one.month.test <- 
function () 
{
    load.packages("quantmod")
    info = sp500.components()
    tickers = info$tickers
    data <- new.env()
    for (i in tickers) try(getSymbols(i, src = "yahoo", from = "1980-01-01", 
        env = data, auto.assign = T), TRUE)
    rm.index = which(sapply(ls(data), function(x) nrow(data[[x]])) < 
        1000)
    rm(list = names(rm.index), envir = data)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    bt.prep(data, align = "keep.all", dates = "1994::")
    tickers = data$symbolnames
    sector = info$sector[match(tickers, info$tickers)]
    data.spy <- new.env()
    getSymbols("SPY", src = "yahoo", from = "1970-01-01", env = data.spy, 
        auto.assign = T)
    bt.prep(data.spy, align = "keep.all", dates = "1994::")
    save(data, data.spy, tickers, sector, file = "data.sp500.components.Rdata")
    prices = data$prices
    n = ncol(prices)
    periodicity = "months"
    period.ends = endpoints(data$prices, periodicity)
    period.ends = period.ends[period.ends > 0]
    prices = prices[period.ends, ]
    models = list()
    n.skip = 36
    data.spy$weight[] = NA
    data.spy$weight[] = 1
    data.spy$weight[1:period.ends[n.skip], ] = NA
    models$spy = bt.run(data.spy)
    data$weight[] = NA
    data$weight[period.ends, ] = ntop(prices, n)
    data$weight[1:period.ends[n.skip], ] = NA
    models$equal.weight = bt.run(data)
    factors = get.fama.french.data("F-F_Research_Data_Factors", 
        periodicity = periodicity, download = T, clean = F)
    if (periodicity == "months") {
        map = match(format(index(factors$data), "%Y%m"), format(index(prices), 
            "%Y%m"))
        dates = index(factors$data)
        dates[!is.na(map)] = index(prices)[na.omit(map)]
        index(factors$data) = as.Date(dates)
    }
    data.fa <- new.env()
    for (i in tickers) data.fa[[i]] = data[[i]][period.ends, 
        ]
    data.fa$factors = factors$data/100
    bt.prep(data.fa, align = "remove.na")
    index = match(index(data.fa$prices), index(data$prices))
    prices = data$prices[index, ]
    temp = NA * prices
    factors = list()
    factors$last.e = temp
    factors$last.e_s = temp
    for (i in tickers) {
        cat(i, "\n")
        obj = factor.rolling.regression(data.fa, i, 36, silent = T, 
            factor.rolling.regression.custom.stats)
        for (j in 1:len(factors)) factors[[j]][, i] = obj$fl$custom[, 
            j]
    }
    nlag = iif(periodicity == "months", 1, 4)
    factors$one.month = coredata(prices/mlag(prices, nlag))
    save(factors, file = "data.ff.factors.Rdata")
    quantiles = list()
    for (name in names(factors)) {
        cat(name, "\n")
        quantiles[[name]] = bt.make.quintiles(factors[[name]], 
            data, index, start.t = 1 + 36, prefix = paste(name, 
                "_", sep = ""))
    }
    quantiles.sn = list()
    for (name in names(factors)) {
        cat(name, "\n")
        quantiles.sn[[name]] = bt.make.quintiles.sector(sector, 
            factors[[name]], data, index, start.t = 1 + 36, prefix = paste(name, 
                "_", sep = ""))
    }
    save(quantiles, quantiles.sn, file = "model.quantiles.Rdata")
    png(filename = "plot1.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(quantiles$one.month$spread, quantiles$last.e$spread, 
        quantiles$last.e_s$spread, quantiles.sn$one.month$spread.sn, 
        quantiles.sn$last.e$spread.sn, quantiles.sn$last.e_s$spread.sn)
    dev.off()
    png(filename = "plot2.png", width = 800, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.strategy.sidebyside(quantiles$one.month$spread, quantiles$last.e$spread, 
        quantiles$last.e_s$spread, quantiles.sn$one.month$spread.sn, 
        quantiles.sn$last.e$spread.sn, quantiles.sn$last.e_s$spread.sn)
    dev.off()
    png(filename = "plot3.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(quantiles.sn$one.month)
    dev.off()
    png(filename = "plot4.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(quantiles.sn$last.e_s)
    dev.off()
    png(filename = "plot1a.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(c(models, quantiles$one.month))
    dev.off()
    png(filename = "plot2a.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(c(models, quantiles$one.month$spread))
    dev.off()
    png(filename = "plot1b.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(quantiles$one.month$spread, quantiles$last.e$spread, 
        quantiles$last.e_s$spread)
    dev.off()
    png(filename = "plot2b.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.strategy.sidebyside(quantiles$one.month$spread, quantiles$last.e$spread, 
        quantiles$last.e_s$spread)
    dev.off()
    png(filename = "plot3b.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(quantiles$last.e)
    dev.off()
    png(filename = "plot4b.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(quantiles$last.e_s)
    dev.off()
}

bt.fa.value.quantiles.test <- 
function () 
{
    load.packages("quantmod")
    tickers = sp500.components()$tickers
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "1970-01-01", env = data, 
        auto.assign = T)
    rm.index = which(sapply(ls(data), function(x) nrow(data[[x]])) < 
        1000)
    rm(list = names(rm.index), envir = data)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    bt.prep(data, align = "keep.all", dates = "1994::")
    tickers = data$symbolnames
    data.spy <- new.env()
    getSymbols("SPY", src = "yahoo", from = "1970-01-01", env = data.spy, 
        auto.assign = T)
    bt.prep(data.spy, align = "keep.all", dates = "1994::")
    prices = data$prices
    nperiods = nrow(prices)
    n = ncol(prices)
    models = list()
    data.spy$weight[] = NA
    data.spy$weight[] = 1
    models$spy = bt.run(data.spy)
    data$weight[] = NA
    data$weight[] = ntop(prices, n)
    models$equal.weight = bt.run(data)
    periodicity = "weeks"
    factors = get.fama.french.data("F-F_Research_Data_Factors", 
        periodicity = periodicity, download = F, clean = F)
    period.ends = endpoints(data$prices, periodicity)
    period.ends = period.ends[period.ends > 0]
    data.fa <- new.env()
    for (i in tickers) data.fa[[i]] = data[[i]][period.ends, 
        ]
    data.fa$factors = factors$data/100
    bt.prep(data.fa, align = "remove.na")
    index = match(index(data.fa$prices), index(data$prices))
    measure = data$prices[index, ]
    for (i in tickers) {
        cat(i, "\n")
        obj = factor.rolling.regression(data.fa, i, 36, silent = T)
        measure[, i] = coredata(obj$fl$estimate$HML)
    }
    n.quantiles = 5
    start.t = 1 + 36
    quantiles = weights = coredata(measure) * NA
    for (t in start.t:nrow(weights)) {
        factor = as.vector(coredata(measure[t, ]))
        ranking = ceiling(n.quantiles * rank(factor, na.last = "keep", 
            "first")/count(factor))
        quantiles[t, ] = ranking
        weights[t, ] = 1/tapply(rep(1, n), ranking, sum)[ranking]
    }
    quantiles = ifna(quantiles, 0)
    for (i in 1:n.quantiles) {
        temp = weights * NA
        temp[] = 0
        temp[quantiles == i] = weights[quantiles == i]
        data$weight[] = NA
        data$weight[index, ] = temp
        models[[paste("Q", i, sep = "_")]] = bt.run(data, silent = T)
    }
    png(filename = "plot1.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(models)
    dev.off()
    png(filename = "plot2.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.strategy.sidebyside(models)
    dev.off()
}

bt.one.month.test <- 
function () 
{
    load.packages("quantmod")
    tickers = sp500.components()$tickers
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "1970-01-01", env = data, 
        auto.assign = T)
    rm.index = which(sapply(ls(data), function(x) nrow(data[[x]])) < 
        1000)
    rm(list = names(rm.index), envir = data)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    bt.prep(data, align = "keep.all", dates = "1994::")
    tickers = data$symbolnames
    data.spy <- new.env()
    getSymbols("SPY", src = "yahoo", from = "1970-01-01", env = data.spy, 
        auto.assign = T)
    bt.prep(data.spy, align = "keep.all", dates = "1994::")
    prices = data$prices
    n = ncol(prices)
    periodicity = "months"
    period.ends = endpoints(data$prices, periodicity)
    period.ends = period.ends[period.ends > 0]
    prices = prices[period.ends, ]
    models = list()
    n.skip = 36
    n.skip = 2
    data.spy$weight[] = NA
    data.spy$weight[] = 1
    data.spy$weight[1:period.ends[n.skip], ] = NA
    models$spy = bt.run(data.spy)
    data$weight[] = NA
    data$weight[period.ends, ] = ntop(prices, n)
    data$weight[1:period.ends[n.skip], ] = NA
    models$equal.weight = bt.run(data)
    one.month = coredata(prices/mlag(prices))
    models = c(models, bt.make.quintiles(one.month, data, period.ends, 
        start.t = 1 + n.skip, prefix = "M1_"))
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(models)
    dev.off()
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(models[spl("spy,equal.weight,spread")])
    dev.off()
}

bt.pca.test <- 
function () 
{
    tickers = spl("XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU")
    tickers.desc = spl("ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities")
    sector.map = c()
    for (i in 1:len(tickers)) {
        sector.map = rbind(sector.map, cbind(sector.spdr.components(tickers[i]), 
            tickers.desc[i]))
    }
    colnames(sector.map) = spl("ticker,sector")
    load.packages("quantmod")
    tickers = dow.jones.components()
    sectors = factor(sector.map[match(tickers, sector.map[, "ticker"]), 
        "sector"])
    names(sectors) = tickers
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "2000-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    bt.prep(data, align = "keep.all", dates = "2012")
    sectors = sectors[data$symbolnames]
    save(data, tickers, sectors, file = "bt.pca.test.Rdata")
    prices = data$prices
    ret = prices/mlag(prices) - 1
    p = princomp(na.omit(ret))
    loadings = p$loadings[]
    p.variance.explained = p$sdev^2/sum(p$sdev^2)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    barplot(100 * p.variance.explained, las = 2, xlab = "", ylab = "% Variance Explained")
    dev.off()
    x = loadings[, 1]
    y = loadings[, 2]
    z = loadings[, 3]
    cols = as.double(sectors)
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plot(x, y, type = "p", pch = 20, col = cols, xlab = "Comp.1", 
        ylab = "Comp.2")
    text(x, y, data$symbolnames, col = cols, cex = 0.8, pos = 4)
    legend("topright", cex = 0.8, legend = levels(sectors), fill = 1:nlevels(sectors), 
        merge = F, bty = "n")
    dev.off()
    load.packages("scatterplot3d")
    png(filename = "plot3.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    s3d = scatterplot3d(x, y, z, xlab = "Comp.1", ylab = "Comp.2", 
        zlab = "Comp.3", color = cols, pch = 20)
    s3d.coords = s3d$xyz.convert(x, y, z)
    text(s3d.coords$x, s3d.coords$y, labels = data$symbolnames, 
        col = cols, cex = 0.8, pos = 4)
    legend("topleft", cex = 0.8, legend = levels(sectors), fill = 1:nlevels(sectors), 
        merge = F, bty = "n")
    dev.off()
}

bt.pca.trading.test <- 
function () 
{
    load.packages("quantmod")
    tickers = dow.jones.components()
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "2009-01-01", env = data, 
        auto.assign = T)
    bt.prep(data, align = "remove.na")
    prices = last(data$prices, 1000)
    n = len(tickers)
    ret = prices/mlag(prices) - 1
    p = princomp(na.omit(ret[1:250, ]))
    loadings = p$loadings[]
    components = loadings[, 1:4]
    components = components/rep.row(colSums(abs(components)), 
        len(tickers))
    market = ret[1:250, ] %*% rep(1/n, n)
    temp = cbind(market, -ret[1:250, ] %*% components)
    colnames(temp)[1] = "Market"
    round(cor(temp, use = "complete.obs", method = "pearson"), 
        1)
    round(100 * sd(temp, na.rm = T), 1)
    library(tseries)
    layout(1:2)
    temp = rnorm(100)
    plot(temp, type = "b", main = adf.test(temp)$p.value)
    plot(cumsum(temp), type = "b", main = adf.test(cumsum(temp))$p.value)
    library(tseries)
    equity = bt.apply.matrix(1 + ifna(-ret %*% components, 0), 
        cumprod)
    equity = make.xts(equity, index(prices))
    adf.test(as.numeric(equity[, 1]))$p.value
    adf.test(as.numeric(equity[, 2]))$p.value
    adf.test(as.numeric(equity[, 3]))$p.value
    adf.test(as.numeric(equity[, 4]))$p.value
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(1:2)
    i.comp = 4
    bbands1 = BBands(rep.col(equity[, i.comp], 3), n = 200, sd = 1)
    bbands2 = BBands(rep.col(equity[, i.comp], 3), n = 200, sd = 2)
    temp = cbind(equity[, i.comp], bbands1[, "up"], bbands1[, 
        "dn"], bbands1[, "mavg"], bbands2[, "up"], bbands2[, 
        "dn"])
    colnames(temp) = spl("Comp. 4,1SD Up,1SD Down,200 SMA,2SD Up,2SD Down")
    plota.matplot(temp, main = paste(i.comp, "Principal component"))
    barplot.with.labels(sort(components[, i.comp]), "weights")
    dev.off()
    ts.sample = ts(as.numeric(equity[, i.comp]), frequency = 252)
    fit.stl = stl(ts.sample, s.window = "periodic")
    plot(fit.stl)
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(1:2)
    plota.matplot(prices, plotX = F)
    plota.matplot(equity)
    dev.off()
}

bt.rolling.cor.test <- 
function () 
{
    load.packages("quantmod")
    tickers = sp500.components()$tickers
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "1970-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    bt.prep(data, align = "keep.all", dates = "1970::")
    spy = getSymbols("SPY", src = "yahoo", from = "1970-01-01", 
        auto.assign = F)
    ret.spy = coredata(Cl(spy)/mlag(Cl(spy)) - 1)
    prices = data$prices["1993:01:29::"]
    nperiods = nrow(prices)
    ret = prices/mlag(prices) - 1
    ret = coredata(ret)
    index = which((count(t(prices)) > 100))
    index = index[-c(1:252)]
    avg.cor = NA * prices[, 1]
    avg.cor.spy = NA * prices[, 1]
    for (i in index) {
        hist = ret[(i - 252 + 1):i, ]
        hist = hist[, count(hist) == 252, drop = F]
        nleft = ncol(hist)
        correlation = cor(hist, use = "complete.obs", method = "pearson")
        avg.cor[i, ] = (sum(correlation) - nleft)/(nleft * (nleft - 
            1))
        avg.cor.spy[i, ] = sum(cor(ret.spy[(i - 252 + 1):i, ], 
            hist, use = "complete.obs", method = "pearson"))/nleft
        if (i%%100 == 0) 
            cat(i, "out of", nperiods, "\n")
    }
    png(filename = "plot.sp500.cor.png", width = 600, height = 500, 
        units = "px", pointsize = 12, bg = "white")
    sma50 = SMA(Cl(spy), 50)
    sma200 = SMA(Cl(spy), 200)
    cols = col.add.alpha(spl("green,red"), 50)
    plota.control$col.x.highlight = iif(sma50 > sma200, cols[1], 
        cols[2])
    highlight = sma50 > sma200 | sma50 < sma200
    plota(avg.cor, type = "l", ylim = range(avg.cor, avg.cor.spy, 
        na.rm = T), x.highlight = highlight, main = "Average 252 day Pairwise Correlation for stocks in SP500")
    plota.lines(avg.cor.spy, type = "l", col = "blue")
    plota.legend("Pairwise Correlation,Correlation with SPY,SPY 50-day SMA > 200-day SMA,SPY 50-day SMA < 200-day SMA", 
        c("black,blue", cols))
    dev.off()
}

bt.run <- 
function (b, trade.summary = F, do.lag = 1, do.CarryLastObservationForwardIfNA = TRUE, 
    type = c("weight", "share"), silent = F, capital = 1e+05, 
    commission = 0, weight = b$weight, dates = 1:nrow(b$prices)) 
{
    dates.index = dates2index(b$prices, dates)
    type = type[1]
    weight[] = ifna(weight, NA)
    if (do.lag > 0) 
        weight = mlag(weight, do.lag)
    if (do.CarryLastObservationForwardIfNA) 
        weight[] = apply(coredata(weight), 2, ifna.prev)
    weight[is.na(weight)] = 0
    weight1 = mlag(weight, -1)
    tstart = weight != weight1 & weight1 != 0
    tend = weight != 0 & weight != weight1
    trade = ifna(tstart | tend, FALSE)
    prices = b$prices
    if (sum(trade) > 0) {
        execution.price = coredata(b$execution.price)
        prices1 = coredata(b$prices)
        prices1[trade] = iif(is.na(execution.price[trade]), prices1[trade], 
            execution.price[trade])
        prices[] = prices1
    }
    if (type == "weight") {
        ret = prices/mlag(prices) - 1
        ret[] = ifna(ret, NA)
        ret[is.na(ret)] = 0
    }
    else {
        ret = prices
    }
    temp = b$weight
    temp[] = weight
    weight = temp
    bt = bt.summary(weight, ret, type, b$prices, capital, commission)
    bt$dates.index = dates.index
    bt = bt.run.trim.helper(bt, dates.index)
    if (trade.summary) 
        bt$trade.summary = bt.trade.summary(b, bt)
    if (!silent) {
        cat("Latest weights :\n")
        print(round(100 * last(bt$weight), 2))
        cat("\n")
        cat("Performance summary :\n")
        cat("", spl("CAGR,Best,Worst"), "\n", sep = "\t")
        cat("", sapply(cbind(bt$cagr, bt$best, bt$worst), function(x) round(100 * 
            x, 1)), "\n", sep = "\t")
        cat("\n")
    }
    return(bt)
}

bt.run.trim.helper <- 
function (bt, dates.index) 
{
    n.dates = len(dates.index)
    for (n in ls(bt)) {
        if (!is.null(dim(bt[[n]]))) {
            if (nrow(bt[[n]]) > n.dates) 
                bt[[n]] = bt[[n]][dates.index, , drop = F]
        }
        else if (len(bt[[n]]) > n.dates) 
            bt[[n]] = bt[[n]][dates.index]
    }
    bt$equity = bt$equity/as.double(bt$equity[1])
    bt$best = max(bt$ret)
    bt$worst = min(bt$ret)
    bt$cagr = compute.cagr(bt$equity)
    bt
}

bt.seasonality.test <- 
function () 
{
    load.packages("quantmod")
    tickers = dow.jones.components()
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "1970-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    bt.prep(data, align = "keep.all", dates = "1970::2011")
    prices = data$prices
    n = ncol(prices)
    month.ends = endpoints(prices, "months")
    prices = prices[month.ends, ]
    ret = prices/mlag(prices) - 1
    ret = ret[date.month(index(ret)) == 1, ]
    ret = last(ret, 20)
    stats = matrix(rep(NA, 2 * n), nc = n)
    colnames(stats) = colnames(prices)
    rownames(stats) = spl("N,Positive")
    for (i in 1:n) {
        stats["N", i] = sum(!is.na(ret[, i]))
        stats["Positive", i] = sum(ret[, i] > 0, na.rm = T)
    }
    sort(stats["Positive", ], decreasing = T)
    png(filename = "plot1.png", width = 600, height = 200, units = "px", 
        pointsize = 12, bg = "white")
    plot.table(stats[, order(stats["Positive", ], decreasing = T)[1:10]])
    dev.off()
}

bt.summary <- 
function (weight, ret, type = c("weight", "share"), close.prices, 
    capital = 1e+05, commission = 0) 
{
    if (!is.list(commission)) {
        if (type == "weight") 
            commission = list(cps = 0, fixed = 0, percentage = commission)
        else commission = list(cps = commission, fixed = 0, percentage = 0)
    }
    type = type[1]
    n = nrow(ret)
    bt = list()
    bt$weight = weight
    bt$type = type
    com.weight = mlag(weight, -1)
    if (type == "weight") {
        temp = ret[, 1]
        temp[] = rowSums(ret * weight) - rowSums(abs(com.weight - 
            mlag(com.weight)) * commission$percentage, na.rm = T)
        -rowSums(sign(abs(com.weight - mlag(com.weight))) * commission$fixed, 
            na.rm = T)
        bt$ret = temp
    }
    else {
        bt$share = weight
        bt$capital = capital
        prices = ret
        prices[] = bt.apply.matrix(coredata(prices), ifna.prev)
        close.prices[] = bt.apply.matrix(coredata(close.prices), 
            ifna.prev)
        cash = capital - rowSums(bt$share * mlag(close.prices), 
            na.rm = T)
        share.nextday = mlag(bt$share, -1)
        tstart = bt$share != share.nextday & share.nextday != 
            0
        tend = bt$share != 0 & bt$share != share.nextday
        trade = ifna(tstart | tend, FALSE)
        tstart = trade
        index = mlag(apply(tstart, 1, any))
        index = ifna(index, FALSE)
        index[1] = T
        totalcash = NA * cash
        totalcash[index] = cash[index]
        totalcash = ifna.prev(totalcash)
        totalcash = ifna(totalcash, 0)
        portfolio.ret = (totalcash + rowSums(bt$share * prices, 
            na.rm = T) - rowSums(abs(com.weight - mlag(com.weight)) * 
            commission$cps, na.rm = T) - rowSums(sign(abs(com.weight - 
            mlag(com.weight))) * commission$fixed, na.rm = T) - 
            rowSums(prices * abs(com.weight - mlag(com.weight)) * 
                commission$percentage, na.rm = T))/(totalcash + 
            rowSums(bt$share * mlag(prices), na.rm = T)) - 1
        bt$weight = bt$share * mlag(prices)/(totalcash + rowSums(bt$share * 
            mlag(prices), na.rm = T))
        bt$weight[is.na(bt$weight)] = 0
        temp = ret[, 1]
        temp[] = ifna(portfolio.ret, 0)
        temp[1] = 0
        bt$ret = temp
    }
    bt$best = max(bt$ret)
    bt$worst = min(bt$ret)
    bankrupt = which(bt$ret <= -1)
    if (len(bankrupt) > 0) 
        bt$ret[bankrupt[1]:n] = -1
    bt$equity = cumprod(1 + bt$ret)
    bt$cagr = compute.cagr(bt$equity)
    return(bt)
}

bt.trade.summary <- 
function (b, bt) 
{
    if (bt$type == "weight") 
        weight = bt$weight
    else weight = bt$share
    out = NULL
    weight1 = mlag(weight, -1)
    tstart = weight != weight1 & weight1 != 0
    tend = weight != 0 & weight != weight1
    tstart[1, weight[1, ] != 0] = T
    n = nrow(weight)
    tend[n, weight[n, ] != 0] = T
    trade = ifna(tstart | tend, FALSE)
    prices = b$prices[bt$dates.index, , drop = F]
    if (sum(trade) > 0) {
        execution.price = coredata(b$execution.price[bt$dates.index, 
            , drop = F])
        prices1 = coredata(b$prices[bt$dates.index, , drop = F])
        prices1[trade] = iif(is.na(execution.price[trade]), prices1[trade], 
            execution.price[trade])
        prices1[is.na(prices1)] = ifna(mlag(prices1), NA)[is.na(prices1)]
        prices[] = prices1
        weight = bt$weight
        symbolnames = b$symbolnames
        nsymbols = len(symbolnames)
        trades = c()
        for (i in 1:nsymbols) {
            tstarti = which(tstart[, i])
            tendi = which(tend[, i])
            if (len(tstarti) > 0) {
                if (len(tendi) > len(tstarti)) 
                  tstarti = c(1, tstarti)
                trades = rbind(trades, cbind(i, weight[(tstarti + 
                  1), i], tstarti, tendi, tendi - tstarti, as.vector(prices[tstarti, 
                  i]), as.vector(prices[tendi, i])))
            }
        }
        colnames(trades) = spl("symbol,weight,entry.date,exit.date,nhold,entry.price,exit.price")
        out = list()
        out$stats = cbind(bt.trade.summary.helper(trades), bt.trade.summary.helper(trades[trades[, 
            "weight"] >= 0, ]), bt.trade.summary.helper(trades[trades[, 
            "weight"] < 0, ]))
        colnames(out$stats) = spl("All,Long,Short")
        temp.x = index.xts(weight)
        trades = data.frame(coredata(trades))
        trades$symbol = symbolnames[trades$symbol]
        trades$nhold = as.numeric(temp.x[trades$exit.date] - 
            temp.x[trades$entry.date])
        trades$entry.date = temp.x[trades$entry.date]
        trades$exit.date = temp.x[trades$exit.date]
        trades$return = round(100 * (trades$weight) * (trades$exit.price/trades$entry.price - 
            1), 2)
        trades$entry.price = round(trades$entry.price, 2)
        trades$exit.price = round(trades$exit.price, 2)
        trades$weight = round(100 * (trades$weight), 1)
        out$trades = as.matrix(trades)
    }
    return(out)
}

bt.trade.summary.helper <- 
function (trades) 
{
    if (nrow(trades) <= 0) 
        return(NA)
    out = list()
    tpnl = trades[, "weight"] * (trades[, "exit.price"]/trades[, 
        "entry.price"] - 1)
    tlen = trades[, "exit.date"] - trades[, "entry.date"]
    out$ntrades = nrow(trades)
    out$avg.pnl = mean(tpnl)
    out$len = mean(tlen)
    out$win.prob = len(which(tpnl > 0))/out$ntrades
    out$win.avg.pnl = mean(tpnl[tpnl > 0])
    out$win.len = mean(tlen[tpnl > 0])
    out$loss.prob = 1 - out$win.prob
    out$loss.avg.pnl = mean(tpnl[tpnl < 0])
    out$loss.len = mean(tlen[tpnl < 0])
    out$expectancy = (out$win.prob * out$win.avg.pnl + out$loss.prob * 
        out$loss.avg.pnl)/100
    out$profitfactor = -(out$win.prob * out$win.avg.pnl)/(out$loss.prob * 
        out$loss.avg.pnl)
    return(as.matrix(unlist(out)))
}

bt.volatility.quantiles.test <- 
function () 
{
    load.packages("quantmod")
    tickers = sp500.components()$tickers
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "1970-01-01", env = data, 
        auto.assign = T)
    rm.index = which(sapply(ls(data), function(x) nrow(data[[x]])) < 
        1000)
    rm(list = names(rm.index), envir = data)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    bt.prep(data, align = "keep.all", dates = "1994::")
    data.spy <- new.env()
    getSymbols("SPY", src = "yahoo", from = "1970-01-01", env = data.spy, 
        auto.assign = T)
    bt.prep(data.spy, align = "keep.all", dates = "1994::")
    prices = data$prices
    nperiods = nrow(prices)
    n = ncol(prices)
    models = list()
    data.spy$weight[] = NA
    data.spy$weight[] = 1
    models$spy = bt.run(data.spy)
    data$weight[] = NA
    data$weight[] = ntop(prices, 500)
    models$equal.weight = bt.run(data)
    period.ends = endpoints(prices, "weeks")
    period.ends = period.ends[period.ends > 0]
    p = bt.apply.matrix(coredata(prices), ifna.prev)
    ret = p/mlag(p) - 1
    sd252 = bt.apply.matrix(ret, runSD, 252)
    n.quantiles = 5
    start.t = which(period.ends >= (252 + 2))[1]
    quantiles = weights = p * NA
    for (t in start.t:len(period.ends)) {
        i = period.ends[t]
        factor = sd252[i, ]
        ranking = ceiling(n.quantiles * rank(factor, na.last = "keep", 
            "first")/count(factor))
        quantiles[i, ] = ranking
        weights[i, ] = 1/tapply(rep(1, n), ranking, sum)[ranking]
    }
    quantiles = ifna(quantiles, 0)
    for (i in 1:n.quantiles) {
        temp = weights * NA
        temp[period.ends, ] = 0
        temp[quantiles == i] = weights[quantiles == i]
        data$weight[] = NA
        data$weight[] = temp
        models[[paste("Q", i, sep = "_")]] = bt.run(data, silent = T)
    }
    rowSums(models$Q_2$weight, na.rm = T)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.custom.report.part1(models)
    dev.off()
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plotbt.strategy.sidebyside(models)
    dev.off()
}

cap.weighted.mean <- 
function (data, capitalization) 
{
    capitalization = capitalization * (!is.na(data))
    weight = capitalization/rowSums(capitalization, na.rm = T)
    rowSums(data * weight, na.rm = T)
}

cboe.volatility.term.structure.SPX <- 
function (make.plot = T) 
{
    url = "http://www.cboe.com/data/volatilityindexes/volatilityindexes.aspx"
    txt = join(readLines(url))
    temp.table = extract.table.from.webpage(txt, "Trade Date", 
        has.header = T)
    colnames(temp.table) = gsub(" ", ".", trim(tolower(colnames(temp.table))))
    temp.table = data.frame(temp.table)
    temp.table$trade.date = as.POSIXct(temp.table$trade.date, 
        format = "%m/%d/%Y %I:%M:%S %p")
    temp.table$expiration.date = as.Date(temp.table$expiration.date, 
        "%d-%b-%y")
    temp.table[, 3] = as.numeric(as.character(temp.table[, 3]))
    temp.table[, 4] = as.numeric(as.character(temp.table[, 4]))
    temp.table
    if (make.plot) {
        plot(temp.table$expiration.date, temp.table$vix, type = "b", 
            main = paste("VIX Term Structure, generated ", max(temp.table$trade.date)), 
            xlab = "Expiration Month", ylab = "VIX Index Level")
        grid()
    }
    temp.table
}

compute.annual.factor <- 
function (x) 
{
    possible.values = c(252, 52, 26, 13, 12, 6, 4, 3, 2, 1)
    index = which.min(abs(compute.raw.annual.factor(x) - possible.values))
    round(possible.values[index])
}

compute.cagr <- 
function (equity, nyears = NA) 
{
    if (is.numeric(nyears)) 
        as.double(last(equity, 1)^(1/nyears) - 1)
    else as.double(last(equity, 1)^(1/compute.nyears(equity)) - 
        1)
}

compute.nyears <- 
function (x) 
{
    as.double(diff(as.Date(range(index.xts(x)))))/365
}

compute.quantiles <- 
function (data, next.month.ret, smain = "", n.quantiles = 5, 
    plot = T) 
{
    n = ncol(data)
    nperiods = nrow(data)
    data = coredata(ifna(data, NA))
    next.month.ret = coredata(ifna(next.month.ret, NA))
    temp = matrix(NA, nperiods, n.quantiles)
    hist.factor.quantiles = hist.ret.quantiles = temp
    temp = matrix(NA, nperiods, n)
    quantiles = weights = ranking = temp
    index = which(rowSums(!is.na(data)) >= n.quantiles)
    for (t in index) {
        factor = data[t, ]
        ret = next.month.ret[t, ]
        ranking[t, ] = rank(factor, na.last = "keep", "first")
        t.ranking = ceiling(n.quantiles * ranking[t, ]/count(factor))
        quantiles[t, ] = t.ranking
        weights[t, ] = 1/tapply(rep(1, n), t.ranking, sum)[t.ranking]
        hist.factor.quantiles[t, ] = tapply(factor, t.ranking, 
            mean)
        hist.ret.quantiles[t, ] = tapply(ret, t.ranking, mean)
    }
    if (plot) {
        par(mar = c(4, 4, 2, 1))
        temp = 100 * apply(hist.ret.quantiles, 2, mean, na.rm = T)
        barplot(temp, names.arg = paste(1:n.quantiles), ylab = "%", 
            main = paste(smain, ", spread =", round(temp[n.quantiles] - 
                temp[1], 2), "%"))
    }
    return(list(quantiles = quantiles, weights = weights, ranking = ranking, 
        hist.factor.quantiles = hist.factor.quantiles, hist.ret.quantiles = hist.ret.quantiles))
}

compute.raw.annual.factor <- 
function (x) 
{
    round(nrow(x)/compute.nyears(x))
}

consecutive.changes <- 
function (data, positive = T) 
{
    if (positive) 
        dir = diff(data) > 0
    else dir = diff(data) < 0
    temp = cumsum(iif(dir, 1, 0))
    temp - ifna.prev(iif(dir, NA, coredata(temp)))
}

count <- 
function (x, side = 2) 
{
    if (is.null(dim(x))) {
        sum(!is.na(x))
    }
    else {
        apply(!is.na(x), side, sum)
    }
}

custom.date <- 
function (expr, dates) 
{
    if (xts::is.xts(dates)) 
        dates = index(dates)
    dates = as.Date(dates)
    expr = gsub("the ", "", tolower(expr))
    expr = gsub(" in every ", " every ", expr)
    expr = gsub(" of every ", " every ", expr)
    tokens = trim(spl(spl(spl(expr, " in "), " every "), " of "))
    stack = list(splits = date.all(dates), dates.index = 1:len(dates))
    selected = rep.row(c(1, len(dates)), len(dates))
    selected.n = 1
    for (token in rev(tokens[nchar(tokens) > 0])) {
        selected0 = selected[1:selected.n, , drop = F]
        selected.n = 0
        for (i in 1:nrow(selected0)) {
            temp = custom.date.token(token, dates, stack, selected0[i, 
                ])
            selected[(selected.n + 1):(selected.n + nrow(temp)), 
                ] = temp
            selected.n = selected.n + nrow(temp)
        }
    }
    selected[1:selected.n, 1]
}

custom.date.bus <- 
function (expr, dates, calendar = NULL) 
{
    apply.business.days(dates, function(x) custom.date(expr, 
        x), calendar)
}

custom.date.debug <- 
function () 
{
    dates[custom.date("last day in first week in Apr", dates)]
    dates = seq(Sys.Date() - 1000, Sys.Date(), 1)
    custom.date("last day in Apr", dates)
    custom.date("3rd Mon in 1st Q", dates)
    custom.date("Mon in 3rd W in 1st Q", dates)
    dates = seq(Sys.Date() - 1000, Sys.Date(), 1)
    stack = env(splits, dates.index = 1:len(dates))
    i0 = 1
    i1 = len(dates)
    freq = "month"
    temp = custom.date.extract(i0, i1, freq, stack)
    temp = temp[rownames(temp) == month.map.abbr["apr"], ]
    dates[temp[, 1]]
    dates[temp[, 2]]
    i0 = temp[1, 1]
    i1 = temp[1, 2]
    freq = "week"
    temp1 = custom.date.extract(i0, i1, freq, stack)
    temp1 = temp1[1, , drop = F]
    dates[temp1[, 1]]
    dates[temp1[, 2]]
}

custom.date.extract <- 
function (i0, i1, freq, stack) 
{
    label = stack$splits[[freq]][i0:i1]
    label.n = len(label)
    temp = unique(c(0, stack$dates.index[1:label.n][diff(label) != 
        0], label.n))
    temp.n = len(temp)
    temp = cbind(1 + temp[1:(temp.n - 1)], temp[2:temp.n])
    rownames(temp) = label[temp[, 1]]
    (i0 - 1) + temp
}

custom.date.test <- 
function () 
{
    dates = seq(Sys.Date() - 1000, Sys.Date(), 1)
    dates[custom.date("last day in Apr", dates)]
    dates[custom.date("first day in Apr", dates)]
    dates[custom.date("last day in first week in Apr", dates)]
    dates[custom.date("last Mon in Apr", dates)]
    dates[custom.date("last Fri in Apr", dates)]
    dates[custom.date("first day in Apr", dates)]
    dates[custom.date("1st day in Apr", dates)]
    dates[custom.date("10th day in Apr", dates)]
    dates[custom.date("50th day in Apr", dates)]
    dates[custom.date("10th to last day in Apr", dates)]
    dates[custom.date("3rd Mon in Q", dates)]
    dates[custom.date("3rd Mon in 1st Q", dates)]
    dates[custom.date("3rd Mon in Q1", dates)]
    dates[custom.date("3rd Mon in last M in Q1", dates)]
    format(dates[custom.date("3rd Fri in Q", dates)], "%Y %b %d %w")
    dates = seq(as.Date("1-jan-2010", "%d-%b-%Y"), as.Date("29-apr-2015", 
        "%d-%b-%Y"), 1)
    dates[custom.date("last day in Apr", dates)]
    dates = seq(as.Date("1-jan-2010", "%d-%b-%Y"), as.Date("30-apr-2015", 
        "%d-%b-%Y"), 1)
    dates[custom.date("last day in Apr", dates)]
    dates = seq(as.Date("1-jan-2010", "%d-%b-%Y"), as.Date("29-apr-2015", 
        "%d-%b-%Y"), 1)
    dates[custom.date.bus("last day in Apr", dates)]
    dates = seq(as.Date("1-jan-2010", "%d-%b-%Y"), as.Date("20-oct-2015", 
        "%d-%b-%Y"), 1)
    dates[custom.date("last day in Apr", dates)]
    expect_identical(dates[custom.date("last day in Apr", dates)], 
        as.Date(c("2010-04-30", "2011-04-30", "2012-04-30", "2013-04-30", 
            "2014-04-30", "2015-04-30"), "%Y-%m-%d"))
}

custom.date.token <- 
function (expr, dates, stack, selected) 
{
    tokens = trim(spl(tolower(expr), " "))
    tokens = tokens[nchar(tokens) > 0]
    n.tokens = len(tokens)
    freq = custom.date.map(tokens[n.tokens])
    periods = date.periodicity.map(freq$freq)
    if (is.null(periods)) 
        warning("unknown freq", freq$freq)
    if (periods == "days") {
        temp = cbind(selected[1]:selected[2], selected[1]:selected[2])
        rownames(temp) = stack$splits$dayofweek[selected[1]:selected[2]]
    }
    else temp = custom.date.extract(selected[1], selected[2], 
        periods, stack)
    if (!is.null(freq$pick)) 
        temp = temp[rownames(temp) == freq$pick, , drop = F]
    if (n.tokens == 1) 
        return(temp)
    if (n.tokens == 2) {
        if (tokens[1] == "last") 
            return(mlast(temp))
        if (tokens[1] == "first") 
            return(temp[1, , drop = F])
    }
    offset = stringr::str_match(tokens[1], "^[0-9]+")[1]
    if (is.na(offset)) 
        warning("unknown offset", tokens[1])
    offset = as.numeric(offset)
    if (offset > nrow(temp)) {
        if (n.tokens == 2) 
            mlast(temp)
        else temp[1, , drop = F]
    }
    else {
        if (n.tokens == 2) 
            temp[offset, , drop = F]
        else temp[nrow(temp) - offset, , drop = F]
    }
}

dates2index <- 
function (x, dates = 1:nrow(x)) 
{
    dates.index = dates
    if (!is.numeric(dates)) {
        temp = x[, 1]
        temp[] = 1:nrow(temp)
        dates.index = as.numeric(temp[dates])
    }
    return(dates.index)
}

dow.jones.components <- 
function () 
{
    url = "http://money.cnn.com/data/dow30/"
    txt = join(readLines(url))
    temp = gsub(pattern = "\">", replacement = "<td>", txt, perl = TRUE)
    temp = gsub(pattern = "</a>", replacement = "</td>", temp, 
        perl = TRUE)
    temp = extract.table.from.webpage(temp, "Volume", has.header = T)
    trim(temp[, "Company"])
}

dow.jones.components.0 <- 
function () 
{
    url = "http://finance.yahoo.com/q/cp?s=^DJI+Components"
    txt = join(readLines(url))
    temp = extract.table.from.webpage(txt, "Volume", has.header = T)
    temp[, "Symbol"]
}

draw.cell <- 
function (title, r, c, text.cex = 1, bg.col = "white", frame.cell = T) 
{
    if (!frame.cell) 
        bcol = bg.col
    else bcol = "black"
    rect((2 * (c - 1) + 0.5), -(r - 0.5), (2 * c + 0.5), -(r + 
        0.5), col = bg.col, border = bcol)
    if (c == 1) {
        text((2 * (c - 1) + 0.5), -r, title, adj = 0, cex = text.cex)
    }
    else if (r == 1) {
        text((2 * (c - 1) + 0.5), -r, title, adj = 0, cex = text.cex)
    }
    else {
        text((2 * c + 0.5), -r, title, adj = 1, cex = text.cex)
    }
}

edgar.info <- 
function (ticker) 
{
    url = paste0("http://www.sec.gov/cgi-bin/browse-edgar?CIK=", 
        ticker, "&Find=Search&owner=exclude&action=getcompany")
    txt = join(readLines(url))
    out = list()
    temp = extract.table.from.webpage(txt, "seriesDiv,Filings", 
        has.header = T)
    out$fillings = clean.table(temp)
    temp = extract.token(txt, "contentDiv,mailer,Mailing Address", 
        "</div>")
    out$mailing = t(clean.table(extract.table.from.webpage(temp, 
        has.header = F, end.marker = "</span>")))
    colnames(out$mailing) = "Mailing Address"
    temp = extract.token(txt, "contentDiv,mailer,Business Address", 
        "</div>")
    out$business = t(clean.table(extract.table.from.webpage(temp, 
        has.header = F, end.marker = "</span>")))
    colnames(out$business) = "Business Address"
    temp = extract.token(txt, "contentDiv,companyInfo,>", "</div>")
    temp = gsub("\\|", "</span>", replace.token(temp, "<br", 
        ">", "</span>"))
    temp = clean.table(extract.table.from.webpage(temp, has.header = F, 
        end.marker = "</span>"))
    out$company = t(temp)
    colnames(out$company) = "Company Info"
    out$sic = trim(spl(spl(temp[grep("SIC", temp)], ":")[2], 
        "-"))
    return(out)
}

extract.table.from.webpage <- 
function (txt, marker = NA, has.header = T, end.marker = NA) 
{
    tryCatch({
        pos1 = 1
        if (!is.na(marker)) {
            marker = spl(marker)
            if (len(marker) > 0 && nchar(marker[1]) > 0) 
                for (i in 1:len(marker)) pos1 = regexpr(marker[i], 
                  substr(txt, pos1, nchar(txt))) + pos1
        }
        pos0 = tail(gregexpr("<table", substr(txt, 1, pos1))[[1]], 
            1)
        if (pos0 == -1) 
            pos0 = pos1
        pos2 = head(gregexpr("</table", substr(txt, pos1, nchar(txt)))[[1]], 
            1)
        if (pos2 == -1) 
            pos2 = nchar(txt) + 1
        temp = substr(txt, pos0, pos1 + pos2 - 2)
        temp = gsub(pattern = "<br>", replacement = "", temp, 
            perl = TRUE)
        temp = gsub(pattern = "</tr>", replacement = ";row;", 
            temp, perl = TRUE)
        temp = gsub(pattern = "</td>", replacement = ";col;", 
            temp, perl = TRUE)
        temp = gsub(pattern = "</th>", replacement = ";col;", 
            temp, perl = TRUE)
        if (!is.na(end.marker)) {
            marker = spl(end.marker)
            if (len(marker) > 0 && nchar(marker[1]) > 0) 
                for (i in 1:len(marker)) temp = gsub(pattern = marker[i], 
                  replacement = ";row;", temp, perl = TRUE)
        }
        temp = gsub(pattern = "<.*?>", replacement = "", temp, 
            perl = TRUE)
        temp = gsub(pattern = "\r", replacement = "", temp, perl = TRUE)
        temp = gsub(pattern = "\n", replacement = "", temp, perl = TRUE)
        temp = gsub(pattern = "\t", replacement = "", temp, perl = TRUE)
        temp = gsub(pattern = "&nbsp;", replacement = "", temp, 
            perl = TRUE)
        temp = gsub(pattern = "&amp;", replacement = "", temp, 
            perl = TRUE)
        temp = gsub(pattern = "&raquo;", replacement = "", temp, 
            perl = TRUE)
        temp = lapply(strsplit(temp, ";row;"), strsplit, ";col;")
        n = max(sapply(temp[[1]], function(x) len(x)))
        temp = t(sapply(temp[[1]], function(x) x[1:n]))
        if (has.header) {
            colnames(temp) = trim(temp[(has.header + 0), ])
            temp = temp[-c(1:(has.header + 0)), , drop = F]
        }
    }, error = function(ex) {
        temp <<- txt
    }, finally = {
        return(temp)
    })
}

extract.table.from.webpage.test <- 
function () 
{
    load.packages("quantmod")
    Symbol = "IBM"
    url = paste("http://finance.yahoo.com/q/ks?s=", Symbol, sep = "")
    txt = join(readLines(url))
    temp = extract.table.from.webpage(txt, "Market Cap", has.header = F)
    temp = rbind(c("", Symbol), temp)
    data = getSymbols(Symbol, from = "1980-01-01", auto.assign = FALSE)
    y = data["2010::2011"]
    sma50 = SMA(Cl(y), 50)
    png(filename = "plot1.png", width = 500, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(c(1, 1, 2, 3, 3))
    plota(y, type = "candle", main = Symbol, plotX = F)
    plota.lines(sma50, col = "blue")
    plota.legend(c(Symbol, "SMA 50"), "green,blue", list(y, sma50))
    y = plota.scale.volume(y)
    plota(y, type = "volume")
    plot.table(temp)
    dev.off()
}

extract.token <- 
function (txt, smarker, emarker, pos = 1, keep.marker = F) 
{
    pos1 = 1
    if (nchar(smarker) > 0) 
        pos1 = find.tokens(txt, smarker, pos, pos.start = keep.marker)
    if (pos1 < 0) 
        return("")
    pos1.marker = iif(keep.marker, pos1 + nchar(last(spl(smarker))), 
        pos1)
    pos2 = nchar(txt)
    if (nchar(emarker) > 0) 
        pos2 = find.tokens(txt, emarker, pos1.marker, pos.start = !keep.marker) - 
            1
    if (pos2 < 0) 
        return("")
    return(substr(txt, pos1, pos2))
}

extract.VXX.CBOE <- 
function (data, field, index, exact.match = T) 
{
    map = 1:ncol(data$prices)
    temp = bt.apply(data, function(x) x[, field])
    temp = coredata(temp)
    t(apply(temp, 1, function(x) {
        if (exact.match) {
            pos = map[!is.na(x)][1] - 1
            x[(index + pos)]
        }
        else {
            pos = map[!is.na(x)][index]
            x[pos]
        }
    }))
}

find.tokens <- 
function (txt, marker, pos = 1, pos.start = T) 
{
    marker = spl(marker)
    for (i in 1:len(marker)) {
        if (pos < 2) 
            pos1 = regexpr(marker[i], txt)
        else pos1 = regexpr(marker[i], substr(txt, pos, nchar(txt)))
        if (pos1 < 0) 
            return(pos1)
        else {
            if (pos < 2) 
                pos = pos1
            else pos = pos1 + pos - 1
        }
        pos = pos + attr(pos1, "match.length")
    }
    if (pos.start) 
        pos = pos - attr(pos1, "match.length")
    return(pos)
}

fm.all.factor.test <- 
function () 
{
    tickers = spl("XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU")
    tickers.desc = spl("ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities")
    sector.map = c()
    for (i in 1:len(tickers)) {
        sector.map = rbind(sector.map, cbind(sector.spdr.components(tickers[i]), 
            tickers.desc[i]))
    }
    colnames(sector.map) = spl("ticker,sector")
    load.packages("quantmod,abind")
    tickers = dow.jones.components()
    sectors = factor(sector.map[match(tickers, sector.map[, "ticker"]), 
        "sector"])
    names(sectors) = tickers
    if (FALSE) {
        data.fund <- new.env()
        temp = paste(iif(nchar(tickers) <= 3, "NYSE:", "NASDAQ:"), 
            tickers, sep = "")
        for (i in 1:len(tickers)) data.fund[[tickers[i]]] = fund.data(temp[i], 
            80)
        save(data.fund, file = "data.fund.Rdata")
        data <- new.env()
        getSymbols(tickers, src = "yahoo", from = "1970-01-01", 
            env = data, auto.assign = T)
        for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], 
            use.Adjusted = T)
        save(data, file = "data.Rdata")
    }
    else {
        load(file = "data.fund.Rdata")
        load(file = "data.Rdata")
    }
    data.clean(data, min.ratio = 3)
    tickers = ls(data)
    for (i in tickers) {
        fund = data.fund[[i]]
        fund.date = date.fund.data(fund)
        nperiods = ncol(fund)
        D = list()
        D$EPS = get.fund.data("Diluted EPS from Total Operations", 
            fund, fund.date, is.12m.rolling = T)
        D$SALE = get.fund.data("total revenue", fund, fund.date, 
            is.12m.rolling = T)
        D$CSHO = get.fund.data("total common shares out", fund, 
            fund.date)
        D$CEQ = get.fund.data("total equity", fund, fund.date)
        D$DV.PS = get.fund.data("dividends paid per share", fund, 
            fund.date, is.12m.rolling = T)
        D$CFL = get.fund.data("net cash from operating activities", 
            fund, fund.date, cash.flow = T, is.12m.rolling = T)
        D$CFL.CON.CHG = consecutive.changes(D$CFL)
        D$EPS.CON.CHG = consecutive.changes(D$EPS)
        temp = get.fund.data("net cash from operating activities", 
            fund, fund.date, cash.flow = T)
        D$CFL.CHG = temp/mlag(temp, 4)
        D$SALE.3YR.GR = D$SALE
        if (!all(is.na(D$SALE))) 
            D$SALE.3YR.GR = SMA(ifna(D$SALE/mlag(D$SALE, 4) - 
                1, NA), 3 * 4)
        D$EPS.3YR.GR = SMA(D$EPS/mlag(D$EPS, 4) - 1, 3 * 4)
        D$EPS.TREND = D$EPS * NA
        D$EPS.TREND[12:nperiods] = sapply(12:nperiods, function(i) beta.degree(ols(cbind(1, 
            1:12), D$EPS[(i - 12 + 1):i])$coefficients[2]))
        D$CFL.TREND = D$CFL * NA
        D$CFL.TREND[4:nperiods] = sapply(4:nperiods, function(i) beta.degree(ols(cbind(1, 
            1:4), D$CFL[(i - 4 + 1):i])$coefficients[2]))
        RECT = get.fund.data("receivables", fund, fund.date)
        INVT = get.fund.data("inventories", fund, fund.date)
        D$AT = get.fund.data("total assets", fund, fund.date)
        XSGA = get.fund.data("Selling, General & Administrative (SG&A) Expense", 
            fund, fund.date, is.12m.rolling = T)
        D$RS.CON.CHG = consecutive.changes((RECT + INVT)/D$SALE, 
            F)
        D$CS.CON.CHG = consecutive.changes(D$CFL/D$SALE)
        D$OS.CON.CHG = consecutive.changes(XSGA/D$SALE, F)
        D$RS = (RECT + INVT)/D$SALE
        D$SA = D$SALE/D$AT
        D$OS = XSGA/D$SALE
        D$ES = D$EPS/D$SALE
        temp = abind(D, along = 2)
        colnames(temp) = names(D)
        data[[i]] = merge(adjustOHLC(data[[i]], use.Adjusted = T), 
            as.xts(temp, fund.date))
    }
    bt.prep(data, align = "keep.all", fill.gaps = T, dates = "1995::")
    prices = data$prices
    prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
    sectors = sectors[colnames(prices)]
    factors = list()
    factor.names = list()
    factors$TV = list()
    factor.names$TV = "Traditional Value"
    CSHO = bt.apply(data, function(x) ifna.prev(x[, "CSHO"]))
    MKVAL = prices * CSHO
    EPS = bt.apply(data, function(x) ifna.prev(x[, "EPS"]))
    factors$TV$EP = EPS/prices
    SALE = bt.apply(data, function(x) ifna.prev(x[, "SALE"]))
    factors$TV$SP = SALE/MKVAL
    CFL = bt.apply(data, function(x) ifna.prev(x[, "CFL"]))
    factors$TV$CFP = CFL/MKVAL
    DV.PS = bt.apply(data, function(x) ifna.prev(x[, "DV.PS"]))
    factors$TV$DY = DV.PS/prices
    CEQ = bt.apply(data, function(x) ifna.prev(x[, "CEQ"]))
    factors$TV$BP = CEQ/MKVAL
    factors$TV$SP[, sectors == "Financials"] = NA
    factors$TV$CFP[, sectors == "Financials"] = NA
    factors$HG = list()
    factor.names$HG = "Historical Growth"
    for (i in spl("CFL.CON.CHG,EPS.CON.CHG,CFL.CHG,SALE.3YR.GR,EPS.3YR.GR,EPS.TREND,CFL.TREND")) {
        factors$HG[[i]] = bt.apply(data, function(x) ifna.prev(x[, 
            i]))
    }
    factors$PT = list()
    factor.names$PT = "Profit Trends"
    for (i in spl("RS.CON.CHG,CS.CON.CHG,OS.CON.CHG,RS,SA,OS,ES")) {
        factors$PT[[i]] = bt.apply(data, function(x) ifna.prev(x[, 
            i]))
    }
    factors$PM = list()
    factor.names$PM = "Price Momentum"
    week.ends = endpoints(prices, "weeks")
    week.prices = prices[week.ends, ]
    week.nperiods = nrow(week.prices)
    factors$PM$S52W.TREND = bt.apply.matrix(week.prices, function(x) {
        c(rep(NA, 51), sapply(52:week.nperiods, function(i) beta.degree(ols(cbind(1, 
            1:52), x[(i - 52 + 1):i])$coefficients[2])))
    })
    factors$PM$PP04.52W = bt.apply.matrix(week.prices, EMA, 4)/bt.apply.matrix(week.prices, 
        EMA, 52)
    factors$PM$R39W = week.prices/mlag(week.prices, 39)
    temp = bt.apply(data, function(x) cumsum(ifna(Vo(x), 0)))
    temp = temp[week.ends, ]
    week.volume = temp - mlag(temp)
    temp = (week.prices - mlag(week.prices)) * week.volume
    factors$PM$VPT51W = bt.apply.matrix(temp, runSum, 51)
    for (i in 1:len(factors$PM)) {
        temp = prices * NA
        temp[week.ends, ] = factors$PM[[i]]
        factors$PM[[i]] = bt.apply.matrix(temp, function(x) ifna.prev(x))
    }
    factors$PM$P260LOW = prices/bt.apply.matrix(prices, runMin, 
        260)
    for (i in names(factors$PM)) factors$PM[[i]] = -factors$PM[[i]]
    factors$PR = list()
    factor.names$PR = "Price Reversal"
    factors$PR$r5DR = prices/mlag(prices, 5)
    factors$PR$r5DR = factors$PR$r5DR/sector.mean(factors$PR$r5DR, 
        sectors)
    factors$PR$MFV = bt.apply(data, function(x) {
        MFI(cbind(ifna.prev(Hi(x)), ifna.prev(Lo(x)), ifna.prev(Cl(x))), 
            5)/ifna.prev(Vo(x))
    })
    factors$PR$MACD = bt.apply.matrix(prices, function(x) {
        temp = MACD(x, 10)
        temp[, "macd"] - temp[, "signal"]
    })
    factors$PR$RSI = bt.apply.matrix(prices, RSI, 14)
    factors$PR$STOCH = bt.apply(data, function(x) {
        stoch(cbind(ifna.prev(Hi(x)), ifna.prev(Lo(x)), ifna.prev(Cl(x))), 
            14)[, "slowD"]
    })
    factors$PR$rR4W = week.prices/mlag(week.prices, 4)
    factors$PR$rR4W = factors$PR$rR4W/sector.mean(factors$PR$rR4W, 
        sectors)
    temp = prices * NA
    temp[week.ends, ] = factors$PR$rR4W
    factors$PR$rR4W = bt.apply.matrix(temp, function(x) ifna.prev(x))
    volume = bt.apply(data, function(x) ifna.prev(Vo(x)))
    factors$PR$VOMO = (prices/mlag(prices, 10) - 1) * bt.apply.matrix(volume, 
        runMean, 22)/bt.apply.matrix(volume, runMean, 66)
    for (i in names(factors$PR)) factors$PR[[i]] = -factors$PR[[i]]
    factors$SS = list()
    factor.names$SS = "Small Size"
    factors$SS$MC = log(MKVAL)
    factors$SS$MC3 = log(MKVAL)^3
    factors$SS$P = log(prices)
    factors$SS$AT = log(bt.apply(data, function(x) ifna.prev(x[, 
        "AT"])))
    factors$SS$SALE = log(bt.apply(data, function(x) ifna.prev(x[, 
        "SALE"])))
    for (i in names(factors$SS)) factors$SS[[i]] = -factors$SS[[i]]
    month.ends = endpoints(prices, "months")
    prices = prices[month.ends, ]
    n = ncol(prices)
    nperiods = nrow(prices)
    ret = prices/mlag(prices) - 1
    next.month.ret = mlag(ret, -1)
    MKVAL = MKVAL[month.ends, ]
    for (j in 1:len(factors)) {
        for (i in 1:len(factors[[j]])) {
            factors[[j]][[i]] = factors[[j]][[i]][month.ends, 
                ]
            factors[[j]][[i]][] = ifna(factors[[j]][[i]], NA)
        }
    }
    factors$RV = list()
    factor.names$RV = "Relative Value"
    for (i in spl("EP,SP,CFP")) {
        factors$RV[[paste("r", i, sep = "")]] = factors$TV[[i]]/sector.mean(factors$TV[[i]], 
            sectors)
    }
    for (i in spl("rEP,rSP,rCFP")) {
        factors$RV[[paste("s", i, sep = "")]] = factors$RV[[i]] - 
            bt.apply.matrix(factors$RV[[i]], function(x) if (all(is.na(x))) 
                x
            else SMA(x, 60)[1:len(x)])
    }
    for (i in spl("RS,SA")) {
        factors$PT[[paste("r", i, sep = "")]] = factors$PT[[i]]/sector.mean(factors$PT[[i]], 
            sectors)
    }
    for (j in 1:len(factors)) {
        for (i in 1:len(factors[[j]])) {
            factors[[j]][[i]][] = ifna(factors[[j]][[i]], NA)
        }
    }
    for (j in names(factors)) {
        factors[[j]] = normalize.normal(factors[[j]])
        factors[[j]] = add.avg.factor(factors[[j]])
    }
    factors.avg = list()
    for (j in names(factors)) factors.avg[[j]] = factors[[j]]$AVG
    factors.avg = add.avg.factor(factors.avg)
    png(filename = "plot1.png", width = 600, height = 800, units = "px", 
        pointsize = 12, bg = "white")
    plot.quantiles(factors.avg, next.month.ret, "Average")
    dev.off()
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plot.bt.quantiles(factors.avg$AVG, next.month.ret, "Composite Average", 
        data)
    dev.off()
    save(data, sectors, factors.avg, next.month.ret, file = "data.factors.Rdata")
    factors.avg = factors.avg[which(names(factors.avg) != "AVG")]
    nperiods = nrow(next.month.ret)
    n = ncol(next.month.ret)
    factors.matrix = abind(factors.avg, along = 3)
    all.data = factors.matrix
    beta = all.data[, 1, ] * NA
    all.data = abind(next.month.ret, all.data, along = 3)
    dimnames(all.data)[[3]][1] = "Ret"
    for (t in 12:(nperiods - 1)) {
        temp = all.data[t:t, , ]
        x = temp[, -1]
        y = temp[, 1]
        beta[(t + 1), ] = lm(y ~ x - 1)$coefficients
    }
    alpha = next.month.ret * NA
    for (t in 18:(nperiods - 1)) {
        coef = colMeans(beta[(t - 5):t, ], na.rm = T)
        coef = iif(coef > 0, coef, 0)
        alpha[t, ] = rowSums(all.data[t, , -1] * t(repmat(coef, 
            1, n)), na.rm = T)
    }
    png(filename = "plot4.png", width = 600, height = 800, units = "px", 
        pointsize = 12, bg = "white")
    layout(1:2)
    temp = compute.quantiles(alpha, next.month.ret, plot = T)
    plot.bt.quantiles(alpha, next.month.ret, "Alpha", data)
    dev.off()
}

fm.fund.data.test <- 
function () 
{
    symbol = "WMT"
    symbol = paste(iif(nchar(symbol) <= 3, "NYSE:", "NASDAQ:"), 
        symbol, sep = "")
    fund = fund.data(symbol, 80)
    fund.date = date.fund.data(fund)
    total.capitalization = get.fund.data("total capitalization", 
        fund, fund.date)
    barplot(total.capitalization)
    EPS.Q = as.double(fund["Diluted EPS from Total Operations", 
        ])
    EPS.Q = as.xts(EPS.Q, fund.date)
    EPS = runSum(EPS.Q, 4)
    EPS.Q = get.fund.data("Diluted EPS from Total Operations", 
        fund, fund.date)
    EPS = get.fund.data("Diluted EPS from Total Operations", 
        fund, fund.date, is.12m.rolling = T)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(1:2)
    par(mar = c(2, 2, 2, 1))
    x = barplot(EPS.Q, main = "Wal-Mart Quarterly Earnings per share", 
        border = NA)
    text(x, EPS.Q, fund["quarterly indicator", ], adj = c(0.5, 
        -0.3), cex = 0.8, xpd = TRUE)
    barplot(EPS, main = "Wal-Mart Rolling Annual Earnings per share", 
        border = NA)
    dev.off()
    load.packages("quantmod")
    tickers = "WMT"
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "1980-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    data$WMT = merge(data$WMT, EPS)
    data$WMT$EPS = ifna.prev(coredata(data$WMT$EPS))
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    y = data$WMT["1990::"]
    plota(Cl(y), type = "l", LeftMargin = 3)
    plota2Y(y$EPS, type = "l", las = 1, col = "red", col.axis = "red")
    plota.legend("WMT(rhs),WMT.EPS(lhs)", "blue,red", list(Cl(y), 
        y$EPS))
    dev.off()
    load.packages("quantmod")
    tickers = dow.jones.components()
    data.fund <- new.env()
    temp = paste(iif(nchar(tickers) <= 3, "NYSE:", "NASDAQ:"), 
        tickers, sep = "")
    for (i in 1:len(tickers)) data.fund[[tickers[i]]] = fund.data(temp[i], 
        80)
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "1970-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    for (i in tickers) {
        fund = data.fund[[i]]
        fund.date = date.fund.data(fund)
        EPS.Q = as.double(fund["Diluted EPS from Total Operations", 
            ])
        EPS.Q = as.xts(EPS.Q, fund.date)
        EPS = runSum(EPS.Q, 4)
        data[[i]] = merge(data[[i]], EPS)
    }
    bt.prep(data, align = "keep.all", dates = "1995::2011")
    prices = data$prices
    prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
    factors = list()
    EPS = bt.apply(data, function(x) ifna.prev(x[, "EPS"]))
    factors$EP = EPS/prices
    volume = bt.apply(data, function(x) ifna.prev(Vo(x)))
    factors$VOMO = (prices/mlag(prices, 10) - 1) * bt.apply.matrix(volume, 
        runMean, 22)/bt.apply.matrix(volume, runMean, 66)
    month.ends = endpoints(prices, "months")
    prices = prices[month.ends, ]
    n = ncol(prices)
    nperiods = nrow(prices)
    ret = prices/mlag(prices) - 1
    next.month.ret = mlag(ret, -1)
    factors$EP = factors$EP[month.ends, ]
    factors$VOMO = factors$VOMO[month.ends, ]
    x = as.vector(factors$EP)
    y = as.vector(next.month.ret)
    cor.test(x, y, use = "complete.obs", method = "pearson")
    png(filename = "plot3.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    par(mar = c(4, 4, 2, 1))
    plot(x, y, pch = 20, main = "Correlation Analysis for EP factor", 
        xlab = "EP", ylab = "Next Month Return")
    abline(lm(y ~ x), col = "blue", lwd = 2)
    dev.off()
}

fm.fund.factor.test <- 
function () 
{
    tickers = spl("XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU")
    tickers.desc = spl("ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities")
    sector.map = c()
    for (i in 1:len(tickers)) {
        sector.map = rbind(sector.map, cbind(sector.spdr.components(tickers[i]), 
            tickers.desc[i]))
    }
    colnames(sector.map) = spl("ticker,sector")
    load.packages("quantmod,abind")
    tickers = dow.jones.components()
    sectors = factor(sector.map[match(tickers, sector.map[, "ticker"]), 
        "sector"])
    names(sectors) = tickers
    if (FALSE) {
        data.fund <- new.env()
        temp = paste(iif(nchar(tickers) <= 3, "NYSE:", "NASDAQ:"), 
            tickers, sep = "")
        for (i in 1:len(tickers)) data.fund[[tickers[i]]] = fund.data(temp[i], 
            80)
        save(data.fund, file = "data.fund.Rdata")
        data <- new.env()
        getSymbols(tickers, src = "yahoo", from = "1970-01-01", 
            env = data, auto.assign = T)
        for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], 
            use.Adjusted = T)
        save(data, file = "data.Rdata")
    }
    else {
        load(file = "data.fund.Rdata")
        load(file = "data.Rdata")
    }
    for (i in tickers) {
        fund = data.fund[[i]]
        fund.date = date.fund.data(fund)
        EPS = get.fund.data("Diluted EPS from Total Operations", 
            fund, fund.date, is.12m.rolling = T)
        SALE = get.fund.data("total revenue", fund, fund.date, 
            is.12m.rolling = T)
        CSHO = get.fund.data("total common shares out", fund, 
            fund.date)
        CEQ = get.fund.data("total equity", fund, fund.date)
        DV.PS = get.fund.data("dividends paid per share", fund, 
            fund.date, is.12m.rolling = T)
        CFL = get.fund.data("net cash from operating activities", 
            fund, fund.date, cash.flow = T, is.12m.rolling = T)
        data[[i]] = merge(data[[i]], EPS, SALE, CSHO, CEQ, DV.PS, 
            CFL)
    }
    bt.prep(data, align = "keep.all", dates = "1995::2011")
    prices = data$prices
    prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
    sectors = sectors[colnames(prices)]
    factors = list()
    factors$TV = list()
    CSHO = bt.apply(data, function(x) ifna.prev(x[, "CSHO"]))
    MKVAL = prices * CSHO
    EPS = bt.apply(data, function(x) ifna.prev(x[, "EPS"]))
    factors$TV$EP = EPS/prices
    SALE = bt.apply(data, function(x) ifna.prev(x[, "SALE"]))
    factors$TV$SP = SALE/MKVAL
    CFL = bt.apply(data, function(x) ifna.prev(x[, "CFL"]))
    factors$TV$CFP = CFL/MKVAL
    DV.PS = bt.apply(data, function(x) ifna.prev(x[, "DV.PS"]))
    factors$TV$DY = DV.PS/prices
    CEQ = bt.apply(data, function(x) ifna.prev(x[, "CEQ"]))
    factors$TV$BP = CEQ/MKVAL
    factors$TV$SP[, sectors == "Financials"] = NA
    factors$TV$CFP[, sectors == "Financials"] = NA
    month.ends = endpoints(prices, "months")
    prices = prices[month.ends, ]
    n = ncol(prices)
    nperiods = nrow(prices)
    ret = prices/mlag(prices) - 1
    next.month.ret = mlag(ret, -1)
    MKVAL = MKVAL[month.ends, ]
    for (j in 1:len(factors)) {
        for (i in 1:len(factors[[j]])) {
            factors[[j]][[i]] = factors[[j]][[i]][month.ends, 
                ]
        }
    }
    sapply(factors$TV, count)
    for (i in names(factors$TV)) {
        factors$TV[[i]] = (factors$TV[[i]] - cap.weighted.mean(factors$TV[[i]], 
            MKVAL))/apply(factors$TV[[i]], 1, sd, na.rm = T)
    }
    load.packages("abind")
    temp = abind(factors$TV, along = 3)
    factors$TV$AVG = factors$TV[[1]]
    factors$TV$AVG[] = apply(temp, c(1, 2), mean, na.rm = T)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:6, nc = 2))
    sapply(1:len(factors$TV), function(i) compute.quantiles(factors$TV[[i]], 
        next.month.ret, paste(names(factors$TV)[i], "Traditional Value")))
    dev.off()
    out = compute.quantiles(factors$TV$AVG, next.month.ret, plot = F)
    prices = data$prices
    prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
    models = list()
    for (i in 1:5) {
        data$weight[] = NA
        data$weight[month.ends, ] = iif(out$quantiles == i, out$weights, 
            0)
        capital = 1e+05
        data$weight[] = (capital/prices) * (data$weight)
        models[[paste("Q", i, sep = "")]] = bt.run(data, type = "share", 
            capital = capital)
    }
    data$weight[] = NA
    data$weight[month.ends, ] = iif(out$quantiles == 5, out$weights, 
        iif(out$quantiles == 1, -out$weights, 0))
    capital = 1e+05
    data$weight[] = (capital/prices) * (data$weight)
    models$Q5_Q1 = bt.run(data, type = "share", capital = capital)
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plotbt(models, plotX = T, log = "y", LeftMargin = 3)
    mtext("Cumulative Performance", side = 2, line = 1)
    dev.off()
}

ftse100.components <- 
function () 
{
    url = "http://uk.ishares.com/en/rc/products/ISF/all-holdings/"
    txt = join(readLines(url))
    txt = gsub("&#37;", "%", txt)
    temp = extract.table.from.webpage(txt, "Security", has.header = T)
    temp = trim(temp)
    colnames(temp) = temp[1, ]
    temp = temp[-1, ]
    holdings = temp
    page.label = ""
    ticker2ISIN = c()
    for (i in 1:100) {
        cat(i, "\n")
        url = paste("http://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/constituents-indices.html?index=UKX&page=", 
            i, sep = "")
        txt = join(readLines(url))
        pos = regexpr("Page [0-9]+ of [0-9]+", txt, ignore.case = T)
        page.label.new = substr(txt, pos, pos + attr(pos, "match.length") - 
            1)
        if (page.label == page.label.new) 
            break
        page.label = page.label.new
        temp.table = extract.table.from.webpage(txt, "Price", 
            has.header = T)
        colnames(temp.table)[1] = "tickers"
        temp = gsub(pattern = "<a", replacement = "<td>", txt, 
            perl = TRUE)
        temp = gsub(pattern = "</a>", replacement = "</td>", 
            temp, perl = TRUE)
        temp = extract.table.from.webpage(temp, "Price", has.header = T)
        pos = regexpr("fourWayKey=", temp[, 2])
        ISIN = as.vector(sapply(1:nrow(temp), function(j) substr(temp[j, 
            2], pos[j] + attr(pos, "match.length")[j], pos[j] + 
            attr(pos, "match.length")[j] + 12 - 1)))
        ticker2ISIN = rbind(ticker2ISIN, cbind(temp.table[, spl("ticker,Name,Price"), 
            drop = F], ISIN))
    }
    ISIN = intersect(holdings[, "ISIN"], ticker2ISIN[, "ISIN"])
    holdings = cbind(holdings[match(ISIN, holdings[, "ISIN"]), 
        ], ticker2ISIN[match(ISIN, ticker2ISIN[, "ISIN"]), spl("ticker,Name,Price")])
    return(apply(holdings, 2, list))
}

fund.data <- 
function (Symbol, n = 10, mode = c("quarterly", "annual"), max.attempts = 5) 
{
    all.data = c()
    option.value = -1
    start_date = spl("istart_date,start_date")
    names(start_date) = spl("quarterly,annual")
    repeat {
        if (option.value >= 0) {
            url = paste("http://uk.advfn.com/p.php?pid=financials&symbol=", 
                Symbol, "&btn=", mode[1], "_reports&", start_date[mode[1]], 
                "=", option.value, sep = "")
        }
        else {
            url = paste("http://uk.advfn.com/p.php?pid=financials&symbol=", 
                Symbol, "&btn=", mode[1], "_reports", sep = "")
        }
        cat("Downloading", url, "\n")
        for (iattempt in 1:max.attempts) {
            flag = T
            tryCatch({
                txt = join(readLines(url))
            }, interrupt = function(ex) {
                flag <<- F
                Sys.sleep(0.1)
            }, error = function(ex) {
                flag <<- F
                Sys.sleep(0.1)
            }, finally = {
                if (flag) 
                  break
            })
        }
        if (len(grep("INDICATORS", txt, ignore.case = T)) == 
            0) {
            cat("No Data Found for", Symbol, "\n")
            return(all.data)
        }
        pos = regexpr(pattern = "<title>(.*?)</title>", txt, 
            ignore.case = TRUE, perl = TRUE)
        if (len(pos) == 1) 
            title = substr(txt, attr(pos, "capture.start"), attr(pos, 
                "capture.start") + attr(pos, "capture.length") - 
                1)
        data = extract.table.from.webpage(txt, "INDICATORS", 
            has.header = T)
        colnames(data) = data[1, ]
        rownames(data) = data[, 1]
        data = data[, -1, drop = F]
        add.index = which(is.na(match(colnames(data), colnames(all.data))))
        all.data = cbind(data[, add.index, drop = F], all.data)
        if (ncol(all.data) >= n) 
            break
        if (option.value == 0) 
            break
        temp = gsub(pattern = "<option", replacement = "<tr>", 
            txt, perl = TRUE)
        temp = gsub(pattern = "</option>", replacement = "</tr>", 
            temp, perl = TRUE)
        temp = extract.table.from.webpage(temp, "All amounts", 
            has.header = T)
        temp = apply(temp, 1, join)
        index.selected = grep("selected", temp)
        option.value = 0
        if (len(index.selected)) 
            option.value = as.double(gsub(".*value='([0-9]*).*", 
                "\\1", temp[index.selected]))
        if (option.value > 0) {
            option.value = option.value - 5
            option.value = max(0, option.value)
        }
        else {
            break
        }
    }
    all.data = all.data[, colSums(nchar(trim(all.data))) > 0, 
        drop = F]
    all.data = rbind(all.data, title)
    rownames(all.data)[nrow(all.data)] = "HTMLTITLEtext"
    if (ncol(all.data) > n) {
        return(all.data[, (ncol(all.data) - n + 1):ncol(all.data), 
            drop = F])
    }
    else {
        return(all.data)
    }
}

fundamental.dcf.test <- 
function () 
{
    load.packages("quantmod")
    tickers = spl("AAPL")
    tickers.temp = paste(iif(nchar(tickers) <= 3, "NYSE:", "NASDAQ:"), 
        tickers, sep = "")
    data.fund <- new.env()
    for (i in 1:len(tickers)) data.fund[[tickers[i]]] = fund.data(tickers.temp[i], 
        80, "annual")
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "1970-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    fund = data.fund[[tickers[1]]]
    fund.date = date.fund.data(fund)
    price = Cl(data[[tickers[1]]]["1995::"])
    FCF = get.fund.data("free cash flow", fund, fund.date)
    IC = get.fund.data("invested capital", fund, fund.date)
    SALE = get.fund.data("total revenue", fund, fund.date)
    CEQ = get.fund.data("total equity", fund, fund.date)
    CSHO = get.fund.data("total common shares out", fund, fund.date)
    CROIC = FCF/IC
    g = runMean(CROIC, 5)
    cash = runMean(FCF, 5)
    compute.DCF.IV <- function(cash, eqity, shares, g, R) {
        if (cash <= 0) 
            return(NA)
        if (len(R) == 1) 
            R = rep(R, len(g))
        value = eqity + sum(cash * cumprod(1 + g)/cumprod(1 + 
            R))
        return(value/shares)
    }
    dcf.price = NA * g
    i.start = which(!is.na(g))[1]
    for (i in i.start:nrow(g)) {
        g.scenario = c(rep(g[i], 3), rep(g[i], 4) * 0.8, rep(g[i], 
            3) * 0.8 * 0.8, rep(3/100, 10))
        dcf.price[i] = compute.DCF.IV(cash[i], CEQ[i], CSHO[i], 
            g.scenario, 9/100)
    }
    png(filename = "plot1.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plota(price, type = "l", log = "y", col = "blue", main = tickers[1], 
        ylim = range(price, dcf.price, na.rm = T))
    plota.lines(dcf.price, type = "s", col = "red", lwd = 2)
    plota.legend("Close,Intrinsic Value", "blue,red", list(price, 
        dcf.price))
    dev.off()
    png(filename = "plot2.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plota(g, type = "b", col = "blue", pch = 0, main = "Growth Rate")
    dev.off()
    png(filename = "plot3.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plota(cash, type = "b", col = "blue", pch = 0, main = "Free Cash Flows")
    dev.off()
}

fundamental.fb.test <- 
function () 
{
    load.packages("quantmod")
    tickers = spl("FB,LNKD,GRPN,AAPL,GOOG")
    tickers.temp = spl("NASDAQ:FB,NYSE:LNKD,NASDAQ:GRPN,NASDAQ:AAPL,NASDAQ:GOOG")
    data.fund <- new.env()
    for (i in 1:len(tickers)) {
        if (is.null(data.fund[[tickers[i]]])) {
            cat(tickers[i], "\n")
            data.fund[[tickers[i]]] = fund.data(tickers.temp[i], 
                80)
        }
    }
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "1970-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    for (i in tickers) {
        fund = data.fund[[i]]
        fund.date = date.fund.data(fund)
        EPS = 4 * get.fund.data("Diluted EPS from Total Operations", 
            fund, fund.date)
        if (nrow(EPS) > 3) 
            EPS = rbind(EPS[1:3], get.fund.data("Diluted EPS from Total Operations", 
                fund, fund.date, is.12m.rolling = T)[-c(1:3)])
        data[[i]] = merge(data[[i]], EPS)
    }
    bt.prep(data, align = "keep.all", dates = "1995::")
    prices = data$prices
    prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
    EPS = bt.apply(data, function(x) ifna.prev(x[, "EPS"]))
    PE = ifna(prices/EPS, NA)
    PE[abs(EPS) < 0.001] = NA
    png(filename = "plot1.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plota.matplot(PE)
    dev.off()
    png(filename = "plot2.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plota.matplot(PE, type = "b", pch = 20, dates = "2012::")
    dev.off()
    png(filename = "plot3.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plota.matplot(EPS)
    dev.off()
    png(filename = "plot4.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    plota.matplot(prices)
    dev.off()
}

get.FOMC.dates <- 
function (download = TRUE, fomc.filename = "FOMC.Rdata") 
{
    if (!download && file.exists(fomc.filename)) {
        load(file = fomc.filename)
        return(FOMC)
    }
    url = "http://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"
    txt = join(readLines(url))
    data = c()
    for (year in 2009:(1 + date.year(Sys.Date()))) {
        temp = extract.table.from.webpage(txt, paste(year, "FOMC Meetings"))
        if (nrow(temp) == 0) 
            next
        temp = tolower(trim(temp[, 1:2]))
        temp = temp[nchar(temp[, 1]) > 0, ]
        month = temp[, 1]
        day = gsub("\\(", "", gsub("\\)", "", temp[, 2]))
        day = trim(day)
        status = rep("", len(day))
        index = grep("\\*", day)
        if (any(index)) 
            status[index] = "*"
        index = grep(" ", day)
        if (any(index)) 
            for (j in index) status[j] = spl(day[j], " ")[2]
        day = gsub("\\*", "", sapply(day, function(x) spl(x, 
            " ")[1]))
        temp = apply(cbind(day, month, status), 1, function(x) paste(year, 
            spl(x[2], "/"), spl(x[1], "-"), "|", x[3]))
        data = cbind(data, trim(sapply(unlist(temp), spl, "\\|")))
    }
    recent.days = as.Date(data[1, ], "%Y %B %d")
    status = as.vector(data[2, ])
    data = c()
    for (year in 1936:2008) {
        cat(year, "\n")
        url = paste0("http://www.federalreserve.gov/monetarypolicy/fomchistorical", 
            year, ".htm")
        txt = join(readLines(url))
        tokens = spl(txt, "<div id=\"historical\">")
        days = c()
        for (token in tokens[-1]) days = c(days, colnames(extract.table.from.webpage(token, 
            "table"))[1])
        data = rbind(data, cbind(year, days))
    }
    day = tolower(data[, 2])
    day = gsub(",", "-", gsub("and", "", gsub("conference call", 
        "", gsub("meeting", "", day))))
    day = unlist(lapply(day, function(x) join(rev(rev(spl(x, 
        " "))[-1]), " ")))
    temp = unlist(apply(cbind(day, data[, 1]), 1, function(x) paste(trim(spl(x[1], 
        "-")), x[2])))
    temp = sapply(lapply(temp, spl, " "), function(x) iif(len(x) == 
        3, x, c(NA, x)))
    temp[1, ] = ifna.prev(temp[1, ])
    days = as.Date(apply(temp, 2, join, " "), "%B %d %Y ")
    FOMC = list(day = c(days, recent.days), status = c(rep("", 
        len(days)), status))
    save(FOMC, file = fomc.filename)
    FOMC
}

google.search <- 
function (query) 
{
    url = paste("http://google.com/search?ie=utf-8&oe=utf-8&q=", 
        URLencode(query), "&num=10&gws_rd=cr", sep = "")
    txt = join(readLines(url))
    tokens = spl(txt, "<li class=\"g\">")
    if (len(tokens) < 2) 
        return(NULL)
    records = matrix("", nrow = len(tokens) - 1, nc = 2)
    colnames(records) = c("label", "url")
    for (i in 2:len(tokens)) {
        token = tokens[i]
        token = extract.token(token, "<a href=", "</a>", keep.marker = T)
        url = extract.token(token, "url\\?q=", "&amp;sa=U&amp;")
        label = remove.tags(token)
        records[i - 1, ] = c(label, url)
    }
    return(records)
}

ifna <- 
function (x, y) 
{
    return(iif(is.na(x) | is.nan(x) | is.infinite(x), y, x))
}

ifna.prev <- 
function (y) 
{
    y1 = !is.na(y)
    y1[1] = T
    return(y[cummax((1:length(y)) * y1)])
}

iif <- 
function (cond, truepart, falsepart) 
{
    if (len(cond) == 1) {
        if (cond) 
            truepart
        else falsepart
    }
    else {
        if (length(falsepart) == 1) {
            temp = falsepart
            falsepart = cond
            falsepart[] = temp
        }
        if (length(truepart) == 1) 
            falsepart[cond] = truepart
        else {
            cond = ifna(cond, F)
            if (is.xts(truepart)) 
                falsepart[cond] = coredata(truepart)[cond]
            else falsepart[cond] = truepart[cond]
        }
        return(falsepart)
    }
}

index.xts <- 
function (x) 
{
    temp = attr(x, "index")
    class(temp) = c("POSIXct", "POSIXt")
    type = attr(x, ".indexCLASS")[1]
    if (type == "Date" || type == "yearmon" || type == "yearqtr") 
        temp = as.Date(temp)
    return(temp)
}

inv <- 
function (x) 
{
    solve(x)
}

inv1 <- 
function (x) 
{
    1/x
}

inv2 <- 
function (x) 
{
    matrix(c(x[2, 2], -x[1, 2], -x[2, 1], x[1, 1]), nrow = 2, 
        byrow = T)/(x[1, 1] * x[2, 2] - x[1, 2] * x[2, 1])
}

inv3 <- 
function (x) 
{
    matrix(c(x[3, 3] * x[2, 2] - x[3, 2] * x[2, 3], -(x[3, 3] * 
        x[1, 2] - x[3, 2] * x[1, 3]), x[2, 3] * x[1, 2] - x[2, 
        2] * x[1, 3], -(x[3, 3] * x[2, 1] - x[3, 1] * x[2, 3]), 
        x[3, 3] * x[1, 1] - x[3, 1] * x[1, 3], -(x[2, 3] * x[1, 
            1] - x[2, 1] * x[1, 3]), x[3, 2] * x[2, 1] - x[3, 
            1] * x[2, 2], -(x[3, 2] * x[1, 1] - x[3, 1] * x[1, 
            2]), x[2, 2] * x[1, 1] - x[2, 1] * x[1, 2]), nrow = 3, 
        byrow = T)/(x[1, 1] * (x[3, 3] * x[2, 2] - x[3, 2] * 
        x[2, 3]) - x[2, 1] * (x[3, 3] * x[1, 2] - x[3, 2] * x[1, 
        3]) + x[3, 1] * (x[2, 3] * x[1, 2] - x[2, 2] * x[1, 3]))
}

join <- 
function (v, delim = "") 
{
    return(paste(v, collapse = delim))
}

len <- 
function (x) 
length(x)

load.dow.jones <- 
function (align = "remove.na", dates = NULL) 
{
    tickers = spl("XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU")
    tickers.desc = spl("ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities")
    sector.map = c()
    for (i in 1:len(tickers)) {
        sector.map = rbind(sector.map, cbind(sector.spdr.components(tickers[i]), 
            tickers.desc[i]))
    }
    colnames(sector.map) = spl("ticker,sector")
    load.packages("quantmod")
    tickers = dow.jones.components()
    sectors = factor(sector.map[match(tickers, sector.map[, "ticker"]), 
        "sector"])
    names(sectors) = tickers
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "1900-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    bt.prep(data, align = align, dates = dates)
    data$sectors = sectors[data$symbolnames]
    return(data)
}

load.packages <- 
function (packages, repos = "http://cran.r-project.org", dependencies = c("Depends", 
    "Imports"), ...) 
{
    packages = spl(packages)
    for (ipackage in packages) {
        if (!require(ipackage, quietly = TRUE, character.only = TRUE)) {
            install.packages(ipackage, repos = repos, dependencies = dependencies, 
                ...)
            if (!require(ipackage, quietly = TRUE, character.only = TRUE)) {
                stop("package", sQuote(ipackage), "is needed.  Stopping")
            }
        }
    }
}

make.data.proxy <- 
function () 
{
    load.packages("quantmod")
    raw.data = env()
    filename = "data/TR_CC-CRB"
    if (file.exists(filename)) {
        temp = extract.table.from.webpage(join(readLines(filename)), 
            "EODValue")
        temp = join(apply(temp, 1, join, ","), "\n")
        raw.data$CRB = make.stock.xts(read.xts(temp, format = "%m/%d/%y"))
    }
    filename = "data/TB3M.Rdata"
    if (!file.exists(filename)) {
        TB3M = quantmod::getSymbols("DTB3", src = "FRED", auto.assign = FALSE)
        save(TB3M, file = filename)
    }
    load(file = filename)
    TB3M[] = ifna.prev(TB3M)
    raw.data$TB3M = make.stock.xts(processTBill(TB3M, timetomaturity = 1/4, 
        261))
    filename = "data/TB3Y.Rdata"
    if (!file.exists(filename)) {
        TB3Y = quantmod::getSymbols("DGS3", src = "FRED", auto.assign = FALSE)
        save(TB3Y, file = filename)
    }
    load(file = filename)
    TB3Y[] = ifna.prev(TB3Y)
    raw.data$TB3Y = make.stock.xts(processTBill(TB3Y, timetomaturity = 3, 
        261))
    filename = "data/TB10Y.Rdata"
    if (!file.exists(filename)) {
        TB10Y = quantmod::getSymbols("DGS10", src = "FRED", auto.assign = FALSE)
        save(TB10Y, file = filename)
    }
    load(file = filename)
    TB10Y[] = ifna.prev(TB10Y)
    raw.data$TB10Y = make.stock.xts(processTBill(TB10Y, timetomaturity = 10, 
        261))
    filename = "data/TB20Y.Rdata"
    if (!file.exists(filename)) {
        TB20Y = quantmod::getSymbols("GS20", src = "FRED", auto.assign = FALSE)
        save(TB20Y, file = filename)
    }
    load(file = filename)
    TB20Y[] = ifna.prev(TB20Y)
    raw.data$TB20Y = make.stock.xts(processTBill(TB20Y, timetomaturity = 20, 
        12))
    filename = "data/GOLD.Rdata"
    if (!file.exists(filename)) {
        GOLD = bundes.bank.data.gold()
        save(GOLD, file = filename)
    }
    load(file = filename)
    raw.data$GOLD = make.stock.xts(GOLD)
    filename = "data/NAREIT.xls"
    if (!file.exists(filename)) {
        url = "http://returns.reit.com/returns/MonthlyHistoricalReturns.xls"
        download.file(url, filename, mode = "wb")
    }
    load.packages("readxl")
    temp = read_excel(filename, sheet = "Index Data", skip = 7)
    NAREIT = make.xts(temp$Index, as.Date(temp$Date))
    raw.data$NAREIT = make.stock.xts(NAREIT)
    tickers = "\nCOM = DBC;GSG + CRB\nRExUS = [RWX] + VNQ + VGSIX\nRE = [RWX] + VNQ + VGSIX\nRE.US = [ICF] + VGSIX\nEMER.EQ = [EEM] + VEIEX\nEMER.FI = [EMB] + PREMX\nGOLD = [GLD] + GOLD,\nUS.CASH = [BIL] + TB3M,\nSHY + TB3Y,\nUS.HY = [HYG] + VWEHX\nUS.BOND = [AGG] + VBMFX\nINTL.BOND = [BWX] + BEGBX\nJAPAN.EQ = [EWJ] + FJPNX\nEUROPE.EQ = [IEV] + FIEUX\nUS.SMCAP = IWM;VB + NAESX\nTECH.EQ = [QQQ] + ^NDX\nUS.EQ = [VTI] + VTSMX + VFINX\nUS.MID = [VO] + VIMSX\nEAFE = [EFA] + VDMIX + VGTSX\nMID.TR = [IEF] + VFITX\nCORP.FI = [LQD] + VWESX\nTIPS = [TIP] + VIPSX + LSGSX\nLONG.TR = [TLT] + VUSTX\n"
    data.proxy = env()
    getSymbols.extra(tickers, src = "yahoo", from = "1970-01-01", 
        env = data.proxy, raw.data = raw.data, auto.assign = T)
    data.proxy.raw = raw.data
    save(data.proxy.raw, file = "data/data.proxy.raw.Rdata", 
        compress = "gzip")
    save(data.proxy, file = "data/data.proxy.Rdata", compress = "gzip")
}

make.table <- 
function (nr, nc) 
{
    savepar = par(mar = rep(1, 4))
    plot(c(0.5, nc * 2 + 0.5), c(-0.5, -(nr + 0.5)), xaxs = "i", 
        yaxs = "i", type = "n", xlab = "", ylab = "", axes = FALSE)
    savepar
}

mlag <- 
function (m, nlag = 1) 
{
    if (is.null(dim(m))) {
        n = len(m)
        if (nlag > 0) {
            m[(nlag + 1):n] = m[1:(n - nlag)]
            m[1:nlag] = NA
        }
        else if (nlag < 0) {
            m[1:(n + nlag)] = m[(1 - nlag):n]
            m[(n + nlag + 1):n] = NA
        }
    }
    else {
        n = nrow(m)
        if (nlag > 0) {
            m[(nlag + 1):n, ] = m[1:(n - nlag), ]
            m[1:nlag, ] = NA
        }
        else if (nlag < 0) {
            m[1:(n + nlag), ] = m[(1 - nlag):n, ]
            m[(n + nlag + 1):n, ] = NA
        }
    }
    return(m)
}

normal.transform <- 
function (data) 
{
    rk = rank(data, na.last = "keep", ties.method = "first")
    n = count(data)
    x = qnorm((1:n)/(n + 1))
    return(x[rk])
}

normalize.normal <- 
function (data) 
{
    for (i in names(data)) {
        data[[i]][] = t(apply(data[[i]], 1, normal.transform))
    }
    return(data)
}

ols <- 
function (x, y, computeSummary = F) 
{
    xx = t(x) %*% x
    if (is.null(ncol(xx))) {
        xinv = inv1(xx)
    }
    else if (ncol(xx) == 1) {
        xinv = inv1(xx)
    }
    else if (ncol(xx) == 2) {
        xinv = inv2(xx)
    }
    else if (ncol(xx) == 3) {
        xinv = inv3(xx)
    }
    else {
        xinv = inv(xx)
    }
    coefficients = xinv %*% t(x) %*% y
    if (computeSummary) {
        return(ols.summary(x, y, coefficients, xinv))
    }
    else {
        return(list(coefficients = coefficients))
    }
}

ols.summary <- 
function (x, y, coefficients, xinv = NULL) 
{
    n = length(y)
    p = length(coefficients)
    rdf = n - p
    e = y - x %*% coefficients
    ess = sum(e^2)
    mss = sum((y - sum(y)/n)^2)
    r.squared = 1 - ess/mss
    if (!is.null(xinv)) {
        s2 = ess/(rdf)
        seb = sqrt(diag(s2 * xinv))
        tratio = coefficients/seb
        return(list(coefficients = coefficients, seb = seb, tratio = tratio, 
            r.squared = r.squared))
    }
    else {
        return(list(coefficients = coefficients, r.squared = r.squared))
    }
}

plot.table <- 
function (plot.matrix, smain = NULL, text.cex = 1, frame.cell = T, 
    highlight = F, colorbar = FALSE, keep_all.same.cex = FALSE) 
{
    if (is.null(rownames(plot.matrix)) & is.null(colnames(plot.matrix))) {
        temp.matrix = plot.matrix
        if (nrow(temp.matrix) == 1) 
            temp.matrix = rbind("", temp.matrix)
        if (ncol(temp.matrix) == 1) 
            temp.matrix = cbind("", temp.matrix)
        plot.matrix = temp.matrix[-1, -1, drop = FALSE]
        colnames(plot.matrix) = temp.matrix[1, -1]
        rownames(plot.matrix) = temp.matrix[-1, 1]
        smain = iif(is.null(smain), temp.matrix[1, 1], smain)
    }
    else if (is.null(rownames(plot.matrix))) {
        temp.matrix = plot.matrix
        if (ncol(plot.matrix) == 1) 
            temp.matrix = cbind("", temp.matrix)
        plot.matrix = temp.matrix[, -1, drop = FALSE]
        colnames(plot.matrix) = colnames(temp.matrix)[-1]
        rownames(plot.matrix) = temp.matrix[, 1]
        smain = iif(is.null(smain), colnames(temp.matrix)[1], 
            smain)
    }
    else if (is.null(colnames(plot.matrix))) {
        temp.matrix = plot.matrix
        if (nrow(temp.matrix) == 1) 
            temp.matrix = rbind("", temp.matrix)
        plot.matrix = temp.matrix[-1, , drop = FALSE]
        rownames(plot.matrix) = rownames(temp.matrix)[-1]
        colnames(plot.matrix) = temp.matrix[1, ]
        smain = iif(is.null(smain), rownames(temp.matrix)[1], 
            smain)
    }
    smain = iif(is.null(smain), "", smain)
    plot.matrix[which(trim(plot.matrix) == "NA")] = ""
    plot.matrix[which(trim(plot.matrix) == "NA%")] = ""
    plot.matrix[which(is.na(plot.matrix))] = ""
    if (colorbar) {
        plot.matrix = cbind(plot.matrix, "")
        if (!is.null(highlight)) 
            if (!is.logical(highlight)) {
                highlight = cbind(highlight, NA)
            }
    }
    nr = nrow(plot.matrix) + 1
    nc = ncol(plot.matrix) + 1
    is_highlight = T
    if (is.logical(highlight)) {
        is_highlight = highlight
        if (highlight) 
            highlight = plot.table.helper.color(plot.matrix)
    }
    if (!is_highlight) {
        plot.matrix.cex = matrix(1, nr = nr, nc = nc)
        plot.matrix_bg.col = matrix("white", nr = nr, nc = nc)
        plot.matrix_bg.col[seq(1, nr, 2), ] = "yellow"
        plot.matrix_bg.col[1, ] = "gray"
        plot.table.param(plot.matrix, smain, plot.matrix.cex, 
            plot.matrix_bg.col, frame.cell, keep_all.same.cex)
    }
    else {
        plot.matrix.cex = matrix(1, nr = nr, nc = nc)
        plot.matrix_bg.col = matrix("white", nr = nr, nc = nc)
        plot.matrix_bg.col[1, ] = "gray"
        plot.matrix_bg.col[2:nr, 2:nc] = highlight
        plot.table.param(plot.matrix, smain, plot.matrix.cex, 
            plot.matrix_bg.col, frame.cell, keep_all.same.cex)
    }
    if (colorbar) 
        plot.table.helper.colorbar(plot.matrix)
}

plot.table.helper.auto.adjust.cex <- 
function (temp.table, keep.all.same.cex = FALSE) 
{
    nr = nrow(temp.table)
    nc = ncol(temp.table)
    all.xrange = diff(par()$usr[1:2])/nc
    xrange = matrix(strwidth(paste("  ", temp.table), units = "user", 
        cex = 1), nc = nc)
    all.yrange = diff(par()$usr[3:4])/nr
    yrange = matrix(5/3 * strheight(temp.table, units = "user", 
        cex = 1), nc = nc)
    plot.matrix.cex = pmin(round(all.yrange/yrange, 2), round(all.xrange/xrange, 
        2))
    header.col.cex = min(plot.matrix.cex[1, -1])
    header.row.cex = min(plot.matrix.cex[-1, 1])
    title.cex = plot.matrix.cex[1, 1]
    data.cex = min(plot.matrix.cex[-1, -1])
    if (keep.all.same.cex) {
        plot.matrix.cex[] = min(plot.matrix.cex)
    }
    else {
        plot.matrix.cex[1, -1] = min(c(header.col.cex, header.row.cex))
        plot.matrix.cex[-1, 1] = min(c(header.col.cex, header.row.cex))
        plot.matrix.cex[-1, -1] = min(c(header.col.cex, header.row.cex, 
            data.cex))
        plot.matrix.cex[1, 1] = min(c(header.col.cex, header.row.cex, 
            data.cex, title.cex))
        plot.matrix.cex[1, -1] = min(c(header.col.cex))
        plot.matrix.cex[-1, 1] = min(c(header.row.cex))
        plot.matrix.cex[-1, -1] = min(c(data.cex))
        plot.matrix.cex[1, 1] = min(c(title.cex))
    }
    return(plot.matrix.cex)
}

plot.table.helper.color <- 
function (temp) 
{
    temp = matrix(as.double(gsub("[%,$]", "", temp)), nrow(temp), 
        ncol(temp))
    highlight = as.vector(temp)
    cols = rep(NA, len(highlight))
    ncols = len(highlight[!is.na(highlight)])
    cols[1:ncols] = rainbow(ncols, start = 0, end = 0.3)
    o = sort.list(highlight, na.last = TRUE, decreasing = FALSE)
    o1 = sort.list(o, na.last = TRUE, decreasing = FALSE)
    highlight = matrix(cols[o1], nrow = nrow(temp))
    highlight[is.na(temp)] = NA
    return(highlight)
}

plot.table.helper.colorbar <- 
function (plot.matrix) 
{
    nr = nrow(plot.matrix) + 1
    nc = ncol(plot.matrix) + 1
    c = nc
    r1 = 1
    r2 = nr
    rect((2 * (c - 1) + 0.5), -(r1 - 0.5), (2 * c + 0.5), -(r2 + 
        0.5), col = "white", border = "white")
    rect((2 * (c - 1) + 0.5), -(r1 - 0.5), (2 * (c - 1) + 0.5), 
        -(r2 + 0.5), col = "black", border = "black")
    y1 = c(-(r2):-(r1))
    graphics::image(x = c((2 * (c - 1) + 1.5):(2 * c + 0.5)), 
        y = y1, z = t(matrix(y1, ncol = 1)), col = t(matrix(rainbow(len(y1), 
            start = 0, end = 0.3), ncol = 1)), add = T)
}

plot.table.param <- 
function (plot.matrix, smain = "", plot.matrix.cex, plot.matrix_bg.col, 
    frame.cell = T, keep.all.same.cex = FALSE) 
{
    n = nrow(plot.matrix)
    pages = unique(c(seq(0, n, by = 120), n))
    for (p in 1:(len(pages) - 1)) {
        rindex = (pages[p] + 1):pages[p + 1]
        temp.table = matrix("", nr = len(rindex) + 1, nc = ncol(plot.matrix) + 
            1)
        temp.table[-1, -1] = plot.matrix[rindex, ]
        temp.table[1, -1] = colnames(plot.matrix)
        temp.table[-1, 1] = rownames(plot.matrix)[rindex]
        temp.table[1, 1] = smain
        nr = nrow(temp.table)
        nc = ncol(temp.table)
        par(mar = c(0, 0, 0, 0), cex = 0.5)
        oldpar = make.table(nr, nc)
        text.cex = plot.matrix.cex[c(1, 1 + rindex), ]
        text.cex = plot.table.helper.auto.adjust.cex(temp.table, 
            keep.all.same.cex)
        bg.col = plot.matrix_bg.col[c(1, 1 + rindex), ]
        for (r in 1:nr) {
            for (c in 1:nc) {
                draw.cell(paste("", temp.table[r, c], "", sep = " "), 
                  r, c, text.cex = text.cex[r, c], bg.col = bg.col[r, 
                    c], frame.cell = frame.cell)
            }
        }
    }
}

plota <- 
function (y, main = NULL, plotX = TRUE, LeftMargin = 0, x.highlight = NULL, 
    y.highlight = NULL, las = 1, type = "n", xlab = "", ylab = "", 
    ylim = NULL, log = "", ...) 
{
    hasTitle = !is.null(main)
    par(mar = c(iif(plotX, 2, 0), LeftMargin, iif(hasTitle, 2, 
        0), 3))
    if (has.Cl(y)) 
        y1 = Cl(y)
    else y1 = y[, 1]
    if (is.null(ylim)) {
        ylim = range(y1, na.rm = T)
        switch(type, ohlc = , hl = , candle = {
            ylim = range(OHLC(y), na.rm = T)
        }, volume = {
            y1 = Vo(y)
            ylim = range(Vo(y), na.rm = T)
        })
    }
    temp.x = attr(y, "index")
    plot(temp.x, y1, xlab = xlab, ylab = ylab, main = main, type = "n", 
        yaxt = "n", xaxt = "n", ylim = ylim, log = log, ...)
    axis(4, las = las)
    class(temp.x) = c("POSIXct", "POSIXt")
    plota.control$xaxis.ticks = axis.POSIXct(1, temp.x, labels = plotX, 
        tick = plotX)
    if (!is.null(x.highlight)) 
        plota.x.highlight(y, x.highlight)
    if (!is.null(y.highlight)) 
        plota.y.highlight(y, y.highlight)
    plota.grid()
    switch(type, candle = plota.candle(y, ...), hl = plota.hl(y, 
        ...), ohlc = plota.ohlc(y, ...), volume = plota.volume(y, 
        ...), {
        lines(temp.x, y1, type = type, ...)
    })
    box()
}

plota.candle <- 
function (y, col = plota.candle.col(y)) 
{
    dx = plota.dx(y)
    dxi0 = (dx/xinch()) * 96
    if (dxi0 < 1) {
        plota.hl.lwd(y, col = col, lwd = 1)
    }
    else if (dxi0 < 1.75) {
        plota.ohlc.lwd(y, col = col, lwd = 1)
    }
    else {
        temp.x = attr(y, "index")
        rect(temp.x - dx/10, Lo(y), temp.x + dx/10, Hi(y), col = plota.control$col.border, 
            border = plota.control$col.border)
        rect(temp.x - dx/2, Op(y), temp.x + dx/2, Cl(y), col = col, 
            border = plota.control$col.border)
    }
}

plota.candle.col <- 
function (y) 
{
    return(iif(Cl(y) > Op(y), plota.control$col.up, plota.control$col.dn))
}

plota.dx <- 
function (y) 
{
    xlim = par("usr")[1:2]
    class(xlim) = c("POSIXct", "POSIXt")
    y1 = y[paste(format(xlim, "%Y:%m:%d %H:%M:%S"), sep = "", 
        collapse = "::")]
    xlim = par("usr")[1:2]
    xportion = min(1, diff(unclass(range(attr(y1, "index")))) * 
        1.08/diff(xlim))
    return(xportion * diff(xlim)/(2 * nrow(y1)))
}

plota.format <- 
function (temp, nround = 2, sprefix = "", eprefix = "") 
{
    return(paste(sprefix, format(round(as.numeric(temp), nround), 
        big.mark = ",", scientific = FALSE), eprefix, sep = ""))
}

plota.grid <- 
function () 
{
    abline(h = axTicks(2), col = "lightgray", lty = "dotted")
    abline(v = plota.control$xaxis.ticks, col = "lightgray", 
        lty = "dotted")
}

plota.hl <- 
function (y, col = plota.volume.col(y), border = plota.control$col.border) 
{
    dx = plota.dx(y)
    dxi0 = (dx/xinch()) * 96
    if (dxi0 < 1.75) {
        plota.hl.lwd(y, col = col, lwd = 1)
    }
    else {
        temp.x = attr(y, "index")
        rect(temp.x - dx/2, Lo(y), temp.x + dx/2, Hi(y), col = col, 
            border = border)
    }
}

plota.hl.lwd <- 
function (y, lwd = 1, ...) 
{
    temp.x = attr(y, "index")
    segments(temp.x, Lo(y), temp.x, Hi(y), lwd = lwd, lend = 2, 
        ...)
}

plota.legend <- 
function (labels, fill = NULL, lastobs = NULL, x = "topleft", 
    merge = F, bty = "n", yformat = plota.format, ...) 
{
    if (!is.null(fill)) 
        fill = spl(as.character(fill))
    labels = spl(as.character(labels))
    if (!is.null(lastobs)) {
        if (is.list(lastobs)) {
            labels1 = sapply(lastobs, function(x) unclass(last(x))[1])
        }
        else {
            labels1 = unclass(last(lastobs))[1]
        }
        labels = paste(labels, match.fun(yformat)(labels1))
    }
    legend(x, legend = labels, fill = fill, merge = merge, bty = bty, 
        ...)
}

plota.lines <- 
function (y, type = "l", col = par("col"), ...) 
{
    if (has.Cl(y)) 
        y1 = Cl(y)
    else y1 = y[, 1]
    temp.x = attr(y, "index")
    if (type == "l" & len(col) > 1) {
        for (icol in unique(col)) {
            lines(temp.x, iif(col == icol, y1, NA), type = type, 
                col = icol, ...)
        }
    }
    else {
        lines(temp.x, y1, type = type, col = col, ...)
    }
}

plota.ohlc <- 
function (y, col = plota.control$col.border) 
{
    dx = plota.dx(y)
    dxi0 = (dx/xinch()) * 96
    if (dxi0 < 1) {
        plota.hl.lwd(y, col = col, lwd = 1)
    }
    else if (dxi0 < 1.75) {
        plota.ohlc.lwd(y, col = col, lwd = 1)
    }
    else {
        temp.x = attr(y, "index")
        rect(temp.x - dx/8, Lo(y), temp.x + dx/8, Hi(y), col = col, 
            border = col)
        segments(temp.x - dx/2, Op(y), temp.x, Op(y), col = col)
        segments(temp.x + dx/2, Cl(y), temp.x, Cl(y), col = col)
    }
}

plota.ohlc.lwd <- 
function (y, lwd = 1, ...) 
{
    dx = plota.dx(y)
    temp.x = attr(y, "index")
    segments(temp.x, Lo(y), temp.x, Hi(y), lwd = lwd, lend = 2, 
        ...)
    segments(temp.x - dx/2, Op(y), temp.x, Op(y), lwd = lwd, 
        lend = 2, ...)
    segments(temp.x + dx/2, Cl(y), temp.x, Cl(y), lwd = lwd, 
        lend = 2, ...)
}

plota.scale.volume <- 
function (y) 
{
    Volumes = Vo(y)
    max.vol = max(Volumes, na.rm = T)
    vol.scale = list(100, "100s")
    if (max.vol > 10000) 
        vol.scale = list(1000, "1000s")
    if (max.vol > 1e+05) 
        vol.scale = list(10000, "10,000s")
    if (max.vol > 1e+06) 
        vol.scale = list(1e+05, "100,000s")
    if (max.vol > 1e+07) 
        vol.scale = list(1e+06, "millions")
    idv = grep("Volume", colnames(y))
    y[, idv] = Volumes/vol.scale[[1]]
    colnames(y)[idv] = paste(colnames(y)[idv], vol.scale[[2]], 
        sep = ";")
    return(y)
}

plota.volume <- 
function (y, col = plota.volume.col(y), border = plota.control$col.border) 
{
    dx = plota.dx(y)
    dxi0 = (dx/xinch()) * 96
    temp.x = attr(y, "index")
    if (dxi0 < 1.75) {
        segments(temp.x, 0, temp.x, Vo(y), col = col, lwd = 1, 
            lend = 2)
    }
    else {
        rect(temp.x - dx/2, 0, temp.x + dx/2, Vo(y), col = col, 
            border = border)
    }
    idv = grep("Volume", colnames(y))
    temp = spl(colnames(y)[idv], ";")
    if (len(temp) > 1) 
        legend("topright", legend = temp[len(temp)], bty = "n")
}

plota.volume.col <- 
function (y) 
{
    return(iif(Cl(y) > mlag(Cl(y)), plota.control$col.up, plota.control$col.dn))
}

plota.x.highlight <- 
function (y, highlight, col = plota.control$col.x.highlight) 
{
    if (len(col) == 1) {
        plota.x.highlight.helper(y, highlight, col = col)
    }
    else {
        for (icol in unique(col[highlight])) {
            plota.x.highlight.helper(y, iif(col == icol, highlight, 
                FALSE), col = icol)
        }
    }
}

plota.x.highlight.helper <- 
function (y, highlight, col = plota.control$col.x.highlight) 
{
    dx = plota.dx(y)
    hl_index = highlight
    if (is.logical(highlight)) 
        hl_index = which(highlight)
    if (identical(unique(highlight), c(0, 1))) 
        hl_index = which(as.logical(highlight))
    hl_index1 = which(diff(hl_index) > 1)
    hl_index = hl_index[sort(c(1, len(hl_index), hl_index1, (hl_index1 + 
        1)))]
    temp.y = par("usr")[3:4]
    if (par("ylog")) 
        temp.y = 10^temp.y
    temp.x = attr(y, "index")
    for (i in seq(1, len(hl_index), 2)) {
        rect(temp.x[hl_index[i]] - dx/2, temp.y[1], temp.x[hl_index[(i + 
            1)]] + dx/2, temp.y[2], col = col, border = col)
    }
    box()
}

plota.y.highlight <- 
function (y, highlight, col = plota.control$col.y.highlight) 
{
    temp.y = par("usr")[3:4]
    if (par("ylog")) 
        temp.y = 10^temp.y
    temp.x = par("usr")[1:2]
    if (par("xlog")) 
        temp.x = 10^temp.x
    highlight[highlight == Inf] = temp.y[2]
    highlight[highlight == -Inf] = temp.y[1]
    for (i in seq(1, len(highlight), by = 2)) {
        rect(temp.x[1], highlight[i], temp.x[2], highlight[(i + 
            1)], col = col, border = col)
    }
    box()
}

plotbt <- 
function (..., dates = NULL, plottype = spl("line,12M"), xfun = function(x) {
    x$equity
}, main = NULL, plotX = T, log = "", x.highlight = NULL, LeftMargin = 0) 
{
    models = variable.number.arguments(...)
    plottype = plottype[1]
    n = length(models)
    temp = list()
    for (i in 1:n) {
        msg = try(match.fun(xfun)(models[[i]]), silent = TRUE)
        if (class(msg)[1] != "try-error") {
            temp[[i]] = msg
        }
    }
    nlag = max(1, compute.annual.factor(temp[[1]]))
    yrange = c()
    for (i in 1:n) {
        itemp = temp[[i]]
        if (!is.null(dates)) {
            itemp = itemp[dates]
            if (itemp[1] != 0) 
                itemp = itemp/as.double(itemp[1])
        }
        if (plottype == "12M") {
            itemp = 100 * (itemp/mlag(itemp, nlag) - 1)
        }
        temp[[i]] = itemp
        yrange = range(yrange, itemp, na.rm = T)
    }
    plota(temp[[1]], main = main, plotX = plotX, type = "l", 
        col = 1, ylim = yrange, log = log, LeftMargin = LeftMargin, 
        x.highlight = x.highlight)
    if (n > 1) {
        for (i in 2:n) plota.lines(temp[[i]], col = i)
    }
    if (plottype == "12M") 
        legend("topright", legend = "12 Month Rolling", bty = "n")
    plota.legend(names(models), paste("", 1:n, sep = ""), temp)
}

portopt.mathprog.test <- 
function () 
{
    load.packages("quantmod,quadprog,corpcor")
    tickers = dow.jones.components()
    ia = aa.test.create.ia.custom(tickers, dates = "2000::2010")
    n = ia$n
    constraints = new.constraints(n, lb = 0, ub = 1)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    x = min.var.portfolio.gmpl(ia, constraints)
    png(filename = "plot1.png", width = 600, height = 400, units = "px", 
        pointsize = 12, bg = "white")
    barplot(100 * x, las = 2, main = "Minimum Variance Portfolio")
    dev.off()
    load.packages("Rglpk")
    model.file = "model1.mod"
    cat("\nset SYMBOLS ;\nvar weight{i in SYMBOLS} >= 0, <= 1 ;\nminimize alpha : sum{i in SYMBOLS} weight[i] ;\nsubject to fully_invested : sum{i in SYMBOLS} weight[i] = 1 ;\ndata;\nset SYMBOLS := ", 
        ia$symbols, ";\n", file = model.file, append = FALSE)
    model = Rglpk.read.model(model.file, type = "MathProg")
    constraints = Rglpk.create.constraints(model)$constraints
    x = min.var.portfolio.gmpl(ia, constraints)
    png(filename = "plot2.png", width = 600, height = 400, units = "px", 
        pointsize = 12, bg = "white")
    barplot(100 * x, las = 2, main = "Minimum Variance Portfolio using GNU MathProg model")
    dev.off()
    model.file = "model2.mod"
    cat("\nset SYMBOLS ;\nvar weight{i in SYMBOLS} >= 0, <= 1 ;\nvar held{SYMBOLS} binary;\nminimize alpha : sum{i in SYMBOLS} weight[i] ;\nsubject to fully_invested : sum{i in SYMBOLS} weight[i] = 1 ;\nsubject to MinWgt {i in SYMBOLS} : weight[i] >= 0.025 * held[i];\nsubject to MaxWgt {i in SYMBOLS} : weight[i] <= .20 * held[i] ;\nsubject to MaxAssetsLB : 0 <= sum {i in SYMBOLS} held[i] ;\nsubject to MaxAssetsUB : sum {i in SYMBOLS} held[i] <= 6 ;\ndata;\nset SYMBOLS := ", 
        ia$symbols, ";\n", file = model.file, append = FALSE)
    model = Rglpk.read.model(model.file, type = "MathProg")
    constraints = Rglpk.create.constraints(model)$constraints
    x = min.var.portfolio.gmpl(ia, constraints)
    png(filename = "plot3.png", width = 600, height = 400, units = "px", 
        pointsize = 12, bg = "white")
    barplot(100 * x, las = 2, main = "Minimum Variance Portfolio using GNU MathProg model \n with Minimum Investment and Number of Assets Constraints")
    dev.off()
    model.file = "model3.mod"
    cat("\nset SYMBOLS ;\nvar long {i in SYMBOLS} >= 0, <= 0.8 ;\nvar short{i in SYMBOLS} >= 0, <= 0.5 ;\nvar islong{SYMBOLS} binary;\nminimize alpha : sum{i in SYMBOLS} long[i] ;\nsubject to fully_invested : sum{i in SYMBOLS} (long[i] - short[i]) = 1 ;\nsubject to leverage : sum{i in SYMBOLS} (long[i] + short[i]) = 1.6 ;\nsubject to long_flag  {i in SYMBOLS} : long[i]  <= islong[i] ;\nsubject to short_flag {i in SYMBOLS} : short[i] <= (1 - islong[i]) ;\ndata;\nset SYMBOLS := ", 
        ia$symbols, ";\n", file = model.file, append = FALSE)
    model = Rglpk.read.model(model.file, type = "MathProg")
    constraints = Rglpk.create.constraints(model)$constraints
    x = min.var.portfolio.gmpl(aa.test.ia.add.short(ia), constraints)
    x = x[1:ia$n] - x[-c(1:ia$n)]
    png(filename = "plot4.png", width = 600, height = 400, units = "px", 
        pointsize = 12, bg = "white")
    barplot(100 * x, las = 2, main = "Minimum Variance Portfolio using GNU MathProg model \n with 130:30 Constraints")
    dev.off()
    ia = aa.test.create.ia.custom(tickers[1:15], dates = "2000::2010")
    model.file = "model4.mod"
    param = ia$cov[, 1, drop = F]
    colnames(param) = "CurWgt"
    param[, "CurWgt"] = 1/ia$n
    cat("\nset SYMBOLS ;\nparam CurWgt{SYMBOLS} ;\nvar weight{i in SYMBOLS} >= 0, <= 1 ;\nvar TradePos{i in SYMBOLS} >= 0 ;\nvar TradeNeg{i in SYMBOLS} >= 0 ;\nvar TradeFlag{SYMBOLS} binary;\nvar trade{SYMBOLS} binary;\nminimize alpha : sum{i in SYMBOLS} weight[i] ;\nsubject to fully_invested : sum{i in SYMBOLS} weight[i] = 1 ;\nsubject to TradeRange {i in SYMBOLS} : (CurWgt[i] - weight[i]) = (TradePos[i] - TradeNeg[i]) ;\nsubject to TradeFlagPos {i in SYMBOLS} : TradePos[i] <= 100 * TradeFlag[i];\nsubject to TradeFlagNeg {i in SYMBOLS} : TradeNeg[i] <= 100 * (1 - TradeFlag[i]);\nsubject to MinTradeSize {i in SYMBOLS} : (TradePos[i] + TradeNeg[i]) >= 0.01 * trade[i];\nsubject to MaxTradeSize {i in SYMBOLS} : (TradePos[i] + TradeNeg[i]) <= .90 * trade[i] ;\nsubject to MaxTrade : sum {i in SYMBOLS} trade[i] <= 48 ;\ndata;\nset SYMBOLS := ", 
        ia$symbols, ";\nparam : CurWgt:=\n", file = model.file, 
        append = FALSE)
    write.table(param, sep = "\t", quote = F, col.names = F, 
        file = model.file, append = TRUE)
    cat(";\n", file = model.file, append = TRUE)
    model = Rglpk.read.model(model.file, type = "MathProg")
    constraints = Rglpk.create.constraints(model)$constraints
    x = min.var.portfolio.gmpl(ia, constraints)
    sqrt(x %*% ia$cov %*% x)
    png(filename = "plot5.png", width = 600, height = 400, units = "px", 
        pointsize = 12, bg = "white")
    barplot(100 * x, las = 2, main = "Minimum Variance Portfolio using GNU MathProg model \n with Turnover Constraints")
    dev.off()
    ia = aa.test.create.ia.custom(tickers[1:10], dates = "2000::2010")
    model.file = "model4.mod"
    param = ia$cov[, 1, drop = F]
    colnames(param) = "CurWgt"
    param[, "CurWgt"] = 1/ia$n
    cat("\nset SYMBOLS ;\nparam CurWgt{SYMBOLS} ;\nvar weight{i in SYMBOLS} >= 0, <= 1 ;\nvar TradePos{i in SYMBOLS} >= 0 ;\nvar TradeNeg{i in SYMBOLS} >= 0 ;\nvar TradeFlag{SYMBOLS} binary;\nvar trade{SYMBOLS} binary;\nminimize alpha : sum{i in SYMBOLS} weight[i] ;\nsubject to fully_invested : sum{i in SYMBOLS} weight[i] = 1 ;\nsubject to TradeRange {i in SYMBOLS} : (CurWgt[i] - weight[i]) = (TradePos[i] - TradeNeg[i]) ;\nsubject to TradeFlagPos {i in SYMBOLS} : TradePos[i] <= 100 * TradeFlag[i];\nsubject to TradeFlagNeg {i in SYMBOLS} : TradeNeg[i] <= 100 * (1 - TradeFlag[i]);\nsubject to MinTradeSize {i in SYMBOLS} : (TradePos[i] + TradeNeg[i]) >= 0.05 * trade[i];\nsubject to MaxTradeSize {i in SYMBOLS} : (TradePos[i] + TradeNeg[i]) <= .20 * trade[i] ;\nsubject to MaxTrade : sum {i in SYMBOLS} trade[i] <= 8 ;\ndata;\nset SYMBOLS := ", 
        ia$symbols, ";\nparam : CurWgt:=\n", file = model.file, 
        append = FALSE)
    write.table(param, sep = "\t", quote = F, col.names = F, 
        file = model.file, append = TRUE)
    cat(";\n", file = model.file, append = TRUE)
    model = Rglpk.read.model(model.file, type = "MathProg")
    constraints = Rglpk.create.constraints(model)$constraints
    x = min.var.portfolio.gmpl(ia, constraints)
    sqrt(x %*% ia$cov %*% x)
    png(filename = "plot6.png", width = 600, height = 400, units = "px", 
        pointsize = 12, bg = "white")
    barplot(100 * x, las = 2, main = "Minimum Variance Portfolio using GNU MathProg model \n with Turnover Constraints")
    dev.off()
}

proxy.example.test <- 
function () 
{
    load.packages("quantmod")
    tickers = spl("GSG,DBC")
    data = new.env()
    getSymbols(tickers, src = "yahoo", from = "1970-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    temp = extract.table.from.webpage(join(readLines("TRJ_CRB")), 
        "EODValue")
    temp = join(apply(temp, 1, join, ","), "\n")
    data$CRB_1 = make.stock.xts(read.xts(temp, format = "%m/%d/%y"))
    data$CRB_2 = make.stock.xts(read.xts("prfmdata.csv", format = "%m/%d/%Y"))
    jpeg(filename = "plot1.jpg", width = 500, height = 500, units = "px", 
        pointsize = 12)
    proxy.test(data)
    dev.off()
    jpeg(filename = "plot2.jpg", width = 500, height = 500, units = "px", 
        pointsize = 12)
    proxy.overlay.plot(data)
    dev.off()
    load.packages("quantmod")
    tickers = spl("IYR,VGSIX,RWO")
    data = new.env()
    getSymbols(tickers, src = "yahoo", from = "1970-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    jpeg(filename = "plot3.jpg", width = 500, height = 500, units = "px", 
        pointsize = 12)
    proxy.test(data)
    dev.off()
    jpeg(filename = "plot4.jpg", width = 500, height = 500, units = "px", 
        pointsize = 12)
    proxy.overlay.plot(data)
    dev.off()
}

quantumonline.info <- 
function (id, type = c("cusip", "symbol", "sname")) 
{
    url = paste0("http://www.quantumonline.com/search.cfm?tickersymbol=", 
        id, "&sopt=", type[1], "&1.0.1=Search")
    txt = join(readLines(url))
    out = list()
    out$main = extract.table.from.webpage(gsub("&nbsp;", ",", 
        txt), "Company's Online Profile", has.header = F)
    out$address = extract.table.from.webpage(txt, "Address:", 
        has.header = F)
    return(out)
}

reconstruct.VXX.CBOE <- 
function (exact.match = T) 
{
    data = load.VXX.CBOE()
    dt = extract.VXX.CBOE(data, "dt", 1, exact.match)[1, ]
    dr = extract.VXX.CBOE(data, "dr", 1, exact.match)[1, ]
    x = extract.VXX.CBOE(data, "Settle", 1:2, exact.match)
    w = cbind(dr/dt, (dt - dr)/dt)
    val.cur = rowSums(x * mlag(w))
    val.yest = rowSums(mlag(x) * mlag(w))
    ret = val.cur/val.yest - 1
    index = ifna(mlag(dr) == 0, F)
    ret[index] = (x[, 1]/mlag(x[, 2]) - 1)[index]
    Close = cumprod(1 + ifna(ret, 0))
    VXX = make.xts(cbind(Close, x, dt, dr, ret), data$dates)
    x = extract.VXX.CBOE(data, "Settle", 4:7, exact.match)
    w = cbind(dr/dt, 1, 1, (dt - dr)/dt)
    val.cur = rowSums(x * mlag(w))
    val.yest = rowSums(mlag(x) * mlag(w))
    ret = val.cur/val.yest - 1
    index = ifna(mlag(dr) == 0, F)
    ret[index] = (rowSums(x[, -4])/mlag(rowSums(x[, -1])) - 1)[index]
    Close = cumprod(1 + ifna(ret, 0))
    VXZ = make.xts(cbind(Close, x, dt, dr, ret), data$dates)
    list(VXX = VXX, VXZ = VXZ)
}

replace.token <- 
function (txt, smarker, emarker, replacement, pos = 1) 
{
    token = extract.token(txt, smarker, emarker, pos, keep.marker = T)
    if (nchar(token) == 0) 
        txt
    else replace.token(gsub(pattern = token, replacement = replacement, 
        txt), smarker, emarker, replacement)
}

sp100.components <- 
function () 
{
    url = "http://www.barchart.com/stocks/sp100.php"
    txt = join(readLines(url))
    temp = extract.table.from.webpage(txt, "Components", has.header = T)
    i.start = grep("Name", temp[, 2])
    tickers = trim(temp[-c(1:i.start), 1])
    return(tickers)
}

sp500.components <- 
function () 
{
    url = "http://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
    txt = join(readLines(url))
    temp = extract.table.from.webpage(txt, "Ticker", has.header = T)
    tickers = temp[, "Ticker symbol"]
    sector = temp[, "GICS Sector"]
    return(list(tickers = tickers, sector = sector))
}

spl <- 
function (s, delim = ",") 
unlist(strsplit(s, delim))

test.implied.div.split <- 
function (ticker) 
{
    load.packages("quantmod")
    ticker = "IBM"
    data <- new.env()
    getSymbols.extra(ticker, src = "yahoo", from = "1970-01-01", 
        env = data, auto.assign = T, set.symbolnames = T)
    dividend = getDividends(ticker, from = "1900-01-01")
    split = getSplits(ticker, from = "1900-01-01")
    if (is.xts(split) && is.xts(dividend) && nrow(split) > 0 && 
        nrow(dividend) > 0) 
        dividend1 = dividend * 1/adjRatios(splits = merge(split, 
            index(dividend)))[, 1]
    else dividend1 = dividend
    close = Cl(data$IBM)
    adjusted = Ad(data$IBM)
    implied.split = close/mlag(close) * mlag(adjusted)/adjusted
    isplit.index = implied.split < 0.8 | implied.split > 1.2
    isplit = implied.split[isplit.index]
    isplit = round(100 * isplit)/100
    cbind(isplit["1970::"], split["1970::"])
    implied.div = mlag(close) * adjusted/mlag(adjusted) - close
    idiv.index = implied.div > 0.001
    idiv = implied.div[idiv.index & !isplit.index]
    idiv = round(1000 * idiv)/1000
    len(idiv["1970::"])
    len(dividend1["1970::"])
    setdiff(index(dividend1["1970::"]), index(idiv["1970::"]))
    setdiff(index(idiv["1970::"]), index(dividend1["1970::"]))
    cbind(idiv["1970::"], dividend1["1970::"])
    tickers = dow.jones.components()
    for (ticker in tickers) {
        data <- new.env()
        getSymbols.extra(ticker, src = "yahoo", from = "1970-01-01", 
            env = data, auto.assign = T)
        dividend = getDividends(ticker, from = "1900-01-01")
        split = getSplits(ticker, from = "1900-01-01")
        split = split[split > 0]
        dividend = dividend[dividend > 0]
        if (is.xts(split) && is.xts(dividend) && nrow(split) > 
            0 && nrow(dividend) > 0) 
            dividend1 = dividend * 1/adjRatios(splits = merge(split, 
                index(dividend)))[, 1]
        else dividend1 = dividend
        close = Cl(data[[ticker]])
        adjusted = Ad(data[[ticker]])
        implied.split = close/mlag(close) * mlag(adjusted)/adjusted
        isplit.index = ifna(implied.split < 0.9 | implied.split > 
            1.1, F)
        isplit = implied.split[isplit.index]
        isplit = round(100 * isplit)/100
        if (len(isplit) > 0) 
            cat(ticker, "SPL", len(isplit["1970::"]) - len(split["1970::"]), 
                max(round(isplit["1970::"], 3) - round(split["1970::"], 
                  3)), "\n")
        else cat(ticker, "SPL", len(isplit["1970::"]) - len(split["1970::"]), 
            "\n")
        implied.div = mlag(close) * adjusted/mlag(adjusted) - 
            close
        idiv.index = ifna(implied.div > 0.001, F)
        idiv = implied.div[idiv.index & !isplit.index]
        idiv = round(1000 * idiv)/1000
        len(idiv["1970::"])
        len(dividend1["1970::"])
        cat(ticker, "DIV", len(idiv["1970::"]) - len(dividend1["1970::"]), 
            len(setdiff(index(dividend1["1970::"]), index(idiv["1970::"]))), 
            len(setdiff(index(idiv["1970::"]), index(dividend1["1970::"]))), 
            max(round(idiv["1970::"], 3) - round(dividend1["1970::"], 
                3)), "\n")
        setdiff(index(dividend1["1970::"]), index(idiv["1970::"]))
        setdiff(index(idiv["1970::"]), index(dividend1["1970::"]))
    }
}

trim <- 
function (s) 
{
    s = sub(pattern = "^\\s+", replacement = "", x = s)
    sub(pattern = "\\s+$", replacement = "", x = s)
}

us.ishares.components <- 
function (Symbol = "DVY", date = NULL, debug = F) 
{
    url = paste("http://us.ishares.com/product_info/fund/holdings/", 
        Symbol, ".htm?periodCd=d", sep = "")
    if (!is.null(date)) 
        url = paste("http://us.ishares.com/product_info/fund/holdings/", 
            Symbol, ".htm?asofDt=", date.end(date), "&periodCd=m", 
            sep = "")
    txt = join(readLines(url))
    temp = remove.tags(extract.token(txt, "Holdings Detail", 
        "Holdings subject to change"))
    date = as.Date(spl(trim(temp), " ")[3], "%m/%d/%Y")
    temp = extract.table.from.webpage(txt, "Symbol", has.header = T)
    colnames(temp) = trim(colnames(temp))
    temp = trim(temp)
    tickers = temp[, "Symbol"]
    keep.index = nchar(tickers) > 1
    weights = as.double(temp[keep.index, "% Net Assets"])/100
    tickers = tickers[keep.index]
    out = list(tickers = tickers, weights = weights, date = date)
    if (debug) 
        out$txt = txt
    out
}

variable.number.arguments <- 
function (...) 
{
    out = list(...)
    if (is.list(out[[1]][[1]])) 
        return(out[[1]])
    names(out) = as.character(substitute(c(...))[-1])
    return(out)
}

wealthsimple.portfolio <- 
function (portfolio.number = 10) 
{
    url = paste0("http://faq.wealthsimple.com/article/", 120 + 
        portfolio.number, "-how-has-the-risk-level-", portfolio.number, 
        "-portfolio-performed")
    txt = join(readLines(url))
    temp = extract.table.from.webpage(txt, "Breakdown", has.header = F)
    temp = gsub(pattern = "%", replacement = "", temp)
    temp = trim(temp[, c(2, 4)])
    temp = temp[!is.na(temp[, 1]), ]
    value = as.numeric(temp[, 2])
    names(value) = temp[, 1]
    value
}

wealthsimple.portfolio.test <- 
function () 
{
    portfolios = list()
    for (i in 1:10) portfolios[[i]] = wealthsimple.portfolio(i)
    portfolios = t(sapply(portfolios, identity))
    plota.stacked(1:10, portfolios/100, flip.legend = T, type = "s", 
        xaxp = c(1, 10, 9), las = 1, main = "Wealthsimple Transition Matrix", 
        xlab = "Risk Portfolio")
}

zacks.info <- 
function (ticker = "IBM") 
{
    url = paste0("http://www.zacks.com/stock/research/", ticker, 
        "/earnings-announcements")
    txt = join(readLines(url))
    out = list()
    require(jsonlite)
    for (i in spl("earnings,webcasts,revisions,splits,dividends,guidance")) {
        data = extract.token(txt, paste0("<script>,window.app_data_", 
            i, ",=,\"data\""), "</script>")
        data = fromJSON(paste("{\"data\"", data))
        out[[i]] = data$data
    }
    out
}

