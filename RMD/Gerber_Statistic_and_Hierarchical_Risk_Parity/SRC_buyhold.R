Sys.setenv(TZ = 'GMT')

br.rank <- 
function (x) 
{
    t(apply(coredata(-x), 1, rank, na.last = "keep"))
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

bt.detail.summary <- 
function (bt, trade.summary = NULL) 
{
    out.all = list()
    out = list()
    out$Period = join(format(range(index.xts(bt$equity)), "%b%Y"), 
        " - ")
    out$Cagr = compute.cagr(bt$equity)
    out$Sharpe = compute.sharpe(bt$ret)/100
    out$DVR = compute.DVR(bt)/100
    out$Volatility = compute.risk(bt$ret)
    out$MaxDD = compute.max.drawdown(bt$equity)
    out$AvgDD = compute.avg.drawdown(bt$equity)
    if (!is.null(trade.summary)) {
        out$Profit.Factor = trade.summary$stats["profitfactor", 
            "All"]
    }
    out$VaR = compute.var(bt$ret)
    out$CVaR = compute.cvar(bt$ret)
    out$Exposure = compute.exposure(bt$weight)
    out.all$System = lapply(out, function(x) if (is.double(x)) 
        round(100 * x, 2)
    else x)
    if (!is.null(bt$trade.summary)) 
        trade.summary = bt$trade.summary
    out = list()
    if (!is.null(trade.summary)) {
        out$Win.Percent = trade.summary$stats["win.prob", "All"]
        out$Avg.Trade = trade.summary$stats["avg.pnl", "All"]
        out$Avg.Win = trade.summary$stats["win.avg.pnl", "All"]
        out$Avg.Loss = trade.summary$stats["loss.avg.pnl", "All"]
        out = lapply(out, function(x) if (is.double(x)) 
            round(100 * x, 1)
        else x)
        out$Best.Trade = max(as.double(trade.summary$trades[, 
            "return"]))
        out$Worst.Trade = min(as.double(trade.summary$trades[, 
            "return"]))
        out$WinLoss.Ratio = round(-trade.summary$stats["win.avg.pnl", 
            "All"]/trade.summary$stats["loss.avg.pnl", "All"], 
            2)
        out$Avg.Len = round(trade.summary$stats["len", "All"], 
            2)
        out$Num.Trades = trade.summary$stats["ntrades", "All"]
    }
    out.all$Trade = out
    out = list()
    out$Win.Percent.Day = sum(bt$ret > 0, na.rm = T)/len(bt$ret)
    out$Best.Day = bt$best
    out$Worst.Day = bt$worst
    month.ends = endpoints(bt$equity, "months")
    mret = ROC(bt$equity[month.ends, ], type = "discrete")
    out$Win.Percent.Month = sum(mret > 0, na.rm = T)/len(mret)
    out$Best.Month = max(mret, na.rm = T)
    out$Worst.Month = min(mret, na.rm = T)
    year.ends = endpoints(bt$equity, "years")
    mret = ROC(bt$equity[year.ends, ], type = "discrete")
    out$Win.Percent.Year = sum(mret > 0, na.rm = T)/len(mret)
    out$Best.Year = max(mret, na.rm = T)
    out$Worst.Year = min(mret, na.rm = T)
    out.all$Period = lapply(out, function(x) if (is.double(x)) 
        round(100 * x, 1)
    else x)
    return(out.all)
}

bt.exrem <- 
function (weight) 
{
    bt.apply.matrix(weight, exrem)
}

bt.merge <- 
function (b, align = c("keep.all", "remove.na"), dates = NULL) 
{
    align = align[1]
    symbolnames = b$symbolnames
    nsymbols = len(symbolnames)
    ncount = sapply(symbolnames, function(i) nrow(b[[i]]))
    all.dates = double(sum(ncount))
    itemp = 1
    for (i in 1:nsymbols) {
        all.dates[itemp:(itemp + ncount[i] - 1)] = attr(b[[symbolnames[i]]], 
            "index")
        itemp = itemp + ncount[i]
    }
    temp = sort(all.dates)
    unique.dates = c(temp[1], temp[-1][diff(temp) != 0])
    if (!is.null(dates)) {
        class(unique.dates) = c("POSIXct", "POSIXt")
        temp = make.xts(integer(len(unique.dates)), unique.dates)
        unique.dates = attr(temp[dates], "index")
    }
    date.map = matrix(NA, nr = len(unique.dates), nsymbols)
    itemp = 1
    for (i in 1:nsymbols) {
        index = match(all.dates[itemp:(itemp + ncount[i] - 1)], 
            unique.dates)
        sub.index = which(!is.na(index))
        date.map[index[sub.index], i] = sub.index
        itemp = itemp + ncount[i]
    }
    index = c()
    if (align == "remove.na") {
        index = which(count(date.map, side = 1) < nsymbols)
    }
    if (len(index) > 0) {
        date.map = date.map[-index, , drop = FALSE]
        unique.dates = unique.dates[-index]
    }
    class(unique.dates) = c("POSIXct", "POSIXt")
    return(list(all.dates = unique.dates, date.map = date.map))
}

bt.prep <- 
function (b, align = c("keep.all", "remove.na"), dates = NULL, 
    fill.gaps = F) 
{
    if (!exists("symbolnames", b, inherits = F)) 
        b$symbolnames = ls(b)
    symbolnames = b$symbolnames
    nsymbols = len(symbolnames)
    if (nsymbols > 1) {
        out = bt.merge(b, align, dates)
        for (i in 1:nsymbols) {
            b[[symbolnames[i]]] = make.xts(coredata(b[[symbolnames[i]]])[out$date.map[, 
                i], , drop = FALSE], out$all.dates)
            map.col = find.names("Close,Volume,Open,High,Low,Adjusted", 
                b[[symbolnames[i]]])
            if (fill.gaps & !is.na(map.col$Close)) {
                close = coredata(b[[symbolnames[i]]][, map.col$Close])
                n = len(close)
                last.n = max(which(!is.na(close)))
                close = ifna.prev(close)
                if (last.n + 5 < n) 
                  close[last.n:n] = NA
                b[[symbolnames[i]]][, map.col$Close] = close
                index = !is.na(close)
                if (!is.na(map.col$Volume)) {
                  index1 = is.na(b[[symbolnames[i]]][, map.col$Volume]) & 
                    index
                  b[[symbolnames[i]]][index1, map.col$Volume] = 0
                }
                for (field in spl("Open,High,Low,Adjusted")) {
                  j = map.col[[field]]
                  if (!is.null(j)) {
                    index1 = is.na(b[[symbolnames[i]]][, j]) & 
                      index
                    b[[symbolnames[i]]][index1, j] = close[index1]
                  }
                }
            }
        }
    }
    else {
        if (!is.null(dates)) 
            b[[symbolnames[1]]] = b[[symbolnames[1]]][dates, 
                ]
        out = list(all.dates = index.xts(b[[symbolnames[1]]]))
    }
    b$dates = out$all.dates
    dummy.mat = matrix(double(), len(out$all.dates), nsymbols)
    colnames(dummy.mat) = symbolnames
    dummy.mat = make.xts(dummy.mat, out$all.dates)
    b$weight = dummy.mat
    b$execution.price = dummy.mat
    for (i in 1:nsymbols) {
        if (has.Cl(b[[symbolnames[i]]])) {
            dummy.mat[, i] = Cl(b[[symbolnames[i]]])
        }
    }
    b$prices = dummy.mat
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
    bt = bt.summary(weight, ret, type, b$prices, capital, commission, 
        dates.index)
    bt$dates.index = dates.index
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

bt.run.share <- 
function (b, prices = b$prices, clean.signal = T, trade.summary = F, 
    do.lag = 1, do.CarryLastObservationForwardIfNA = TRUE, silent = F, 
    capital = 1e+05, commission = 0, weight = b$weight, dates = 1:nrow(b$prices)) 
{
    prices[] = bt.apply.matrix(coredata(prices), ifna.prev)
    weight = mlag(weight, do.lag - 1)
    do.lag = 1
    if (clean.signal) {
        weight[] = (capital/prices) * bt.exrem(weight)
    }
    else {
        weight[] = (capital/prices) * weight
    }
    bt.run(b, trade.summary = trade.summary, do.lag = do.lag, 
        do.CarryLastObservationForwardIfNA = do.CarryLastObservationForwardIfNA, 
        type = "share", silent = silent, capital = capital, commission = commission, 
        weight = weight, dates = dates)
}

bt.summary <- 
function (weight, ret, type = c("weight", "share"), close.prices, 
    capital = 1e+05, commission = 0, dates.index = 1:nrow(weight)) 
{
    if (!is.list(commission)) {
        if (type == "weight") 
            commission = list(cps = 0, fixed = 0, percentage = commission)
        else commission = list(cps = commission, fixed = 0, percentage = 0)
    }
    if (len(dates.index) != nrow(weight)) {
        weight = weight[dates.index, , drop = F]
        ret = ret[dates.index, , drop = F]
        close.prices = close.prices[dates.index, , drop = F]
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
    n = nrow(weight)
    tend[n, weight[n, ] != 0] = T
    tend[1, ] = F
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

compute.annual.factor <- 
function (x) 
{
    possible.values = c(252, 52, 26, 13, 12, 6, 4, 3, 2, 1)
    index = which.min(abs(compute.raw.annual.factor(x) - possible.values))
    round(possible.values[index])
}

compute.avg.drawdown <- 
function (x) 
{
    drawdown = c(0, compute.drawdown(coredata(x)), 0)
    dstart = which(drawdown == 0 & mlag(drawdown, -1) != 0)
    dend = which(drawdown == 0 & mlag(drawdown, 1) != 0)
    drawdowns = apply(cbind(dstart, dend), 1, function(x) min(drawdown[x[1]:x[2]], 
        na.rm = T))
    mean(drawdowns)
}

compute.cagr <- 
function (equity, nyears = NA) 
{
    if (is.numeric(nyears)) 
        as.double(last(equity, 1)^(1/nyears) - 1)
    else as.double(last(equity, 1)^(1/compute.nyears(equity)) - 
        1)
}

compute.cvar <- 
function (x, probs = 0.05) 
{
    x = coredata(x)
    mean(x[x < quantile(x, probs = probs)])
}

compute.drawdown <- 
function (x) 
{
    return(x/cummax(c(1, x))[-1] - 1)
}

compute.DVR <- 
function (bt) 
{
    return(compute.sharpe(bt$ret) * compute.R2(bt$equity))
}

compute.exposure <- 
function (weight) 
{
    sum(apply(weight, 1, function(x) sum(x != 0)) != 0)/nrow(weight)
}

compute.max.drawdown <- 
function (x) 
{
    as.double(min(compute.drawdown(x)))
}

compute.nyears <- 
function (x) 
{
    as.double(diff(as.Date(range(index.xts(x)))))/365
}

compute.R2 <- 
function (equity) 
{
    x = as.double(index.xts(equity))
    y = equity
    return(cor(y, x)^2)
}

compute.raw.annual.factor <- 
function (x) 
{
    round(nrow(x)/compute.nyears(x))
}

compute.risk <- 
function (x) 
{
    temp = compute.annual.factor(x)
    x = as.vector(coredata(x))
    return(sqrt(temp) * sd(x))
}

compute.sharpe <- 
function (x) 
{
    temp = compute.annual.factor(x)
    x = as.vector(coredata(x))
    return(sqrt(temp) * mean(x)/sd(x))
}

compute.var <- 
function (x, probs = 0.05) 
{
    quantile(coredata(x), probs = probs)
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

date.month <- 
function (dates) 
{
    return(as.double(format(dates, "%m")))
}

date.month.ends <- 
function (dates, last.date = T) 
{
    ends = which(diff(100 * date.year(dates) + date.month(dates)) != 
        0)
    ends.add.last.date(ends, len(dates), last.date)
}

date.year <- 
function (dates) 
{
    return(as.double(format(dates, "%Y")))
}

date.year.ends <- 
function (dates, last.date = T) 
{
    ends = which(diff(date.year(dates)) != 0)
    ends.add.last.date(ends, len(dates), last.date)
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

ends.add.last.date <- 
function (ends, last.date, action = T) 
{
    if (action) 
        unique(c(ends, last.date))
    else ends
}

engineering.returns.kpi <- 
function (bt, trade.summary = NULL) 
{
    if (!is.null(bt$trade.summary)) 
        trade.summary = bt$trade.summary
    out = list()
    out$Period = join(format(range(index(bt$equity)), "%b%Y"), 
        " - ")
    out$Cagr = compute.cagr(bt$equity)
    out$Sharpe = compute.sharpe(bt$ret)/100
    out$DVR = compute.DVR(bt)/100
    out$R2 = compute.R2(bt$equity)/100
    out$Volatility = compute.risk(bt$ret)
    out$MaxDD = compute.max.drawdown(bt$equity)
    out$Exposure = compute.exposure(bt$weight)
    if (!is.null(trade.summary)) {
        out$Win.Percent = trade.summary$stats["win.prob", "All"]
        out$Avg.Trade = trade.summary$stats["avg.pnl", "All"]
        out$Profit.Factor = trade.summary$stats["profitfactor", 
            "All"]/100
    }
    out = lapply(out, function(x) if (is.double(x)) 
        round(100 * x, 2)
    else x)
    if (!is.null(trade.summary)) 
        out$Num.Trades = trade.summary$stats["ntrades", "All"]
    return(list(System = out))
}

env <- 
function (..., hash = TRUE, parent = parent.frame(), size = 29L) 
{
    temp = new.env(hash = hash, parent = parent, size = size)
    values = list(...)
    if (len(values) == 0) 
        return(temp)
    values.names = names(values)
    names = as.character(substitute(c(...))[-1])
    names = iif(nchar(values.names) > 0, values.names, names)
    for (i in 1:len(values)) temp[[names[i]]] = values[[i]]
    temp
}

exrem <- 
function (x) 
{
    temp = c(0, ifna(ifna.prev(x), 0))
    itemp = which(temp != mlag(temp))
    x[] = NA
    x[(itemp - 1)] = temp[itemp]
    return(x)
}

extend.data <- 
function (current, hist, scale = F) 
{
    colnames(current) = sapply(colnames(current), function(x) last(spl(x, 
        "\\.")))
    colnames(hist) = sapply(colnames(hist), function(x) last(spl(x, 
        "\\.")))
    close.index = find.names("Close", hist)
    if (len(close.index) == 0) 
        close.index = 1
    adjusted.index = find.names("Adjusted", hist)
    if (len(adjusted.index) == 0) 
        adjusted.index = close.index
    if (scale) {
        cur.close.index = find.names("Close", current)
        if (len(cur.close.index) == 0) 
            cur.close.index = 1
        cur.adjusted.index = find.names("Adjusted", current)
        if (len(cur.adjusted.index) == 0) 
            cur.adjusted.index = cur.close.index
        common = merge(current[, cur.close.index], hist[, close.index], 
            join = "inner")
        scale = as.numeric(common[1, 1])/as.numeric(common[1, 
            2])
        if (close.index == adjusted.index) 
            hist = hist * scale
        else {
            hist[, -adjusted.index] = hist[, -adjusted.index] * 
                scale
            common = merge(current[, cur.adjusted.index], hist[, 
                adjusted.index], join = "inner")
            scale = as.numeric(common[1, 1])/as.numeric(common[1, 
                2])
            hist[, adjusted.index] = hist[, adjusted.index] * 
                scale
        }
    }
    hist = hist[format(index(current[1]) - 1, "::%Y:%m:%d"), 
        , drop = F]
    if (ncol(hist) != ncol(current)) 
        hist = rep.col(hist[, adjusted.index], ncol(current))
    else hist = hist[, colnames(current)]
    colnames(hist) = colnames(current)
    rbind(hist, current)
}

find.names <- 
function (names, data, return.index = T) 
{
    names = spl(names)
    all.names = colnames(data)
    out = list()
    for (n in names) {
        loc = grep(n, all.names, ignore.case = TRUE)
        if (len(loc) == 0 && ncol(data) == 1 && (grepl(n, "close", 
            ignore.case = TRUE) || grepl(n, "adjusted", ignore.case = TRUE))) 
            loc = 1
        if (len(loc) > 0) 
            out[[n]] = iif(return.index, loc, all.names[loc])
    }
    iif(len(names) == 1 && len(out) == 1, out[[1]][1], out)
}

getSymbols.extra <- 
function (Symbols = NULL, env = parent.frame(), getSymbols.fn = getSymbols, 
    raw.data = new.env(), set.symbolnames = F, auto.assign = T, 
    ...) 
{
    if (is.character(Symbols)) 
        Symbols = spl(Symbols)
    if (len(Symbols) < 1) 
        return(Symbols)
    Symbols = spl(toupper(gsub("\n", ",", join(Symbols, ","))))
    map = list()
    for (s in Symbols) {
        if (nchar(trim(s)) == 0) 
            next
        if (substring(trim(s)[1], 1, 1) == "#") 
            next
        temp = spl(spl(s, "#")[1], "=")
        if (len(temp) > 1) {
            name = temp[1]
            values = trim(spl(temp[2], "\\+"))
            value1 = values[1]
            value1.name = grepl("\\[", value1)
            value1 = gsub("\\]", "", gsub("\\[", "", value1))
            value1 = trim(spl(value1, ";"))
            values = values[-1]
            for (n in trim(spl(name, ";"))) map[[n]] = c(value1[1], 
                values)
            if (len(value1) > 1 || value1.name) 
                for (n in value1) map[[n]] = c(n, values)
        }
        else {
            temp = spl(temp, "\\+")
            name = temp[1]
            values = trim(temp[-1])
            for (n in trim(spl(name, ";"))) map[[n]] = c(n, values)
        }
    }
    Symbols = unique(unlist(map))
    Symbols = setdiff(Symbols, ls(raw.data))
    data <- new.env()
    if (len(Symbols) > 0) 
        match.fun(getSymbols.fn)(Symbols, env = data, auto.assign = T, 
            ...)
    for (n in ls(raw.data)) data[[n]] = raw.data[[n]]
    if (set.symbolnames) 
        env$symbolnames = names(map)
    for (s in names(map)) {
        env[[s]] = data[[gsub("\\^", "", map[[s]][1])]]
        if (len(map[[s]]) > 1) 
            for (i in 2:len(map[[s]])) if (is.null(data[[gsub("\\^", 
                "", map[[s]][i])]])) 
                cat("Not Downloaded, main =", s, "missing", gsub("\\^", 
                  "", map[[s]][i]), "\n", sep = "\t")
            else env[[s]] = extend.data(env[[s]], data[[gsub("\\^", 
                "", map[[s]][i])]], scale = T)
        if (!auto.assign) 
            return(env[[s]])
    }
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

ifnull <- 
function (x, y) 
{
    return(iif(is.null(x), y, x))
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

join <- 
function (v, delim = "") 
{
    return(paste(v, collapse = delim))
}

last.signals <- 
function (..., n = 20, make.plot = T, return.table = F, smain = NULL) 
{
    models = variable.number.arguments(...)
    model = models[[1]]
    name = ifnull(names(models), NULL)[1]
    if (!is.null(model$period.weight)) {
        data = round(100 * model$period.weight, 0)
        ntrades = min(n, nrow(data))
        trades = last(data, ntrades)
        if (!is.null(smain) || !is.null(name)) 
            smain = iif(is.null(smain), name, smain)
        else smain = "Date"
        if (make.plot) {
            layout(1)
            plot.table(as.matrix(trades))
        }
        if (return.table) 
            trades
    }
}

last.trades <- 
function (..., n = 20, make.plot = T, return.table = F, smain = NULL) 
{
    models = variable.number.arguments(...)
    model = models[[1]]
    name = ifnull(names(models), NULL)[1]
    if (!is.null(model$trade.summary)) {
        ntrades = min(n, nrow(model$trade.summary$trades))
        trades = last(model$trade.summary$trades, ntrades)
        if (!is.null(smain) || !is.null(name)) 
            colnames(trades)[1] = iif(is.null(smain), name, smain)
        if (make.plot) {
            layout(1)
            plot.table(trades)
        }
        if (return.table) 
            trades
    }
}

len <- 
function (x) 
{
    return(length(x))
}

list2matrix <- 
function (ilist, keep.names = TRUE) 
{
    if (is.list(ilist[[1]])) {
        inc = 1
        if (keep.names) 
            inc = 2
        out = matrix("", nr = max(unlist(lapply(ilist, len))), 
            nc = inc * len(ilist))
        colnames(out) = rep("", inc * len(ilist))
        for (i in 1:len(ilist)) {
            nr = len(ilist[[i]])
            colnames(out)[inc * i] = names(ilist)[i]
            if (nr > 0) {
                if (keep.names) {
                  out[1:nr, (2 * i - 1)] = names(ilist[[i]])
                }
                else {
                  rownames(out) = names(ilist[[i]])
                }
                out[1:nr, inc * i] = unlist(ilist[[i]])
            }
        }
        return(out)
    }
    else {
        return(as.matrix(unlist(ilist)))
    }
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

make.table <- 
function (nr, nc) 
{
    savepar = par(mar = rep(1, 4))
    plot(c(0.5, nc * 2 + 0.5), c(-0.5, -(nr + 0.5)), xaxs = "i", 
        yaxs = "i", type = "n", xlab = "", ylab = "", axes = FALSE)
    savepar
}

make.xts <- 
function (x, order.by) 
{
    tzone = Sys.getenv("TZ")
    orderBy = class(order.by)
    index = as.numeric(as.POSIXct(order.by, tz = tzone))
    if (is.null(dim(x))) {
        if (len(order.by) == 1) 
            x = t(as.matrix(x))
        else dim(x) = c(len(x), 1)
    }
    x = as.matrix(x)
    x = structure(.Data = x, index = structure(index, tzone = tzone, 
        tclass = orderBy), class = c("xts", "zoo"), .indexCLASS = orderBy, 
        tclass = orderBy, .indexTZ = tzone, tzone = tzone)
    return(x)
}

map2monthly <- 
function (equity) 
{
    if (compute.annual.factor(equity) >= 12) 
        return(equity)
    dates = index(equity)
    equity = coredata(equity)
    temp = as.Date(c("", 10000 * date.year(dates) + 100 * date.month(dates) + 
        1), "%Y%m%d")[-1]
    new.dates = seq(temp[1], last(temp), by = "month")
    map = match(100 * date.year(dates) + date.month(dates), 100 * 
        date.year(new.dates) + date.month(new.dates))
    temp = rep(NA, len(new.dates))
    temp[map] = equity
    return(make.xts(ifna.prev(temp), new.dates))
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

plota.colors <- 
function (N) 
{
    col = rev(c("yellow", "cyan", "magenta", "red", "gray", "green", 
        "blue"))
    temp = list()
    for (j in 1:length(col)) {
        temp[[j]] = colors()[grep(col[j], colors())]
        temp[[j]] = temp[[j]][grep("^[^0-9]*$", temp[[j]])]
        temp[[j]] = temp[[j]][order(nchar(temp[[j]]))]
        index = which(colSums(col2rgb(temp[[j]])) < 100)
        if (length(index) > 0) 
            temp[[j]] = temp[[j]][-index]
        index = which(colSums(255 - col2rgb(temp[[j]])) < 100)
        if (length(index) > 0) 
            temp[[j]] = temp[[j]][-index]
    }
    index = 1
    col = rep("", N)
    for (i in 1:10) {
        for (j in 1:length(temp)) {
            if (length(temp[[j]]) >= i) {
                col[index] = temp[[j]][i]
                index = index + 1
                if (index > N) 
                  break
            }
        }
        if (index > N) 
            break
    }
    return(col)
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

plota.stacked <- 
function (x, y, xlab = "", col = plota.colors(ncol(y)), type = c("l", 
    "s"), flip.legend = F, ...) 
{
    y = 100 * y
    y1 = list()
    y1$positive = y
    y1$positive[y1$positive < 0] = 0
    y1$negative = y
    y1$negative[y1$negative > 0] = 0
    ylim = c(min(rowSums(y1$negative, na.rm = T)), max(1, rowSums(y1$positive, 
        na.rm = T)))
    if (class(x)[1] != "Date" & class(x)[1] != "POSIXct") {
        plot(x, rep(0, len(x)), ylim = ylim, t = "n", xlab = "", 
            ylab = "", cex = par("cex"), ...)
        grid()
    }
    else {
        plota(make.xts(y[, 1], x), ylim = ylim, cex = par("cex"), 
            LeftMargin = 4, ...)
        axis(2, las = 1)
        x = unclass(as.POSIXct(x))
    }
    mtext("Allocation %", side = 2, line = 3, cex = par("cex"))
    mtext(xlab, side = 1, line = 2, cex = par("cex"))
    if (type[1] == "l") {
        prep.x = c(x[1], x, x[len(x)])
        for (y in y1) {
            for (i in ncol(y):1) {
                prep.y = c(0, rowSums(y[, 1:i, drop = FALSE]), 
                  0)
                polygon(prep.x, prep.y, col = col[i], border = NA, 
                  angle = 90)
            }
        }
    }
    else {
        dx = mean(diff(x))
        prep.x = c(rep(x, each = 2), x[len(x)] + dx, x[len(x)] + 
            dx)
        for (y in y1) {
            for (i in ncol(y):1) {
                prep.y = c(0, rep(rowSums(y[, 1:i, drop = FALSE]), 
                  each = 2), 0)
                polygon(prep.x, prep.y, col = col[i], border = NA, 
                  angle = 90)
            }
        }
    }
    if (flip.legend) 
        plota.legend(rev(colnames(y)), rev(col), cex = par("cex"))
    else plota.legend(colnames(y), col, cex = par("cex"))
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

plotbt.monthly.table <- 
function (equity, make.plot = TRUE, smain = "") 
{
    equity = map2monthly(equity)
    dates = index.xts(equity)
    equity = coredata(equity)
    if (T) {
        month.ends = date.month.ends(dates)
        year.ends = date.year.ends(dates[month.ends])
        year.ends = month.ends[year.ends]
        nr = len(year.ends) + 1
    }
    else {
        month.ends = unique(c(endpoints(dates, "months"), len(dates)))
        month.ends = month.ends[month.ends > 0]
        year.ends = unique(c(endpoints(dates[month.ends], "years"), 
            len(month.ends)))
        year.ends = year.ends[year.ends > 0]
        year.ends = month.ends[year.ends]
        nr = len(year.ends) + 1
    }
    temp = matrix(double(), nr, 12 + 2)
    rownames(temp) = c(date.year(dates[year.ends]), "Avg")
    colnames(temp) = spl("Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Year,MaxDD")
    index = c(1, year.ends)
    for (iyear in 2:len(index)) {
        iequity = equity[index[(iyear - 1)]:index[iyear]]
        iequity = ifna(ifna.prev(iequity), 0)
        temp[(iyear - 1), "Year"] = last(iequity, 1)/iequity[1] - 
            1
        temp[(iyear - 1), "MaxDD"] = min(iequity/cummax(iequity) - 
            1, na.rm = T)
    }
    index = month.ends
    monthly.returns = c(NA, diff(equity[index])/equity[index[-len(index)]])
    index = date.month(range(dates[index]))
    monthly.returns = c(rep(NA, index[1] - 1), monthly.returns, 
        rep(NA, 12 - index[2]))
    temp[1:(nr - 1), 1:12] = matrix(monthly.returns, ncol = 12, 
        byrow = T)
    temp = ifna(temp, NA)
    temp[nr, ] = apply(temp[-nr, ], 2, mean, na.rm = T)
    if (make.plot) {
        highlight = temp
        highlight[] = iif(temp > 0, "lightgreen", iif(temp < 
            0, "red", "white"))
        highlight[nr, ] = iif(temp[nr, ] > 0, "green", iif(temp[nr, 
            ] < 0, "orange", "white"))
        highlight[, 13] = iif(temp[, 13] > 0, "green", iif(temp[, 
            13] < 0, "orange", "white"))
        highlight[, 14] = "yellow"
    }
    temp[] = plota.format(100 * temp, 1, "", "")
    if (make.plot) 
        plot.table(temp, highlight = highlight, smain = smain)
    return(temp)
}

plotbt.strategy.sidebyside <- 
function (..., perfromance.metric = spl("System,Trade,Period"), 
    perfromance.fn = "bt.detail.summary", return.table = FALSE, 
    make.plot = TRUE) 
{
    models = variable.number.arguments(...)
    out = list()
    for (i in 1:len(models)) {
        out[[names(models)[i]]] = match.fun(perfromance.fn)(models[[i]])[[perfromance.metric[1]]]
    }
    temp = list2matrix(out, keep.names = F)
    if (make.plot) 
        plot.table(temp, smain = perfromance.metric[1])
    if (return.table) 
        return(temp)
}

plotbt.transition.map <- 
function (weight, name = "", col = rainbow(ncol(weight), start = 0, 
    end = 0.9), x.highlight = NULL) 
{
    par(mar = c(2, 4, 1, 1), cex = 0.8, cex.main = 0.8, cex.sub = 0.8, 
        cex.axis = 0.8, cex.lab = 0.8)
    weight[is.na(weight)] = 0
    weight = weight[, sort.list(colSums(weight != 0), decreasing = T)]
    plota.stacked(index.xts(weight), weight, col = col, type = "s", 
        flip.legend = T, main = iif(nchar(name) > 0, paste("Transition Map for", 
            name), ""), x.highlight = x.highlight)
}

rep.col <- 
function (m, nc) 
{
    if (nc == 1) 
        m
    else {
        if (is.xts(m)) 
            make.xts(matrix(coredata(m), nr = len(m), nc = nc, 
                byrow = F), index(m))
        else matrix(m, nr = len(m), nc = nc, byrow = F)
    }
}

spl <- 
function (s, delim = ",") 
{
    return(unlist(strsplit(s, delim)))
}

trim <- 
function (s) 
{
    s = sub(pattern = "^\\s+", replacement = "", x = s)
    s = sub(pattern = "\\s+$", replacement = "", x = s)
    return(s)
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

