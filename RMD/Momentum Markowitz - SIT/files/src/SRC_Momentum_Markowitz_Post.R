Sys.setenv(TZ='GMT')

add.constraints <- 
function (A, b, type = c("=", ">=", "<="), constraints) 
{
    if (is.null(constraints)) 
        constraints = new.constraints(n = nrow(A))
    if (is.null(dim(A))) 
        A = matrix(A)
    if (len(b) == 1) 
        b = rep(b, ncol(A))
    if (type[1] == "=") {
        constraints$A = cbind(A, constraints$A)
        constraints$b = c(b, constraints$b)
        constraints$meq = constraints$meq + len(b)
    }
    if (type[1] == ">=") {
        constraints$A = cbind(constraints$A, A)
        constraints$b = c(constraints$b, b)
    }
    if (type[1] == "<=") {
        constraints$A = cbind(constraints$A, -A)
        constraints$b = c(constraints$b, -b)
    }
    return(constraints)
}

barplot.with.labels <- 
function (data, main, plotX = TRUE, label = c("level", "name", 
    "both")) 
{
    par(mar = c(iif(plotX, 6, 2), 4, 2, 2))
    x = barplot(100 * data, main = main, las = 2, names.arg = iif(plotX, 
        names(data), ""))
    if (label[1] == "level") 
        text(x, 100 * data, round(100 * data, 1), adj = c(0.5, 
            1), xpd = TRUE)
    if (label[1] == "name") 
        text(x, 0 * data, names(data), adj = c(-0.1, 1), srt = 90, 
            xpd = TRUE)
    if (label[1] == "both") 
        text(x, 0 * data, paste(round(100 * data), "% ", names(data), 
            sep = ""), adj = c(-0.1, 1), srt = 90, xpd = TRUE)
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

bt.apply.min.weight <- 
function (weight, long.min.weight = 0.1, short.min.weight = long.min.weight) 
{
    if (is.null(dim(weight))) 
        dim(weight) = c(1, len(weight))
    pos = apply(weight, 1, function(row) sum(row[row > 0]))
    neg = rowSums(weight) - pos
    pos.mat = iif(weight >= long.min.weight, weight, 0)
    neg.mat = iif(weight <= -short.min.weight, weight, 0)
    pos.mat = pos.mat * ifna(pos/rowSums(pos.mat), 1)
    neg.mat = neg.mat * ifna(neg/rowSums(neg.mat), 1)
    return(pos.mat + neg.mat)
}

bt.apply.round.weight <- 
function (weight, long.round.weight = 5/100, short.round.weight = long.round.weight) 
{
    if (is.null(dim(weight))) 
        dim(weight) = c(1, len(weight))
    pos = apply(weight, 1, function(row) sum(row[row > 0]))
    neg = rowSums(weight) - pos
    pos.mat = iif(weight >= 0, round(weight/long.round.weight) * 
        long.round.weight, 0)
    neg.mat = iif(weight <= 0, round(weight/short.round.weight) * 
        short.round.weight, 0)
    pos.mat = pos.mat * ifna(pos/rowSums(pos.mat), 1)
    neg.mat = neg.mat * ifna(neg/rowSums(neg.mat), 1)
    return(pos.mat + neg.mat)
}

bt.change.periodicity <- 
function (b, periodicity = "months", period.ends = NULL, date.map.fn = NULL) 
{
    require(xts)
    b1 = env()
    for (n in ls(b)) if (is.xts(b[[n]])) {
        if (!is.null(periodicity)) 
            period.ends = endpoints(b[[n]], periodicity)
        temp = b[[n]][period.ends, ]
        if (!is.null(date.map.fn)) 
            index(temp) = date.map.fn(index(temp))
        colnames(temp) = colnames(b[[n]])
        b1[[n]] = temp
    }
    else b1[[n]] = b[[n]]
    if (!is.null(b$dates)) 
        b1$dates = index(b1$prices)
    b1
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

bt.trim <- 
function (..., dates = "::") 
{
    models = variable.number.arguments(...)
    for (i in 1:len(models)) {
        bt = models[[i]]
        n = len(bt$equity)
        first = which.max(!is.na(bt$equity) & bt$equity != 1)
        if (first > 1 && !is.na(bt$equity[(first - 1)])) 
            first = first - 1
        if (first < n) {
            index = first:n
            dates.range = range(dates2index(bt$equity[index], 
                dates))
            index = index[dates.range[1]]:index[dates.range[2]]
            bt$dates.index = bt$dates.index[index]
            bt$equity = bt$equity[index]
            bt$equity = bt$equity/as.double(bt$equity[1])
            bt$ret = bt$ret[index]
            bt$weight = bt$weight[index, , drop = F]
            if (!is.null(bt$share)) 
                bt$share = bt$share[index, , drop = F]
            bt$best = max(bt$ret)
            bt$worst = min(bt$ret)
            bt$cagr = compute.cagr(bt$equity)
        }
        models[[i]] = bt
    }
    return(models)
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

compute.turnover <- 
function (bt, b) 
{
    year.ends = unique(c(endpoints(bt$weight, "years"), nrow(bt$weight)))
    year.ends = year.ends[year.ends > 0]
    nr = len(year.ends)
    period.index = c(1, year.ends)
    if (bt$type == "weight") {
        portfolio.value = rowSums(abs(bt$weight), na.rm = T)
        portfolio.turnover = rowSums(abs(bt$weight - mlag(bt$weight)), 
            na.rm = T)
        portfolio.turnover[rowSums(!is.na(bt$weight) & !is.na(mlag(bt$weight))) == 
            0] = NA
    }
    else {
        prices = mlag(b$prices[bt$dates.index, , drop = F])
        cash = bt$capital - rowSums(bt$share * prices, na.rm = T)
        share.nextday = mlag(bt$share, -1)
        tstart = bt$share != share.nextday & share.nextday != 
            0
        index = mlag(apply(tstart, 1, any))
        index = ifna(index, FALSE)
        totalcash = NA * cash
        totalcash[index] = cash[index]
        totalcash = ifna.prev(totalcash)
        portfolio.value = totalcash + rowSums(bt$share * prices, 
            na.rm = T)
        portfolio.turnover = rowSums(prices * abs(bt$share - 
            mlag(bt$share)), na.rm = T)
        portfolio.turnover[rowSums(!is.na(bt$share) & !is.na(mlag(bt$share)) & 
            !is.na(prices)) == 0] = NA
    }
    portfolio.turnover[1:2] = 0
    temp = NA * period.index
    for (iyear in 2:len(period.index)) {
        temp[iyear] = sum(portfolio.turnover[period.index[(iyear - 
            1)]:period.index[iyear]], na.rm = T)/mean(portfolio.value[period.index[(iyear - 
            1)]:period.index[iyear]], na.rm = T)
    }
    return(ifna(mean(temp, na.rm = T), 0))
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

cov.sample <- 
function (h) 
{
    T = nrow(h)
    x = h - rep.row(colMeans(h), T)
    (t(x) %*% x)/T
}

cov.shrink <- 
function (h, prior = NULL, shrinkage = NULL, roff.method = 1) 
{
    require(tawny)
    T = nrow(h)
    S = cov.sample(h)
    if (is.function(prior)) 
        prior = prior(h)
    if (is.null(prior)) 
        prior = tawny::cov.prior.cc(S)
    if (is.null(shrinkage)) {
        p = tawny::shrinkage.p(h, S)
        if (roff.method == 0) 
            r = sum(p$diags, na.rm = TRUE)
        else r = tawny::shrinkage.r(h, S, p)
        c = tawny::shrinkage.c(prior, S)
        k = (p$sum - r)/c
        shrinkage = max(0, min(k/T, 1))
    }
    return(list(sigma = shrinkage * prior + (1 - shrinkage) * 
        S, shrinkage = shrinkage))
}

create.basic.constraints <- 
function (n, const.lb = 0, const.ub = 1, const.sum = 1) 
{
    if (len(const.lb) == 1) 
        const.lb = rep(const.lb, n)
    if (len(const.ub) == 1) 
        const.ub = rep(const.ub, n)
    constraints = new.constraints(n, lb = const.lb, ub = const.ub)
    constraints = add.constraints(diag(n), type = ">=", b = const.lb, 
        constraints)
    constraints = add.constraints(diag(n), type = "<=", b = const.ub, 
        constraints)
    if (!is.na(const.sum)) 
        constraints = add.constraints(rep(1, n), type = "=", 
            b = const.sum, constraints)
    return(constraints)
}

create.ia <- 
function (hist.returns, index = 1:ncol(hist.returns), nperiod = nrow(hist.returns)) 
{
    ia = list()
    ia$hist.returns = hist.returns
    ia$nperiod = nperiod
    ia$index = index
    ia$n = ncol(ia$hist.returns)
    ia$symbols = colnames(ia$hist.returns)
    ia$risk = apply(ia$hist.returns, 2, sd, na.rm = T)
    ia$correlation = cor(ia$hist.returns, use = "complete.obs", 
        method = "pearson")
    ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
    ia$expected.return = apply(ia$hist.returns, 2, mean, na.rm = T)
    return(ia)
}

create.ia.averaged <- 
function (lookbacks, n.lag) 
{
    lookbacks = lookbacks
    n.lag = n.lag
    function(hist.returns, index, nperiod) {
        temp = ia.build.hist(hist.returns, lookbacks, n.lag)
        create.ia(temp, index, nperiod)
    }
}

create.strategies <- 
function (obj, data, leverage = 1, min.weight = NA, round.weight = NA, 
    execution.price = NA, close.all.positions.index = NULL, silent = F, 
    log = log.fn(), prefix = "", suffix = "", clean.signal = F, 
    ...) 
{
    if (len(leverage) > 1 || leverage[1] != 1) {
        if (len(leverage) == 1) 
            leverage = rep(leverage, len(obj$weights))
        for (i in 1:len(obj$weights)) obj$weights[[i]] = leverage[i] * 
            obj$weights[[i]]
    }
    if (!is.na(min.weight) && min.weight != 0) 
        for (i in names(obj$weights)) obj$weights[[i]][] = bt.apply.min.weight(coredata(obj$weights[[i]]), 
            min.weight)
    if (!is.na(round.weight) && round.weight != 0) 
        for (i in names(obj$weights)) {
            obj$weights[[i]][] = bt.apply.round.weight(coredata(obj$weights[[i]]), 
                round.weight)
            obj$weights[[i]][] = bt.apply.round.weight(coredata(obj$weights[[i]]), 
                round.weight)
            obj$weights[[i]][] = bt.apply.round.weight(coredata(obj$weights[[i]]), 
                round.weight)
        }
    models = list()
    n = len(names(obj$weights))
    for (j in 1:n) {
        i = names(obj$weights)[j]
        i = paste(prefix, i, suffix, sep = "")
        if (!silent) 
            log(i, percent = j/n)
        data$weight[] = NA
        data$execution.price[] = execution.price
        data$weight[obj$period.ends, ] = obj$weights[[j]]
        if (!is.null(close.all.positions.index)) 
            data$weight[close.all.positions.index, ] = 0
        models[[i]] = bt.run.share(data, clean.signal = clean.signal, 
            silent = silent, ...)
        models[[i]]$period.weight = obj$weights[[j]]
    }
    obj$models = models
    return(obj)
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

get.risky.asset.index <- 
function (ia) 
{
    if (is.null(ia$risk)) 
        ia$risk = sqrt(diag(ia$cov))
    (ia$risk > 0) & !is.na(ia$risk)
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

ia.build.hist <- 
function (hist.returns, lookbacks, n.lag) 
{
    nperiods = nrow(hist.returns)
    temp = c()
    for (n.lookback in lookbacks) temp = rbind(temp, hist.returns[(nperiods - 
        n.lookback - n.lag + 1):(nperiods - n.lag), , drop = F])
    return(temp)
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

join <- 
function (v, delim = "") 
{
    return(paste(v, collapse = delim))
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

log.fn <- 
function (p.start = 0, p.end = 1) 
{
    p.start = p.start
    p.end = p.end
    function(..., percent = NULL) {
        cat(..., iif(is.null(percent), "", paste(", percent = ", 
            round(100 * (p.start + percent * (p.end - p.start)), 
                1), "%", sep = "")), "\n")
    }
}

lp.obj.portfolio <- 
function (ia, constraints, f.obj = c(ia$expected.return, rep(0, 
    nrow(constraints$A) - ia$n)), direction = "min") 
{
    x = NA
    binary.vec = 0
    if (!is.null(constraints$binary.index)) 
        binary.vec = constraints$binary.index
    sol = try(solve.LP.bounds(direction, f.obj, t(constraints$A), 
        c(rep("=", constraints$meq), rep(">=", len(constraints$b) - 
            constraints$meq)), constraints$b, lb = constraints$lb, 
        ub = constraints$ub, binary.vec = binary.vec), TRUE)
    if (!inherits(sol, "try-error")) {
        x = sol$solution
    }
    return(x)
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

max.return.portfolio <- 
function (ia, constraints) 
{
    lp.obj.portfolio(ia, constraints, direction = "max")
}

min.var.portfolio <- 
function (ia, constraints, cov.matrix = ia$cov, dvec = rep(0, 
    ia$n)) 
{
    risk.index = get.risky.asset.index(ia)
    Dmat = cov.matrix[risk.index, risk.index]
    sol = try(solve.QP(Dmat = Dmat, dvec = dvec[risk.index], 
        Amat = constraints$A[risk.index, , drop = F], bvec = constraints$b, 
        meq = constraints$meq), silent = TRUE)
    if (inherits(sol, "try-error")) 
        sol = try(solve.QP(Dmat = make.positive.definite(Dmat, 
            1e-09), dvec = dvec[risk.index], Amat = constraints$A[risk.index, 
            , drop = F], bvec = constraints$b, meq = constraints$meq), 
            silent = TRUE)
    if (inherits(sol, "try-error")) {
        gia <<- ia
        stop(sol)
    }
    set.risky.asset(sol$solution, risk.index)
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

new.constraints <- 
function (n, A = NULL, b = NULL, type = c("=", ">=", "<="), lb = NA, 
    ub = NA) 
{
    meq = 0
    if (is.null(A) || is.na(A) || is.null(b) || is.na(b)) {
        A = matrix(0, n, 0)
        b = c()
    }
    else {
        if (is.null(dim(A))) 
            dim(A) = c(len(A), 1)
        if (type[1] == "=") 
            meq = len(b)
        if (type[1] == "<=") {
            A = -A
            b = -b
        }
    }
    if (is.null(lb) || is.na(lb)) 
        lb = rep(NA, n)
    if (len(lb) != n) 
        lb = rep(lb[1], n)
    if (is.null(ub) || is.na(ub)) 
        ub = rep(NA, n)
    if (len(ub) != n) 
        ub = rep(ub[1], n)
    return(list(n = n, A = A, b = b, meq = meq, lb = lb, ub = ub))
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

plota.matplot <- 
function (y, dates = NULL, ylim = NULL, type = "l", ...) 
{
    if (is.list(y)) {
        if (!is.null(dates)) 
            y[[1]] = y[[1]][dates]
        if (is.null(ylim)) {
            ylim = c()
            n = len(y)
            for (i in 1:n) {
                if (!is.null(dates)) 
                  y[[i]] = y[[i]][dates]
                ylim = range(ylim, y[[i]], na.rm = T)
            }
        }
        plota(y[[1]], ylim = ylim, col = 1, type = type, ...)
        if (n > 1) {
            for (i in 2:n) plota.lines(y[[i]], col = i, type = type, 
                ...)
        }
        plota.legend(names(y), paste(1:n), y)
    }
    else {
        n = ncol(y)
        if (!is.null(dates)) 
            y = y[dates]
        if (is.null(ylim)) 
            ylim = range(y, na.rm = T)
        plota(y[, 1], ylim = ylim, col = 1, type = type, ...)
        if (n > 1) {
            for (i in 2:n) plota.lines(y[, i], col = i, type = type, 
                ...)
        }
        plota.legend(names(y), paste(1:n), as.list(y))
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

portfolio.allocation.helper <- 
function (prices, periodicity = "weeks", period.ends = endpoints(prices, 
    periodicity), lookback.len = 60, n.skip = 1, universe = prices[period.ends, 
    , drop = F] > 0, prefix = "", min.risk.fns = "min.var.portfolio", 
    custom.stats.fn = NULL, shrinkage.fns = "sample.shrinkage", 
    create.ia.fn = create.ia, update.ia.fn = update.ia, adjust2positive.definite = T, 
    silent = F, log = log.fn(), log.frequency = 10, const.lb = 0, 
    const.ub = 1, const.sum = 1) 
{
    load.packages("quadprog,corpcor")
    load.packages("quadprog,corpcor,lpSolve,kernlab")
    period.ends = period.ends[period.ends > 0]
    if (nrow(universe) != len(period.ends)) {
        if (nrow(universe) == nrow(prices)) 
            universe = universe[period.ends, , drop = F]
        else stop("universe incorrect number of rows")
    }
    universe[is.na(universe)] = F
    if (len(const.lb) == 1) 
        const.lb = rep(const.lb, ncol(prices))
    if (len(const.ub) == 1) 
        const.ub = rep(const.ub, ncol(prices))
    if (is.character(min.risk.fns)) {
        min.risk.fns = spl(min.risk.fns)
        names(min.risk.fns) = min.risk.fns
        min.risk.fns = as.list(min.risk.fns)
    }
    for (i in 1:len(min.risk.fns)) {
        f = spl(names(min.risk.fns)[i], "_")
        f.name = paste(prefix, gsub("\\.portfolio", "", f[1]), 
            sep = "")
        if (is.character(min.risk.fns[[i]])) {
            if (len(f) == 1) {
                min.risk.fns[[i]] = match.fun(f[1])
            }
            else {
                f.name = paste(f.name, f[-1], sep = "_")
                min.risk.fns[[i]] = match.fun(f[1])(f[-1])
            }
        }
        names(min.risk.fns)[i] = f.name
    }
    if (is.character(shrinkage.fns)) {
        shrinkage.fns = spl(shrinkage.fns)
        names(shrinkage.fns) = shrinkage.fns
        shrinkage.fns = as.list(shrinkage.fns)
    }
    for (i in 1:len(shrinkage.fns)) {
        f = names(shrinkage.fns)[i]
        f.name = gsub("\\.shrinkage", "", f[1])
        if (is.character(shrinkage.fns[[i]])) 
            shrinkage.fns[[i]] = match.fun(f)
        names(shrinkage.fns)[i] = f.name
    }
    dates = index(prices)[period.ends]
    weight = NA * prices[period.ends, , drop = F]
    prices = coredata(prices)
    ret = prices/mlag(prices) - 1
    start.i = which(period.ends >= (lookback.len + n.skip))[1]
    weight[] = 0
    weights = list()
    for (f in names(min.risk.fns)) for (c in names(shrinkage.fns)) weights[[paste(f, 
        c, sep = ".")]] = weight
    custom = list()
    if (!is.null(custom.stats.fn)) {
        custom.stats.fn = match.fun(custom.stats.fn)
        dummy = matrix(NA, nr = nrow(weight), nc = len(weights))
        colnames(dummy) = names(weights)
        dummy = make.xts(dummy, dates)
        temp = ret
        temp[] = rnorm(prod(dim(ret)))
        temp = custom.stats.fn(1:ncol(ret), create.ia(temp))
        for (ci in names(temp)) {
            temp1 = NA * dummy
            if (len(temp[[ci]]) > 1) {
                temp1 = list()
                for (w in names(weights)) temp1[[w]] = NA * weights[[w]]
            }
            custom[[ci]] = temp1
        }
    }
    index.map = 1:ncol(ret)
    for (j in start.i:len(period.ends)) {
        i = period.ends[j]
        hist = ret[(i - lookback.len + 1):i, , drop = F]
        include.index = count(hist) == lookback.len
        index = universe[j, ] & include.index
        n = sum(index)
        if (n > 0) {
            hist = hist[, index, drop = F]
            hist.all = ret[1:i, index, drop = F]
            if (n > 1) {
                constraints = create.basic.constraints(n, const.lb[index], 
                  const.ub[index], const.sum)
                ia.base = create.ia.fn(hist, index.map[index], 
                  i)
                for (c in names(shrinkage.fns)) {
                  cov.shrink = shrinkage.fns[[c]](hist, hist.all)
                  ia = update.ia.fn(ia.base, c, cov.shrink)
                  if (adjust2positive.definite) {
                    temp = try(make.positive.definite(ia$cov, 
                      1e-09), TRUE)
                    if (!inherits(temp, "try-error")) 
                      ia$cov = temp
                    temp = try(make.positive.definite(ia$correlation, 
                      1e-09), TRUE)
                    if (!inherits(temp, "try-error")) 
                      ia$correlation = temp
                  }
                  for (f in names(min.risk.fns)) {
                    fname = paste(f, c, sep = ".")
                    if (j > 1) 
                      constraints$x0 = as.vector(weights[[fname]][(j - 
                        1), index])
                    weights[[fname]][j, index] = min.risk.fns[[f]](ia, 
                      constraints)
                  }
                }
            }
            else {
                ia = create.ia.fn(hist, index.map[index], i)
                for (c in names(shrinkage.fns)) {
                  for (f in names(min.risk.fns)) {
                    fname = paste(f, c, sep = ".")
                    weights[[fname]][j, index] = 1
                  }
                }
            }
            if (!is.null(custom.stats.fn)) {
                for (w in names(weights)) {
                  x = as.vector(weights[[w]][j, index])
                  temp = custom.stats.fn(x, ia)
                  for (ci in names(temp)) {
                    if (is.list(custom[[ci]])) 
                      custom[[ci]][[w]][j, index] = temp[[ci]]
                    else custom[[ci]][j, w] = temp[[ci]]
                  }
                }
            }
        }
        if (j%%log.frequency == 0) 
            if (!silent) 
                log(j, percent = (j - start.i)/(len(period.ends) - 
                  start.i))
    }
    if (len(shrinkage.fns) == 1) {
        names(weights) = gsub(paste("\\.", names(shrinkage.fns), 
            "$", sep = ""), "", names(weights))
        for (ci in names(custom)) names(custom[[ci]]) = gsub(paste("\\.", 
            names(shrinkage.fns), "$", sep = ""), "", names(custom[[ci]]))
    }
    return(c(list(weights = weights, period.ends = period.ends, 
        periodicity = periodicity, lookback.len = lookback.len), 
        custom))
}

portfolio.return <- 
function (weight, ia) 
{
    if (is.null(dim(weight))) 
        dim(weight) = c(1, len(weight))
    weight = weight[, 1:ia$n, drop = F]
    portfolio.return = weight %*% ia$expected.return
    return(portfolio.return)
}

portfolio.risk <- 
function (weight, ia) 
{
    if (is.null(dim(weight))) 
        dim(weight) = c(1, len(weight))
    weight = weight[, 1:ia$n, drop = F]
    cov = ia$cov[1:ia$n, 1:ia$n]
    return(apply(weight, 1, function(x) sqrt(t(x) %*% cov %*% 
        x)))
}

portfolio.turnover <- 
function (weight) 
{
    if (is.null(dim(weight))) 
        dim(weight) = c(1, len(weight))
    out = weight[, 1] * NA
    out[] = rowSums(abs(weight - mlag(weight)))/2
    return(out)
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

rep.row <- 
function (m, nr) 
{
    if (nr == 1) 
        m
    else matrix(m, nr = nr, nc = len(m), byrow = T)
}

sample.shrinkage <- 
function (hist, hist.all) 
{
    cov(hist, use = "complete.obs", method = "pearson")
}

scale.one <- 
function (x, overlay = F, main.index = which(!is.na(x[1, ]))[1]) 
{
    index = 1:nrow(x)
    if (overlay) 
        x/rep.row(apply(x, 2, function(v) {
            i = index[!is.na(v)][1]
            v[i]/as.double(x[i, main.index])
        }), nrow(x))
    else x/rep.row(apply(x, 2, function(v) v[index[!is.na(v)][1]]), 
        nrow(x))
}

set.risky.asset <- 
function (x, risk.index) 
{
    out = rep(0, len(risk.index))
    out[risk.index] = x
    return(out)
}

solve.LP.bounds <- 
function (direction, objective.in, const.mat, const.dir, const.rhs, 
    binary.vec = 0, lb = 0, ub = +Inf, default.lb = -100) 
{
    n = len(objective.in)
    if (len(lb) == 1) 
        lb = rep(lb, n)
    if (len(ub) == 1) 
        ub = rep(ub, n)
    lb = ifna(lb, default.lb)
    ub = ifna(ub, +Inf)
    lb[lb < default.lb] = default.lb
    dvec = lb
    index = which(ub < +Inf)
    if (len(index) > 0) {
        const.rhs = c(const.rhs, ub[index])
        const.dir = c(const.dir, rep("<=", len(index)))
        const.mat = rbind(const.mat, diag(n)[index, ])
    }
    if (binary.vec[1] == 0) {
        sol = lp(direction, objective.in, const.mat, const.dir, 
            const.rhs - const.mat %*% dvec)
    }
    else {
        dvec[binary.vec] = 0
        sol = lp(direction, objective.in, const.mat, const.dir, 
            const.rhs - const.mat %*% dvec, binary.vec = binary.vec)
    }
    sol$solution = sol$solution + dvec
    sol$value = objective.in %*% sol$solution
    return(sol)
}

spl <- 
function (s, delim = ",") 
{
    return(unlist(strsplit(s, delim)))
}

target.return.portfolio.helper <- 
function (ia, constraints, target.return) 
{
    constraints.target = add.constraints(ia$expected.return, 
        type = ">=", b = target.return, constraints)
    sol = try(min.var.portfolio(ia, constraints.target), silent = TRUE)
    if (inherits(sol, "try-error")) 
        sol = max.return.portfolio(ia, constraints)
    sol
}

target.risk.portfolio <- 
function (target.risk, annual.factor = 252) 
{
    target.risk = as.double(target.risk[1])
    if (target.risk > 1) 
        target.risk = target.risk/100
    target.risk = target.risk/sqrt(annual.factor)
    function(ia, constraints) {
        target.risk.portfolio.helper(ia, constraints, target.risk)
    }
}

target.risk.portfolio.helper <- 
function (ia, constraints, target.risk, silent = T, min.w = NA, 
    max.w = NA) 
{
    if (is.na(max.w)) 
        max.w = max.return.portfolio(ia, constraints)
    if (is.na(min.w)) 
        min.w = min.var.portfolio(ia, constraints)
    max.r = portfolio.return(max.w, ia)
    min.r = portfolio.return(min.w, ia)
    max.s = portfolio.risk(max.w, ia)
    min.s = portfolio.risk(min.w, ia)
    if (target.risk >= min.s & target.risk <= max.s) {
        f <- function(x, ia, constraints, target.risk) {
            portfolio.risk(target.return.portfolio.helper(ia, 
                constraints, x), ia) - target.risk
        }
        f.lower = min.s - target.risk
        f.upper = max.s - target.risk
        sol = uniroot(f, c(min.r, max.r), f.lower = f.lower, 
            f.upper = f.upper, tol = 1e-04, ia = ia, constraints = constraints, 
            target.risk = target.risk)
        if (!silent) 
            cat("Found solution in", sol$iter, "itterations", 
                "\n")
        return(target.return.portfolio.helper(ia, constraints, 
            sol$root))
    }
    else if (target.risk < min.s) {
        return(min.w)
    }
    else {
        return(max.w)
    }
    stop(paste("target.risk =", target.risk, "is not possible, max risk =", 
        max.s, ", min risk =", min.s))
}

trim <- 
function (s) 
{
    s = sub(pattern = "^\\s+", replacement = "", x = s)
    s = sub(pattern = "\\s+$", replacement = "", x = s)
    return(s)
}

update.ia <- 
function (ia, name, cov.shrink) 
{
    if (name != "sample") {
        ia$cov = cov.shrink
        s0 = 1/sqrt(diag(ia$cov))
        ia$correlation = ia$cov * (s0 %*% t(s0))
    }
    ia
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


#*****************************************************************
# Appendix B. CLA code (in R) by Ilya Kipnis (QuantStratTradeR10) SSRN-id2606884.pdf 
#*****************************************************************

CCLA <- function(covMat, retForecast, maxIter = 1000,
                 verbose = FALSE, scale = 252,
                 weightLimit = .7, volThresh = .1) 
{
  if(length(retForecast) > length(unique(retForecast))) {
    sequentialNoise <- seq(1:length(retForecast)) * 1e-12
    retForecast <- retForecast + sequentialNoise
  }
  
  #initialize original out/in/up status
  if(length(weightLimit) == 1) {
    weightLimit <- rep(weightLimit, ncol(covMat))
  }
  
  # sort return forecasts
  rankForecast <- length(retForecast) - rank(retForecast) + 1
  remainingWeight <- 1 #have 100% of weight to allocate
  upStatus <- inStatus <- rep(0, ncol(covMat))
  i <- 1
  
  # find max return portfolio
  while(remainingWeight > 0) {
    securityLimit <- weightLimit[rankForecast == i]
    if(securityLimit < remainingWeight) {
      upStatus[rankForecast == i] <- 1 #if we can't invest all remaining weight into the security
      remainingWeight <- remainingWeight - securityLimit
    } else {
      inStatus[rankForecast == i] <- 1
      remainingWeight <- 0
    }
    i <- i + 1
  }
  
  #initial matrices (W, H, K, identity, negative identity)
  covMat <- as.matrix(covMat)
  retForecast <- as.numeric(retForecast)
  init_W <- cbind(2*covMat, rep(-1, ncol(covMat)))
  init_W <- rbind(init_W, c(rep(1, ncol(covMat)), 0))
  H_vec <- c(rep(0, ncol(covMat)), 1)
  K_vec <- c(retForecast, 0)
  negIdentity <- -1*diag(ncol(init_W))
  identity <- diag(ncol(init_W))
  matrixDim <- nrow(init_W)
  weightLimMat <- matrix(rep(weightLimit, matrixDim), ncol=ncol(covMat), byrow=TRUE)
  #out status is simply what isn't in or up
  outStatus <- 1 - inStatus - upStatus
  
  #initialize expected volatility/count/turning points data structure
  expVol <- Inf
  lambda <- 100
  count <- 0
  turningPoints <- list()
  
  while(lambda > 0 & count < maxIter) {
    #old lambda and old expected volatility for use with numerical algorithms
    oldLambda <- lambda
    oldVol <- expVol
    count <- count + 1
    
    #compute W, A, B
    inMat <- matrix(rep(c(inStatus, 1), matrixDim), nrow = matrixDim, byrow = TRUE)
    upMat <- matrix(rep(c(upStatus, 0), matrixDim), nrow = matrixDim, byrow = TRUE)
    outMat <- matrix(rep(c(outStatus, 0), matrixDim), nrow = matrixDim, byrow = TRUE)
    W <- inMat * init_W + upMat * identity + outMat * negIdentity
    
    inv_W <- solve(W)
    modified_H <- H_vec - rowSums(weightLimMat* upMat[,-matrixDim] * init_W[,-matrixDim])
    A_vec <- inv_W %*% modified_H
    B_vec <- inv_W %*% K_vec
    
    #remove the last elements from A and B vectors
    truncA <- A_vec[-length(A_vec)]
    truncB <- B_vec[-length(B_vec)]
    
    #compute in Ratio (aka Ratio(1) in Kwan.xls)  
    inRatio <- rep(0, ncol(covMat))
    inRatio[truncB > 0] <- -truncA[truncB > 0]/truncB[truncB > 0]
    
    #compute up Ratio (aka Ratio(2) in Kwan.xls)
    upRatio <- rep(0, ncol(covMat))
    upRatioIndices <- which(inStatus==TRUE & truncB < 0)
    
    if(length(upRatioIndices) > 0) {
      upRatio[upRatioIndices] <- (weightLimit[upRatioIndices] - truncA[upRatioIndices]) / truncB[upRatioIndices]
    }
    
    #find lambda -- max of up and in ratios
    maxInRatio <- max(inRatio)
    maxUpRatio <- max(upRatio)
    lambda <- max(maxInRatio, maxUpRatio)
    
    #compute new weights
    wts <- inStatus*(truncA + truncB * lambda) + upStatus * weightLimit + outStatus * 0
    
    #compute expected return and new expected volatility
    expRet <- t(retForecast) %*% wts
    expVol <- sqrt(wts %*% covMat %*% wts) * sqrt(scale)
    
    #create turning point data row and append it to turning points
    turningPoint <- cbind(count, expRet, lambda, expVol, t(wts))
    colnames(turningPoint) <- c("CP", "Exp. Ret.", "Lambda", "Exp. Vol.", colnames(covMat))
    turningPoints[[count]] <- turningPoint
    
    #binary search for volatility threshold -- if the first iteration is lower than the threshold,
    #then immediately return, otherwise perform the binary search until convergence of lambda
    if(oldVol == Inf & expVol < volThresh) {
      turningPoints <- do.call(rbind, turningPoints)
      threshWts <- tail(turningPoints, 1)
      return(list(turningPoints, threshWts))
    } else if(oldVol > volThresh & expVol < volThresh) {
      upLambda <- oldLambda
      dnLambda <- lambda
      meanLambda <- (upLambda + dnLambda)/2
      
      while(upLambda - dnLambda > .00001) {
        #compute mean lambda and recompute weights, expected return, and expected vol
        meanLambda <- (upLambda + dnLambda)/2
        wts <- inStatus*(truncA + truncB * meanLambda) + upStatus * weightLimit + outStatus * 0
        expRet <- t(retForecast) %*% wts
        expVol <- sqrt(wts %*% covMat %*% wts) * sqrt(scale)
        
        #if new expected vol is less than threshold, mean becomes lower bound
        #otherwise, it becomes the upper bound, and loop repeats
        if(expVol < volThresh) {
          dnLambda <- meanLambda
        } else {
          upLambda <- meanLambda
        }
      }
      
      #once the binary search completes, return those weights, and the corner points
      #computed until the binary search. The corner points aren't used anywhere, but they're there.
      threshWts <- cbind(count, expRet, meanLambda, expVol, t(wts))
      colnames(turningPoint) <- colnames(threshWts) <- c("CP", "Exp. Ret.", "Lambda", "Exp. Vol.", colnames(covMat))
      turningPoints[[count]] <- turningPoint
      turningPoints <- do.call(rbind, turningPoints)
      return(list(turningPoints, threshWts))
    }
    
    #this is only run for the corner points during which binary search doesn't take place
    #change status of security that has new lambda
    if(maxInRatio > maxUpRatio) {
      inStatus[inRatio == maxInRatio] <- 1 - inStatus[inRatio == maxInRatio]
      upStatus[inRatio == maxInRatio] <- 0
    } else {
      upStatus[upRatio == maxUpRatio] <- 1 - upStatus[upRatio == maxUpRatio]
      inStatus[upRatio == maxUpRatio] <- 0
    }
    outStatus <- 1 - inStatus - upStatus
  }
  
  
  #we only get here if the volatility threshold isn't reached
  #can actually happen if set sufficiently low
  turningPoints <- do.call(rbind, turningPoints)
  threshWts <- tail(turningPoints, 1)
  return(list(turningPoints, threshWts))
}


sumIsNa <- function(column) {
  return(sum(is.na(column)))
}

returnForecast <- function(prices) {
  forecast <- (ROC(prices, n = 1, type="discrete") + ROC(prices, n = 3, type="discrete") +
                 ROC(prices, n = 6, type="discrete") + ROC(prices, n = 12, type="discrete"))/22
  forecast <- as.numeric(tail(forecast, 1))
  return(forecast)
}


kellerCLAfun <- function(prices, returnWeights = FALSE,
                         weightLimit, volThresh, uncappedAssets) 
{
  if(sum(colnames(prices) %in% uncappedAssets) == 0) {
    stop("No assets are uncapped.")
  }
  
  #initialize data structure to contain our weights
  weights <- list()
  #compute returns
  returns <- Return.calculate(prices)
  returns[1,] <- 0 #impute first month with zeroes
  ep <- endpoints(returns, on = "months")
  
  for(i in 2:(length(ep) - 12)) {
    priceSubset <- prices[ep[i]:ep[i+12]] #subset prices
    retSubset <- returns[ep[i]:ep[i+12]] #subset returns
    assetNAs <- apply(retSubset, 2, sumIsNa)
    zeroNAs <- which(assetNAs == 0)
    priceSubset <- priceSubset[, zeroNAs]
    retSubset <- retSubset[, zeroNAs]
    
    #remove perfectly correlated assets
    retCors <- cor(retSubset)
    diag(retCors) <- NA
    corMax <- round(apply(retCors, 2, max, na.rm = TRUE), 7)
    while(max(corMax) == 1) {
      ones <- which(corMax == 1)
      valid <- which(!names(corMax) %in% uncappedAssets)
      toRemove <- intersect(ones, valid)
      toRemove <- max(valid)
      retSubset <- retSubset[, -toRemove]
      priceSubset <- priceSubset[, -toRemove]
      retCors <- cor(retSubset)
      diag(retCors) <- NA
      corMax <- round(apply(retCors, 2, max, na.rm = TRUE), 7)
    }
    
    covMat <- cov(retSubset) #compute covariance matrix
    
    #Dr. Keller's return forecast
    retForecast <- returnForecast(priceSubset)
    uncappedIndex <- which(colnames(covMat) %in% uncappedAssets)
    weightLims <- rep(weightLimit, ncol(covMat))
    weightLims[uncappedIndex] <- 1
    
    cla <- CCLA(covMat = covMat, retForecast = retForecast, scale = 12,
                weightLimit = weightLims, volThresh = volThresh) #run CCLA algorithm
    CPs <- cla[[1]] #corner points
    wts <- cla[[2]] #binary search volatility targeting -- change this line and the next
    
    #if using max sharpe ratio golden search
    wts <- wts[, 5:ncol(wts)] #from 5th column to the end
    if(length(wts) == 1) {
      names(wts) <- colnames(covMat)
    }
    
    zeroes <- rep(0, ncol(prices) - length(wts))
    names(zeroes) <- colnames(prices)[!colnames(prices) %in% names(wts)]
    wts <- c(wts, zeroes)
    wts <- wts[colnames(prices)]
    
    #append to weights
    wts <- xts(t(wts), order.by=tail(index(retSubset), 1))
    weights[[i]] <- wts
  }
  
  weights <- do.call(rbind, weights)
  #compute strategy returns
  stratRets <- Return.portfolio(returns, weights = weights)
  if(returnWeights) {
    return(list(weights, stratRets))
  }
  return(stratRets)
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
