
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

aa.arithmetic.geometric.test <- 
function () 
{
    ia = aa.test.create.ia.rebal()
    n = ia$n
    constraints = new.constraints(n, lb = 0, ub = 1)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ef.risk = portopt(ia, constraints, 50, "Arithmetic", equally.spaced.risk = T)
    ef.risk.geometric = ef.risk
    ef.risk.geometric$name = "Geometric"
    ef.risk.geometric$return = portfolio.geometric.return(ef.risk$weight, 
        ia)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plot.ef(ia, list(ef.risk, ef.risk.geometric), portfolio.risk, 
        T)
    dev.off()
    ef.risk.A10 = ef.risk
    ef.risk.A10$name = "A(1;0)"
    ef.risk.A10$return = apply(cbind(ef.risk$return, ef.risk$risk), 
        1, function(x) aritm2geom(x[1], x[2], 1, 0))
    ef.risk.A11 = ef.risk
    ef.risk.A11$name = "A(1;1)"
    ef.risk.A11$return = apply(cbind(ef.risk$return, ef.risk$risk), 
        1, function(x) aritm2geom(x[1], x[2], 1, 1))
    ia.G = ia
    ia.G$expected.return = apply(cbind(ia$geometric.return, ia$risk), 
        1, function(x) geom2aritm(x[1], x[2], 1, 0))
    ef.risk.G10 = portopt(ia.G, constraints, 50, "G(1;0)", equally.spaced.risk = T)
    ef.risk.G10$return = apply(cbind(ef.risk.G10$return, ef.risk.G10$risk), 
        1, function(x) aritm2geom(x[1], x[2], 1, 0))
    ia.G$expected.return = apply(cbind(ia$geometric.return, ia$risk), 
        1, function(x) geom2aritm(x[1], x[2], 1, 1))
    ef.risk.G11 = portopt(ia.G, constraints, 50, "G(1;1)", equally.spaced.risk = T)
    ef.risk.G11$return = apply(cbind(ef.risk.G11$return, ef.risk.G11$risk), 
        1, function(x) aritm2geom(x[1], x[2], 1, 1))
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.A10), 
        portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.A11), 
        portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.G10), 
        portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.G11), 
        portfolio.risk, F)
    dev.off()
    ef.risk.A4 = ef.risk
    ef.risk.A4$name = "Risk A4"
    ef.risk.A4$return = apply(cbind(ef.risk$return, ef.risk$risk), 
        1, function(x) aritm2geom4(x[1], x[2]))
    ia.G = ia
    ia.G$expected.return = apply(cbind(ia$geometric.return, ia$risk), 
        1, function(x) geom2aritm4(x[1], x[2]))
    ef.risk.G4 = portopt(ia.G, constraints, 50, "Risk G4", equally.spaced.risk = T)
    ef.risk.G4$return = apply(cbind(ef.risk.G4$return, ef.risk.G4$risk), 
        1, function(x) aritm2geom4(x[1], x[2]))
    png(filename = "plot3.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:2, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.A4), 
        portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.G4), 
        portfolio.risk, F)
    dev.off()
    ef.true.geometric = ef.risk
    ef.true.geometric$name = "True Geometric"
    constraints$x0 = ef.risk$weight[1, ]
    for (i in 1:len(ef.risk$risk)) {
        cat("i =", i, "\n")
        ef.true.geometric$weight[i, ] = max.geometric.return.portfolio(ia, 
            constraints, ef.risk$risk[i], ef.risk$risk[i])
        constraints$x0 = ef.true.geometric$weight[i, ]
    }
    ef.true.geometric$return = portfolio.geometric.return(ef.true.geometric$weight, 
        ia)
    png(filename = "plot4.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk.geometric, ef.risk, ef.true.geometric), 
        portfolio.risk, T, T)
    plot.ef(ia, list(ef.true.geometric, ef.risk, ef.risk.geometric), 
        portfolio.risk, T, T)
    dev.off()
    ef.random = list()
    ef.random$name = "Random"
    ef.random$weight = randfixedsum(100000, n, 1, 0, 1)
    ef.random$risk = portfolio.risk(ef.random$weight, ia)
    ef.random$return = portfolio.geometric.return(ef.random$weight, 
        ia)
    png(filename = "plot5.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(1)
    plot(100 * ef.random$risk, 100 * ef.random$return, type = "p", 
        pch = 20, xlim = 100 * range(ef.random$risk, ef.true.geometric$risk), 
        ylim = 100 * range(ef.random$return, ef.true.geometric$return), 
        main = "True Geometric Efficient Frontier vs Random Portfolios", 
        xlab = "portfolio.risk", ylab = "Return")
    lines(100 * ef.true.geometric$risk, 100 * ef.true.geometric$return, 
        type = "l", lwd = 2, col = "red")
    dev.off()
    return()
    ef.risk.unrebalanced = ef.risk
    ef.risk.unrebalanced$name = "Unrebalanced"
    ef.risk.unrebalanced$return = portfolio.unrebalanced.return(ef.risk$weight, 
        ia)
    plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.unrebalanced), 
        portfolio.risk, T)
    ia.G = ia
    ia.G$expected.return = ia$geometric.return
    ef.risk.geometric1 = portopt(ia.G, constraints, 50, "Geometric1", 
        equally.spaced.risk = T)
    plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.geometric1), 
        portfolio.risk, T)
    x = max.geometric.return.portfolio(ia, constraints, 0, 1)
    lines(portfolio.risk(t(x), ia), portfolio.geometric.return(t(x), 
        ia), type = "p", pch = 20, col = "blue")
}

aa.avg.cor.test <- 
function () 
{
    ia = aa.test.create.ia()
    n = ia$n
    constraints = new.constraints(n, lb = 0, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ef.risk = portopt(ia, constraints, 50, "Risk")
    ef.cor.insteadof.cov = portopt(ia, constraints, 50, "Cor instead of Cov", 
        min.cor.insteadof.cov.portfolio)
    ef.avgcor = portopt(ia, constraints, 50, "AvgCor", min.avgcor.portfolio)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(1:2)
    plot.ef(ia, list(ef.risk, ef.avgcor, ef.cor.insteadof.cov), 
        portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.avgcor, ef.cor.insteadof.cov), 
        portfolio.avgcor, F)
    dev.off()
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.transition.map(ef.risk)
    plot.transition.map(ef.avgcor)
    plot.transition.map(ef.cor.insteadof.cov)
    dev.off()
    png(filename = "plot3.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plot.ia(ia)
    dev.off()
    ef.random = list()
    ef.random$name = "Random"
    ef.random$weight = randfixedsum(1000000, n, 1, 0, 0.8)
    ef.random$risk = portfolio.avgcor(ef.random$weight, ia)
    ef.random$return = portfolio.return(ef.random$weight, ia)
    png(filename = "plot4.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(1)
    plot(100 * ef.random$risk, 100 * ef.random$return, type = "p", 
        pch = 20, xlim = 100 * range(0, ef.random$risk, ef.avgcor$risk), 
        ylim = 100 * range(0, ef.random$return, ef.avgcor$return), 
        main = "Average Correlation Efficient Frontier vs Random Portfolios", 
        xlab = "portfolio.avgcor", ylab = "Return")
    lines(100 * portfolio.avgcor(ef.avgcor$weight, ia), 100 * 
        ef.avgcor$return, type = "l", lwd = 2, col = "red")
    dev.off()
}

aa.black.litterman.test <- 
function () 
{
    hist.caps = aa.test.hist.capitalization()
    hist.caps.weight = hist.caps/rowSums(hist.caps)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plot.transition.map(hist.caps.weight, index(hist.caps.weight), 
        xlab = "", name = "Market Capitalization Weight History")
    dev.off()
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:9, nrow = 3, byrow = T))
    col = plota.colors(ncol(hist.caps))
    for (i in 1:ncol(hist.caps)) {
        plota(hist.caps[, i], type = "l", lwd = 5, col = col[i], 
            main = colnames(hist.caps)[i])
    }
    dev.off()
    ia = aa.test.create.ia.country()
    ir = get.fedfunds.rate()
    period = join(format(range(index(ia$hist.returns)), "%Y:%m"), 
        "::")
    risk.aversion = bl.compute.risk.aversion(ia$hist.returns$USA, 
        ir[period]/ia$annual.factor)
    risk.aversion = bl.compute.risk.aversion(ia$hist.returns$USA)
    cap.weight = last(hist.caps.weight)
    ia.bl = ia
    ia.bl$expected.return = bl.compute.eqret(risk.aversion, ia$cov, 
        cap.weight, last(ir[period]))
    png(filename = "plot3.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = T))
    pie(coredata(cap.weight), paste(colnames(cap.weight), round(100 * 
        cap.weight), "%"), main = paste("Country Market Capitalization Weights for", 
        format(last(index(ia$hist.returns)), "%b %Y")), col = plota.colors(ia$n))
    plot.ia(ia.bl, T)
    dev.off()
    n = ia$n
    constraints = new.constraints(n, lb = 0, ub = 1)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ef.risk = portopt(ia, constraints, 50, "Historical", equally.spaced.risk = T)
    ef.risk.bl = portopt(ia.bl, constraints, 50, "Black-Litterman", 
        equally.spaced.risk = T)
    png(filename = "plot4.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk), portfolio.risk, T, T)
    plot.ef(ia.bl, list(ef.risk.bl), portfolio.risk, T, T)
    dev.off()
    temp = matrix(rep(0, n), nrow = 1)
    colnames(temp) = ia$symbols
    temp[, "Japan"] = 1
    temp[, "UK"] = -1
    pmat = temp
    qmat = c(0.02)
    temp[] = 0
    temp[, "Australia"] = 1
    pmat = rbind(pmat, temp)
    qmat = c(qmat, 0.12)
    post = bl.compute.posterior(ia.bl$expected.return, ia$cov, 
        pmat, qmat, tau = 0.025)
    ia.bl.view = ia.bl
    ia.bl.view$expected.return = post$expected.return
    ia.bl.view$cov = post$cov
    ia.bl.view$risk = sqrt(diag(ia.bl.view$cov))
    ef.risk.bl.view = portopt(ia.bl.view, constraints, 50, "Black-Litterman + View(s)", 
        equally.spaced.risk = T)
    png(filename = "plot5.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia.bl, list(ef.risk.bl), portfolio.risk, T, T)
    plot.ef(ia.bl.view, list(ef.risk.bl.view), portfolio.risk, 
        T, T)
    dev.off()
}

aa.cardinality.test <- 
function () 
{
    ia = aa.test.create.ia()
    n = ia$n
    constraints = new.constraints(n, lb = 0, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ef.risk = portopt(ia, constraints, 50, "Risk")
    ef.mad = portopt(ia, constraints, 50, "MAD", min.mad.portfolio)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    aa.plot.ef.summary.test <- function(ef) {
        layout(1:2)
        par(mar = c(4, 4, 2, 1), cex = 0.8)
        y = iif(ef$weight > 0.000001, ef$weight, NA)
        plot(as.vector(sort(100 * y)), pch = 20, xaxt = "n", 
            ylim = c(0, 80), xlab = "", ylab = "Weight", main = "Portfolio Weights")
        abline(h = 0, col = "red")
        abline(h = 10, col = "red")
        plot(100 * ef$risk, rowSums(!is.na(y), na.rm = T), pch = 20, 
            type = "b", xlab = "Risk", ylab = "Number of Assets", 
            main = "Number of Assets")
    }
    aa.plot.ef.summary.test(ef.risk)
    dev.off()
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.mad)
    dev.off()
    constraints = new.constraints(n, rep(1, n), 1, type = "=")
    constraints = add.variables(n, constraints)
    constraints$binary.index = (n + 1):(2 * n)
    constraints = add.constraints(rbind(diag(n), -0.1 * diag(n)), 
        rep(0, n), type = ">=", constraints)
    constraints = add.constraints(rbind(diag(n), -0.8 * diag(n)), 
        rep(0, n), type = "<=", constraints)
    ef.risk = portopt(ia, constraints, 50, "Risk")
    ef.risk$weight = ef.risk$weight[, 1:n]
    ef.mad = portopt(ia, constraints, 50, "MAD", min.mad.portfolio)
    ef.mad$weight = ef.mad$weight[, 1:n]
    png(filename = "plot3.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    aa.plot.ef.summary.test(ef.risk)
    dev.off()
    png(filename = "plot4.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.mad)
    dev.off()
    constraints = new.constraints(n, rep(1, n), 1, type = "=")
    constraints = add.variables(n, constraints)
    constraints$binary.index = (n + 1):(2 * n)
    constraints = add.constraints(rbind(diag(n), -0.00001 * diag(n)), 
        rep(0, n), type = ">=", constraints)
    constraints = add.constraints(rbind(diag(n), -0.8 * diag(n)), 
        rep(0, n), type = "<=", constraints)
    constraints = add.constraints(c(rep(0, n), rep(1, n)), 3, 
        type = "=", constraints)
    ef.risk = portopt(ia, constraints, 50, "Risk")
    ef.risk$weight = ef.risk$weight[, 1:n]
    ef.mad = portopt(ia, constraints, 50, "MAD", min.mad.portfolio)
    ef.mad$weight = ef.mad$weight[, 1:n]
    png(filename = "plot5.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    aa.plot.ef.summary.test(ef.risk)
    dev.off()
    png(filename = "plot6.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.mad)
    dev.off()
}

aa.control.risk.return.test <- 
function () 
{
    tickers = spl("EEM,EFA,GLD,IWM,IYR,QQQ,SPY,TLT")
    data <- new.env()
    getSymbols(tickers, src = "yahoo", from = "1980-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    bt.prep(data, align = "keep.all", dates = "2012:12::")
    prices = data$prices
    n = ncol(prices)
    ret = na.omit(prices/mlag(prices) - 1)
    ia = create.historical.ia(ret, 252)
    constraints = new.constraints(n, lb = 0, ub = 1)
    constraints = add.constraints(diag(n), type = ">=", b = 0, 
        constraints)
    constraints = add.constraints(diag(n), type = "<=", b = 1, 
        constraints)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ef = portopt(ia, constraints, 50, "Efficient Frontier")
    risk.fn = portfolio.risk
    plot.ef(ia, list(ef), risk.fn, transition.map = F)
    weight = min.var.portfolio(ia, constraints)
    points(100 * risk.fn(weight, ia), 100 * portfolio.return(weight, 
        ia), pch = 15, col = "red")
    weight = max.sharpe.portfolio()(ia, constraints)
    points(100 * risk.fn(weight, ia), 100 * portfolio.return(weight, 
        ia), pch = 15, col = "orange")
    weight = max.return.portfolio(ia, constraints)
    points(100 * risk.fn(weight, ia), 100 * portfolio.return(weight, 
        ia), pch = 15, col = "green")
    weight = risk.parity.portfolio()(ia, constraints)
    points(100 * risk.fn(weight, ia), 100 * portfolio.return(weight, 
        ia), pch = 15, col = "green")
    target.return = 24/100
    constraints1 = add.constraints(ia$expected.return, type = ">=", 
        b = target.return, constraints)
    weight = min.var.portfolio(ia, constraints1)
    points(100 * risk.fn(weight, ia), 100 * portfolio.return(weight, 
        ia), pch = 15, col = "orange")
    target.risk = 12/100
    target.mad = approx(portfolio.risk(ef$weight, ia), portfolio.mad(ef$weight, 
        ia), target.risk, method = "linear")$y
    constraints1 = add.constraint.mad(ia, type = "<=", value = target.mad, 
        constraints)
    weight = max.return.portfolio(ia, constraints1)
    points(100 * risk.fn(weight, ia), 100 * portfolio.return(weight, 
        ia), pch = 15, col = "orange")
    target.return = 24/100
    target.risk = 12/100
    target.mad = approx(portfolio.risk(ef$weight, ia), portfolio.mad(ef$weight, 
        ia), target.risk, method = "linear")$y
    target.mad = target.mad
    constraints1 = add.constraints(ia$expected.return, type = ">=", 
        b = target.return, constraints)
    constraints1 = add.constraint.mad(ia, type = ">=", value = target.mad, 
        constraints1)
    f.obj.return = c(ia$expected.return, rep(0, nrow(constraints1$A) - 
        ia$n))
    f.obj.mad = constraints1$A[, ncol(constraints1$A)]
    weight = lp.obj.portfolio(ia, constraints1, f.obj.return + 
        f.obj.mad)
    points(100 * risk.fn(weight, ia), 100 * portfolio.return(weight, 
        ia), pch = 15, col = "orange")
    100 * portfolio.mad(weight, ia)
    100 * target.mad
    100 * portfolio.risk(weight, ia)
    100 * portfolio.return(weight, ia)
}

aa.cvar.test <- 
function () 
{
    ia = aa.test.create.ia()
    n = ia$n
    constraints = new.constraints(n, lb = 0, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ia$parameters.alpha = 0.95
    ef.risk = portopt(ia, constraints, 50, "Risk")
    ef.maxloss = portopt(ia, constraints, 50, "MaxLoss", min.maxloss.portfolio)
    ef.mad = portopt(ia, constraints, 50, "MAD", min.mad.portfolio)
    ef.cvar = portopt(ia, constraints, 50, "CVaR", min.cvar.portfolio)
    ef.cdar = portopt(ia, constraints, 50, "CDaR", min.cdar.portfolio)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio.risk, 
        F)
    plot.ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio.cvar, 
        F)
    plot.ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio.cdar, 
        F)
    dev.off()
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.transition.map(ef.risk)
    plot.transition.map(ef.cvar)
    plot.transition.map(ef.cdar)
    dev.off()
    return()
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), 
        portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), 
        portfolio.maxloss, F)
    plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), 
        portfolio.cvar, F)
    plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), 
        portfolio.cdar, F)
    layout(matrix(1:4, nrow = 2))
    plot.transition.map(ef.maxloss)
    plot.transition.map(ef.mad)
    plot.transition.map(ef.cvar)
    plot.transition.map(ef.cdar)
}

aa.downside.test <- 
function () 
{
    ia = aa.test.create.ia()
    n = ia$n
    constraints = new.constraints(n, lb = 0, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ia$parameters.mar = 0/100
    ia$parameters.mar = ia$parameters.mar/12
    ef.mad = portopt(ia, constraints, 50, "MAD", min.mad.portfolio)
    ef.mad.downside = portopt(ia, constraints, 50, "S-MAD", min.mad.downside.portfolio)
    ef.risk = portopt(ia, constraints, 50, "Risk")
    ef.risk.downside = portopt(ia, constraints, 50, "S-Risk", 
        min.risk.downside.portfolio)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.mad.downside, ef.mad), portfolio.mad, 
        F)
    plot.ef(ia, list(ef.mad.downside, ef.mad), portfolio.mad.downside, 
        F)
    plot.transition.map(ef.mad)
    plot.transition.map(ef.mad.downside)
    dev.off()
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk.downside, ef.risk), portfolio.risk, 
        F)
    plot.ef(ia, list(ef.risk.downside, ef.risk), portfolio.risk.downside, 
        F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.risk.downside)
    dev.off()
}

aa.erc.test <- 
function () 
{
    ia = aa.test.create.ia()
    n = ia$n
    constraints = new.constraints(n, lb = 0, ub = 1)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ef.risk = portopt(ia, constraints, 50, "Risk")
    layout(1:3)
    plot.ef(ia, list(ef.risk), portfolio.risk, F)
    plot.transition.map(ef.risk)
    plot.transition.map(portfolio.risk.contribution(ef.risk$weight, 
        ia), ef.risk$risk, name = "Risk Contribution")
    x = rep(1/ia$n, ia$n)
    round(100 * portfolio.risk.contribution(x, ia), 1)
    x = find.erc.portfolio(ia, constraints)
    round(100 * portfolio.risk.contribution(x, ia), 1)
    s = (c(1, 2, 3, 4)/10)
    cor = 0.5 + 0 * diag(4)
    diag(cor) = 1
    cov = cor * (s %*% t(s))
    weight = rep(1/4, 4)
    weight = c(100, 0, 0, 0)/100
    weight = c(48, 24, 16, 12)/100
    ia$n = 4
    ia$cov = cov
    round(100 * portfolio.risk(weight, ia), 1)
    round(100 * portfolio.risk.contribution(weight, ia), 1)
    s = c(12, 10, 11, 13, 12)/100
    cor = 0.6 + 0 * diag(5)
    diag(cor) = 1
    cov = cor * (s %*% t(s))
    weight = c(23.96, 6.43, 16.92, 28.73, 23.96)/100
    weight = c(19.2, 23, 20.8, 17.7, 19.2)/100
    ia$n = 5
    ia$cov = cov
    round(100 * portfolio.risk(weight, ia), 1)
    round(100 * portfolio.risk.contribution(weight, ia), 1)
}

aa.gini.test <- 
function () 
{
    ia = aa.test.create.ia.rebal()
    ia$risk = apply(coredata(ia$hist.returns), 2, sd)
    ia$correlation = cor(coredata(ia$hist.returns), use = "complete.obs", 
        method = "pearson")
    ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
    n = ia$n
    constraints = new.constraints(n, lb = 0, ub = 1)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ef.risk = portopt(ia, constraints, 50, "Risk")
    ef.gini = portopt(ia, constraints, 50, "GINI", min.gini.portfolio)
    png(filename = "plot1g.png", width = 600, height = 600, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.gini), portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.gini), portfolio.gini.coefficient, 
        F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.gini)
    dev.off()
    ia = list()
    ia$n = 3
    ia$hist.returns = matrix(0, 3, 3)
    ia$hist.returns[1, ] = c(10, 9, 6)/100
    ia$hist.returns[2, ] = c(15, 8, 12)/100
    ia$hist.returns[3, ] = c(12, 7, 15)/100
}

aa.long.short.test <- 
function () 
{
    ia = aa.test.create.ia()
    n = ia$n
    constraints = new.constraints(n, lb = -0.5, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ef.risk = portopt(ia, constraints, 50, "Risk")
    ef.mad = portopt(ia, constraints, 50, "MAD", min.mad.portfolio)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.mad)
    dev.off()
    constraints = new.constraints(n, lb = -0.5, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    constraints = add.variables(n, constraints)
    constraints = add.constraints(rbind(diag(n), diag(n)), rep(0, 
        n), type = ">=", constraints)
    constraints = add.constraints(rbind(diag(n), -diag(n)), rep(0, 
        n), type = "<=", constraints)
    constraints = add.constraints(c(rep(0, n), rep(1, n)), 1.6, 
        type = "=", constraints)
    ef.risk = portopt(ia, constraints, 50, "Risk")
    ef.risk$weight = ef.risk$weight[, (1:n)]
    ef.mad = portopt(ia, constraints, 50, "MAD", min.mad.portfolio)
    ef.mad$weight = ef.mad$weight[, (1:n)]
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.mad)
    dev.off()
    ia.ls = aa.test.ia.add.short(ia)
    constraints = new.constraints(2 * n, lb = 0, ub = c(rep(0.8, 
        n), rep(0.5, n)))
    constraints = add.constraints(c(rep(1, n), -rep(1, n)), 1, 
        type = "=", constraints)
    constraints = add.constraints(c(rep(1, n), rep(1, n)), 1.6, 
        type = "=", constraints)
    ef.risk = portopt(ia.ls, constraints, 50, "Risk")
    ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, 
        (n + 1):(2 * n)]
    ef.mad = portopt(ia.ls, constraints, 50, "MAD", min.mad.portfolio)
    ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n + 
        1):(2 * n)]
    png(filename = "plot3.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.mad)
    dev.off()
    constraints = new.constraints(2 * n, lb = 0, ub = c(rep(0.8, 
        n), rep(0.5, n)))
    constraints = add.constraints(c(rep(1, n), -rep(1, n)), 1, 
        type = "=", constraints)
    constraints = add.constraints(c(rep(1, n), rep(1, n)), 3, 
        type = "=", constraints)
    ef.risk = portopt(ia.ls, constraints, 50, "Risk")
    ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, 
        (n + 1):(2 * n)]
    ef.mad = portopt(ia.ls, constraints, 50, "MAD", min.mad.portfolio)
    ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n + 
        1):(2 * n)]
    png(filename = "plot4.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.mad)
    dev.off()
    constraints = new.constraints(2 * n, lb = 0, ub = c(rep(0.8, 
        n), rep(0.5, n)))
    constraints = add.constraints(c(rep(1, n), -rep(1, n)), 1, 
        type = "=", constraints)
    constraints = add.constraints(c(rep(1, n), rep(1, n)), 3, 
        type = "=", constraints)
    constraints = add.variables(n, constraints)
    constraints$binary.index = (2 * n + 1):(3 * n)
    constraints = add.constraints(rbind(diag(n), 0 * diag(n), 
        -diag(n)), rep(0, n), type = "<=", constraints)
    constraints = add.constraints(rbind(0 * diag(n), diag(n), 
        diag(n)), rep(1, n), type = "<=", constraints)
    ef.risk = portopt(ia.ls, constraints, 50, "Risk")
    ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, 
        (n + 1):(2 * n)]
    ef.mad = portopt(ia.ls, constraints, 50, "MAD", min.mad.portfolio)
    ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n + 
        1):(2 * n)]
    png(filename = "plot5.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.mad)
    dev.off()
}

aa.multiple.risk.measures.test <- 
function () 
{
    ia = aa.test.create.ia()
    n = ia$n
    constraints = new.constraints(n, lb = 0, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ef.risk = portopt(ia, constraints, 50, "Risk")
    ef.maxloss = portopt(ia, constraints, 50, "MaxLoss", min.maxloss.portfolio)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.maxloss), portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.maxloss), portfolio.maxloss, 
        F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.maxloss)
    dev.off()
    constraints = add.constraint.maxloss(ia, 12/100, "<=", constraints)
    ef.risk.maxloss = portopt(ia, constraints, 50, "Risk+MaxLoss")
    ef.risk.maxloss$weight = ef.risk.maxloss$weight[, 1:n]
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk.maxloss, ef.risk, ef.maxloss), portfolio.risk, 
        F)
    plot.ef(ia, list(ef.risk.maxloss, ef.risk, ef.maxloss), portfolio.maxloss, 
        F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.risk.maxloss)
    dev.off()
    return()
    constraints = new.constraints(n, lb = 0, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ia$parameters.alpha = 0.95
    ef.risk = portopt(ia, constraints, 50, "Risk")
    ef.maxloss = portopt(ia, constraints, 50, "MaxLoss", min.maxloss.portfolio)
    ef.mad = portopt(ia, constraints, 50, "MAD", min.mad.portfolio)
    ef.cvar = portopt(ia, constraints, 50, "CVaR", min.cvar.portfolio)
    ef.cdar = portopt(ia, constraints, 50, "CDaR", min.cdar.portfolio)
    layout(1)
    plot.ef(ia, list(ef.risk, ef.maxloss), portfolio.maxloss, 
        F)
    constraints = new.constraints(n, lb = 0, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    constraints = add.constraint.maxloss(ia, 15/100, "<=", constraints)
    ef.risk.new = portopt(ia, constraints, 50, "Risk+")
    ef.risk.new$weight = ef.risk.new$weight[, 1:n]
    layout(1:2)
    plot.ef(ia, list(ef.risk), portfolio.maxloss, F)
    plot.ef(ia, list(ef.risk.new), portfolio.maxloss, F)
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk.new, ef.risk, ef.maxloss), portfolio.maxloss, 
        F)
    plot.ef(ia, list(ef.risk.new, ef.risk, ef.maxloss), portfolio.risk, 
        F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.risk.new)
    layout(1)
    plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
    constraints = new.constraints(n, lb = 0, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    constraints = add.constraint.mad(ia, 2.9/100, "<=", constraints)
    ef.risk.new = portopt(ia, constraints, 50, "Risk+")
    ef.risk.new$weight = ef.risk.new$weight[, 1:n]
    layout(1:2)
    plot.ef(ia, list(ef.risk), portfolio.mad, F)
    plot.ef(ia, list(ef.risk.new), portfolio.mad, F)
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk.new, ef.risk, ef.mad), portfolio.mad, 
        F)
    plot.ef(ia, list(ef.risk.new, ef.risk, ef.mad), portfolio.risk, 
        F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.risk.new)
    layout(1)
    plot.ef(ia, list(ef.risk, ef.cvar), portfolio.cvar, F)
    constraints = new.constraints(n, lb = 0, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    constraints = add.constraint.cvar(ia, 8/100, "<=", constraints)
    ef.risk.new = portopt(ia, constraints, 50, "Risk+")
    ef.risk.new$weight = ef.risk.new$weight[, 1:n]
    layout(1:2)
    plot.ef(ia, list(ef.risk), portfolio.cvar, F)
    plot.ef(ia, list(ef.risk.new), portfolio.cvar, F)
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk.new, ef.risk, ef.cvar), portfolio.cvar, 
        F)
    plot.ef(ia, list(ef.risk.new, ef.risk, ef.cvar), portfolio.risk, 
        F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.risk.new)
    layout(1)
    plot.ef(ia, list(ef.risk, ef.cdar), portfolio.cdar, F)
    constraints = new.constraints(n, lb = 0, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    constraints = add.constraint.cdar(ia, 15/100, "<=", constraints)
    ef.risk.new = portopt(ia, constraints, 50, "Risk+")
    ef.risk.new$weight = ef.risk.new$weight[, 1:n]
    layout(1:2)
    plot.ef(ia, list(ef.risk), portfolio.cdar, F)
    plot.ef(ia, list(ef.risk.new), portfolio.cdar, F)
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk.new, ef.risk, ef.cdar), portfolio.cdar, 
        F)
    plot.ef(ia, list(ef.risk.new, ef.risk, ef.cdar), portfolio.risk, 
        F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.risk.new)
    layout(1:2)
    plot.ef(ia, list(ef.risk, ef.maxloss), portfolio.maxloss, 
        F)
    plot.ef(ia, list(ef.risk, ef.cdar), portfolio.cdar, F)
    constraints = new.constraints(n, lb = 0, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    constraints = add.constraint.maxloss(ia, 15/100, "<=", constraints)
    constraints = add.constraint.cdar(ia, 15/100, "<=", constraints)
    ef.risk.new = portopt(ia, constraints, 50, "Risk+")
    ef.risk.new$weight = ef.risk.new$weight[, 1:n]
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk), portfolio.maxloss, F)
    plot.ef(ia, list(ef.risk.new), portfolio.maxloss, F)
    plot.ef(ia, list(ef.risk), portfolio.cdar, F)
    plot.ef(ia, list(ef.risk.new), portfolio.cdar, F)
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk.new, ef.risk, ef.maxloss, ef.cdar), 
        portfolio.maxloss, F)
    plot.ef(ia, list(ef.risk.new, ef.risk, ef.maxloss, ef.cdar), 
        portfolio.cdar, F)
    plot.ef(ia, list(ef.risk.new, ef.risk, ef.maxloss, ef.cdar), 
        portfolio.risk, F)
    layout(matrix(1:4, nrow = 2))
    plot.transition.map(ef.risk)
    plot.transition.map(ef.risk.new)
    plot.transition.map(ef.maxloss)
    plot.transition.map(ef.cdar)
}

aa.omega.test <- 
function () 
{
    ia = aa.test.create.ia()
    n = ia$n
    constraints = new.constraints(n, lb = 0, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ia$parameters.omega = 13/100
    ia$parameters.omega = 12/100
    ia$parameters.omega = ia$parameters.omega/12
    ef.risk = portopt(ia, constraints, 50, "Risk")
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2, byrow = T))
    rownames(ef.risk$weight) = paste("Risk", "weight", 1:50, 
        sep = "_")
    plot.omega(ef.risk$weight[c(1, 10, 40, 50), ], ia)
    temp = diag(n)
    rownames(temp) = ia$symbols
    plot.omega(temp, ia)
    plot.ef(ia, list(ef.risk), portfolio.omega, T, T)
    dev.off()
    ef.omega = portopt.omega(ia, constraints, 50, "Omega")
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2, byrow = T))
    plot.omega(ef.risk$weight[c(1, 10, 40, 50), ], ia)
    rownames(ef.omega$weight) = paste("Omega", "weight", 1:50, 
        sep = "_")
    plot.omega(ef.omega$weight[c(1, 10, 40, 50), ], ia)
    plot.ef(ia, list(ef.omega, ef.risk), portfolio.omega, T, 
        T)
    dev.off()
    png(filename = "plot3.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.omega), portfolio.risk, F)
    plot.ef(ia, list(ef.risk, ef.omega), portfolio.omega, F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.omega)
    dev.off()
}

aa.solutions2instability.test <- 
function () 
{
    ia = aa.test.create.ia.rebal()
    n = ia$n
    constraints = new.constraints(n, lb = 0, ub = 1)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ef.risk = portopt(ia, constraints, 50, "Risk", equally.spaced.risk = T)
    ef.risk.resampled = portopt.resampled(ia, constraints, 50, 
        "Risk Resampled", nsamples = 200, sample.len = 10)
    png(filename = "plot1.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = T))
    plot.ef(ia, list(ef.risk, ef.risk.resampled), portfolio.risk, 
        F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.risk.resampled)
    dev.off()
    load.packages("tawny")
    ia.original = ia
    ia$cov = tawny::cov.shrink(ia$hist.returns)
    ef.risk.cov.shrink = portopt(ia, constraints, 50, "Risk Ledoit-Wolf", 
        equally.spaced.risk = T)
    ia = ia.original
    png(filename = "plot2.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = T))
    plot.ef(ia, list(ef.risk, ef.risk.cov.shrink), portfolio.risk, 
        F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.risk.cov.shrink)
    dev.off()
    ef.risk.resampled.shrink = portopt.resampled(ia, constraints, 
        50, "Risk Ledoit-Wolf+Resampled", nsamples = 200, sample.len = 10, 
        shrinkage.fn = tawny::cov.shrink)
    png(filename = "plot3.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(c(1:4), nrow = 2, byrow = T))
    plot.ef(ia, list(ef.risk, ef.risk.resampled, ef.risk.resampled.shrink), 
        portfolio.risk, F)
    plot.transition.map(ef.risk)
    plot.transition.map(ef.risk.resampled)
    plot.transition.map(ef.risk.resampled.shrink)
    dev.off()
}

aa.test <- 
function () 
{
    ia = aa.test.create.ia()
    png(filename = "plot1.png", width = 500, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plot.ia(ia)
    dev.off()
    png(filename = "plot2.png", width = 500, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(1)
    par(mar = c(4, 4, 2, 1), cex = 0.8)
    x = 100 * ia$risk
    y = 100 * ia$expected.return
    plot(x, y, xlim = range(c(0, x)), ylim = range(c(0, y)), 
        xlab = "Risk", ylab = "Return", main = "Risk vs Return", 
        col = "black")
    grid()
    text(x, y, ia$symbols, col = "blue", adj = c(1, 1), cex = 0.8)
    dev.off()
    n = ia$n
    constraints = new.constraints(n, lb = 0, ub = 0.8)
    constraints = add.constraints(rep(1, n), 1, type = "=", constraints)
    ef = portopt(ia, constraints, 50, "Efficient Frontier")
    png(filename = "plot3.png", width = 500, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plot.ef(ia, list(ef))
    dev.off()
    ef.risk = portopt(ia, constraints, 50, "Risk")
    ef.maxloss = portopt(ia, constraints, 50, "Max Loss", min.maxloss.portfolio)
    ef.mad = portopt(ia, constraints, 50, "MAD", min.mad.portfolio)
    png(filename = "plot4.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio.risk, 
        F)
    plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio.maxloss, 
        F)
    plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio.mad, 
        F)
    dev.off()
    png(filename = "plot5.png", width = 600, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    layout(matrix(1:4, nrow = 2))
    plot.transition.map(ef.risk)
    plot.transition.map(ef.maxloss)
    plot.transition.map(ef.mad)
    dev.off()
}

aa.test.create.ia.custom <- 
function (symbols, symbol.names = symbols, dates = NULL) 
{
    load.packages("quantmod,quadprog")
    data <- new.env()
    getSymbols(symbols, src = "yahoo", from = "1970-01-01", env = data, 
        auto.assign = T)
    for (i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted = T)
    bt.prep(data, align = "remove.na", dates = dates)
    hist.prices = data$prices
    period.ends = endpoints(hist.prices, "months")
    hist.prices = hist.prices[period.ends, ]
    colnames(hist.prices) = symbol.names
    annual.factor = 12
    hist.returns = na.omit(ROC(hist.prices, type = "discrete"))
    ia = create.historical.ia(hist.returns, annual.factor, symbol.names, 
        symbol.names)
    return(ia)
}

aa.test.ia.add.short <- 
function (ia) 
{
    ia$symbols = c(ia$symbols, ia$symbols)
    ia$n = 2 * ia$n
    ia$hist.returns = cbind(ia$hist.returns, -ia$hist.returns)
    ia$expected.return = c(ia$expected.return, -ia$expected.return)
    ia$risk = c(ia$risk, ia$risk)
    ia$correlation = cbind(rbind(ia$correlation, -ia$correlation), 
        rbind(-ia$correlation, ia$correlation))
    ia$cov = cbind(rbind(ia$cov, -ia$cov), rbind(-ia$cov, ia$cov))
    return(ia)
}

add.constraint.omega <- 
function (ia, value, type = c("=", ">=", "<="), constraints) 
{
    if (is.null(ia$parameters.omega)) 
        omega = 0
    else omega = ia$parameters.omega
    n0 = ncol(ia$hist.returns)
    n = nrow(constraints$A)
    nt = nrow(ia$hist.returns)
    constraints = add.variables(2 * nt + 1, constraints, lb = c(rep(0, 
        2 * nt), -Inf))
    constraints$A[n + 2 * nt + 1, ] = -constraints$b
    constraints$b[] = 0
    index = which(constraints$ub[1:n] < +Inf)
    if (len(index) > 0) {
        a = rbind(diag(n), matrix(0, 2 * nt, n), -constraints$ub[1:n])
        constraints = add.constraints(a[, index], rep(0, len(index)), 
            "<=", constraints)
    }
    index = which(constraints$lb[1:n] > -Inf)
    if (len(index) > 0) {
        a = rbind(diag(n), matrix(0, 2 * nt, n), -constraints$lb[1:n])
        constraints = add.constraints(a[, index], rep(0, len(index)), 
            ">=", constraints)
    }
    constraints$lb[1:n] = -Inf
    constraints$ub[1:n] = Inf
    a = rbind(matrix(0, n, nt), -diag(nt), diag(nt), -omega)
    a[1:n0, ] = t(ia$hist.returns)
    constraints = add.constraints(a, rep(0, nt), "=", constraints)
    constraints = add.constraints(c(rep(0, n), rep(0, nt), (1/nt) * 
        rep(1, nt), 0), 1, "=", constraints)
    constraints = add.constraints(c(rep(0, n), (1/nt) * rep(1, 
        nt), rep(0, nt), 0), value, type[1], constraints)
    return(constraints)
}

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

add.variables <- 
function (n, constraints, lb = NA, ub = NA) 
{
    constraints$A = rbind(constraints$A, matrix(0, n, len(constraints$b)))
    if (is.null(lb) || is.na(lb)) 
        lb = rep(NA, n)
    if (len(lb) != n) 
        lb = rep(lb[1], n)
    if (is.null(ub) || is.na(ub)) 
        ub = rep(NA, n)
    if (len(ub) != n) 
        ub = rep(ub[1], n)
    constraints$lb = c(constraints$lb, lb)
    constraints$ub = c(constraints$ub, ub)
    constraints$n = constraints$n + n
    return(constraints)
}

bbb_control <- 
function (itermax = 200, depthmax = Inf, bineps = 0.0001, precisioneps = 0, 
    silent = T, branchvar = c("first", "max", "min"), proborder = c("0", 
        "1", "mindiff"), searchdir = c("depth", "breadth", "best", 
        "normbest")) 
{
    branchvar = switch(branchvar[1], first = 0, max = 1, min = 2, 
        0)
    branchvar = iif(is.null(branchvar), 0, branchvar)
    proborder = switch(proborder[1], `0` = 0, `1` = 1, mindiff = 2, 
        0)
    proborder = iif(is.null(proborder), 0, proborder)
    searchdir = switch(searchdir[1], depth = 0, breadth = 1, 
        best = 2, normbest = 2, 0)
    searchdir = iif(is.null(searchdir), 0, searchdir)
    control = list(itermax = itermax, depthmax = depthmax, bineps = bineps, 
        precisioneps = precisioneps, silent = silent, branchvar = branchvar, 
        proborder = proborder, searchdir = searchdir)
    return(control)
}

bbb_decision <- 
function (xi, control) 
{
    if (control$branchvar == 0) {
        branchvar = 1
    }
    else if (control$branchvar == 1) {
        branchvar = which.max(abs(xi - round(xi, 0)))
    }
    else if (control$branchvar == 2) {
        branchvar = which.min(abs(xi - round(xi, 0)))
    }
    else {
        branchvar = 1
    }
    return(branchvar)
}

bbb_pop <- 
function (stack) 
{
    i = stack$pointer[length(stack$data)]
    subprob = stack$data[[i]]
    stack$pointer[stack$pointer > i] = stack$pointer[stack$pointer > 
        i] - 1
    stack$data[[i]] = NULL
    length(stack$cost) = length(stack$data)
    length(stack$pointer) = length(stack$data)
    return(subprob)
}

bbb_push <- 
function (stack, element1, element2, cost) 
{
    n = length(stack$data)
    i = match(TRUE, stack$cost <= cost)
    if (is.na(i)) 
        i = n
    else i = i - 1
    stack$data[[(n + 1)]] = element1
    stack$data[[(n + 2)]] = element2
    if (i == 0) {
        stack$pointer = c((n + 1), (n + 2), stack$pointer)
        stack$cost = c(cost, cost, stack$cost)
    }
    else {
        stack$pointer = c(stack$pointer[1:i], (n + 1), (n + 2), 
            stack$pointer[-c(1:i)])
        stack$cost = c(stack$cost[1:i], cost, cost, stack$cost[-c(1:i)])
    }
}

bbb_separate <- 
function (prob, branchvar, fval) 
{
    if (length(prob$var) >= 1) {
        p0 = prob
        p0$fval = fval
        p0$level = prob$level + 1
        p0$var = prob$var[-branchvar]
        p0$path[prob$var[branchvar]] = 1 + max(p0$path)
        p1 = p0
        p0$lb[prob$var[branchvar]] = 0
        p0$ub[prob$var[branchvar]] = 0
        p1$lb[prob$var[branchvar]] = 1
        p1$ub[prob$var[branchvar]] = 1
    }
    else {
        stop("no more integer variables to branch on")
    }
    return(list(p0 = p0, p1 = p1))
}

binary_branch_bound <- 
function (index_binvar, bbb_data, bbb_solve, control = bbb_control()) 
{
    fbest = Inf
    xbest = 0 * bbb_data$x0
    counter = 0
    nbinvar = length(index_binvar)
    flag = 7
    stack = new.env()
    stack$data = list()
    stack$cost = c()
    stack$pointer = c()
    stack$data[[1]] = list(lb = bbb_data$lb, ub = bbb_data$ub, 
        var = 1:nbinvar, path = rep(0, nbinvar), level = 0, fval = Inf)
    stack$cost = 0
    stack$pointer = 1
    control$proborder.selected = control$proborder
    if (F) {
        lb = bbb_data$lb
        ub = bbb_data$ub
        for (i in 0:1) {
            lb[] = i
            ub[] = i
            sol = match.fun(bbb_solve)(bbb_data, lb, ub)
            if (sol$ok) {
                x = sol$x
                fval = sol$fval
                xi = x[index_binvar]
                if (max(abs(round(xi, 0) - xi)) < control$bineps) {
                  fbest = fval
                  xbest = x
                  flag = 1
                  if (!control$silent) 
                    cat("FOUND SOLUTION =", fbest, "\n")
                }
            }
        }
    }
    while (length(stack$data) > 0) {
        subprob = bbb_pop(stack)
        if (!control$silent) {
            cat("-----------------------------------------------------", 
                "\n")
            if (max(subprob$path) > 0) {
                temp.index = order(-subprob$path)[1:sum(subprob$path > 
                  0)]
                cat("\t", paste("b", temp.index, " = ", subprob$lb[temp.index], 
                  sep = ""), "\n")
            }
            else {
                cat(counter, "\t", "FIRST NODE", "\n")
            }
            cat(counter, "\t", subprob$lb, "\t", subprob$var, 
                "\t", subprob$fval, "\t", fbest, "\n")
            cat("\t", subprob$ub, "\n")
            cat("stack size =", len(stack$pointer), "\n")
        }
        if (is.finite(subprob$fval) & is.finite(fbest) & fbest <= 
            subprob$fval) {
            if (!control$silent) 
                cat("SKIP this problem because a solution with lower FVAL already found\n")
        }
        else {
            counter = counter + 1
            sol = match.fun(bbb_solve)(bbb_data, subprob$lb, 
                subprob$ub)
            if (!sol$ok) {
                if (!control$silent) 
                  cat("NO SOLUTION EXISTS\n\n")
            }
            else {
                x = sol$x
                fval = sol$fval
                if (!control$silent) {
                  cat("SOLUTION OK", "\t", sol$fval, "\n")
                  cat("\t", round(x[index_binvar[subprob$var]], 
                    3), "\n\n")
                }
                if (flag != 1) 
                  flag = 5
                if (fval <= fbest) {
                  if (length(subprob$var) == 0) {
                    fbest = fval
                    xbest = x
                    flag = 1
                    if (!control$silent) 
                      cat("FOUND SOLUTION =", fbest, "\n")
                  }
                  else {
                    xi = x[index_binvar[subprob$var]]
                    if (max(abs(round(xi, 0) - xi)) < control$bineps) {
                      fbest = fval
                      xbest = x
                      flag = 1
                      if (!control$silent) 
                        cat("FOUND SOLUTION =", fbest, "\n")
                    }
                    else {
                      branchvar = bbb_decision(xi, control)
                      probs = bbb_separate(subprob, branchvar, 
                        fval)
                      p0 = probs$p0
                      p1 = probs$p1
                      if (!control$silent) 
                        cat("Branch on =", subprob$var[branchvar], 
                          "\n")
                      if (control$searchdir == 0) {
                        cost = 1/(subprob$level + 1)
                      }
                      else if (control$searchdir == 1) {
                        cost = subprob$level + 1
                      }
                      else if (control$searchdir == 2) {
                        cost = fval
                      }
                      else if (control$searchdir == 3) {
                        cost = fval/(subprob$level + 1)
                      }
                      if (control$proborder == 2) {
                        control$proborder.selected = round(xi[branchvar], 
                          0)
                      }
                      if (control$proborder.selected == 0) {
                        bbb_push(stack, p1, p0, cost)
                      }
                      else {
                        bbb_push(stack, p0, p1, cost)
                      }
                    }
                  }
                }
            }
            if (F) {
                cat("counter =", counter, "\n")
                cat("fbest     =", fbest, "\n")
                cat("stack$pointer =", stack$pointer, "\n")
                cat("\n")
            }
        }
    }
    rm(list = ls(stack, all = TRUE), envir = stack)
    return(list(xmin = xbest, fmin = fbest, counter = counter, 
        flag = flag))
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
    fill.gaps = F, basic = F) 
{
    if (!exists("symbolnames", b, inherits = F)) 
        b$symbolnames = ls(b)
    symbolnames = b$symbolnames
    nsymbols = len(symbolnames)
    if (nsymbols > 1) {
        out = bt.merge(b, align, dates)
        for (i in 1:nsymbols) {
            temp = coredata(b[[symbolnames[i]]])[out$date.map[, 
                i], , drop = FALSE]
            b[[symbolnames[i]]] = iif(basic, temp, make.xts(temp, 
                out$all.dates))
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
        if (basic) 
            b[[symbolnames[1]]] = coredata(b[[symbolnames[1]]])
    }
    b$dates = out$all.dates
    dummy.mat = matrix(double(), len(out$all.dates), nsymbols)
    colnames(dummy.mat) = symbolnames
    if (!basic) 
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

create.historical.ia <- 
function (hist.returns, annual.factor) 
{
    ia = create.ia(hist.returns)
    ia$annual.factor = annual.factor
    ia$arithmetic.return = apply(hist.returns, 2, mean, na.rm = T)
    ia$geometric.return = apply(hist.returns, 2, function(x) prod(1 + 
        x)^(1/len(x)) - 1)
    ia$arithmetic.return = (1 + ia$arithmetic.return)^ia$annual.factor - 
        1
    ia$geometric.return = (1 + ia$geometric.return)^ia$annual.factor - 
        1
    ia$risk = sqrt(ia$annual.factor) * ia$risk
    ia$risk = iif(ia$risk == 0, 0.000001, ia$risk)
    ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
    ia$expected.return = ia$arithmetic.return
    ia
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

delete.constraints <- 
function (delete.index, constraints) 
{
    constraints$A = constraints$A[, -delete.index, drop = F]
    constraints$b = constraints$b[-delete.index]
    constraints$meq = constraints$meq - len(intersect((1:constraints$meq), 
        delete.index))
    return(constraints)
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
            if (requireNamespace("xts", quietly = T) && xts::is.xts(truepart)) 
                falsepart[cond] = coredata(truepart)[cond]
            else falsepart[cond] = truepart[cond]
        }
        falsepart
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
    paste(v, collapse = delim)
}

len <- 
function (x) 
length(x)

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

max.omega.portfolio <- 
function (ia, constraints, type = c("mixed", "lp", "nlp")) 
{
    n = nrow(constraints$A)
    nt = nrow(ia$hist.returns)
    type = type[1]
    if (type == "mixed" || type == "lp") {
        sol = optimize.portfolio(ia, constraints, add.constraint.omega, 
            portfolio.omega, "max", T)
        x = rep(NA, n)
        if (!inherits(sol, "try-error") && sol$status == 0) {
            x0 = sol$solution[1:n]
            u = sol$solution[(1 + n):(n + nt)]
            d = sol$solution[(n + nt + 1):(n + 2 * nt)]
            t = sol$solution[(n + 2 * nt + 1):(n + 2 * nt + 1)]
            x = x0/t
        }
    }
    if ((type == "mixed" && (sol$status != 0 || any(u * d != 
        0))) || type == "nlp") {
        if (is.null(ia$parameters.omega)) 
            omega = 0
        else omega = ia$parameters.omega
        fn <- function(x) {
            portfolio.returns = x %*% t(ia$hist.returns)
            mean(pmax(portfolio.returns - omega, 0))/mean(pmax(omega - 
                portfolio.returns, 0))
        }
        x = optimize.portfolio.nlp(ia, constraints, fn, direction = "max")
    }
    return(x)
}

max.return.portfolio <- 
function (ia, constraints) 
{
    lp.obj.portfolio(ia, constraints, direction = "max")
}

max.sharpe.portfolio.test <- 
function () 
{
    ia = aa.test.create.ia()
    n = ia$n
    constraints = create.basic.constraints(n, 0, 1, 1)
    ef = portopt(ia, constraints, 50, "Efficient Frontier")
    png(filename = "plot1.png", width = 500, height = 500, units = "px", 
        pointsize = 12, bg = "white")
    plot.ef(ia, list(ef), transition.map = F)
    max(portfolio.return(ef$weight, ia)/portfolio.risk(ef$weight, 
        ia))
    weight = min.var.portfolio(ia, constraints)
    points(100 * portfolio.risk(weight, ia), 100 * portfolio.return(weight, 
        ia), pch = 15, col = "red")
    portfolio.return(weight, ia)/portfolio.risk(weight, ia)
    weight = max.sharpe.portfolio()(ia, constraints)
    points(100 * portfolio.risk(weight, ia), 100 * portfolio.return(weight, 
        ia), pch = 15, col = "orange")
    portfolio.return(weight, ia)/portfolio.risk(weight, ia)
    plota.legend("Minimum Variance,Maximum Sharpe", "red,orange", 
        x = "topright")
    dev.off()
    weight = max.sharpe.portfolio("long-only")(ia, constraints)
    round(weight, 2)
    round(c(sum(weight[weight < 0]), sum(weight[weight > 0])), 
        2)
    weight = max.sharpe.portfolio("long-short")(ia, constraints)
    round(weight, 2)
    round(c(sum(weight[weight < 0]), sum(weight[weight > 0])), 
        2)
    weight = max.sharpe.portfolio("long-short", -1)(ia, constraints)
    round(weight, 2)
    round(c(sum(weight[weight < 0]), sum(weight[weight > 0])), 
        2)
    weight = max.sharpe.portfolio("market-neutral")(ia, constraints)
    round(weight, 2)
    round(c(sum(weight[weight < 0]), sum(weight[weight > 0])), 
        2)
}

min.risk.portfolio <- 
function (ia, constraints) 
{
    x = NA
    binary.vec = 0
    if (!is.null(constraints$binary.index)) 
        binary.vec = constraints$binary.index
    if (is.null(ia$cov.temp)) 
        ia$cov.temp = ia$cov
    sol = try(solve.QP.bounds(Dmat = ia$cov.temp, dvec = rep(0, 
        nrow(ia$cov.temp)), Amat = constraints$A, bvec = constraints$b, 
        constraints$meq, lb = constraints$lb, ub = constraints$ub, 
        binary.vec = binary.vec), TRUE)
    if (!inherits(sol, "try-error")) {
        if (binary.vec[1] != 0) 
            cat(sol$counter, "QP calls made to solve problem with", 
                len(constraints$binary.index), "binary variables using Branch&Bound", 
                "\n")
        x = sol$solution
    }
    return(x)
}

min.var.portfolio.gmpl <- 
function (ia, constraints) 
{
    load.packages("quadprog,corpcor")
    cov.temp = ia$cov
    n0 = ia$n
    n = nrow(constraints$A)
    if (n != nrow(cov.temp)) {
        temp = matrix(0, n, n)
        temp[1:n0, 1:n0] = cov.temp[1:n0, 1:n0]
        cov.temp = temp
    }
    if (!is.positive.definite(cov.temp)) {
        cov.temp <- make.positive.definite(cov.temp, 0.000000001)
    }
    binary.vec = 0
    if (!is.null(constraints$binary.index)) 
        binary.vec = constraints$binary.index
    sol = solve.QP.bounds(Dmat = cov.temp, dvec = rep(0, nrow(cov.temp)), 
        Amat = constraints$A, bvec = constraints$b, constraints$meq, 
        lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec)
    if (binary.vec[1] != 0) 
        cat(sol$counter, "QP calls made to solve problem with", 
            len(binary.vec), "binary variables using Branch&Bound", 
            "\n")
    x = sol$solution[1:ia$n]
    names(x) = ia$symbols
    return(x)
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

optimize.portfolio <- 
function (ia, constraints, add.constraint.fn, min.risk.fn, direction = "min", 
    full.solution = F) 
{
    load.packages("quadprog,corpcor,lpSolve,kernlab")
    n = nrow(constraints$A)
    nt = nrow(ia$hist.returns)
    constraints = match.fun(add.constraint.fn)(ia, 0, ">=", constraints)
    f.obj = constraints$A[, ncol(constraints$A)]
    constraints = delete.constraints(ncol(constraints$A), constraints)
    f.con = constraints$A
    f.dir = c(rep("=", constraints$meq), rep(">=", len(constraints$b) - 
        constraints$meq))
    f.rhs = constraints$b
    x = NA
    binary.vec = 0
    if (!is.null(constraints$binary.index)) 
        binary.vec = constraints$binary.index
    sol = try(solve.LP.bounds(direction, f.obj, t(f.con), f.dir, 
        f.rhs, lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec, 
        default.lb = -100), TRUE)
    if (!inherits(sol, "try-error")) {
        x = sol$solution[1:n]
        if (F) {
            f.obj %*% sol$solution - match.fun(min.risk.fn)(t(x), 
                ia)
        }
    }
    if (full.solution) 
        x = sol
    return(x)
}

optimize.portfolio.nlp <- 
function (ia, constraints, fn, nl.constraints = NULL, direction = "min", 
    full.solution = F) 
{
    load.packages("Rdonlp2", repos = "http://R-Forge.R-project.org")
    if (direction == "min") 
        fnscale = 1
    else fnscale = -1
    cntl = donlp2Control()
    cntl$silent = T
    cntl$fnscale = fnscale
    cntl$iterma = 10000
    cntl$nstep = 100
    cntl$epsx = 0.0000000001
    par.l = constraints$lb
    par.u = constraints$ub
    p = rep(1, nrow(constraints$A))
    if (!is.null(constraints$x0)) {
        if (sum(is.na(constraints$x0)) == 0) 
            p = constraints$x0
    }
    A = t(constraints$A)
    lin.l = constraints$b
    lin.u = constraints$b
    lin.u[-c(1:constraints$meq)] = +Inf
    x = NA
    if (!is.null(nl.constraints)) {
        sol = donlp2(p, fn, par.lower = par.l, par.upper = par.u, 
            A = A, lin.u = lin.u, lin.l = lin.l, control = cntl, 
            nlin = nl.constraints$constraints, nlin.upper = nl.constraints$upper, 
            nlin.lower = nl.constraints$lower)
    }
    else {
        sol = donlp2(p, fn, par.lower = par.l, par.upper = par.u, 
            A = A, lin.u = lin.u, lin.l = lin.l, control = cntl)
    }
    if (!inherits(sol, "try-error")) {
        x = sol$par
    }
    if (full.solution) 
        x = sol
    return(x)
}

portfolio.omega <- 
function (weight, ia) 
{
    weight = weight[, 1:ia$n]
    if (is.null(ia$parameters.omega)) 
        omega = 0
    else omega = ia$parameters.omega
    portfolio.returns = weight %*% t(ia$hist.returns)
    return(apply(portfolio.returns, 1, function(x) mean(pmax(x - 
        omega, 0))/mean(pmax(omega - x, 0))))
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

portopt.omega <- 
function (ia, constraints = NULL, nportfolios = 50, name = "Omega") 
{
    out = list(weight = matrix(NA, nportfolios, nrow(constraints$A)))
    colnames(out$weight) = rep("", ncol(out$weight))
    colnames(out$weight)[1:ia$n] = ia$symbols
    ef.risk = portopt(ia, constraints, 2)
    out$weight[nportfolios, ] = ef.risk$weight[2, ]
    out$weight[1, ] = ef.risk$weight[1, ]
    constraints$x0 = out$weight[1, ]
    out$return = portfolio.return(out$weight, ia)
    target = seq(out$return[1], out$return[nportfolios], length.out = nportfolios)
    constraints = add.constraints(c(ia$expected.return, rep(0, 
        nrow(constraints$A) - ia$n)), target[1], type = "<=", 
        constraints)
    for (i in 1:nportfolios) {
        cat("i =", i, "\n")
        constraints$b[len(constraints$b)] = -target[i]
        out$weight[i, ] = max.omega.portfolio(ia, constraints)
        constraints$x0 = out$weight[i, ]
    }
    out$return = portfolio.return(out$weight, ia)
    out$risk = portfolio.risk(out$weight, ia)
    out$name = name
    return(out)
}

qp_delete <- 
function (qp_data) 
{
    rm(list = ls(qp_data, all = TRUE), envir = qp_data)
}

qp_new <- 
function (index_binvar, Dmat, dvec, Amat, bvec, meq = 0, factorized = FALSE) 
{
    nbinvar = length(index_binvar)
    nx = nrow(Dmat)
    nbvec = length(bvec)
    Amat = cbind(Amat, diag(nx)[, index_binvar], -diag(nx)[, 
        index_binvar])
    bvec = c(bvec, rep(0, nx)[index_binvar], rep(1, nx)[index_binvar])
    lb_bin_index = (1:nbinvar) + nbvec
    ub_bin_index = (1:nbinvar) + nbvec + nbinvar
    qp_data = new.env()
    qp_data$Dmat = Dmat
    qp_data$dvec = dvec
    qp_data$Amat = Amat
    qp_data$bvec = bvec
    qp_data$meq = meq
    qp_data$factorized = factorized
    qp_data$x0 = rep(0, nx)
    qp_data$lb_bin_index = lb_bin_index
    qp_data$ub_bin_index = ub_bin_index
    qp_data$lb = bvec[lb_bin_index]
    qp_data$ub = bvec[ub_bin_index]
    return(qp_data)
}

qp_solve <- 
function (qp_data, lb, ub) 
{
    bvec = qp_data$bvec
    bvec[qp_data$lb_bin_index] = lb
    bvec[qp_data$ub_bin_index] = -ub
    qp.data.final = solve.QP.remove.equality.constraints(qp_data$Dmat, 
        qp_data$dvec, qp_data$Amat, bvec, qp_data$meq)
    sol = tryCatch(solve.QP(Dmat = qp.data.final$Dmat, dvec = qp.data.final$dvec, 
        Amat = qp.data.final$Amat, bvec = qp.data.final$bvec, 
        meq = qp.data.final$meq, factorized = qp_data$factorized), 
        error = function(err) FALSE, warning = function(warn) FALSE)
    if (!is.logical(sol)) {
        x = qp.data.final$solution
        x[qp.data.final$var.index] = sol$solution
        return(list(ok = TRUE, x = x, fval = sol$value))
    }
    else {
        return(list(ok = FALSE))
    }
}

Rglpk.create.constraints <- 
function (prob) 
{
    n = prob$n_objective_vars
    lb = rep(NA, n)
    lb[prob$bounds$lower$ind] = prob$bounds$lower$val
    ub = rep(NA, n)
    ub[prob$bounds$upper$ind] = prob$bounds$upper$val
    constraints = new.constraints(n, lb = lb, ub = ub)
    constraints$binary.index = which(prob$objective_var_is_binary == 
        1)
    if (len(constraints$binary.index) == 0) 
        constraints$binary.index = 0
    if (is.null(dim(prob$constraint_matrix))) {
        prob$constraint_matrix = matrix(prob$constraint_matrix)
    }
    else {
        prob$constraint_matrix = t(prob$constraint_matrix)
    }
    index = which(prob$direction_of_constraints == "==")
    if (len(index) > 0) 
        constraints = add.constraints(prob$constraint_matrix[, 
            index], type = "=", b = prob$right_hand_side[index], 
            constraints)
    index = which(prob$direction_of_constraints == "<=")
    if (len(index) > 0) 
        constraints = add.constraints(prob$constraint_matrix[, 
            index], type = "<=", b = prob$right_hand_side[index], 
            constraints)
    index = which(prob$direction_of_constraints == ">=")
    if (len(index) > 0) 
        constraints = add.constraints(prob$constraint_matrix[, 
            index], type = ">=", b = prob$right_hand_side[index], 
            constraints)
    f.obj = prob$objective_coefficients
    dir = ifelse(prob$maximize, "max", "min")
    prob$names = prob$objective_vars_names
    prob$tickers = prob$objective_vars_names
    if (len(grep("\\[", prob$objective_vars_names)) > 0) {
        temp = matrix(spl(gsub("]", "", prob$objective_vars_names), 
            "\\["), nr = 2)
        prob$names = temp[1, ]
        prob$tickers = temp[2, ]
    }
    return(list(constraints = constraints, f.obj = f.obj, dir = dir, 
        prob = prob))
}

Rglpk.read.model <- 
function (file, type = c("MPS_fixed", "MPS_free", "CPLEX_LP", 
    "MathProg"), ignore_first_row = FALSE, verbose = FALSE) 
{
    if (!file.exists(file)) 
        stop(paste("There is no file called", file, "!"))
    type_db <- c(MPS_fixed = 1L, MPS_free = 2L, CPLEX_LP = 3L, 
        MathProg = 4L)
    obj <- list(file = tools::file_path_as_absolute(file), type = type_db[match.arg(type)])
    meta_data <- Rglpk:::glp_get_meta_data_from_file(obj, verbose)
    milp_data <- Rglpk:::glp_retrieve_MP_from_file(meta_data, 
        ignore_first_row, verbose)
    MP_data <- Rglpk:::glp_merge_MP_data(meta_data, milp_data)
    dir_db <- c(free = 1L, `>=` = 2L, `<=` = 3L, DB = 4L, `==` = 5L)
    MP_data$direction_of_constraints <- names(dir_db[MP_data$direction_of_constraints])
    types <- rep("C", length.out = MP_data$n_objective_vars)
    if (any(MP_data$objective_var_is_integer)) 
        types[MP_data$objective_var_is_integer] <- "I"
    if (any(MP_data$objective_var_is_binary)) 
        types[MP_data$objective_var_is_binary] <- "B"
    MP_data$types = types
    index = which(MP_data$direction_of_constraints == "free")
    if (length(index) > 0) {
        MP_data$constraint_matrix = as.matrix(MP_data$constraint_matrix)[-index, 
            ]
        MP_data$direction_of_constraints = MP_data$direction_of_constraints[-index]
        MP_data$right_hand_side = MP_data$right_hand_side[-index]
    }
    MP_data
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

solve.QP.bounds <- 
function (Dmat, dvec, Amat, bvec, meq = 0, factorized = FALSE, 
    binary.vec = 0, lb = -Inf, ub = +Inf) 
{
    Amat1 = Amat
    bvec1 = bvec
    n = len(dvec)
    if (len(lb) == 1) 
        lb = rep(lb, n)
    if (len(ub) == 1) 
        ub = rep(ub, n)
    lb = ifna(lb, -Inf)
    ub = ifna(ub, +Inf)
    index = which(ub < +Inf)
    if (len(index) > 0) {
        bvec = c(bvec, -ub[index])
        Amat = cbind(Amat, -diag(n)[, index])
    }
    index = which(lb > -Inf)
    if (len(index) > 0) {
        bvec = c(bvec, lb[index])
        Amat = cbind(Amat, diag(n)[, index])
    }
    if (binary.vec[1] == 0) {
        qp.data.final = solve.QP.remove.equality.constraints(Dmat, 
            dvec, Amat, bvec, meq)
        Dmat = qp.data.final$Dmat
        dvec = qp.data.final$dvec
        Amat = qp.data.final$Amat
        bvec = qp.data.final$bvec
        meq = qp.data.final$meq
        sol = try(solve.QP(Dmat, dvec, Amat, bvec, meq, factorized), 
            TRUE)
        if (inherits(sol, "try-error")) {
            ok = F
            sol = list()
        }
        else {
            tol = 0.001
            ok = T
            check = sol$solution %*% Amat - bvec
            if (meq > 0) 
                ok = ok & all(abs(check[1:meq]) <= tol)
            ok = ok & all(check[-c(1:meq)] > -tol)
        }
        if (!ok) {
            require(kernlab)
            index.constant.variables = which(!is.na(qp.data.final$solution))
            if (len(index.constant.variables) > 0) {
                Amat1 = Amat[, 1:ncol(Amat1)]
                bvec1 = bvec[1:ncol(Amat1)]
                lb = lb[-index.constant.variables]
                ub = ub[-index.constant.variables]
            }
            sv = ipop(c = matrix(-dvec), H = Dmat, A = t(Amat1), 
                b = bvec1, l = ifna(lb, -100), u = ifna(ub, 100), 
                r = c(rep(0, meq), rep(100, len(bvec1) - meq)))
            sol$solution = primal(sv)
        }
        x = qp.data.final$solution
        x[qp.data.final$var.index] = sol$solution
        sol$solution = x
    }
    else {
        qp_data = qp_new(binary.vec, Dmat = Dmat, dvec = dvec, 
            Amat = Amat, bvec = bvec, meq = meq, factorized = factorized)
        sol = binary_branch_bound(binary.vec, qp_data, qp_solve, 
            control = bbb_control(silent = T, branchvar = "max", 
                searchdir = "best"))
        qp_delete(qp_data)
        sol$value = sol$fmin
        sol$solution = sol$xmin
    }
    return(sol)
}

solve.QP.remove.equality.constraints <- 
function (Dmat, dvec, Amat, bvec, meq = 0) 
{
    qp.data = list()
    qp.data$Amat = Amat
    qp.data$bvec = bvec
    qp.data$Dmat = Dmat
    qp.data$dvec = dvec
    qp.data$meq = meq
    Amat1 = t(qp.data$Amat)
    bvec1 = qp.data$bvec
    Dmat1 = qp.data$Dmat
    dvec1 = qp.data$dvec
    meq1 = qp.data$meq
    qp.data$solution = rep(NA, ncol(Amat1))
    qp.data$var.index = 1:ncol(Amat1)
    while (T) {
        one.non.zero.index = which(rowSums(Amat1 != 0) == 1)
        if (len(one.non.zero.index) == 0) 
            break
        temp0 = rowSums(Amat1[one.non.zero.index, ])
        temp = abs(temp0)
        bvec1[one.non.zero.index] = bvec1[one.non.zero.index]/temp
        Amat1[one.non.zero.index, ] = Amat1[one.non.zero.index, 
            ]/temp
        temp0.index = matrix(1:ncol(Amat1), nr = ncol(Amat1), 
            nc = len(one.non.zero.index))[t(Amat1[one.non.zero.index, 
            ] != 0)]
        equality.constraints = rep(NA, ncol(Amat1))
        lb = ub = rep(NA, ncol(Amat1))
        index = temp0 > 0
        temp = order(bvec1[one.non.zero.index[index]], decreasing = FALSE)
        lb[temp0.index[index][temp]] = bvec1[one.non.zero.index[index]][temp]
        index = temp0 < 0
        temp = order(-bvec1[one.non.zero.index[index]], decreasing = TRUE)
        ub[temp0.index[index][temp]] = -bvec1[one.non.zero.index[index]][temp]
        remove.index = which(lb == ub)
        if (len(remove.index) > 0) {
            equality.constraints[remove.index] = lb[remove.index]
            Dmat1 = Dmat1[-remove.index, -remove.index, drop = F]
            dvec1 = dvec1[-remove.index]
            bvec1 = bvec1 - Amat1[, remove.index, drop = F] %*% 
                equality.constraints[remove.index]
            Amat1 = Amat1[, -remove.index, drop = F]
            qp.data$solution[qp.data$var.index[remove.index]] = lb[remove.index]
            qp.data$var.index = which(is.na(qp.data$solution))
            if (ncol(Amat1) > 0) {
                remove.index = which(rowSums(Amat1 != 0) == 0 & 
                  bvec1 == 0)
                if (len(remove.index) > 0) {
                  bvec1 = bvec1[-remove.index]
                  Amat1 = Amat1[-remove.index, , drop = F]
                  if (meq1 > 0) 
                    meq1 = meq1 - len(intersect((1:meq1), remove.index))
                }
            }
            else break
        }
        else break
    }
    qp.data$Amat = t(Amat1)
    qp.data$bvec = bvec1
    qp.data$Dmat = Dmat1
    qp.data$dvec = dvec1
    qp.data$meq = meq1
    return(qp.data)
}

spl <- 
function (s, delim = ",") 
unlist(strsplit(s, delim))

trim <- 
function (s) 
{
    s = sub(pattern = "^\\s+", replacement = "", x = s)
    sub(pattern = "\\s+$", replacement = "", x = s)
}

