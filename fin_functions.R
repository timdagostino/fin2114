

#-----BASICS-----

#Effective Annual Rate
get.ear <- function (apr, m) {
  ear <- ((1+(apr/m))^m - 1)
  
  return(ear)
}
#Example:
#get.ear(.0325,12)

#Annual Percentage Rate
get.apr <- function (ear, m) {
  apr <- m * (((1+ear)^(1/m))-1)
  
  return(apr)
}
#Example:
#get.apr(.032, 12)

#Present Value
get.pv <- function (fv, r, t) {
  pv <- fv/((1+r)^t)
  
  return(pv)
}
#Example:
#get.pv(1100, .1, 1)

#Present Value of Perpetuity
get.pv.perp <- function (cf, r, g = 0) {
  pv.perp <- cf/(r-g)
  
  return(pv.perp)
}
#Example:
#get.pv.perp(100, .05, .01)

#Present Value of Cash Flow
get.pv.cf <- function (cashflows, d.rate, perp = FALSE, perp.cf = 0, perp.d.rate = 0, perp.growth.rate = 0) {
  t <- 1:length(cashflows)
  
  pv.cf <- mapply(get.pv, cashflows, d.rate, t)
  pv.perp <- 0
  
  
  if (perp == TRUE) {
    perp <- get.pv.perp(perp.cf, perp.d.rate, perp.growth.rate)
    
    pv.perp <- get.pv(perp, perp.d.rate, length(t))
    
  }
  
  total.pv <- sum(pv.cf) + pv.perp
  
  return(total.pv)
}
#Example:
#get.pv.cf(c(100, 200, 300, 400, 500), d.rate = .1 ,perp = TRUE, perp.cf = 300, perp.d.rate = .2, perp.growth.rate = .05)

#Future Value
get.fv <- function (pv, r, t) {
  fv <- pv*((1+r)^t)
  
  return(fv)
}
#Example:
#get.fv(1000, .1, 1)

#Compounded Interest
get.comp.interest <- function (p, r, t, n) {
  amount <- p*((1+r/n)^(n*t))
  
  return(amount)
}
#Example:
#get.comp.interest(1500, .043, 6, 4)

#Earnings Before Interest and Taxes (EBIT) - VI
get.ebit.a <- function (total.rev, cogs, operating.exp) {
  ebit <- total.rev - cogs - operating.exp
  
  return(ebit)
}
#Example:
#get.ebit.a(100, 20, 30)

#Earnings Before Interest and Taxes (EBIT) - V2
get.ebit.b <- function (net.income, interest, taxes) {
  ebit <- net.income + interest + taxes
  
  return(ebit)
}
#Example:
#get.ebit.b(100, 20, 30)

#Net Operating Profit after Taxes (NOPAT) - V1
get.nopat.a <- function (ebit, tax.rate, nopat, solve.for = "nopat") {
  
  if (solve.for == "nopat") {
  value <- ebit*(1-tax.rate)
  }
  
  if (solve.for == "ebit") {
  value <- nopat/(1-tax.rate)
  }
  
  if (solve.for == "tax.rate") {
  value <- ((nopat/ebit) - 1)/-1
  }
  
  return(value)
}
#Example:
#get.nopat.a(tax.rate = .25, nopat = 75)

#Net Operating Profit after Taxes (NOPAT) - V2
get.nopat.b <- function (net.income = 0, net.interest = 0, tax.rate = 0, nopat = 0, solve.for = "nopat") {
  
  if (solve.for == "nopat") {
  value <- net.income + net.interest*(1-tax.rate)
  }
  
  if (solve.for == "net.income") {
  value <- nopat - net.interest*(1-tax.rate)
  }
  
  if (solve.for == "net.interest") {
  value <- (nopat - net.income)/(1-tax.rate)
  }
  
  if (solve.for == "tax.rate") {
  value <- (((nopat - net.income)/net.interest) - 1)/-1
  }
  
  return(value)
}
#Example:
#get.nopat.b(net.income = 100, net.interest = 15, nopat = 80)

#Earnings per Share (EPS)
get.eps <- function (net.income, number.shares) {
  eps <- net.income/number.shares
  
  return(eps)
}

#Change in Net Operating Working Capital (NOWC)
get.change.nowc <- function (ca.end, cl.end, ca.beg, cl.beg) {
  change.nowc <- (ca.end - cl.end) - (ca.beg - cl.beg)
  
  return(change.nowc)
}

#Capital Expenditure
get.capex <- function (ppe.end, ppe.beg, depreciation) {
  capex <- ppe.end - ppe.beg + depreciation
  
  return(capex)
}

#Free Cash Flow - V1
get.fcf.a <- function (nopat, net.investment.oc) {
  value <- nopat - net.investment.oc
  
  return(value)
}

#Free Cash Flow - V2
get.fcf.b <- function (ebit, tax.rate, depreciation, change.nowc, capex, fcf, solve.for = "fcf") {
  if (solve.for == "fcf") {
    value <- ebit*(1-tax.rate) + depreciation - change.nowc - capex
  }
  
  if (solve.for == "ebit") {
    value <- (fcf - depreciation + change.nowc + capex)/(1-tax.rate)
  }
  
  if (solve.for == "depreciation") {
    value <- fcf - (ebit*(1-tax.rate)) + change.nowc + capex
  }
  
  if (solve.for == "change.nowc") {
    value <- (fcf - (ebit*(1-tax.rate)) - depreciation + capex)/-1
  }
  
  if (solve.for == "capex") {
    value <- (fcf - (ebit*(1-tax.rate)) - depreciation + change.nowc)/-1
  }
    
  return(value)
}

#Growth Values
get.growth.values <- function (initial.price, growth.rate, years) {
  n <- 1
  values <- as.list(initial.price)
  n <- n + 1
  
  years <- years
  
  while (years > 0) {
    values[[n]] <- values[[n-1]]*(1+growth.rate)
    
    n <- n + 1
    years <- years - 1
  }
  
  return(unlist(values))
}

#-----BONDS-----

#Bond Price
get.bond.price <- function(fv, cr, ttm, ytm, p, solve.for = "p") {
  
  if (solve.for == "p") {
  cf <- c(rep(fv * cr, ttm - 1), fv * (1 + cr))
  cf <- data.frame(cf)
  cf$t <- as.numeric(rownames(cf))
  cf$pv_factor <- 1 / (1 + y)^cf$t
  cf$pv <- cf$cf * cf$pv_factor
  
  return(sum(cf$pv))
  }
  
  if (solve.for == "ytm") {
    coupon <- fv*cr
    ytm <- (coupon + ((fv-p)/ttm))/((fv + p)/2)
    
    return(ytm)
  }
}
#Example:
#get.bond.price(1000, .05, 10, .03)
#get.bond.price(fv = 1000, cr = .07, ttm = 18, p = 600, solve.for = "ytm")

#-----CAPM-----

#Cost of Equity: E(Ri)
get.ri <- function (rf, b, rm, ri, solve.for = "ri") {
  if (solve.for == "ri") {
    value <- rf + (b*(rm-rf)) 
  }
  
  if (solve.for == "rf") {
    value <- (-ri + (b*rm))/(-1+b)
  }
  
  if (solve.for == "b") {
    value <- (ri - rf)/(rm - rf)
  }
  
  if (solve.for == "rm") {
    value <- ((ri - rf)/b) + rf
  }
  
  return(value)
}
#Example: 
#get.ri(ri = .083, rm = .09, rf = .02, solve.for = "b")

#Beta
get.beta <- function (rf, rm , ri) {
  beta <- (ri-rf)/(rm-rf)
  
  return(beta)
}
#Example:
#get.beta(.02, .09, .083)

#Asset Beta
get.asset.beta <- function (levered.beta, debt, equity, tax.rate) {
  ratio <- debt/equity
  asset.beta <- levered.beta/(1+((1-tax.rate)*ratio))
  
  return (asset.beta)
}
#Example:
#get.asset.beta(1.2, 100000,40000,.3)

#Levered Beta
get.levered.beta <- function (asset.beta, tax.rate, debt = 0, equity = 0, debt.equity.ratio = 0) {
  value <- asset.beta + (asset.beta*(1-tax.rate)*(debt/equity))
  
  if (debt == 0 && equity == 0) {
    value <- asset.beta + (asset.beta*(1-tax.rate)*(debt.equity.ratio))
  }
  
  return(value)
}

#Weighted Average Cost of Capital (WACC)
get.wacc <- function (debt = 0, equity = 0, rd, re, tax.rate, wacc, debt.equity.ratio,  solve.for = "wacc") {
  
  total <- debt + equity
  
  if (solve.for == "wacc") {
    
    value <- (debt/total)*rd*(1-tax.rate) + (equity/total)*re 
  }
  
  if (solve.for == "rd") {
    value <- ((wacc/(debt/total))/(1-t)) - ((equity/total)*re)
  }
  
  if (solve.for == "re") {
    value <- (wacc - ((debt/total)*rd*(1-tax.rate)))/(equity/total)
  }
  
  if (debt == 0 && equity == 0) {
    debt.ratio <- debt.equity.ratio/(debt.equity.ratio + 1)
    equity.ratio <- 1/(debt.equity.ratio + 1)
    
    if (solve.for == "wacc") {
      
      value <- debt.ratio*rd*(1-tax.rate) + equity.ratio*re 
    }
    
    if (solve.for == "rd") {
      value <- ((wacc/debt.ratio)/(1-t)) - (equity.ratio*re)
    }
    
    if (solve.for == "re") {
      value <- (wacc - (debt.ratio*rd*(1-tax.rate)))/equity.ratio
    }
    
  }
  
  return(value)
}

#Effective Market Rate
get.rm <- function (rf, b, ri) {
  rm <- ((ri-rf)/b) + rf
  
  return(rm)
}
#Example:
#get.rm(.02, .9, .083)

#Equity Value
get.equity.value <- function (equity, debt, total, pref.stock = 0, solve.for = "equity") {
  
  if (solve.for == "equity") {
    value <- total - debt - pref.stock
  }
  
  if (solve.for == "debt") {
    value <- equity - total + pref.stock
  }
  
  if (solve.for == "total") {
    value <- equity + debt + pref.stock
  }
  
  return(value)
}

#Enterprise Value
get.ev <- function (value.e = 0, value.d = 0, cash = 0, ev = 0) {
  
  if (ev == 0) {
    value <- value.e + value.d - cash
  }
  
  if (value.e == 0) {
    value <- ev - value.d + cash
  }
  
  if (value.d == 0) {
    value <- ev - value.e + cash
  }
  
  if (cash == 0) {
    value <- ev - value.e - value.d
  }
  
  
  return(value)
}

#Enterprise Value (using multiples)
get.ev.multiple <- function (comps.ev = 0, comps.multiple = 0, multiple, calc.multiple = TRUE) {
  comps <- comps.ev/comps.multiple
  
  median <- median(comps)
  
  value <- median*multiple
  
  if (calc.multiple == FALSE) {
    value <- median(comps.multiple)*multiple
  }
  
  
  return(value)
} 

#Stock Price - V1
get.stock.price.a <- function (value.e, number.shares) {
  price <- value.e/number.shares
  
  return(price)
}

#Stock Price - V2
get.stock.price.b <- function (dividend, r, g, price, solve.for = "price") {
  
  if (solve.for == "price"){
    value <- dividend/(r-g)
  }
  
  if (solve.for == "dividend") {
    value <- price*(r-g)
  }
  
  if (solve.for == "r") {
    value <- (dividend/price) + g
  }
  
  if (solve.for == "g") {
    value <- (dividend-price*r)/-price
  }
  
  return(value)
}

#Net Present Value
get.npv <- function (initial.investment, cashflows, d.rate, profit.index = FALSE) {
    
    t <- 1:length(cashflows)
    
    pv.cf <- mapply(get.pv, cashflows, d.rate, t)
    
    value <- initial.investment + sum(pv.cf)
  
  if (profit.index == TRUE) {
    value <- sum(pv.cf)/(-1*initial.investment)
  }
  
  return(value)
}
#Example:
#get.npv(-1000000, c(100, 200, 300, 400, 500), .1)

#nth Root
nthroot <- function (x, n) {
  value <- x^(1/n)
  
  return(value)
}


get.irr <- function (initial.investment, pv.future.cashflows, t) {
  
 value <-  nthroot(x = (pv.future.cashflows/initial.investment), n = t) - 1
 
 return (value)
}
get.irr(initial.investment = 8454, pv.future.cashflows = 35080.9, t = 5)

#-----Capital Raising-----

#-----Working Capital Management-----

#cash Conversion Cycle - V1
get.ccc.a <- function (inv.days, ar.days, ap.days, ccc, solve.for = "ccc") {
  if (solve.for == "ccc") {
    value <- inv.days + ar.days - ap.days
  }
  
  if (solve.for == "inv.days") {
    value <- ccc - ar.days + ap.days
  }
  
  if (solve.for == "ar.days") {
    value <- ccc - inv.days + ap.days
  }
  
  if (solve.for == "ap.days") {
    value <- ccc - inv.days - ar.days
  }
  
  return(value)
}
#Example:
#get.ccc.a(inv.days = 60, ar.days = 30, ap.days = 45)

#Cash Conversion Cycle - V2
get.ccc.b <- function (sales, cogs, inventory, ar, ap, ccc, days = 365, solve.for = "ccc") {
  if (solve.for == "ccc") {
    inv.days <- inventory/(cogs/days)
    ar.days <- ar/(sales/days)
    ap.days <- ap/(cogs/days)
    
    value <- inv.days + ar.days - ap.days
  }
  
  if (solve.for == "ap") {
    inv.days <- inventory/(cogs/days)
    ar.days <- ar/(sales/days)
    
    value <- (-inv.days - ar.days)*(-1*(cogs/days))
  }
  
  if (solve.for == "ar") {
    inv.days <- inventory/(cogs/days)
    ap.days <- ap/(cogs/days)
    
    value <- (-inv.days + ap.days)*(cogs/days)
  }
  
  return(value)
}
#Example:
#get.ccc.b(sales = 2000000, cogs = 1000000, inventory = 100000, ar = 20000, solve.for = "ap")

#Net Income
get.net.income <- function (sales, cogs, expenses, tax.rate) {
  ebit <- (sales - cogs - expenses)
  
  value <- ebit*(1-tax.rate)
  
  return(value)
}
#Example (Question 6, HW 9):
#incomes <- get.net.income(sales = c(2000, 2000), cogs = c(1000, 1000), expenses = c(50, 50), tax.rate = .2)
#get.fcf.b(ebit = get.ebit.a(total.rev = 2000, cogs = 1000, operating.exp = 50), tax.rate = .2, depreciation = 0, change.nowc = -1000, capex = 0)
#get.fcf.b(ebit = get.ebit.a(total.rev = 2000, cogs = 1000, operating.exp = 50), tax.rate = .2, depreciation = 0, change.nowc = -500, capex = 0)


get.pv.perp(48,.12)
400/10


