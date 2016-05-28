---
layout: post
title: "Flexible Asset Allocation Strategy"
---


# Strategy Explanation

This strategy extends the timeseries momentum (or trendfollowing) model towards a generalized momentum model, called Flexible Asset Allocation (FAA). This is done by adding new momentum factors to the traditional momentum factor (R) based on the relative returns among assets. These new factors are called Absolute momentum (A), Volatility momentum (V) and Correlation momentum (C).

Each asset is ranked on each of the four factors R, A, V and C. By using a linearised representation of a loss function representing risk/return, we are able to arrive at simple closed form solutions for our flexible asset allocation strategy based on these four factors. 

The strategy is based on monthly re-balancing, signal is generated one day before the month end, and execution is done at the market close on the month end.

# ETFs Used in the Strategy

To backtest this strategy, we used the following asset classes represented by the following ETFs:

* US STOCKS = VTI + VTSMX
    * US Stocks are represented by VTI (Vanguard Total Stock Market ETF) with its price history being extended by                  VTSMX (Vanguard Total Stock Market Index Fund Investor Shares)
        * VTI - Seeks to track the performance of the CRSP US Total Market Index
        * VTSMX -  Designed to provide investors with exposure to the entire U.S. equity market, including small-, mid-, and                       large-cap growth and value stocks
* FOREIGN STOCKS = VEU + FDIVX
    * Foreign Stocks are represented by VEU (Vanguard FTSE All-World ex-US ETF) with its price history being extended by           FDIVX (Fidelity Diversified International Fund)
        * VEU - Seeks to track the performance of the FTSE All-World ex US Index
        * VTSMX -  Normally investing primarily in non-U.S. securities, primarily in common stocks
* EMERGING MARKETS = EEM + VEIEX
    * Emerging Markets are represented by EEM (iShares MSCI Emerging Markets Index) with its price history being extended by        VEIEX (Vanguard Emerging Markets Stock Index Fund Investor Shares)
        * EEM - Seeks to track the performance of the MSCI Emerging Markets Index
        * VEIEX - The fund invests in stocks of companies located in emerging markets around the world, such as Brazil,                        Russia, India, Taiwan, and China 
* US 10YR GOV BOND = IEF + VFITX
    * US 10 Year Government Bonds are represented by IEF (iShares 7-10 Year Treasury Bond ETF) with its price history being         extended by VFITX (Vanguard Intermediate-Term Treasury Fund Investor Shares)
        * IEF - Seeks to track the performance of the Barclays U.S. 7-10 Year Treasury Bond Index
        * VFITX - This fund invests in debt issued directly by the government in the form of intermediate-term treasuries
        
\pagebreak

* REAL ESTATE = VNQ + VGSIX
    * Real Estate is represented by VNQ (iShares 7-10 Year Treasury Bond ETF) with its price history being extended by VGSIX       (Vanguard Intermediate-Term Treasury Fund Investor Shares)
        * VNQ - Seeks to track the performance of the MSCI US REIT Index
        * VGSIX - This fund invests in real estate investment trusts — companies that purchase office buildings, hotels, and                     other real estate property   
* COMMODITIES = DBC + CRB
    * Commodities are represented by DBC (PowerShares DB Commodity Tracking ETF) with its price history being extended by CRB        (Thomson Reuters/Core Commodity CRB Index)
        * DBC - The investment seeks to track changes, whether positive or negative, in the level of the DBIQ Optimum Yield                  Diversified Commodity Index Excess Return
        * CRB - This index comprises 19 commodities: Aluminum, Cocoa, Coffee, Copper, Corn, Cotton, Crude Oil, Gold, Heating                 Oil, Lean Hogs, Live Cattle, Natural Gas, Nickel, Orange Juice, Silver, Soybeans, Sugar, Unleaded Gas and                    Wheat; the tenth revision of the index renamed it the Thomson Reuters/Core Commodity CRB Index, or TR/CC CRB.
* CASH = BND + VBMFX
    * Cash is represented by BND (Vanguard Total Bond Market ETF) with its price history being extended by VBMFX                  (Vanguard Total Bond Market Index Fund Investor Shares)
        * BND - Provides broad exposure to U.S. investment grade bonds
        * VBMFX - This fund is designed to provide broad exposure to U.S. investment grade bonds, with the                                         fund investing about 30% in corporate bonds and 70% in U.S. government bonds of all maturities  
        
# Strategy Backtest Summary vs. S&P 500 (SPY)




![plot of chunk unnamed-chunk-2](Figs/unnamed-chunk-2-1.png)

## Strategy Performance:


|              |Strategy          |SPY               |
|:-------------|:-----------------|:-----------------|
|Period        |May1996 - May2016 |May1996 - May2016 |
|Cagr          |11.28             |7.69              |
|Sharpe        |1.15              |0.47              |
|DVR           |1.07              |0.31              |
|R2            |0.93              |0.65              |
|Volatility    |9.75              |19.94             |
|MaxDD         |-16.18            |-55.19            |
|Exposure      |99.72             |99.98             |
|Win.Percent   |63.05             |100               |
|Avg.Trade     |0.36              |340.55            |
|Profit.Factor |2.08              |NaN               |
|Num.Trades    |655               |1                 |

![plot of chunk unnamed-chunk-3](Figs/unnamed-chunk-3-1.png)


## Monthly Results:


|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:---|:----|:-----|
|1996 |NA   |NA   |NA   |NA   |NA   |1.3  |0.2  |-0.2 |1.5  |1.5  |4.0  |4.0 |12.9 |-1.8  |
|1997 |0.2  |-0.7 |-2.5 |-1.3 |5.9  |1.5  |2.3  |-7.4 |5.3  |-0.1 |-1.0 |1.4 |2.9  |-7.4  |
|1998 |0.2  |2.2  |3.2  |1.5  |-0.6 |0.1  |-0.7 |-3.2 |2.5  |-0.6 |0.1  |3.3 |8.1  |-5.8  |
|1999 |-1.7 |-2.9 |2.2  |2.6  |-3.2 |4.3  |-1.0 |1.1  |2.4  |0.3  |4.2  |9.1 |18.0 |-6.7  |
|2000 |-3.5 |4.8  |0.6  |-2.6 |3.3  |2.7  |1.7  |2.4  |1.0  |-1.5 |3.0  |0.9 |13.0 |-5.6  |
|2001 |0.7  |-0.4 |-1.3 |0.2  |1.0  |2.3  |0.9  |-0.5 |-0.2 |0.2  |-1.7 |0.1 |1.2  |-4.8  |
|2002 |1.3  |1.8  |3.0  |0.2  |0.5  |2.5  |-0.9 |2.9  |2.8  |-0.9 |-0.5 |1.6 |15.1 |-4.7  |
|2003 |2.5  |0.9  |-2.1 |0.6  |6.1  |0.9  |0.2  |4.1  |0.2  |3.5  |2.9  |5.3 |27.9 |-4.4  |
|2004 |2.7  |2.3  |2.5  |-6.6 |1.7  |-1.3 |2.0  |1.2  |0.0  |2.5  |3.9  |3.0 |14.2 |-7.5  |
|2005 |-0.8 |1.4  |-4.1 |-2.3 |0.5  |2.6  |1.3  |0.4  |1.3  |-4.0 |2.9  |4.1 |2.9  |-8.5  |
|2006 |7.0  |-2.7 |3.6  |2.5  |-6.3 |-0.8 |2.2  |0.6  |1.3  |3.3  |3.0  |0.8 |14.6 |-16.2 |
|2007 |3.3  |-3.0 |0.3  |2.8  |1.9  |-0.6 |-0.2 |0.0  |7.0  |7.1  |-1.4 |1.8 |20.0 |-9.0  |
|2008 |-0.9 |6.1  |0.2  |1.0  |1.6  |-0.1 |-3.3 |-1.7 |-0.4 |-2.3 |5.2  |5.1 |10.7 |-11.9 |
|2009 |-2.7 |-0.7 |1.8  |6.3  |5.7  |-2.4 |6.1  |0.8  |4.4  |-2.7 |5.3  |0.5 |24.1 |-9.3  |
|2010 |-6.7 |4.3  |5.1  |3.4  |-3.5 |-2.7 |3.7  |2.1  |3.8  |3.7  |-1.8 |4.4 |16.2 |-10.1 |
|2011 |1.8  |4.2  |0.5  |4.4  |-1.7 |-2.7 |2.3  |0.2  |-4.0 |-0.4 |0.1  |2.6 |7.2  |-7.3  |
|2012 |4.1  |1.5  |-0.2 |1.5  |-2.7 |-0.1 |1.5  |-0.1 |0.5  |-1.2 |1.2  |1.7 |7.8  |-4.6  |
|2013 |1.4  |0.1  |2.3  |3.1  |-2.3 |-1.7 |2.1  |-2.0 |0.5  |2.8  |0.9  |0.7 |7.8  |-8.5  |
|2014 |-2.0 |3.4  |0.5  |1.4  |0.9  |1.7  |0.7  |2.5  |-3.7 |2.1  |1.9  |0.6 |10.3 |-5.3  |
|2015 |2.8  |-0.2 |0.4  |-2.0 |-1.1 |-1.9 |0.4  |-2.2 |1.0  |-0.2 |-0.2 |0.4 |-2.9 |-9.7  |
|2016 |-2.7 |0.6  |0.5  |-0.8 |1.1  |NA   |NA   |NA   |NA   |NA   |NA   |NA  |-1.2 |-5.3  |
|Avg  |0.3  |1.1  |0.8  |0.8  |0.4  |0.3  |1.1  |0.0  |1.4  |0.7  |1.6  |2.6 |11.0 |-7.4  |

## 12M Rolling Returns
![plot of chunk unnamed-chunk-5](Figs/unnamed-chunk-5-1.png)


## Drawdowns:
![plot of chunk unnamed-chunk-6](Figs/unnamed-chunk-6-1.png)

