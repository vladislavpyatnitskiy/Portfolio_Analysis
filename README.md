# Portfolio Analysis

[![R](https://img.shields.io/badge/R-4.x-blue.svg)](https://www.r-project.org/)
[![GitHub last commit](https://img.shields.io/github/last-commit/vladislavpyatnitskiy/Portfolio_Analysis.svg)](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/commits/main)

FYI: Yahoo! Finance started to block web parsing from its website. However, parsing is still possible via UK version, so in some cases it is possible to parse by typing "uk." before yahoo in link, for example, https://uk.finance.yahoo.com/quote/AAPL/key-statistics
_______________________________________________________________

## Repository Mission

One of the main issues of being investor is how to track your securities. 

Some investors use apps from their brokers, others prefer third party sites.

The truth is all of them are terrible. They do not give sufficient info about portfolio, but just weights and basic graph. The ones that bring some insights to your portoflio either cost a lot or technically complicated and visually disgusting. 

I think the reason is people who develop these kinds of systems are the ones who do not use them in a regular manner and the ones who actually use them experience lack of time or programming abilities to track their portfolio and positions on their own.  

However, I personally think that there should not be the trade off between convenience and capabilities. The process of portfolio analysis should be straightforward where investors are provided with the necessary information for making decisions. 

Having worked in trading and systems analytics, I came to idea to create if not an app but script to facilitate portfolio tracking. Current R and Python packages are not sufficient enough to obtain info and analysis, but they are powerful enough to build the right package for investing purposes.

Week by week, this repository will be filled with more advanced script.
_______________________________________________________________

Today my R script enables to get the following info about portfolio:

• Weights according to assets, their industries and sectors using pie plot; Barplot script enables to see securities dynamics and check Mean and Median of weights

• Comparative plots to assess portfolio dynamics with becncmarks and securities between each other (Scatter Plot, Heatmap, Treemap, Boxplot) 

• Non Visualisation Capabilities include calculation of Alpha & Beta of Portfolio, its value across multiple currencies, MVaR and Portfolio Risk. 

My first goal is to write powerful scripts that enable to check most necessary info for investor.  

Currently under development: stock fundamentals, more advanced plots, making script suitable for using via Git

In case you are interested in tracking of portfolio consisting of Russian stocks, you may be interested in the [rus-stock-data-analysis
](https://github.com/vladislavpyatnitskiy/rus-stock-data-analysis) repositoty.
_______________________________________________________________

## Installation

You may clone my repository via command line:

```bash
git clone https://github.com/vladislavpyatnitskiy/Portfolio_Analysis.git

```
## Requirements:

• quantmod

• timeSeries

• ggplot2

• ggrepel

• rvest
_______________________________________________________________
## Current Data Analytics Script Capabilities:

### Fama & French 3 Factor Model
```
Call:
lm(formula = M$RPRF ~ M$MRKT + M$SMB + M$HML)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.020934 -0.004838 -0.000629  0.003277  0.051802 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.0154586  0.0004465 -34.625  < 2e-16 ***
M$MRKT       0.0108014  0.0004298  25.132  < 2e-16 ***
M$SMB        0.0048543  0.0007821   6.207 1.84e-09 ***
M$HML        0.0040361  0.0005629   7.170 6.12e-12 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.007676 on 293 degrees of freedom
Multiple R-squared:  0.7509,	Adjusted R-squared:  0.7483 
F-statistic: 294.4 on 3 and 293 DF,  p-value: < 2.2e-16

```
### Fama & French 5 Factor Model
```
Call:
lm(formula = M$RPRF ~ M$MRKT + M$SMB + M$HML + M$RMW + M$CMA)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.020972 -0.004851 -0.000796  0.003456  0.051699 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.0155165  0.0004439 -34.956  < 2e-16 ***
M$MRKT       0.0102556  0.0004884  20.996  < 2e-16 ***
M$SMB        0.0051883  0.0009149   5.671 3.42e-08 ***
M$HML        0.0038052  0.0008431   4.513 9.27e-06 ***
M$RMW        0.0013533  0.0009940   1.361   0.1744    
M$CMA       -0.0029369  0.0013197  -2.225   0.0268 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.007616 on 291 degrees of freedom
Multiple R-squared:  0.7565,	Adjusted R-squared:  0.7523 
F-statistic: 180.8 on 5 and 291 DF,  p-value: < 2.2e-16
```
### Commodity Factors
```
Call:
lm(formula = r, data = d)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.044480 -0.007785  0.000177  0.007535  0.049854 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.000451   0.000759   0.594  0.55282    
Brent       0.083091   0.039509   2.103  0.03632 *  
Copper      0.165133   0.061681   2.677  0.00785 ** 
Cotton      0.118486   0.037906   3.126  0.00195 ** 
Gas         0.064113   0.015289   4.193 3.65e-05 ***
Gold        0.151256   0.095168   1.589  0.11306    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.01303 on 291 degrees of freedom
Multiple R-squared:  0.2043,	Adjusted R-squared:  0.1907 
F-statistic: 14.95 on 5 and 291 DF,  p-value: 4.622e-13

```
### Portfolio Calendar

| | 2022 | 2023 | Median | Mean |
|---|---|---|---|---|
January | NA | 12.9 | 12.9 | 12.9 |
February | NA | -2.81 | -2.81 | -2.81 |
March | NA | -2.58 | -2.58 | -2.58 |
April | NA | -1.13 | -1.13 | -1.13 |
May | NA | -3.94 | -3.94 | -3.94 |
June | NA | 4.9 | 4.9 | 4.9 |
July | 8.23 | 4.85 | 6.54 | 6.54 |
August | -1.21 | -5.96 | -3.58 | -3.58 |
September| -10.55 | -1.74 | -6.14 | -6.14 |
October | 9.35 | NA | 9.35 | 9.35 |
November| 5.82 | NA | 5.82 | 5.82 |
December| -9.3 | NA | -9.3 | -9.3 |
Sum | 2.34 | 4.49 |  |  |
Median | 2.31 | -1.74 |  |  |
Mean | 0.39 | 0.5 |  |  |

### Stock Fundamentals
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Fundamental%20ratios.png?raw=true)

### Altman Z Scores for Portfolio Securities
```
                      Sector Altman Z-Score     Zone
UNM               Financials           <NA>     <NA>
SWBI             Industrials           4.59     Safe
T     Communication Services           0.79 Distress
VSTO  Consumer Discretionary           2.81     Safe
WBA               Healthcare           2.32     Grey
AMZN  Consumer Discretionary           5.21     Safe
JEF               Financials           <NA>     <NA>
HPQ               Technology           1.89     Grey
TPH   Consumer Discretionary           4.68     Safe
F     Consumer Discretionary           1.06 Distress
C                 Financials           <NA>     <NA>
TSN         Consumer Staples           3.18     Safe
WERN             Industrials           2.93     Safe
GOOGL Communication Services          12.31     Safe
MU                Technology           4.76     Safe
AIG               Financials           <NA>     <NA>
NRG                Utilities           <NA>     <NA>
ARCH                  Energy           3.15     Safe
ALLY              Financials           <NA>     <NA>
FL    Consumer Discretionary           2.33     Grey
VALE               Materials            1.7     Grey
STLA  Consumer Discretionary           1.57     Grey
HPE               Technology              1 Distress
BFH               Financials           <NA>     <NA>
PARA  Communication Services           1.28     Grey
COF               Financials           <NA>     <NA>
VOYA              Financials           <NA>     <NA>
VIR               Healthcare           1.83     Grey
ZIM              Industrials           2.09     Grey
PVH   Consumer Discretionary           2.48     Grey
VIRT              Financials           <NA>     <NA>
FLGT              Healthcare           5.33     Safe
```
_______________________________________________________________

## Current Data Visualisation Script Capabilities:
_______________________________________________________________

### Pie Plot

#### Asset Weights
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Portfolio%20Pie.jpeg?raw=true)

#### Asset Weigths of Dividends
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Portfolio%20Dividend%20Structure.png?raw=true)

#### Industry Weights
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Pie%20with%20industries.png?raw=true)

#### Industry Weigths of Dividends
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Portfolio%20Dividend%20Pie%20by%20Industries.png?raw=true)

#### Sector Weights
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Pie%20Plot%20with%20Sectors.png?raw=true)

#### Sector Weigths of Dividends
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Portfolio%20Dividend%20Pie%20by%20Sectors.png?raw=true)

#### Weights by Market Capitalisation
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Pie%20Plot%20by%20Market%20Cap.png?raw=true)

#### Market Cap Weigths of Dividends
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Portfolio%20Dividend%20Pie%20By%20Market%20Cap.png?raw=true)

#### Weights by Countries
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Pie%20Plot%20by%20Countries.png?raw=true)

#### Countries Weigths of Dividends
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Portfolio%20Dividend%20Pie%20By%20Country.png?raw=true)

#### Weights of Securitites by Beta
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Pie%20Plot%20by%20Beta.png?raw=true)
_______________________________________________________________

### Line Plot

#### Portfolio Performance Tracking
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Portfolio%20Performance.jpeg?raw=true)

#### Plot Portfolio with Benchmarks / Indices to visually compare performances
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Comparison%20Plot.jpeg?raw=true)

#### Portfolio Dividends Accumulation Tracking
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Portfolio%20Dividends%20Accumulation%20Tracking.png?raw=true)

#### Portfolio Volatility Tracking
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Volatility.png?raw=true)

#### Portfolio Volatility in Absolute Values
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Volatility%20in%20absolute%20values.png?raw=true)

#### Drawdown Plot of Portfolio Cumulative Fluctuations
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Drawdown.png?raw=true)

#### Drawdown Plot of Portfolio Returns Fluctuations
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Drawdown%20of%20Fluctuations.png?raw=true)

#### Future Portfolio Performance by Monte Carlo Simulation
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Performance%20by%20Monte.png?raw=true)
```
[[1]]

[[2]]
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-69.419  -7.008  21.825  36.331  57.804 236.288 

[[3]]
[1] 0.634
```
_______________________________________________________________

### Bar Plot 

#### Portfolio Returns
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Bar%20Plot%20of%20Portfolio%20Returns.png?raw=true)

#### Portfolio Securities Overall Returns
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Barplot.jpeg?raw=true)

#### Asset Weigths
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Allocation%20Barplot.png?raw=true)

#### Dividend Weigths
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Portfolio%20Dividends%20Allocation.png?raw=true)

#### Median Correlations of securities
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Bar%20Plot%20of%20Portfolio%20Correlations%20by%20Assets.png?raw=true)

```
[1] "Consider to sell one of these Assets: JEF"            
[2] "Check these Assets: BFH, STLA, TPH, C, PVH, COF, ALLY"
[3] "OK to keep Assets: HPE, MU, VOYA, F, HPQ"             
[4] "Good Assets: WBA, AMZN, GOOGL, VSTO, WERN, AIG"       
[5] "Great Assets: SWBI, ZIM, T, TSN, UNM, PARA, NRG, FL"  
[6] "Best Assets: VIR, FLGT, VIRT, ARCH, VALE"     
```
Note: Plot and Script Output have been generated in different time periods

#### Portfolio Securities by Debt to EBITDA
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Bar%20Plot%20for%20Portfolio%20Stocks%20by%20Debt:EBITDA.png?raw=true)
_______________________________________________________________

### Stacked Bar Plot

#### Portfolio Securities Dynamcis
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Stacked%20Bar%20Plot%20by%20Securities%20($).png?raw=true)

#### Securities Portions Dynamcis
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Stacked%20Bar%20Plot.jpeg?raw=true)

#### Dividends Dynamics
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Stacked%20Bar%20Plot%20of%20Dividends%20by%20Amount.png?raw=true)

#### Dividend Portions Dynamics
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Stacked%20Bar%20Plot%20of%20Dividends%20by%20Portions.png?raw=true)

#### Sector Portions Dynamics
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Sector%20Stacked%20Bar%20Plot.png?raw=true)

#### Dividend Dynamics by Sector
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Stacked%20Bar%20Plot%20of%20Portfolio%20Dividends%20by%20Sector%20($).png?raw=true)

#### Dividend Portions Dynamics by Sector
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Stacked%20Bar%20Plot%20of%20Portfolio%20Dividends%20by%20Sector%20(%25).png?raw=true)

#### Market Cap Portions Dynamics
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Stacked%20Bar%20Plot%20by%20Market%20Cap%20in%20Portions.png?raw=true)

#### Dividend Dynamics by Market Cap
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Stacked%20Bar%20Plot%20of%20Portfolio%20Dividends%20by%20Market%20Cap%20($).png?raw=true)

#### Dividend Portions Dynamics by Market Cap
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Stacked%20Bar%20Plot%20of%20Portfolio%20Dividends%20by%20Market%20Cap%20(%25).png?raw=true)

#### Country Portions Dynamics
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Stacked%20Bar%20Plot%20by%20Country%20(%25).png?raw=true)

#### Dividend Dynamics by Country
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Stacked%20Bar%20Plot%20of%20Portfolio%20Dividends%20by%20Country%20($).png?raw=true)

#### Dividend Portions Dynamics by Country
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Stacked%20Bar%20Plot%20of%20Portfolio%20Dividends%20by%20Country%20(%25).png?raw=true)
_______________________________________________________________

### Portfolio's Correlation Heatmap
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Portfolio%20Correlations.jpeg?raw=true)
_______________________________________________________________

### Scatter Plot

#### Comparison of Portfolio & Indices Performances
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20&%20Indices%20Performance.png?raw=true)

#### Risk & Return (Standard Deviation)
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Scatter%20Plot.jpeg?raw=true)

#### Risk & Return (Beta)
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Scatter%20Plot%20with%20Risk%20&%20Return%20(Beta).png?raw=true)
_______________________________________________________________

### Bubble Plot

#### Using Standard Deviation
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Bubble%20Plot.png?raw=true)

#### Using Beta
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Bubble%20Plot%20(Beta).png?raw=true)

#### 3D Version using both Standard Deviation and Beta
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%203D%20Bubble%20Plot.png?raw=true)
_______________________________________________________________

### Treemap for Portfolio Securities

#### Asset Weights
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Treemap%20Plot.jpeg?raw=true)

#### Asset & Sector Weights
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Treemap%20with%20Sectors.png?raw=true)
_______________________________________________________________

### Portfolio Returns QQ Plot
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20QQ%20Plot.jpeg?raw=true)
_______________________________________________________________

### Portfolio Histograms

#### Histogram of Returns
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Histogram.png?raw=true)

#### Histogram of Correlation Values
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Correlations%20Histogram.png?raw=true)
_______________________________________________________________

### Boxplot for Portfolio Securities
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Boxplot.png?raw=true)

_______________________________________________________________

## Data

### Data Frame of Portfolio Positions

Ticker | Start Date | End Date | Number
|---|---|---|---|                      
| UNM | 2022-07-19 | 2024-07-19 | 1 |  
| SWBI | 2022-07-19 | 2024-07-19 | 2 | 
| SWBI | 2022-09-30 | 2024-07-19 | 1 | 
| VSTO | 2022-07-19 | 2024-07-19 | 2 | 
| T | 2022-07-19 | 2024-07-19 | 2 |    
| T | 2022-07-21 | 2024-07-19 | 1 |   
| WBA | 2022-07-19 | 2024-07-19 | 1 |
| AMZN | 2022-07-19 | 2024-07-19 | 1 |
| JEF | 2022-07-19 | 2024-07-19 | 2 |  
| HPQ | 2022-07-19 | 2024-07-19 | 2 |  
| TPH | 2022-07-21 | 2024-07-19 | 3 |
| F | 2022-07-21 | 2024-07-19 | 4 |    
| C | 2022-07-21 | 2024-07-19 | 1 |   
| TSN | 2022-07-21 | 2024-07-19 | 1 |  
| WERN | 2022-07-21 | 2024-07-19 | 2 | 
| GOOGL | 2022-07-26 | 2024-07-19 | 1 |
| MU | 2022-07-26 | 2024-07-19 | 1 |   
| AIG | 2022-07-26 | 2024-07-19 | 1 |  
| NRG | 2022-07-26 | 2024-07-19 | 1 |  
| ARCH | 2022-07-27 | 2024-07-19 | 1 | 
| ALLY | 2022-07-28 | 2024-07-19 | 1 | 
| FL | 2022-07-29 | 2024-07-19 | 2 |   
| VALE | 2022-07-29 | 2024-07-19 | 4 | 
| STLA | 2022-07-29 | 2024-07-19 | 4 | 
| HPE | 2022-07-29 | 2024-07-19 | 4 |  
| BFH | 2022-07-29 | 2024-07-19 | 1 |  
| PARA | 2022-07-29 | 2024-07-19 | 2 | 
| COF | 2022-08-01 | 2024-07-19 | 1 |  
| VOYA | 2022-08-01 | 2024-07-19 | 1 | 
| VIR | 2022-08-15 | 2024-07-19 | 1 |  
| VIR | 2022-09-30 | 2024-07-19 | 1 |  
| ZIM | 2022-08-15 | 2024-07-19 | 1 |  
| ZIM | 2022-08-26 | 2024-07-19 | 1 |  
| ZIM | 2022-09-30 | 2024-07-19 | 1 |  
| PVH | 2022-09-30 | 2024-07-19 | 1 |  
| VIRT | 2022-09-30 | 2024-07-19 | 1 | 
| FLGT | 2022-09-30 | 2024-07-19 | 1 | 

### Dividend Cash Flow

Script:
```
p.df.dividends.cash.flow(df_portfolio_dividend, full = F, tax = 0.1)
```
Output:
```
          Date Ticker  Dividend Number     Total  After Tax Cumulative Total
1   2022-07-28    UNM  0.330000      1  0.330000  0.2970000         0.297000
2   2022-07-29      C  0.510000      1  0.510000  0.4590000         0.756000
3   2022-07-29    NRG  0.350000      1  0.350000  0.3150000         1.071000
4   2022-07-29   ALLY  0.300000      1  0.300000  0.2700000         1.341000
5   2022-08-05    COF  0.600000      1  0.600000  0.5400000         1.881000
6   2022-08-10      F  0.150000      4  0.600000  0.5400000         2.421000
7   2022-08-11    BFH  0.210000      1  0.210000  0.1890000         2.610000
8   2022-08-12    JEF  0.286807      2  0.573614  0.5162526         3.126253
9   2022-08-12   VALE  0.687000      4  2.748000  2.4732000         5.599453
10  2022-08-18    WBA  0.480000      1  0.480000  0.4320000         6.031453
11  2022-08-25   VOYA  0.200000      1  0.200000  0.1800000         6.211453
12  2022-08-26    ZIM  4.750000      2  9.500000  8.5500000        14.761453
13  2022-08-30   ARCH  6.000000      1  6.000000  5.4000000        20.161453
14  2022-08-31    TSN  0.460000      1  0.460000  0.4140000        20.575453
15  2022-09-09    HPE  0.120000      4  0.480000  0.4320000        21.007453
16  2022-09-13    HPQ  0.250000      2  0.500000  0.4500000        21.457453
17  2022-09-14   PARA  0.240000      2  0.480000  0.4320000        21.889453
18  2022-09-15    AIG  0.320000      1  0.320000  0.2880000        22.177453
19  2022-09-21   SWBI  0.100000      2  0.200000  0.1800000        22.357453
20  2022-09-30   WERN  0.130000      2  0.260000  0.2340000        22.591453
21  2022-10-06      T  0.278000      3  0.834000  0.7506000        23.342053
22  2022-10-07     MU  0.115000      1  0.115000  0.1035000        23.445553
23  2022-10-13     FL  0.400000      2  0.800000  0.7200000        24.165553
24  2022-10-27    UNM  0.330000      1  0.330000  0.2970000        24.462553
25  2022-10-31    NRG  0.350000      1  0.350000  0.3150000        24.777553
26  2022-10-31   ALLY  0.300000      1  0.300000  0.2700000        25.047553
27  2022-11-04      C  0.510000      1  0.510000  0.4590000        25.506553
28  2022-11-10    JEF  0.286807      2  0.573614  0.5162526        26.022805
29  2022-11-10    BFH  0.210000      1  0.210000  0.1890000        26.211805
30  2022-11-10    COF  0.600000      1  0.600000  0.5400000        26.751805
31  2022-11-14    WBA  0.480000      1  0.480000  0.4320000        27.183805
32  2022-11-14      F  0.150000      4  0.600000  0.5400000        27.723805
33  2022-11-25   VOYA  0.200000      1  0.200000  0.1800000        27.903805
34  2022-11-28    ZIM  2.950000      3  8.850000  7.9650000        35.868805
35  2022-11-29   ARCH 10.750000      1 10.750000  9.6750000        45.543805
36  2022-11-29    PVH  0.038000      1  0.038000  0.0342000        45.578005
37  2022-11-30    TSN  0.480000      1  0.480000  0.4320000        46.010005
38  2022-11-30   VIRT  0.240000      1  0.240000  0.2160000        46.226005
39  2022-12-13    HPQ  0.263000      2  0.526000  0.4734000        46.699405
40  2022-12-13   VALE  0.056000      4  0.224000  0.2016000        46.901005
41  2022-12-13    HPE  0.120000      4  0.480000  0.4320000        47.333005
42  2022-12-14    AIG  0.320000      1  0.320000  0.2880000        47.621005
43  2022-12-14   PARA  0.240000      2  0.480000  0.4320000        48.053005
44  2022-12-19   SWBI  0.100000      3  0.300000  0.2700000        48.323005
45  2022-12-30   WERN  0.130000      2  0.260000  0.2340000        48.557005
46  2022-12-30     MU  0.115000      1  0.115000  0.1035000        48.660505
47  2023-01-09      T  0.278000      3  0.834000  0.7506000        49.411105
48  2023-01-12     FL  0.400000      2  0.800000  0.7200000        50.131105
49  2023-01-26    UNM  0.330000      1  0.330000  0.2970000        50.428105
50  2023-01-31    NRG  0.378000      1  0.378000  0.3402000        50.768305
51  2023-01-31   ALLY  0.300000      1  0.300000  0.2700000        51.038305
52  2023-02-03      C  0.510000      1  0.510000  0.4590000        51.497305
53  2023-02-03    COF  0.600000      1  0.600000  0.5400000        52.037305
54  2023-02-09    BFH  0.210000      1  0.210000  0.1890000        52.226305
55  2023-02-10    JEF  0.300000      2  0.600000  0.5400000        52.766305
56  2023-02-10      F  0.800000      4  3.200000  2.8800000        55.646305
57  2023-02-15    WBA  0.480000      1  0.480000  0.4320000        56.078305
58  2023-02-23   VOYA  0.200000      1  0.200000  0.1800000        56.258305
59  2023-02-27   ARCH  3.110000      1  3.110000  2.7990000        59.057305
60  2023-02-28    TSN  0.480000      1  0.480000  0.4320000        59.489305
61  2023-02-28   VIRT  0.240000      1  0.240000  0.2160000        59.705305
62  2023-03-07    HPQ  0.263000      2  0.526000  0.4734000        60.178705
63  2023-03-07    PVH  0.038000      1  0.038000  0.0342000        60.212905
64  2023-03-14   VALE  0.354000      4  1.416000  1.2744000        61.487305
65  2023-03-15   SWBI  0.100000      3  0.300000  0.2700000        61.757305
66  2023-03-16    AIG  0.320000      1  0.320000  0.2880000        62.045305
67  2023-03-16    HPE  0.120000      4  0.480000  0.4320000        62.477305
68  2023-03-17   PARA  0.240000      2  0.480000  0.4320000        62.909305
69  2023-04-04    ZIM  6.400000      3 19.200000 17.2800000        80.189305
70  2023-04-06      T  0.278000      3  0.834000  0.7506000        80.939905
71  2023-04-06     MU  0.115000      1  0.115000  0.1035000        81.043405
72  2023-04-13     FL  0.400000      2  0.800000  0.7200000        81.763405
73  2023-04-14   WERN  0.130000      2  0.260000  0.2340000        81.997405
74  2023-04-24   STLA  1.473000      4  5.892000  5.3028000        87.300205
75  2023-04-25      F  0.150000      4  0.600000  0.5400000        87.840205
76  2023-04-27    UNM  0.330000      1  0.330000  0.2970000        88.137205
77  2023-04-28      C  0.510000      1  0.510000  0.4590000        88.596205
78  2023-04-28    NRG  0.378000      1  0.378000  0.3402000        88.936405
79  2023-04-28   ALLY  0.300000      1  0.300000  0.2700000        89.206405
80  2023-05-11    BFH  0.210000      1  0.210000  0.1890000        89.395405
81  2023-05-12    JEF  0.300000      2  0.600000  0.5400000        89.935405
82  2023-05-12    COF  0.600000      1  0.600000  0.5400000        90.475405
83  2023-05-18    WBA  0.480000      1  0.480000  0.4320000        90.907405
84  2023-05-25   VOYA  0.200000      1  0.200000  0.1800000        91.087405
85  2023-05-30   ARCH  2.450000      1  2.450000  2.2050000        93.292405
86  2023-05-31    TSN  0.480000      1  0.480000  0.4320000        93.724405
87  2023-05-31   VIRT  0.240000      1  0.240000  0.2160000        93.940405
88  2023-06-06    PVH  0.038000      1  0.038000  0.0342000        93.974605
89  2023-06-13    HPQ  0.263000      2  0.526000  0.4734000        94.448005
90  2023-06-14    HPE  0.120000      4  0.480000  0.4320000        94.880005
91  2023-06-14   PARA  0.050000      2  0.100000  0.0900000        94.970005
92  2023-06-15    AIG  0.360000      1  0.360000  0.3240000        95.294005
93  2023-06-30   WERN  0.140000      2  0.280000  0.2520000        95.546005
94  2023-07-07      T  0.278000      3  0.834000  0.7506000        96.296605
95  2023-07-07     MU  0.115000      1  0.115000  0.1035000        96.400105
96  2023-07-12   SWBI  0.120000      3  0.360000  0.3240000        96.724105
97  2023-07-13     FL  0.400000      2  0.800000  0.7200000        97.444105
98  2023-07-24      F  0.150000      4  0.600000  0.5400000        97.984105
99  2023-07-27    UNM  0.365000      1  0.365000  0.3285000        98.312605
100 2023-07-31    NRG  0.378000      1  0.378000  0.3402000        98.652805
101 2023-07-31   ALLY  0.300000      1  0.300000  0.2700000        98.922805
102 2023-08-04      C  0.530000      1  0.530000  0.4770000        99.399805
103 2023-08-04    COF  0.600000      1  0.600000  0.5400000        99.939805
104 2023-08-10    BFH  0.210000      1  0.210000  0.1890000       100.128805
105 2023-08-11    JEF  0.300000      2  0.600000  0.5400000       100.668805
106 2023-08-14   VALE  0.406000      4  1.624000  1.4616000       102.130405
107 2023-08-18    WBA  0.480000      1  0.480000  0.4320000       102.562405
108 2023-08-25   VOYA  0.400000      1  0.400000  0.3600000       102.922405
109 2023-08-30   ARCH  3.970000      1  3.970000  3.5730000       106.495405
110 2023-08-31    TSN  0.480000      1  0.480000  0.4320000       106.927405
111 2023-08-31   VIRT  0.240000      1  0.240000  0.2160000       107.143405
112 2023-09-05    PVH  0.038000      1  0.038000  0.0342000       107.177605
113 2023-09-12    HPQ  0.263000      2  0.526000  0.4734000       107.651005
114 2023-09-13    HPE  0.120000      4  0.480000  0.4320000       108.083005
115 2023-09-14    AIG  0.360000      1  0.360000  0.3240000       108.407005
116 2023-09-14   PARA  0.050000      2  0.100000  0.0900000       108.497005
117 2023-09-20   SWBI  0.120000      3  0.360000  0.3240000       108.821005
```
