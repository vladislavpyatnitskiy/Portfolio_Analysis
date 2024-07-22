# Portfolio Analysis

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
_______________________________________________________________

## Requirements:

• quantmod

• timeSeries

• ggplot2

• ggrepel

• rvest
_______________________________________________________________

## Data Frame of Portfolio Positions

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

_______________________________________________________________
## Current Data Analytics Script Capabilities:

### Stock Fundamentals
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Fundamental%20ratios.png?raw=true)

### Altman Z Scores for Portfolio Securities
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Portfolio%20Securities%20Altman%20Z%20Score.png?raw=true)
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
