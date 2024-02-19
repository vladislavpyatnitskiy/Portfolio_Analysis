# Portfolio Analysis
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
## Current Data Analytics Script Capabilities:

### Stock Fundamentals
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Fundamental%20ratios.png?raw=true)

### Altman Z Scores for Portfolio Securities
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Portfolio%20Securities%20Altman%20Z%20Score.png?raw=true)
_______________________________________________________________

## Current Data Visualisation Script Capabilities:

### Pie Plot
_______________________________________________________________

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
_______________________________________________________________

### Portfolio Performance Tracking
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Portfolio%20Performance.jpeg?raw=true)
_______________________________________________________________

### Portfolio Dividends Accumulation Tracking
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Dividends/Portfolio%20Dividends%20Accumulation%20Tracking.png?raw=true)
_______________________________________________________________

### Portfolio Volatility Tracking
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Volatility.png?raw=true)
_______________________________________________________________

### Portfolio Drawdown 
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Drawdown.png?raw=true)
_______________________________________________________________

### Future Portfolio Performance by Monte Carlo Simulation
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Performance%20by%20Monte.png?raw=true)
_______________________________________________________________

### Bar Plot 

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

### Plot Portfolio with Benchmarks / Indices to visually compare performances
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Comparison%20Plot.jpeg?raw=true)
_______________________________________________________________

### Scatter Plot to compare Portfolio & Indices Performance 
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20&%20Indices%20Performance.png?raw=true)
_______________________________________________________________

### Scatter Plot for Portfolio Securities

#### Risk & Return (Standard Deviation)
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Scatter%20Plot.jpeg?raw=true)

#### Risk & Return (Beta)
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Scatter%20Plot%20with%20Risk%20&%20Return%20(Beta).png?raw=true)
_______________________________________________________________

### Bubble Plots for Portfolio Securities 

#### Using Standard Deviation
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Bubble%20Plot.png?raw=true)

#### Using Beta
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Bubble%20Plot%20(Beta).png?raw=true)
_______________________________________________________________

### 3D Bubble Plot for Portfolio Securities
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%203D%20Bubble%20Plot.png?raw=true)
_______________________________________________________________

### Treemap for Portfolio Securities

#### Asset Weights
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Treemap%20Plot.jpeg?raw=true)

#### Asset & Sector Weights
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Treemap%20with%20Sectors.png?raw=true)
_______________________________________________________________

### Barplot for Portfolio Securities
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Barplot.jpeg?raw=true)
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
