# Portfolio Analysis
_______________________________________________________________

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

Requirements:

• quantmod

• timeSeries

• ggplot2

• ggrepel

• rvest
_______________________________________________________________

Current Data Visualisation Script Capabilities:

Asset Weights
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Portfolio%20Pie.jpeg?raw=true)
_______________________________________________________________

Industry Weights
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Pie%20with%20industries.png?raw=true)
_______________________________________________________________

Sector Weights
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Pie%20Plot%20with%20Sectors.png?raw=true)
_______________________________________________________________

Portfolio Performance Tracking
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Portfolio%20Performance.jpeg?raw=true)
_______________________________________________________________

Bar Plot with Portfolio Weights 
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Allocation%20Barplot.png?raw=true)
_______________________________________________________________

Stacked Bar Plot showing Securities Portions Dynamcis
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Stacked%20Bar%20Plot.jpeg?raw=true)
_______________________________________________________________

Portfolio's Correlation Heatmap
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Portfolio%20Correlations.jpeg?raw=true)
_______________________________________________________________

Plot Portfolio with Benchmarks / Indices to visually compare performances
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Comparison%20Plot.jpeg?raw=true)
_______________________________________________________________

Scatter Plot to compare Portfolio & Indices Performance 
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20&%20Indices%20Performance.png?raw=true)
_______________________________________________________________

Scatter Plot for Portfolio Securities
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Scatter%20Plot.jpeg?raw=true)
_______________________________________________________________

Treemap for Portfolio Securities
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Treemap%20Plot.jpeg?raw=true)
_______________________________________________________________

Barplot for Portfolio Securities
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Barplot.jpeg?raw=true)
_______________________________________________________________

Portfolio Returns QQ Plot
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20QQ%20Plot.jpeg?raw=true)
_______________________________________________________________

Portfolio Histogram
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Histogram.png?raw=true)
_______________________________________________________________

Boxplot for Portfolio Securities
![](https://github.com/vladislavpyatnitskiy/Portfolio_Analysis/blob/main/Plots/Portfolio%20Boxplot.png?raw=true)
