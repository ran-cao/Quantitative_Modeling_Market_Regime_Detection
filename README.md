# Quantitative Modeling Define Trading Strategies Under Different Market Regimes
The goal of this project is to define market regimes from the asset returns and factor returns datasets over the 20 years data (2000-2020), and propose the trading strategies and optimize asset allocation under different market regimes. Over the project, two methods are constructed to identify the market regimes and risk-adjusted trading strategies are suggested under different regimes.

### Step1: Market Regime Detection

The first one is to use correlation matrix then clustering the windows. Based on the idea of correlation matrix, we build 20 combinations: 4 window lengths choices (half year, one year, one and half year, and two years) for 3 measures respectivley (correlation matrix difference, five largest eigenvalues differences using Cosine distance, and five largest eigenvalues differences using Euclidean distance), as well as 8 combinations under the new clustering method, which uses raw data transformation’s result as clustering input instead of dissimilarity matrix as an input for previous PAM method (k-means & hierarchical clustering for 0.5 year, 1 year, 1.5 years, and 2 years respectively). 

#### Judging Criteria
Out of these choices, best regime result is based on the two criteria we set for “optimal market regimes”. A good market regime identification should capture the economic life cycle, including the bull markets (2002-2007 & 2009-2020) and major bear markets (2000-2002, 2007-2009, 2020). Another angle to explain market regimes is by volatility. Bull markets cluster of our regime's results should have a low volatility, as bull markets are slow and steady, whereas the bear market should be volatile because assets lose value and prices easily over the bear markets. 

### Step2: Define Trading Strategy and Asset Allocation
With the idea of PCA & Changepoint Detection, we detected three market regimes (bull, bear, and transition period). The next step is to find the trading strategy under each of the regime. We adopted the concept of graph theory - "distance correlation network". With the calculation of intra-portfolio risks based on communicability betweenness, weights of asset are the inverse relationship of intra-portfolio risks. The benefit of distance correlation network is the asset allocation has a low risk. The reason is, the farther the asset is from the centrality, the less risky the asset is, and a higher weight is set.

### result
Our cumulative 20-year return is 310%, which beats our benchmark that having equally weighted naive approach (165%).
