# Stationarity

A stationary time series has three characteristics:

- Constant mean
- Constant variance
- No autocorrelation
- No seasonality

It is possible to decompose a time series into trend, seasonal and residual 
components. However, we must know the frequency or period of the seasonal 
component. So far, I have guessed it from the raw plot of the time series.

A few ways to check if the time series is non-stationary:

- Plot it. If the variations change over time or if there is a marked trend then
it will be visible on the plot.
- Divide the series into reasonable sized chunks and compute the summary stats 
of each. If they change drastically then the time series is not stationary.
- Plot a histogram of the time series data. If the series were stationary then
the histogram will have a pronounced peak around the mean and a sharply dropping
count away from it. A non-stationary time series will have a uniformly spread
histogram.
- Conduct the augmented Dickey-Fuller test. Its null hypothesis is that the 
series is **not** stationary. The test seems to detect auto-correlation structure
better than changing mean and variance. It sometimes gives a very low p-value for
a series that is visibly non-stationary. Therefore, it is advisable to use this
test with others.

It is possible to turn a non-stationary time series into a stationary one using
one or more transformations.

- Removal of trend and seasonality. However, we must know the period to decompose
the time series.
- Wildly fluctuating variance can be dampened by a log-transformation.
- Taking differences can remove the auto-correlation structure. This is akin to 
differentiating the series.

# Smoothing techniques

- Simple moving average
- Exponential smoothing. This one is more sensitive to local changes.

Both of them tend to lag (lead in case of negative values) the original time 
series.

- Single exponential smoothing
The formula is
$$
\hat{S}_t = \alpha\hat{S}_{t-1} + \sum_{k = 1}^n(1 - \alpha)^k \hat{S}_{t-k}
$$
The parameter $\alpha$ is chosen to be between $0$ and $1$. It is optimised to
fit the past data.

- Double exponential smoothing
The formula is
$$
\hat{S}_t = \alpha\hat{S}_{t-1} + \sum_{k = 1}^n(1 - \alpha)^k \hat({S}_{t-k} +
b_{t - k})
$$
where
$$
b_t = \beta(S_t - S_{t-1}) + (1 - \beta)b_{t-1}
$$
The parameter $\alpha$ is chosen to be between $0$ and $1$. It is optimised to
fit the past data. The parameter $\beta$ smooths the trend of the series.

- Triple exponential smoothing
The formula is not too clear to me. I would rather just note the take away.
Single exponential smoothing takes into account recent values. Double exponential
smoothing takes into account trend and the triple exponential smoothing admints
seasonality in the prediction.