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
\hat{S}_t = \alpha\hat{S}_{t-1} + \sum_{k = 1}^n(1 - \alpha)^k (\hat{S}_{t-k} +
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

Moral of the story:

- If the data has no trend and seasonality then single exponential smoothing 
suffices
- If the data has only trend but no seasonality then use double exponential 
smoothing.
- If the data has trend and seasonality use triple exponential smoothing.

# ARMA models

- AR stands for auto-regressive. AR models predict the next value based on the
past values.
- MA stands for moving average. MA models predict the value based on past 
forecast errors.
- ARMA models use past values and forecast errors. Also called Box-Jenkins 
approach.
- An AR(p) model uses that last $p$ values to predict the next value. It is
defined as
$$
\hat{X}_t = \sum_{i=1}^q\phi_i X_{t-i} + \omega_t,
$$
where $\omega_t$ is the estimation error and $\phi_i$ are to be estimated by
minimising the prediction error.
- An MA(q) model uses the last $q$ _errors_ to predict the next values. If 
$\omega_1, \ldots, \omega_q$ are the past $q$ errors then the MA(q) model is
$$
\hat{X}_t = \sum_{i=1}^q\theta_i\omega_{t-i} + \omega_t,
$$
where $\theta_1, \ldots, \theta_p$ are the parameters to be estimated by 
minimising the prediction error.
- An ARMA(p, q) model has the form
$$
\hat{X}_t = \sum_{i=1}^q\phi_i X_{t-i} + \sum_{i=1}^q\theta_i\omega_{t-i} + 
\omega_t,
$$
where the terms have the same meanings as before. Note that the error term
$\omega_t$ is common to AR and MA parts of the model. It links the two parts 
providing a feedback to each one.
- ARMA models work only with stationary time series.
- Should have at least 100 observations.
- The parameters $p$ and $q$ of the ARMA model can be found using:
    - ACF and PACF plots.
    - Hyperparameter search.
- To find $p$, plot the PACF and choose $p$ to be the lag beyond which PACF is
insignificant.
- To find $q$, plot the ACF and choose $q$ to be the lag beyond with the ACF is
insignificant.
- Try not to use too large $p$ or $q$.
- The ACF plot can also be used to determine which type of model should be used.
    - Use AR model if the ACF decays to zero exponentially.
    - Use AR model if the ACF alternates sign and decays.
    - Use MA model if there are one or two spikes but the rest are zero.
    - Use ARMA model if it decays after a few lags.
    - If all lags are zero, the data is probably random.
    - If there is a spike at a high lag, there is perhaps a seasonal component.
    - If there are no lags, there is perhaps a trend.
- Like other modelling algorithms, the fit it good if the residuals are gaussian.