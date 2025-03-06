import numpy as np
import scipy.stats as scstat

rng = np.random.default_rng(18111848)
size = 1000
U = rng.uniform(low=0, high=1, size=size)

beta = 2
X = [-beta * np.log(1 - u) for u in U]
E = rng.exponential(scale=beta, size=size)
result = scstat.ks_2samp(X, E, alternative='two-sided')
H0 = 'X has exponential distribution with parameter {0}.'.format(beta)
if result.pvalue < 0.05:
    print('Reject H0: {0}'.format(H0))
else:
    print('Fail to reject H0: {0}'.format(H0))
