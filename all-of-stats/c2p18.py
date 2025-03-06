import numpy as np
import scipy.stats as scstat

mu = 2
sigma_sq = 16

a1 = np.round(scstat.norm.cdf(7, 2, 4), 3)
print('P(X < 7) = {0}'.format(a1))

a2 = np.round(scstat.norm.cdf(2, 2, 4), 3)
print('P(X > -2) = {0}'.format(a2))

a3 = np.round(scstat.norm.ppf(0.5, 2, 4), 3)
print('x| P(X > x) = 0.5 = {0}'.format(a3))

a4 = np.round(scstat.norm.cdf(4, 2, 4) -
              scstat.norm.cdf(0, 2, 4), 3)
print('P(0 < X < 4) = {0}'.format(a4))

a5 = np.round(scstat.norm.ppf(0.5, 2, 4), 6)
print('x such that P(|X| >= |x|) = {0}'.format(a5))
