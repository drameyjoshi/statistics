import numpy as np
import matplotlib.pyplot as plt

rng = np.random.default_rng()
size = 10000
X = rng.normal(loc=0, scale=1, size=size)
Y = np.exp(X)

nbins = 50
Y_hist = plt.hist(Y, bins=nbins, density=True, label='Data')
max_Y = Y_hist[1][nbins]

y = np.linspace(0.1, max_Y, 200)
f = np.exp(-np.log(y)**2/2)/y/np.sqrt(2 * np.pi)
plt.plot(y, f, label='Theoretical pdf')
plt.legend()
plt.title('Log-normal density')
plt.xlabel(r'$y$')
plt.ylabel(r'$f_Y$')
plt.savefig('c2p13.png')
