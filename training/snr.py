import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

plt.ion()

rng = np.random.default_rng()
N = 1000

# Pure signal
x = np.linspace(-3, 3, N)
s = np.exp(-x**2)/np.sqrt(2 * np.pi)

# Signa; corrupted with noise
t = s + 0.25 * rng.normal(loc=0, scale=1, size=N)

# Averaging a large number of noisy signals
n_samples = 100
result = np.zeros(N)
for n in range(n_samples):
    result += (s + 0.25 * rng.normal(loc=0, scale=1, size=N))

result /= n_samples

# Plotting the results.
fig, ax = plt.subplots(nrows=1, ncols=3, sharey=True)
ax[0].plot(x, s, label='Pure')
ax[0].legend(loc='upper right')
ax[1].plot(x, t, label='Noisy')
ax[1].legend(loc='upper right')
ax[2].plot(x, result, label='Averaged')
ax[2].legend(loc='upper right')
fig.suptitle('Treantment of noisy signals.')
