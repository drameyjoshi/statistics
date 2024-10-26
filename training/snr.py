import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

plt.ion()

rng = np.random.default_rng()
N = 1000
x = np.linspace(-6, 6, N)
s = np.exp(-x**2)/np.sqrt(2 * np.pi)
n = rng.normal(loc=0, scale=1, size=N)
t = s + n
sns.lineplot(x=x, y=t).set_title('Noisy signal')
