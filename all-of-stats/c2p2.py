import numpy as np
import matplotlib.pyplot as plt
import matplotlib

x = np.array([1, 2, 3, 5])
f = np.array([0, 0.1, 0.1, 0.8])
F = np.cumsum(f)

fig, ax = plt.subplots()
ax.plot(x, F)
xy = np.array([np.concatenate([x, np.array([5])]),
               np.concatenate([F, np.array([0])])]).T               
ax.add_patch(matplotlib.patches.Polygon(xy, closed=False))
ax.set_xlabel(r'$x$')
ax.set_ylabel(r'$P(x)$')
ax.set_title('Cumulative distribution function')
plt.savefig('c2p2.png')
