import matplotlib.pyplot as plt

z=2
plt.vlines(x=1, ymin=0, ymax=1, linestyle='dotted')
plt.vlines(x=0, ymin=0, ymax=1, color='black')
plt.vlines(x=1/z, ymin=0, ymax=1, linestyle='dashed')
plt.hlines(y=1, xmin=0, xmax=1, linestyle='dotted')
plt.hlines(y=0, xmin=0, xmax=1, color='black')
plt.axline(xy1=(0, 0), slope=z)
plt.axline(xy1=(0, 0), slope=1/z)

jitter = -0.05
plt.text(x=jitter, y=0, s='O')
plt.text(x=1/z - jitter/2, y=1 + jitter, s='A')
plt.text(x=1 + jitter, y=1/z, s='B')
plt.text(x=1 + jitter/2, y=-jitter, s='C')
plt.text(x=1 + jitter, y=1+jitter, s='D')
plt.text(x=1/z - jitter/2, y=-jitter, s='E')

plt.title(r'$Z \sim X/Y$')
plt.savefig('c2p20.png')
