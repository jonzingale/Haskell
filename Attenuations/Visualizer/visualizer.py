import numpy as np
import matplotlib
import matplotlib.pyplot as plt

from pdb import set_trace as st

# convert file to ary
ary = np.loadtxt('dataTenThousandOnes', dtype='float')

# pxl to plot
xs = [t % 10 for t in range(0,10**2)]
ys = [t //10 for t in range(0,10**2)]

plt.plot(xs, ys, 'ro')
plt.axis([-1, 10, , 10])

plt.show()

# st()