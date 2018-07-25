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


# Take a look at using PIL or Pillow
# from __future__ import print_function
# import os, sys
# from PIL import Image

# for infile in sys.argv[1:]:
#     f, e = os.path.splitext(infile)
#     outfile = f + ".jpg"
#     if infile != outfile:
#         try:
#             Image.open(infile).save(outfile)
#         except IOError:
#             print("cannot convert", infile)