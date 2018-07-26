import numpy as np
from PIL import Image

from pdb import set_trace as st

# HSV: (360, 255, 255)

# convert file to ary
ary = np.loadtxt('datastratifiedArray10K', dtype='float')

# load tmp file as HSV
img = Image.open("tmp.jpeg").convert("HSV")
px = img.load()

for t in range(0,10**4): # value to pixel
  px[t % 100, t // 100] = (int(ary[t]*200), 255, 255) # actual file
  # px[t % 100, t // 100] = (int(ary[t]*170), 255, 255) # nice colors

img.show()
