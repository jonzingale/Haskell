import numpy as np
import matplotlib
import matplotlib.pyplot as plt

from pdb import set_trace as st
from PIL import Image

# convert file to ary
ary = np.loadtxt('dataTenThousandOnes', dtype='float')

# load tmp file as HSV
img = Image.open("tmp.jpeg")
hsv_img = img.convert("HSV")
px = hsv_img.load()

for t in range(0,10**4):
  # value to pixel
  px[t % 100, t // 100] = (int(ary[t]*140), 1000, 100)

hsv_img.show()

# st()
