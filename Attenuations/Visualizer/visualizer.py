from pdb import set_trace as st
from PIL import Image
import numpy as np

# HSV: (360, 255, 255)

# convert file to ary
ary = np.loadtxt('./../Tests/stratified1M', dtype='float')
size = int(np.sqrt(ary.size))

# generate HSV image of corresponding size
img = Image.new('HSV', (size, size), 0)
px = img.load()

for t in range(0,size**2): # value to pixel
  px[t % size, t // size] = (170, 255, int(ary[t]*255)) # actual file LIGHT
  # px[t % 100, t // 100] = (int(ary[t]*200), 255, 255) # actual file HUE
  # px[t % 100, t // 100] = (int(ary[t]*170), 255, 255) # nice colors

img.show()
