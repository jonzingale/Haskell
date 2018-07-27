from pdb import set_trace as st
from PIL import Image
import numpy as np

# http://pillow.readthedocs.io/en/3.0.x/
# HSV: (360, 255, 255)

testTrace = './../Tests/dataTestTrace'
Stratified = './../Tests/dataStratified1M'
randos1M = './../Tests/data1M'

def renderImage(filename):
  ary = np.loadtxt(filename, dtype='float')
  size = int(np.sqrt(ary.size))

  # generate HSV image of corresponding size
  img = Image.new('HSV', (size, size), 0)
  px = img.load()

  for t in range(0,size**2): # value to pixel
    # px[t % size, t // size] = (170, 255, int(ary[t]*255)) # actual file LIGHT
    px[t % size, t // size] = (int(ary[t]*200), 255, 255) # actual file HUE

  img.show()

renderImage(testTrace)