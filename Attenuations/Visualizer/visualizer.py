from pdb import set_trace as st
from PIL import Image
import numpy as np

# http://pillow.readthedocs.io/en/3.0.x/
# HSV: (360, 255, 255)

testTrace = './../Tests/dataTestTrace'

Stratified = './../Tests/dataStratifiedArray3D'
randos1M = './../Tests/data1M'

def renderPixel(t, ary):
  val = ary[t]
  if val == 0:
    return((0,0,0))
  else:
    # return((int(ary[t]*50), 255, 255))
    return((int((ary[t])**1.4), 255, 255))

def renderImage(filename):
  ary = np.loadtxt(filename, dtype='float')
  size = int(np.sqrt(ary.size))

  # generate HSV image of corresponding size
  img = Image.new('HSV', (size, size), 0)
  px = img.load()

  for t in range(0,size**2): # value to pixel
    # px[t % size, t // size] = (170, 255, int(ary[t]*255)) # actual file LIGHT
    px[t % size, t // size] = renderPixel(t, ary) # actual file HUE

  img.show()

renderImage(testTrace)