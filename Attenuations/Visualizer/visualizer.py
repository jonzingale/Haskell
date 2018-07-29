# http://pillow.readthedocs.io/en/3.0.x/
from PIL import Image
import numpy as np

testTrace = './Tests/dataTestTrace'

def renderPixel(t, ary):
  val = ary[t]
  mm = 95
  # mm = max(ary) # max val
  if val == 0: return((0,0,0))
  else: # HSV: (360, 255, 255)
    normedV = int((ary[t]/mm)*240)
    # return(normedV, 255, 255) # HUE
    # return((170, 255, int(ary[t]*4))) # LIGHT
    return(0, 0, normedV) # BLACK AND WHITE

def renderImage(filename):
  ary = np.loadtxt(filename, dtype='float')
  size = int(np.sqrt(ary.size))

  # generate HSV image of corresponding size
  img = Image.new('HSV', (size, size), 0)
  px = img.load()

  for t in range(0,size**2): # value to pixel
    px[t % size, t // size] = renderPixel(t, ary)

  resized = img.resize((800, 800))
  resized.show()

renderImage(testTrace)