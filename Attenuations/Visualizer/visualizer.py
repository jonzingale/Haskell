# http://pillow.readthedocs.io/en/3.0.x/
from PIL import Image
from pdb import set_trace as st
import numpy as np
import datetime

testTrace = './Tests/dataTestTrace'
window = (750, 750)
sqrt3 = np.sqrt(3.0)
mm = 500 * sqrt3
size = 500 # lookup too slow, try by file size.

def time():
  return(datetime.datetime.now().strftime('%s'))

def renderPixel(t, ary): # RGB
  val = ary[t]
  if val == 0: return((0,0,0))
  else:
    normedV = int((val/mm)*255)
    return(normedV, normedV, normedV)

def renderImage(filename):
  ary = np.loadtxt(filename, dtype='d')

  # generate image of corresponding size
  img = Image.new('RGB',(size, size), 0)
  px = img.load()

  for t in range(0, size**2): # value to pixel
    px[t % size, t // size] = renderPixel(t, ary)

  resized = img.resize(window)
  resized.show()

  # resized.save('./Visualizer/niceImage'+time()+'.png')

renderImage(testTrace)

