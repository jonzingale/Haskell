# http://pillow.readthedocs.io/en/3.0.x/
from PIL import Image
import numpy as np
import datetime

size = 1000
window = (750, 750)
testTrace = './Data/savedPlate'
mm = size * np.sqrt(3) # adjust for photo luminosity.

def renderPixel(t, ary): # RGB
  val = ary[t]
  if val <= 0: return((0,0,0))
  else:
    normedV = int((val/mm)*255)
    return(normedV, normedV, normedV)

def renderImage(filename):
  ary = np.loadtxt(filename, dtype='d')
  img = Image.new('RGB',(size, size), 0)
  px = img.load()

  for t in range(0, size**2): # value to pixel
    px[t % size, t // size] = renderPixel(t, ary)

  time = datetime.datetime.now().strftime('%s')
  resized = img.resize(window)
  resized.save('./Images/image_' + time + '.png')
  resized.show()

renderImage(testTrace)
