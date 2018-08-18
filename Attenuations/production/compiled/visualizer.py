# http://pillow.readthedocs.io/en/3.0.x/
from PIL import Image
import numpy as np
import datetime
import sys

size = 1000
window = (750, 750)
testTrace = './Data/savedPlate'
time = datetime.datetime.now().strftime('%s')
savedStr = './Images/image_' + time + '.png'
checkSumMsg = "Test Trace Fails CheckSum: %.9f instead of %.9f"
checkSumVal = 14365.942363022965

def checkSum(ary):
  sumAry = sum(ary)
  if (len(sys.argv) == 1) and (sumAry != checkSumVal):
    print(checkSumMsg % (sumAry, checkSumVal))

def renderPixel(t, ary, mm): # RGB
  val = ary[t]
  if val <= 0: return((0,0,0))
  else:
    normedV = 255 * (1 - int(val/mm))
    return(normedV, normedV, normedV)

def renderImage(filename):
  ary = np.loadtxt(filename, dtype='d')
  img = Image.new('RGB',(size, size), 0)
  px = img.load()
  checkSum(ary)

  mm = np.amax(ary) # normalize photo luminosity.
  for t in range(0, size**2): # value to pixel
    px[t % size, t // size] = renderPixel(t, ary, mm)

  resized = img.resize(window)
  resized.save(savedStr)
  resized.show()


renderImage(testTrace)
