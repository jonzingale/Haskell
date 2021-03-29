class BWString:
  def __init__(self, body, terminator=chr(0x263A)):
    self.body = body
    self.terminator = terminator

    self.transformed = terminator in body

    if terminator in body.replace(terminator, '', 1):
      raise Exception(
        f"chr('{ord(terminator)}') " \
        "is reserved for internal use and can only appear " \
        "once to indicate a transform's end-of-string"
      )

    if (len(terminator) > 1):
      raise Exception("multi-character terminators are not allowed")

    if not(self.transformed):
      self.body += terminator

    self.length = len(self.body)

  def __str__(self):
    return self.body

  def __repr__(self):
    return '{}'.format(self)

  def transform(self):
    if (self.transformed): raise Exception("String is already transformed")

    rots = [*range(1, self.length + 1)] # list of integers (rotation)
    rots.sort(key=lambda i: self.body[i:] + self.body[:i])

    rtn = ''
    for rot in rots:
      rtn += self.body[rot - 1]

    return rtn

  def inverse(self):
    if not(self.transformed): raise Exception("String is not transformed")

    rots = [''] * self.length # list of strings (to prepend & sort)
    for i in range(self.length):
      rots = [ch + rots[i] for i, ch in enumerate(self.body)]
      rots.sort()

    rtn, = (rot for rot in rots if rot[-1] == self.terminator)
    return rtn[:-1]

test = 'SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES'
print(test)

bw = BWString(test)
testTransform = bw.transform()
print(testTransform)

bw = BWString(testTransform)
testInverse = bw.inverse()
print(testInverse)

