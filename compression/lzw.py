test = 'TOBEORNOTTOBEORTOBEORNOT#'

# I cheated and used the ascii table as the initial dictionary and then built my string using characters above 255
# It's super-inefficient for a number of reasons:
# * checking string lengths everywhere
# * no bitmaps, so uses way more memory than necessary
#
# On the other hand:
# * lernt me some more algorithms
# * done in less than an hour
# * result is printable as characters 😀

def lzw(input_string):
  cur_seq = ''
  rtn = ''
  seq_dict = []
  i = 0
  data_length = len(input_string)

  while i < data_length:
    cur_match = ''

    for code, match in enumerate(seq_dict):
      match_size = len(match)
      segment = input_string[i:(i + match_size)]
      if segment == match and match_size > len(cur_match):
        cur_match = match
      if (i + match_size) > data_length:
        break

    if cur_match:
      rtn += chr(seq_dict.index(cur_match) + 256)

      i += len(cur_match)
      if i < data_length: # seq_dict only if we have more to seq
        seq_dict.append(cur_match + input_string[i])

    else:
      seq_dict.append(input_string[i:i+2])
      rtn += input_string[i]
      i += 1

  return rtn

print(lzw(test))
