function split(src as string, del as string, res() as string) as integer
  const MAXDELIMITERS = 256
  dim as integer char, d, i, s, l
  dim as integer delpos(0 to MAXDELIMITERS + 1)
  char = asc(del)
  '' find all delimiters
  d = 0
  delpos(d) = 0
  for i = 0 to len(src) - 1
    if src[i] = char then
      d += 1
      if d > MAXDELIMITERS then
        exit for
      end if
      delpos(d) = i + 1
    end if
  next
  'add 2020-02-01
  if src = "" then return 0
  '' allocate the result array
  redim res(0 to d)
  if d = 0  then
    res(0) = src
    return 1
  end if
  '' copy strings
  delpos(d + 1) = len(src) + 1
  for i = 0 to d
    s = delpos(i) + 1
    l = delpos(i + 1) - s
    if l > 0 then
      res(i) = mid(src, s, l)
    end if
  next
  function = d + 1
end function
