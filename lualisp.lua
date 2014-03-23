local kLPar = '('
local kRPar = ')'
local kQuote = "'"


function makeError(str)
  return { tag = 'error', data = str }
end

local sym_table = {}
function makeSym(str)
  local s = sym_table[str]
  if not(s) then
    s = { tag = 'sym', data = str }
    sym_table[str] = s
  end
  return s
end

function makeNum(num)
  return { tag = 'num', data = num }
end

function isSpace(c)
  return c == ' ' or c == '\t' or c == '\r' or c == '\n'
end


function isDelimiter(c)
  return c == kLPar or c == kRPar or c == kQuote or isSpace(c)
end

function skipSpaces(str)
  for i = 1, string.len(str) do
    if not(isSpace(string.sub(str, i, i))) then
      return string.sub(str, i)
    end
  end
  return ''
end

function isNumChar(c)
  local zero = string.byte('0', 1, 1)
  local nine = string.byte('9', 1, 1)
  return zero <=  string.byte(c, 1, 1) and string.byte(c, 1, 1) <= nine
end

function toNum(c)
  return string.byte(c, 1, 1) - string.byte('0', 1, 1)
end

function makeNumOrSym(str)
  local i = 1
  local sign = 1
  if string.sub(str, 1, 1) == '-' then
    sign = -1
    i = 2
  end
  local is_num = false
  local num = 0
  for j = i, string.len(str) do
    c = string.sub(str, j, j)
    if isNumChar(c) then
      num = num * 10 + toNum(c)
      is_num = true
    else
      is_num = false
      break
    end
  end
  if is_num then
    return makeNum(num * sign)
  end
  return makeSym(str)
end

function readAtom(str)
  for i = 1, string.len(str) do
    if isDelimiter(string.sub(str, i, i)) then
      str = string.sub(str, 1, i - 1)
      break
    end
  end
  return makeNumOrSym(str)
end

function read(str)
  str = skipSpaces(str)
  if str == '' then
    return makeError('empty input')
  else
    c = string.sub(str, 1, 1)
    if c == kRPar then
      return makeError(string.format('invalid syntax: %s', str))
    elseif c == kLPar then
      return makeError('noimpl')
    elseif c == kQuote then
      return makeError('noimpl')
    else
      return readAtom(str)
    end
  end
  return str
end

function printObj(obj)
  if type(obj) ~= 'table' then
    return string.format('non-lisp object: %s', obj)
  end
  if obj.tag == 'num' or obj.tag == 'sym' then
    return tostring(obj.data)
  elseif obj.tag == 'error' then
    return string.format('<error: %s>', obj.data)
  end
  return string.format('unknown tag: %s', obj.tag)
end

while true do
  x = io.read()
  if x then
    print(printObj(read(x)))
  else
    break
  end
end