local kLPar = '('
local kRPar = ')'
local kQuote = "'"
local kNil = { tag = 'nil', data = 'nil' }


function safeCar(obj)
  if obj.tag == 'cons' then
    return obj.car
  end
  return kNil
end

function safeCdr(obj)
  if obj.tag == 'cons' then
    return obj.cdr
  end
  return kNil
end


function makeError(str)
  return { tag = 'error', data = str }
end

local sym_table = {}
function makeSym(str)
  if str == 'nil' then
    return kNil
  end
  local s = sym_table[str]
  if not(s) then
    s = { tag = 'sym', data = str }
    sym_table[str] = s
  end
  return s
end

local sym_t = makeSym('t')
local sym_quote = makeSym('quote')
local sym_if = makeSym('if')
local sym_lambda = makeSym('lambda')
local sym_defun = makeSym('defun')
local sym_setq = makeSym('setq')
local sym_loop = makeSym('loop')
local sym_return = makeSym('return')
local loop_val = kNil

function makeNum(num)
  return { tag = 'num', data = num }
end

function makeCons(a, d)
  return { tag = 'cons', car = a, cdr = d }
end

function makeSubr(fn)
  return { tag = 'subr', data = fn }
end

function makeExpr(args, env)
  return { tag = 'expr',
           args = safeCar(args),
           body = safeCdr(args),
           env = env }
end


function nreverse(lst)
  local ret = kNil
  while lst.tag == 'cons' do
    local tmp = lst.cdr
    lst.cdr = ret
    ret = lst
    lst = tmp
  end
  return ret
end

function pairlis(lst1, lst2)
  local ret = kNil
  while lst1.tag == 'cons' and lst2.tag == 'cons' do
    ret = makeCons(makeCons(lst1.car, lst2.car), ret)
    lst1 = lst1.cdr
    lst2 = lst2.cdr
  end
  return nreverse(ret)
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
  local next = ''
  for i = 1, string.len(str) do
    if isDelimiter(string.sub(str, i, i)) then
      next = string.sub(str, i)
      str = string.sub(str, 1, i - 1)
      break
    end
  end
  return makeNumOrSym(str), next
end

local readList = nil

function read(str)
  local str = skipSpaces(str)
  if str == '' then
    return makeError('empty input'), ''
  else
    c = string.sub(str, 1, 1)
    if c == kRPar then
      return makeError(string.format('invalid syntax: %s', str)), ''
    elseif c == kLPar then
      return readList(string.sub(str, 2))
    elseif c == kQuote then
      local elm, next = read(string.sub(str, 2))
      return makeCons(sym_quote, makeCons(elm, kNil)), next
    else
      return readAtom(str)
    end
  end
  return str
end

readList = function (str)
  local ret = kNil
  while true do
    str = skipSpaces(str)
    if str == '' then
      return makeError('unfinished parenthesis'), ''
    elseif string.sub(str, 1, 1) == kRPar then
      break
    end
    local elm, next = read(str)
    if elm.tag == 'error' then
      return elm
    end
    ret = makeCons(elm, ret)
    str = next
  end
  return nreverse(ret), string.sub(str, 2)
end

local printList = nil

function printObj(obj)
  if type(obj) ~= 'table' then
    return string.format('non-lisp object: %s', obj)
  end
  if obj.tag == 'num' or obj.tag == 'sym' or obj.tag == 'nil' then
    return tostring(obj.data)
  elseif obj.tag == 'error' then
    return string.format('<error: %s>', obj.data)
  elseif obj.tag == 'cons' then
    return printList(obj)
  elseif obj.tag == 'subr' then
    return '<subr>'
  elseif obj.tag == 'expr' then
    return '<expr>'
  end
  return string.format('unknown tag: %s', obj.tag)
end

printList = function (obj)
  local ret = ''
  local first = true
  while obj.tag == 'cons' do
    if first then
      ret = printObj(obj.car)
      first = false
    else
      ret = string.format('%s %s', ret, printObj(obj.car))
    end
    obj = obj.cdr
  end
  if obj.tag == 'nil' then
    ret = string.format('(%s)', ret)
  else
    ret = string.format('(%s . %s)', ret, printObj(obj))
  end
  return ret
end


function findVar(sym, env)
  while env.tag == 'cons' do
    alist = env.car
    while alist.tag == 'cons' do
      if alist.car.car == sym then
        return alist.car
      end
      alist = alist.cdr
    end
    env = env.cdr
  end
  return kNil
end

local g_env = makeCons(kNil, kNil)

function addToEnv(sym, val, env)
  env.car = makeCons(makeCons(sym, val), env.car)
end


local evlis = nil
local apply = nil
local loop = nil

function eval(obj, env)
  if obj.tag == 'nil' or obj.tag == 'num' or obj.tag == 'error' then
    return obj
  elseif obj.tag == 'sym' then
    local bind = findVar(obj, env)
    if bind == kNil then
       return makeError(string.format('%s has no value', obj.data))
    end
    return bind.cdr
  end

  local op = safeCar(obj)
  local args = safeCdr(obj)
  if op == sym_quote then
    return safeCar(args)
  elseif op == sym_if then
    local c = eval(safeCar(args), env)
    if c.tag == 'error' then
      return c
    elseif c == kNil then
      return eval(safeCar(safeCdr(safeCdr(args))), env)
    end
    return eval(safeCar(safeCdr(args)), env)
  elseif op == sym_lambda then
    return makeExpr(args, env)
  elseif op == sym_defun then
    local expr = makeExpr(safeCdr(args), env)
    local sym = safeCar(args)
    addToEnv(sym, expr, g_env)
    return sym
  elseif op == sym_setq then
    local val = eval(safeCar(safeCdr(args)), env)
    if val.tag == 'error' then
      return val
    end
    local sym = safeCar(args)
    local bind = findVar(sym, env)
    if bind == kNil then
      addToEnv(sym, val, g_env)
    else
      bind.cdr = val
    end
    return val
  elseif op == sym_loop then
    return loop(args, env)
  elseif op == sym_return then
    loop_val = eval(safeCar(args), env)
    return makeError('')
  end
  return apply(eval(op, env), evlis(args, env), env)
end

evlis = function (lst, env)
  local ret = kNil
  while lst.tag == 'cons' do
    elm = eval(lst.car, env)
    if elm.tag == 'error' then
      return elm
    end
    ret = makeCons(elm, ret)
    lst = lst.cdr
  end
  return nreverse(ret)
end

function progn(body, env)
  local ret = kNil
  while body.tag == 'cons' do
    ret = eval(body.car, env)
    if ret.tag == 'error' then
      return ret
    end
    body = body.cdr
  end
  return ret
end

loop = function (body, env)
  while true do
    local ret = progn(body, env)
    if ret.tag == 'error' then
      if ret.data == '' then
        return loop_val
      end
      return ret
    end
  end
end

apply = function (fn, args, env)
  if fn.tag == 'error' then
    return fn
  elseif args.tag == 'error' then
    return args
  elseif fn.tag == 'subr' then
    return fn.data(args)
  elseif fn.tag == 'expr' then
    return progn(fn.body, makeCons(pairlis(fn.args, args), fn.env))
  end
  return makeError('noimpl')
end


function subrCar(args)
  return safeCar(safeCar(args))
end

function subrCdr(args)
  return safeCdr(safeCar(args))
end

function subrCons(args)
  return makeCons(safeCar(args), safeCar(safeCdr(args)))
end

function subrEq(args)
  local x = safeCar(args)
  local y = safeCar(safeCdr(args))
  if x.tag == 'num' and y.tag == 'num' then
    if x.data == y.data then
      return sym_t
    end
    return kNil
  elseif x == y then
    return sym_t
  end
  return kNil
end

function subrAtom(args)
  if safeCar(args).tag == 'cons' then
    return kNil
  end
  return sym_t
end

function subrNumberp(args)
  if safeCar(args).tag == 'num' then
    return sym_t
  end
  return kNil
end

function subrSymbolp(args)
  if safeCar(args).tag == 'sym' then
    return sym_t
  end
  return kNil
end

function subrAddOrMul(fn, init_val)
  return function (args)
    ret = init_val
    while args.tag == 'cons' do
      if args.car.tag ~= 'num' then
        return makeError('wrong type')
      end
      ret = fn(ret, args.car.data)
      args = args.cdr
    end
    return makeNum(ret)
  end
end
local subrAdd = subrAddOrMul(function(x,y) return x + y end, 0)
local subrMul = subrAddOrMul(function(x,y) return x * y end, 1)

function subrSubOrDivOrMod(fn)
  return function (args)
    local x = safeCar(args)
    local y = safeCar(safeCdr(args))
    if x.tag ~= 'num' or y.tag ~= 'num' then
      return makeError('wrong type')
    end
    return makeNum(fn(x.data, y.data))
  end
end
local subrSub = subrSubOrDivOrMod(function(x,y) return x - y end)
local subrDiv = subrSubOrDivOrMod(function(x,y) return x / y end)
local subrMod = subrSubOrDivOrMod(function(x,y) return x % y end)


addToEnv(makeSym('car'), makeSubr(subrCar), g_env)
addToEnv(makeSym('cdr'), makeSubr(subrCdr), g_env)
addToEnv(makeSym('cons'), makeSubr(subrCons), g_env)
addToEnv(makeSym('eq'), makeSubr(subrEq), g_env)
addToEnv(makeSym('atom'), makeSubr(subrAtom), g_env)
addToEnv(makeSym('numberp'), makeSubr(subrNumberp), g_env)
addToEnv(makeSym('symbolp'), makeSubr(subrSymbolp), g_env)
addToEnv(makeSym('+'), makeSubr(subrAdd), g_env)
addToEnv(makeSym('*'), makeSubr(subrMul), g_env)
addToEnv(makeSym('-'), makeSubr(subrSub), g_env)
addToEnv(makeSym('/'), makeSubr(subrDiv), g_env)
addToEnv(makeSym('mod'), makeSubr(subrMod), g_env)
addToEnv(sym_t, sym_t, g_env)

while true do
  io.write('> ')
  x = io.read()
  if x then
    print(printObj(eval(read(x), g_env)))
  else
    break
  end
end