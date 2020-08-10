#tag Class
Protected Class XPEvaluator
	#tag Method, Flags = &h0
		Sub compile(s as text, globalvalues as dictionary = nil)
		  
		  dim operatorStack() as text
		  dim t as text
		  dim e,fl as text
		  dim ch as text
		  dim negationpossible as boolean
		  dim opprec() as text
		  dim op, op2 as XPOperator
		  dim rv as new Random
		  dim rvi, i, c as int64
		  dim fn as text
		  dim foundhigher as Boolean
		  
		  source = s
		  rpn.RemoveAllRows
		  tokenize s
		  negationpossible = true
		  
		  if UBound(tokens) = -1 then  return
		  
		  for each t in tokens
		    ch = t.Left(1)
		    
		    if ch="-" and negationpossible then
		      t = "-u"
		    end
		    
		    select case ch
		    case "0" to "9"
		      rpn.AddRow t
		      negationpossible = false
		    case "$"
		      rpn.AddRow t
		      negationpossible = false
		    case "@"
		      operatorStack.AddRow t
		      //operatorStack.AddRow "0"
		      negationpossible = true
		    case "("
		      operatorStack.AddRow("(")
		      //operatorStack.AddRow "0"
		      negationpossible = true
		    case ")"
		      do  // not really clear here
		        if ubound(operatorStack)<0 then
		          raise new XPError("Compile missing open paranthesis "+ text.join(rpn," "),21)
		        end
		        e = operatorStack.pop
		        if left(e,2) = "@(" then
		          fn = ":" +e.mid(2)
		          rpn.AddRow fn
		        elseif e <> "(" and e<>"1" and val(e) = 0 then 
		          rpn.AddRow e
		        end
		      loop until left(e,2) = "@(" or e = "(" 
		      negationpossible = false
		    else
		      for each op in operators
		        if t = op.label then
		          foundhigher = true
		          while ubound(operatorStack) > 0 and foundhigher 
		            // compare with stack
		            e = operatorStack.pop
		            if op.associativity ="L" and op.precedence <= val(e) then
		              rpn.AddRow operatorStack.pop
		            elseif op.associativity ="R" and op.precedence < val(e) then
		              rpn.AddRow operatorStack.pop
		            else
		              foundhigher = false
		              operatorStack.AddRow e
		            end
		          wend
		          if op.functionlabel = ":and" then
		            rvi = rv.InRange(50000,80000)
		            rpn.AddRow str(rvi).totext
		            rpn.AddRow ":andleft"
		            operatorStack.AddRow ":andright#"+str(rvi).totext
		          elseif op.functionlabel = ":or" then
		            rvi = rv.InRange(50000,80000)
		            rpn.AddRow str(rvi).totext
		            rpn.AddRow ":orleft"
		            operatorStack.AddRow ":orright#"+str(rvi).totext
		          else
		            operatorStack.AddRow op.functionlabel
		          end
		          operatorStack.AddRow str(op.precedence).ToText
		          negationpossible = true
		          continue for t
		        end
		      next
		      // not found, it is name
		      rpn.AddRow t
		      negationpossible = false
		    end
		  next 
		  s=s
		  while ubound(operatorStack)>=1
		    e = operatorStack.pop
		    e = operatorStack.pop
		    if e<>"@(comma" then
		      rpn.AddRow e.Replace("@(",":")
		    end
		  wend
		  c = rpn.Count-1
		  for i = c downto 0
		    if rpn(i).length > 9 and rpn(i).left(9) = ":andright" then
		      rpn.AddRowat i+1, rpn(i).mid(9)
		      rpn(i) = ":andright"
		    end
		    if rpn(i).length > 8 and rpn(i).left(8) = ":orright" then
		      rpn.AddRowat i+1, rpn(i).mid(8)
		      rpn(i) = ":orright"
		    end
		  next
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor()
		  operators.AddRow new XPOperator("-u", ":neg",1,11,"L")
		  operators.AddRow new XPOperator("not", ":not",1,100,"L")
		  
		  operators.AddRow new XPOperator("/", ":div",2,9,"L")
		  operators.AddRow new XPOperator("*", ":mul",2,9,"L")
		  operators.AddRow new XPOperator("div", ":div",2,9,"L")
		  operators.AddRow new XPOperator("mod", ":mod",2,9,"L")
		  operators.AddRow new XPOperator("+", ":add",2,8,"L")
		  operators.AddRow new XPOperator("-", ":sub",2,8,"L")
		  
		  operators.AddRow new XPOperator(".", ":concat",2,7,"L")
		  
		  operators.AddRow new XPOperator("=", ":eqn",2,6,"L")
		  operators.AddRow new XPOperator("!=", ":nen",2,6,"L")
		  operators.AddRow new XPOperator(">", ":gtn",2,6,"L")
		  operators.AddRow new XPOperator(">=", ":gen",2,6,"L")
		  operators.AddRow new XPOperator("<", ":ltn",2,6,"L")
		  operators.AddRow new XPOperator("<=", ":len",2,6,"L")
		  
		  operators.AddRow new XPOperator("regex", ":regex",2,5,"L")
		  operators.AddRow new XPOperator("==", ":eqs",2,5,"L")
		  operators.AddRow new XPOperator("!==", ":nes",2,5,"L")
		  operators.AddRow new XPOperator(">>", ":gts",2,5,"L")
		  operators.AddRow new XPOperator(">==", ":ges",2,5,"L")
		  operators.AddRow new XPOperator("<<", ":lts",2,5,"L")
		  operators.AddRow new XPOperator("<==", ":les",2,5,"L")
		  
		  operators.AddRow new XPOperator("and", ":and",2,4,"L")
		  operators.AddRow new XPOperator("or", ":or",2,3,"L")
		  operators.AddRow new XPOperator("xor", ":xor",2,2,"L")
		  
		  operators.AddRow new XPOperator(",",":comma",2,1,"L")
		  
		  functions.AddRow new XPNeg
		  functions.AddRow new XPNot
		  
		  functions.AddRow new XPDiv
		  functions.AddRow new XPMul
		  functions.AddRow new XPIdiv
		  functions.AddRow new XPMod
		  functions.AddRow new XPAdd
		  functions.AddRow new XPSub
		  
		  functions.AddRow new XPConcat
		  
		  functions.AddRow new XPEqN
		  functions.AddRow new XPNeN
		  functions.AddRow new XPGtN
		  functions.AddRow new XPGeN
		  functions.AddRow new XPLtN
		  functions.AddRow new XPleN
		  
		  functions.AddRow new XPEqS
		  functions.AddRow new XPNeS
		  functions.AddRow new XPGtS
		  functions.AddRow new XPGeS
		  functions.AddRow new XPLtS
		  functions.AddRow new XPLeS
		  
		  functions.AddRow new XPAnd
		  functions.AddRow new XPOr
		  functions.AddRow new XPXor
		  
		  functions.AddRow new XPComma
		  
		  ' real functions
		  
		  functions.AddRow new XPAbs
		  functions.AddRow new XPCeil
		  functions.AddRow new XPCos
		  functions.AddRow new XpExp
		  functions.AddRow new XPFloor
		  functions.AddRow new XpLn
		  functions.AddRow new XpLog
		  functions.AddRow new XPPow
		  functions.AddRow new XPRnd
		  functions.AddRow new XPRound
		  functions.AddRow new XPSin
		  functions.AddRow new XPSign
		  functions.AddRow new XPSqrt
		  functions.AddRow new XPTan
		  
		  functions.AddRow new XPLength
		  functions.AddRow new XPLower
		  functions.AddRow new XPRegex
		  functions.AddRow new XPRegexReplace
		  functions.AddRow new XPReplace
		  functions.AddRow new XPSubstr
		  functions.AddRow new XPUpper
		  functions.AddRow new XPurltext
		  functions.AddRow new XPpad
		  functions.AddRow new XPtrim
		  
		  functions.AddRow new XPMax
		  functions.AddRow new XPMin
		  
		  
		  functions.AddRow new XPSecondsToSQL
		  functions.AddRow new XPSQLtoSeconds
		  
		  functions.AddRow new XPFormat
		  // functions.AddRow new XPNumberFormat
		  
		  expectedreturn = 1
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(fn as Dictionary)
		  constructor
		  if fn <> nil then
		    for each f as DictionaryEntry in fn
		      functions.AddRow XPCompiledFunction(f.value)
		    next
		  end
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function cText12(d as double) As text
		  
		  
		  dim a as double
		  dim t as text
		  dim s as string
		  
		  a = abs(d)
		  
		  
		  if a > 10e12 then
		    return format(d,"-0.000000000000e").ToText
		  elseif a < 10e-12 then
		    return format(d,"-0.000000000000e").ToText
		  end
		  
		  s = format(d,"-0.############")
		  if len(s)>12 then
		    s = format(round(d*10000000000000)/10000000000000,"-0.############")
		  end
		  if right(s,1)="." then
		    s = mid(s,1,len(s)-1)
		  end
		  t = s.ToText
		  if t = "0.000000000000e+0" then return "0" else return t
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Evaluate(values as dictionary, globals as dictionary = nil, locals as Dictionary = nil) As text
		  
		  dim e,e2 as text
		  dim ch, dummy as text
		  dim f as XPFunction
		  dim found as Boolean
		  dim localdict as new Dictionary
		  dim currentlabel as text
		  dim jump as text
		  dim cond as text
		  dim i,j,c as int64
		  dim aggregatorfirstrun as Text = "aggregatorfirstrun"
		  dim currentindex as text
		  dim result as text
		  
		  redim stack(-1)
		  
		  if UBound(rpn) =-1 then  return ""
		  
		  c = rpn.Count-1
		  
		  for i = 0 to c
		    e = rpn(i)
		    if e="" then continue
		    ch = e.left(1)
		    
		    select case ch
		    case ":"
		      found = false
		      select case e
		      case ":andleft"
		        jump = "#"+stack.pop
		        cond = stack.pop
		        if val(cond)=0 then
		          stack.AddRow "0"
		          j = rpn.IndexOf(jump)
		          if  j > -1 then
		            i = j
		          else
		            raise new XPError("Goto not defined "+currentindex,31)
		          end
		        end
		        found = true
		      case ":andright"
		        cond = stack.pop
		        if val(cond)=0 then
		          stack.AddRow "0"
		        else
		          stack.AddRow "1"
		        end
		        found = true
		        
		      case ":comma"
		        found = true // nop
		        
		      case ":goto"
		        jump = "#"+stack.pop
		        j = rpn.IndexOf(jump)
		        if  j > -1 then
		          i = j
		        else
		          raise new XPError("Goto not defined "+currentindex,31)
		        end
		        found = true
		        found = true
		      case ":gotoifn"
		        jump = "#"+stack.pop
		        cond = stack.pop
		        if cond = "0" then
		          j = rpn.IndexOf(jump)
		          if  j > -1 then
		            i = j
		          else
		            raise new XPError("Goto not defined "+currentindex,31)
		          end
		        end
		        found = true
		      case ":gotoif"
		        jump = "#"+stack.pop
		        cond = stack.pop
		        if cond <> "0" then
		          j = rpn.IndexOf(jump)
		          if  j > -1 then
		            i = j
		          else
		            raise new XPError("Goto not defined "+currentindex,31)
		          end
		        end
		        found = true
		      case ":init" 
		        if localdict.HasKey(aggregatorfirstrun) then
		          if  localdict.value(aggregatorfirstrun) <> "0" then
		            localdict.Value(currentlabel) = stack.pop
		          else
		            dummy = stack.pop
		          end
		        else
		          raise new XPError("Illegal instruction init "+currentindex,31)
		        end
		        found = true
		        
		      case ":orleft"
		        jump = "#"+stack.pop
		        cond = stack.pop
		        if val(cond)<>0 then
		          stack.AddRow "1"
		          j = rpn.IndexOf(jump)
		          if  j > -1 then
		            i = j
		          else
		            raise new XPError("Goto not defined "+currentindex,31)
		          end
		        end
		        found = true
		      case ":orright"
		        cond = stack.pop
		        if val(cond)=0 then
		          stack.AddRow "0"
		        else
		          stack.AddRow "1"
		        end
		        found = true
		      case ":pop"
		        dummy = stack.pop
		        found = true
		      case ":set" 
		        localdict.Value(currentlabel) = stack.pop
		        found = true
		      case ":stackcount"
		        stack.AddRow str(stack.count).ToText
		        found = true
		      else
		        for each f in functions
		          if e = f.label then
		            found = true
		            f.functions = functions
		            f.run(stack)
		            continue for i
		          end
		        next
		      end
		      if not found then
		        raise new XPError("Function not defined "+currentindex,31)
		      end 
		    case "$"
		      stack.AddRow e.mid(1)
		    case "/"
		      currentlabel = e.mid(1)
		    case "#"
		      currentindex = e
		    else
		      if right(e,1) = "^" then
		        e2 = e.left(e.Length-1) // indirection
		        if localdict.HasKey(e2) then
		          e = localdict.value(e2)
		        elseif values.HasKey(e2) then
		          e = values.value(e2)
		        elseif locals <> nil and locals.HasKey(e2) then
		          e = locals.value(e2)
		        elseif globals <> nil and globals.HasKey(e2) then
		          e = globals.value(e2)
		        end
		      end
		      if localdict.HasKey(e) then
		        stack.AddRow localdict.value(e)
		      elseif values.HasKey(e) then
		        stack.AddRow values.value(e)
		      elseif locals <> nil and locals.HasKey(e) then
		        stack.AddRow locals.value(e)
		      elseif globals <> nil and globals.HasKey(e) then
		        stack.AddRow globals.value(e)
		      else
		        if val(e) > 0 then
		          stack.AddRow e
		        elseif e ="0" then
		          stack.AddRow e
		        else // should not happen
		          stack.Addrow "" //stack.AddRow e
		        end
		      end
		    end
		  next
		  
		  
		  if ubound(stack) > expectedreturn -1  then
		    raise new XPError("Evaluate stack not consumed "+text.join(rpn," ")+" "+currentindex,31)
		  end
		  
		  'if ubound(stack) < expectedreturn -1  then
		  'raise new XPError("Stack too small "+currentindex,31)
		  'end
		  
		  result = stack(0)
		  
		  if result.IsNumeric then
		    result = cText12(val(result))
		    //if val(result)<>0 then
		    //result = cText12(val(result))
		  end
		  
		  return result
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Tokenize(s as text)
		  dim i as int64
		  dim c as int64
		  dim ch as text
		  dim state as text
		  dim acc as text
		  
		  ' strings # hex
		  ' functions @ (
		  ' operators 
		  ' names
		  ' (
		  ' )
		  ' ,
		  
		  tokens.RemoveAllRows
		  
		  state = "start"
		  
		  c = s.Length-1
		  
		  for i = 0 to c
		    ch = s.mid(i,1)
		    select case state
		    case "start", "comma"
		      select case ch
		      case """"
		        state = "string"
		        acc = ""
		      case "0" to "9"
		        state = "number"
		        acc = ch
		      case "A" to "Z", "a" to "z","_"
		        state = "name"
		        acc = ch
		      case "("
		        state = "start"
		        tokens.AddRow "("
		      case ")"
		        state = "start"
		        tokens.AddRow ")"
		      case "-","/","*","+","."
		        state = "start"
		        tokens.AddRow ch
		      case "="
		        state ="equal"
		      case "<"
		        state ="lower"
		      case ">"
		        state = "greater"
		      case "!"
		        state = "not"
		      case ","
		        if state = "comma" or ubound(tokens) < 0 then // CSV empty values field,,field
		          tokens.AddRow "$"
		        end if
		        state = "comma"
		        tokens.AddRow ","
		      case " "
		      else
		        raise new XPError("Tokenize unexpected character """+ch+"""",12)
		      end
		    case "string"
		      select case ch
		      case """"
		        state = "string1"
		      else
		        acc = acc + ch
		      end
		    case "string1"
		      select case ch
		      case """"
		        state = "string"
		        acc = acc + """"
		      else
		        state ="start"
		        tokens.AddRow "$" + acc
		        acc = ""
		        i= i-1
		      end
		    case "number"
		      select case ch
		      case "0" to "9", "."
		        acc = acc + ch
		      case "."
		        state = "numberfraction"
		        acc = acc + ch
		      case "e"
		        state = "numberexponent"
		        acc = acc + ch
		      else
		        state ="start"
		        tokens.AddRow acc
		        acc = ""
		        i = i-1
		      end
		    case "numberfraction"
		      select case ch
		      case "0" to "9", "."
		        acc = acc + ch
		      case "e"
		        state = "numberexponent"
		        acc = acc + ch
		      else
		        state ="start"
		        tokens.AddRow acc
		        acc = ""
		        i = i-1
		      end
		    case "numberexponent"
		      select case ch
		      case "0" to "9", "."
		        acc = acc + ch
		      case "-","+"
		        state = "numberexponentnegatif"
		        acc = acc + ch
		      else
		        state ="start"
		        tokens.AddRow acc
		        acc = ""
		        i = i-1
		      end
		    case "numberexponentnegatif"
		      select case ch
		      case "0" to "9", "."
		        acc = acc + ch
		      else
		        state ="start"
		        tokens.AddRow acc
		        acc = ""
		        i = i-1
		      end
		    case "name"
		      select case ch
		      case "A" to "Z", "a" to "z", "0" to "9", "_"
		        acc = acc + ch
		      case "("
		        state ="start"
		        tokens.AddRow "@(" + acc 
		        acc = ""
		      case "^"
		        state ="start"
		        tokens.AddRow acc+"^"
		        acc = ""
		      else
		        state ="start"
		        tokens.AddRow acc
		        acc = ""
		        i = i-1
		      end
		    case "equal"
		      select case ch
		      case "="
		        state ="start"
		        tokens.AddRow "=="
		      else
		        state ="start"
		        tokens.AddRow "="
		        i = i-1
		      end
		    case "lower"
		      select case ch
		      case "="
		        state ="lowerequal"
		      case "<"
		        state ="start"
		        tokens.AddRow "<<"
		      else
		        state ="start"
		        tokens.AddRow "<"
		        i = i-1
		      end
		    case "lowerequal"
		      select case ch
		      case "="
		        state ="start"
		        tokens.AddRow "<=="
		      else
		        state ="start"
		        tokens.AddRow "<="
		        i = i-1
		      end
		    case "greater"
		      select case ch
		      case "="
		        state ="greaterequal"
		      case ">"
		        state ="start"
		        tokens.AddRow ">>"
		      else
		        state ="start"
		        tokens.AddRow ">"
		        i = i-1
		      end
		    case "greaterequal"
		      select case ch
		      case "="
		        state ="start"
		        tokens.AddRow ">=="
		      else
		        state ="start"
		        tokens.AddRow ">="
		        i = i-1
		      end
		    case "not"
		      select case ch
		      case "="
		        state ="notequal"
		      else
		        raise new XPError("Tokenize unexpected character"""+ch+"""",12)
		      end
		    case "notequal"
		      select case ch
		      case "="
		        tokens.AddRow "!=="
		        state ="start"
		      else
		        state ="start"
		        tokens.AddRow "!="
		        i = i-1
		      end
		    else
		      
		    end
		  next
		  
		  select case state
		  case "start"
		  case "string"
		    raise new XPError("Tokenize open string",13)
		  case "string1"
		    tokens.AddRow "$" + acc
		  case "number", "numberfraction", "numberexponent","numberexponentnegatif", "name"
		    tokens.AddRow acc
		  case "comma"
		    tokens.AddRow "$"
		  else
		    raise new XPError("Tokenize unknown state """+state+"""",11)
		  end
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UnitTest()
		  dim v as new dictionary
		  dim t,tc, te as text
		  dim i, c as integer
		  
		  t = "a"
		  te = "55"
		  v.Value(t) = te
		  t = "b"
		  te = "21"
		  v.value(t) = te
		  t = "c"
		  te = "100"
		  v.value(t) = te
		  t = "d"
		  te = "asdf"
		  v.value(t) = te
		  t = "e"
		  te = "jklö"
		  v.value(t) =te
		  
		  dim tests() as text
		  dim comps() as text 
		  dim results() as text
		  
		  tests.AddRow "a+b"
		  comps.AddRow "a b :add"
		  results.AddRow "76"
		  
		  tests.AddRow "5+10"
		  comps.AddRow "5 10 :add"
		  results.AddRow "15"
		  
		  tests.AddRow "5*10"
		  comps.AddRow "5 10 :mul"
		  results.AddRow "50"
		  
		  tests.AddRow "5/10"
		  comps.AddRow "5 10 :div"
		  results.AddRow "0.5"
		  
		  tests.AddRow "2 + 4 +10"
		  comps.AddRow "2 4 :add 10 :add"
		  results.AddRow "16"
		  
		  tests.AddRow "2 - 4 +10"
		  comps.AddRow "2 4 :sub 10 :add"
		  results.AddRow "8"
		  
		  tests.AddRow "2 + 4 -10"
		  comps.AddRow "2 4 :add 10 :sub"
		  results.AddRow "-4"
		  
		  tests.AddRow "2 + 4 * 10"
		  comps.AddRow "2 4 10 :mul :add"
		  results.AddRow "42"
		  
		  tests.AddRow "-5 +10"
		  comps.AddRow "5 :neg 10 :add"
		  results.AddRow "5"
		  
		  tests.AddRow "5 * -10"
		  comps.AddRow "5 10 :neg :mul"
		  results.AddRow "-50"
		  
		  tests.AddRow "5 +-10"
		  comps.AddRow "5 10 :neg :add"
		  results.AddRow "-5"
		  
		  tests.AddRow "2 * 4 +10"
		  comps.AddRow "2 4 :mul 10 :add"
		  results.AddRow "18"
		  
		  tests.AddRow "2 * (4 +10)"
		  comps.AddRow "2 4 10 :add :mul"
		  results.AddRow "28"
		  
		  tests.AddRow "a/c"
		  comps.AddRow "a c :div"
		  results.AddRow "0.55"
		  
		  tests.AddRow "a/c"
		  comps.AddRow "a c :div"
		  results.AddRow "0.55"
		  
		  tests.AddRow "d+e"
		  comps.AddRow "d e :add"
		  results.AddRow "0"
		  
		  tests.AddRow "d.e"
		  comps.AddRow "d e :concat"
		  results.AddRow "asdfjklö"
		  
		  tests.AddRow "sqrt(16)"
		  comps.AddRow "16 :sqrt"
		  results.AddRow "4"
		  
		  tests.AddRow "pow(2,3)"
		  comps.AddRow "2 3 :comma :pow"
		  results.AddRow "8"
		  
		  tests.AddRow "pow(3+1,3)"
		  comps.AddRow "3 1 :add 3 :comma :pow"
		  results.AddRow "64"
		  
		  
		  tests.AddRow "replace(d.e,""sd"",""sbb"")"
		  comps.AddRow "d e :concat $sd :comma $sbb :comma :replace"
		  results.AddRow "asbbfjklö"
		  
		  tests.AddRow "100 * (5+pow(3+1,3)) / 10000"
		  comps.AddRow "100 5 3 1 :add 3 :comma :pow :add :mul 10000 :div"
		  results.AddRow "0.69"
		  
		  tests.addrow " ""lorem ipsum"" . ""RRR"" "
		  comps.AddRow "$lorem ipsum $RRR :concat"
		  results.AddRow "lorem ipsumRRR"
		  
		  tests.AddRow "sqrt(5+4)"
		  comps.AddRow "5 4 :add :sqrt"
		  results.AddRow "3"
		  
		  tests.AddRow "sqrt(5 + 4)"
		  comps.AddRow "5 4 :add :sqrt"
		  results.AddRow "3"
		  
		  tests.AddRow "2 * (3 + 4)"
		  comps.AddRow "2 3 4 :add :mul"
		  results.AddRow "14"
		  
		  'tests.AddRow "2 * 3 +"
		  'comps.AddRow "2 3 :mul :add"
		  'results.AddRow "ERROR: stack <2"
		  '
		  'tests.AddRow " ""lorem ipsum"
		  'comps.AddRow "ERROR: Tokenize open string"
		  'results.AddRow ""
		  
		  tests.AddRow "pow(3+1,3)"
		  comps.AddRow "3 1 :add 3 :comma :pow"
		  results.AddRow "64"
		  
		  tests.AddRow "(67 + 45 - 66 + 2)"
		  comps.AddRow "67 45 :add 66 :sub 2 :add"
		  results.AddRow "48"
		  
		  tests.AddRow "(67 + 2 * 3 - 67 + 2/1 - 7)"
		  comps.AddRow "67 2 3 :mul :add 67 :sub 2 1 :div :add 7 :sub"
		  results.AddRow "1"
		  
		  tests.AddRow "(2) + (17*2-30) * (5)+2 - (8/2)*4"
		  comps.AddRow "2 17 2 :mul 30 :sub 5 :mul :add 2 :add 8 2 :div 4 :mul :sub"
		  results.AddRow "8"
		  
		  'tests.AddRow "(5*7/5) + (23) - 5 * (98-4)/(6*7-42)"
		  'comps.AddRow "5 7 :mul 5 :div 23 :add 5 98 4 :sub :mul 6 7 :mul 42 :sub :div :sub"
		  'results.AddRow "ERROR: div/0"
		  
		  tests.AddRow "(((((5)))))"
		  comps.AddRow "5"
		  results.AddRow "5"
		  
		  tests.AddRow "((((2)) + 4))*((5))"
		  comps.AddRow "2 4 :add 5 :mul"
		  results.AddRow "30"
		  
		  tests.AddRow "550 > 100"
		  comps.AddRow "550 100 :gtn"
		  results.AddRow "1"
		  
		  tests.AddRow "a*b > c"
		  comps.AddRow "a b :mul c :gtn"
		  results.AddRow "1"
		  
		  
		  
		  tests.AddRow "a*d > c"
		  comps.AddRow "a d :mul c :gtn"
		  results.AddRow "0"
		  
		  tests.AddRow "d < e"
		  comps.AddRow "d e :ltn"
		  results.AddRow "0"
		  
		  tests.AddRow "d << e"
		  comps.AddRow "d e :lts"
		  results.AddRow "1"
		  
		  tests.AddRow "-(5)"
		  comps.AddRow "5 :neg"
		  results.AddRow "-5"
		  
		  tests.AddRow "d regex ""s.f"" "
		  comps.AddRow "d $s.f :regex"
		  results.AddRow "1"
		  
		  tests.AddRow "d regex ""s..f"""
		  comps.AddRow "d $s..f :regex"
		  results.AddRow "0"
		  
		  
		  
		  'test('(67 + 2 * 3 - 67 + 2/1 - 7)','',values,'1');    
		  'test('(2) + (17*2-30) * (5)+2 - (8/2)*4','',values,'8');
		  'test('(5*7/5) + (23) - 5 * (98-4)/(6*7-42)','',values,'ERROR division by zero');
		  'test('(((((5)))))','',values,'5');    
		  'test('(( ((2)) + 4))*((5))','',values,'30');    
		  
		  'values,'ERROR unresolved terms');
		  'test("'lorem ipsum' . 'RRR'",'#lorem%20ipsum #RRR concat',values,'lorem ipsumRRR');
		  'test('%a "lorem ipsum * 5','ERROR unmatched quote',values,'');
		  '/*
		  'test('sqrt(5+4 )','5 4 add sqrt',values,'3');
		  'test('sqrt(5 + 4 )','5 4 add sqrt',values,'3');
		  '*/
		  'test('2  * ( 3 +  4)','2 3 4 add mul',values,'14');
		  'test('2  * 3 + ','ERROR infix operator',values,'');
		  'test('2  * (3 + )','ERROR infix operator',values,'');
		  'test('2*(3+4*(6 +8))','2 3 4 6 8 add mul add mul',values,'118');
		  'test('a * b < c and a + b > c','a b mul c lower a b add c greater logand',values,'0');
		  'test('a * b < c or a + b > c','a b mul c lower a b add c greater logor',values,'1');
		  'test('not(1)','1 not',values,'0');
		  '/*
		  'test('max(4 , 3)','4 3 2 max',values,'4');
		  'test('max(4 * 5 , 13)','4 5 mul 13 2 max',values,'20');
		  'test('max(4 , 13 , 6)','4 13 6 3 max',values,'13');
		  'test('max(4,13,6)','4 13 6 3 max',values,'13'); 
		  'test('max(4,-13,6)','4 13 neg 6 3 max',values,'6');
		  'test('max(4, -13,6)','4 13 neg 6 3 max',values,'6');
		  '*/
		  'test('2 * (3  -4)','2 3 4 sub mul',values,'-2');
		  'test('2 * (3  -a)','2 3 a sub mul',values,'2');
		  'test('2 * (3 -4)','2 3 4 sub mul',values,'-2');
		  'test('2 * (3 -a)','2 3 a sub mul',values,'2');
		  'test('"lorem ipsum" reg /o(r+)em/','#lorem%20ipsum #%2Fo%28r%2B%29em%2F regex',values,'1');
		  'test('a * b < c','a b mul c lower',values,'');
		  'test('a * b lw c','a b mul c lower',values,'');
		  '/*
		  'test('sqrt(16)','16 sqrt',values,'4');
		  'test('sqrt(-16)','16 neg sqrt',values,'ERROR sqrt of negative value -16');
		  'test('sqrt(5 + 4)','5 4 add sqrt',values,'3');
		  '*/
		  'test('2 * (3 + 4)','2 3 4 add mul',values,'14');
		  'test('2 * (3 +( 4)','ERROR unbalanced paranthesis (',values,'');
		  'test('2 * (3 + 4)','2 3 4 add mul',values,'14');
		  'test('2 * (3 + 4)* (6 + 8))','ERROR unbalanced paranthesis )',values,'118');
		  'test('2 * (3 + 4) + 5 * (6 + 8)','2 3 4 add mul 5 6 8 add mul add',values,'84');
		  'test('1+2+3+4+5','1 2 add 3 add 4 add 5 add',values,'15');
		  'test('a+b+b+c+c+c','a b add b add c add c add c add',values,'20');
		  'test('12-6','12 6 sub',values,'6');
		  'test('5*a - 2*a','5 a mul 2 a mul sub',values,'6');
		  'test('12*5','12 5 mul',values,'60');
		  'test('12*5t','12 5t mul',values,'ERROR unexpected term 5t');
		  'test('12*t5','12 t5 mul',values,'ERROR unexpected term t5');
		  '
		  '
		  '// lukaszwrobel.pl tests, we only test results
		  '// https://lukaszwrobel.pl/blog/math-parser-part-4-tests/
		  '
		  'test('2 + 3','',values,'5');
		  'test('2 * 3','',values,'6');
		  'test('89','',values,'89');
		  'test('','',values,'');
		  'test('   12        -  8   ','',values,'4');
		  'test('142        -9   ','',values,'133'); 
		  'test('142-9   ','',values,'133'); 
		  'test(' 12*  4','',values,'48');
		  'test('2.5','',values,'2.5');    
		  'test('4*2.5 + 8.5+1.5 / 3.0','',values,'19');    
		  'test('5.0005 + 0.0095','',values,'5.01');    
		  'test('67+2','',values,'69');    
		  'test(' 2-7','',values,'-5');    
		  'test('5*7','',values,'35');    
		  'test('8/4','',values,'2');    
		  'test('2 - 4 + 6 - 1 - 1 - 0 +8','',values,'10');    
		  'test('2 - 4 +6 - 1 - 1 - 0 +8','',values,'10');    
		  'test('2 -4 +6 -1 -1- 0 +8','',values,'10');    
		  'test('1 - 1   + 2   - 2   +  4 - 4 +    6','',values,'6');    
		  'test('1 -1   + 2   - 2   +  4 - 4 +    6','',values,'6');    
		  'test(' 2*3 - 4*5 + 6/3 ','',values,'-12');
		  'test('2*3*4/8 -   5/2*4 +  6 + 0/3   ','',values,'-1');
		  'test('10/4','',values,'2.5');    
		  'test('5/3','',values,'1.6666666666666667');    
		  'test('3 + 8/5 - 1 - 2*5','',values,'-6.4000000');    
		  'test('3 + 8/5 -1 -2*5','3 8 5 div add 1 sub 2 5 mul sub',values,'-6.4000000');    
		  'test('7 & 2','',values,'ERROR unexpected term &');    
		  'test('  %','%',values,'ERROR unexpected term %');
		  'test(' 5 + + 6','ERROR infix operator',values,'');    
		  'test(' -5 + 2','',values,'-3');
		  '//test('-(5)','',values,'-5');    
		  'test('5/0','',values,'ERROR division by zero');
		  'test(' 2 - 1 + 14/0 + 7','',values,'ERROR division by zero');    
		  'test('(2)','',values,'2');    
		  'test('(5 + 2*3 - 1 + 7 * 8)','',values,'66');    
		  'test('(67 + 2 * 3 - 67 + 2/1 - 7)','',values,'1');    
		  'test('(2) + (17*2-30) * (5)+2 - (8/2)*4','',values,'8');
		  'test('(5*7/5) + (23) - 5 * (98-4)/(6*7-42)','',values,'ERROR division by zero');
		  'test('(((((5)))))','',values,'5');    
		  'test('(( ((2)) + 4))*((5))','',values,'30');    
		  'test('(((((4))))','ERROR unbalanced paranthesis (',values,'5');
		  'test('((2)) * ((3','ERROR unbalanced paranthesis (',values,'5');
		  'test('((9)) * ((1)','ERROR unbalanced paranthesis (',values,'5');
		  
		  
		  
		  c = ubound(tests)
		  
		  for i = 0 to c
		    tc = ""
		    te = ""
		    t =tests(i)
		    try
		      compile(t)
		      tc = Text.join(rpn," ")
		    catch err as XPError
		      tc  = "ERROR: "+err.Message.toText
		      redim rpn(-1)
		    end
		    
		    if tc<>comps(i) then raise new XPError("Unit Test compilation failed",41) 
		    try
		      te = evaluate(v)
		    catch err as XPError
		      te  = "ERROR: "+err.Message.ToText
		    end
		    if te <> results(i) then raise new XPError("Unit Test evaluation failed",42)
		  next
		  
		  'app.beep2
		  
		  
		  Exception err
		    break
		    
		    
		    
		    
		    
		    
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		expectedreturn As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		functions() As XPFunction
	#tag EndProperty

	#tag Property, Flags = &h0
		operators() As XPOperator
	#tag EndProperty

	#tag Property, Flags = &h0
		rpn() As text
	#tag EndProperty

	#tag Property, Flags = &h0
		source As text
	#tag EndProperty

	#tag Property, Flags = &h0
		stack() As text
	#tag EndProperty

	#tag Property, Flags = &h0
		tokens() As text
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="source"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="text"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="expectedreturn"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
