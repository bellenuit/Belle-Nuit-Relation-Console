#tag Class
Protected Class Relation
	#tag Method, Flags = &h0
		Sub AddColumn(s as Text)
		  if not validName(s) then
		    raise new RelationError("Invalid name",102)
		  end
		  if header.IndexOf(s) = -1 then
		    header.AddRow s
		  end
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Arity() As Integer
		  Return header.Count
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Assert(t as text) As boolean
		  dim list() as Text
		  dim s as text
		  
		  list = t.split(" ")
		  
		  if  list.count < 2 then
		    raise new RelationError("Assert missing expression",401)
		  end
		  
		  s = list(0)
		  list.Remove 00
		  
		  return assert(s,text.join(list," "))
		  
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Assert(method as Text, expression as text) As boolean
		  
		  dim xp as new XPEvaluator
		  dim d as dictionary
		  dim test as dictionary
		  dim s as text
		  dim list() as text
		  dim i, c as int64
		  dim au as Auto
		  
		  c = tuples.count-1
		  
		  select case method
		  case "all"
		    xp.Compile(expression)
		    for i = 0 to c
		      au = tuples.Value(i)
		      if au isa tuple then
		        d = tuple(au).Fields
		      else
		        raise new RelationError("assert illegal tuple",605)
		      end
		      if xp.evaluate(d,globals,locals) = "0" then
		        return false
		      end
		    next
		    return true
		  case "exists"
		    xp.Compile(expression)
		    for i = 0 to c
		      au = tuples.Value(i)
		      if au isa tuple then
		        d = tuple(au).Fields
		      else
		        raise new RelationError("assert illegal tuple",605)
		      end
		      if xp.evaluate(d,globals,locals) <> "0" then
		        return true
		      end
		    next
		    return false
		  case "unique"
		    xp.Compile(expression)
		    test = new dictionary
		    for i = 0 to c
		      au = tuples.Value(i)
		      if au isa tuple then
		        d = tuple(au).Fields
		      else
		        raise new RelationError("assert illegal tuple",605)
		      end
		      s = xp.evaluate(d,globals,locals)
		      if test.HasKey(s) then return false
		      test.value(s) = true
		    next
		    return true
		  case "columns"
		    // checks if all columns in list are present
		    list = expression.split(",")
		    for each s in list
		      if header.IndexOf(s.trim) = -1 then return false
		    next
		    return true
		  else
		    raise new RelationError("Assert invalid method",402)
		  end
		  
		  Exception err as RuntimeException
		    raise new RelationError("Assert Runtime Error "+err.message,402)
		    
		    
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Cardinality() As int64
		  return tuples.count
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function CleanColumn(t as text) As text
		  dim result as text
		  dim i, c as integer
		  dim ch as text
		  dim list1 as text = "abcdefghijklmnoprsqtuvwxyz"
		  dim list as text = "abcdefghijklmnoprsqtuvwxyz0123456789_"
		  c = t.Length
		  
		  ch = t.mid(i,1)
		  if  list1.IndexOf(ch) <0 then
		  else
		    result = result + ch
		  end
		  for i = 1 to c-1
		    ch = t.mid(i,1)
		    if  list.IndexOf(ch) <0 then
		      result = result +"_"
		    else
		      result = result + ch
		    end
		  next
		  return result
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Clone() As Relation
		  dim result as new Relation(header)
		  dim i, c as integer
		  
		  result.tuples = tuples.Clone
		  result.Formats = formats.Clone
		  result.labels = labels.Clone
		  
		  result.globals = globals
		  result.functions = functions
		  result.locals = locals
		  
		  
		  return result
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(columns() as text)
		  dim s as text
		  
		  // should check valid names
		  
		  for each s in columns
		    AddColumn s.trim
		  next
		  
		  tuples = new OrderedDictionary
		  formats = new Dictionary
		  labels = new Dictionary
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(t as Text)
		  dim pairs(-1) as text
		  if t<>"" then
		    pairs = t.Split(",")
		  end
		  Constructor pairs
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Delete(condition as text)
		  dim xp as XPEvaluator
		  dim d as Dictionary
		  
		  xp = new XPEvaluator(functions)
		  xp.Compile(condition)
		  
		  For Each t As OrderedDictionaryEntry In tuples
		    d = tuple(t.value).fields
		    if xp.Evaluate(d) <> "0" then
		      t.Remove
		    end
		    
		  next
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Deserialize()
		  dim r as relation 
		  dim i,j,m,n as int64
		  dim list(-1),list2(2) as text
		  dim observationkey as text
		  dim tp, tp2 as tuple
		  dim d as Dictionary
		  dim au as auto
		  dim lastobservation as text
		  dim nulltext as text = ""
		  
		  if header.Count<3 then
		    raise new XPError("Deserialize less than 3 properties",602)
		  end
		  if header.Count>3 then
		    raise new XPError("Deserialize more than 3 properties",602)
		  end
		  
		  // find all columns
		  
		  order(header(0))
		  
		  r = new Relation("")
		  r.AddColumn(header(0))
		  
		  n = tuples.Count-1
		  for i = 0 to n
		    au = tuples.Value(i)
		    if au isa tuple then
		      tp = tuple(au)
		    else
		      raise new XPError("Deserialize illegal tuple",602)
		    end
		    r.AddColumn(cleancolumn(tp.Value(header(1))))
		  next
		  
		  m = r.header.Count-1
		  
		  lastobservation = ""
		  
		  for i = 0 to n
		    au = tuples.Value(i)
		    if au isa tuple then
		      tp = tuple(au)
		    else
		      raise new XPError("Deserialize illegal tuple",602)
		    end
		    if tp.Value(header(0))<>lastobservation then
		      if d <> nil then
		        // complete all values
		        for each mf as text in r.header
		          if not d.HasKey(mf) then d.Value(mf) = nulltext
		        next
		        
		        tp2 = new tuple(d)
		        
		        
		        
		        
		        r.tuples.SetValue(tp2.hash,tp2)
		      end
		      d = new Dictionary
		      d.Value(header(0)) = tp.value(header(0))
		      lastobservation = tp.value(header(0))
		    end
		    d.Value(tp.value(header(1))) = tp.value(header(2))
		  next
		  
		  if d <> nil then
		    for each mf as text in r.header
		      if not d.HasKey(mf) then d.Value(mf) = nulltext
		    next
		    tp2 = new tuple(d)
		    r.tuples.SetValue(tp2.hash,tp2)
		  end
		  
		  self.header = r.header
		  self.tuples = r.tuples
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Difference(r as relation)
		  dim e as tuple 
		  dim tp as tuple
		  dim i, c as int64
		  dim au as auto
		  
		  e = EmptyTuple
		  
		  c = r.tuples.Count-1
		  for i = c downto 0
		    au = r.tuples.value(i)
		    if au isa Tuple then
		      tp= tuple(au)
		    else
		      raise new RelationError("Difference invalid tuple",302)
		    end
		    
		    if tp.SameFamily(e) then
		      if tuples.hashash(tp.hash) then
		        tuples.Remove tp.hash
		      end
		    else
		      raise new RelationError("Difference different columns",301)
		    end
		  next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function EmptyTuple() As Tuple
		  dim t0 as tuple
		  dim d as Dictionary
		  dim et as text = ""
		  
		  d = new Dictionary
		  for each t as text in header
		    d.value(t) = et
		  next
		  t0 = new tuple(d)
		  
		  return t0
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Extend(t as text)
		  dim list(), first, rest, eq as text
		  list = t.split(" ")
		  
		  first = list(0)
		  list.Remove 0
		  
		  rest = Text.join(list," ").trim
		  
		  list = rest.split(" ")
		  
		  eq = list(0)
		  
		  if eq <> "=" then
		    raise new XPError("Extend missing =",601)
		  end
		  
		  list.Remove 0
		  
		  rest = Text.join(list," ").trim
		  
		  Extend first, rest
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Extend(label as text, expression as text)
		  dim xp as XPEvaluator
		  dim d as dictionary
		  dim v as Text
		  dim newtuples as new OrderedDictionary
		  dim tp as tuple
		  dim i, c as int64
		  dim a as auto
		  const rownumberlabel as text = "rownumber"
		  
		  if header.IndexOf(label) > -1 then
		    raise new XPError("Extend column exists already",601)
		  end
		  
		  AddColumn(label)
		  
		  xp = new XPEvaluator(functions)
		  xp.Compile(expression)
		  
		  c = tuples.Count-1
		  
		  for i = 0 to c
		    if Notifier <> nil then
		      if c>4000 and i mod 4000 = 0 then
		        Notifier.SetMessage(i*100/(c+1))
		      end
		    end
		    a = tuples.value(i)
		    if a isa tuple then
		      d = tuple(a).Fields
		    else
		      raise new XPError("Extend Illegal tuple",601)
		    end
		    locals.Value(rownumberlabel) = str(i+1).totext
		    v = xp.evaluate(d,globals,locals)
		    d.value(label) = v
		    tp = new tuple(d)
		    newtuples.setvalue tp.hash, tp
		  next
		  
		  tuples = newtuples
		  Notifier = nil
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Format1(pairs() as text)
		  dim p as text
		  dim i as int64
		  dim fields() as text
		  
		  
		  for each p in pairs
		    fields = p.trim.split(" ")
		    if fields.Count<>2 then
		      raise new RelationError("Invalid format",121)
		    end
		    fields(0) = fields(0).Trim
		    fields(1) = fields(1).Trim
		    
		    if fields(1).left(1) <> """" or fields(1).right(1) <> """"  then
		      raise new RelationError("Invalid format missing quotes",121)
		    end
		    
		    fields(1) = fields(1).mid(1,fields(1).Length-2) // remove quotes
		    
		    i = header.indexof(fields(0)) 
		    if  i= -1 then
		      raise new RelationError("Unknown field " +fields(0),122)
		    end
		    
		    formats.Value(fields(0)) = fields(1)
		    
		  next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Format1(t as text)
		  dim pairs() as text
		  pairs = t.Split(",")
		  format1 pairs
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function format2(d as double, f as string) As string
		  dim result as string
		  //result = format(d,f)
		  result = StringUtils.sprintf(f,d)
		  if abs(d)<10e-12 and f="#" then
		    result = ""
		  end
		  if left(result,1)="'" then
		    result = mid(result,2)
		  end
		  return result
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Import(value as text, fn as text = "")
		  // imports all into one column
		  dim s as text
		  dim tp as tuple
		  dim d,d2 as Dictionary
		  dim lines(-1) as text
		  dim l as text
		  dim ext as text
		  dim i,c as int64
		  dim js as JSONItem
		  dim xl as XmlDocument
		  dim c1,c2 as text
		  
		  if fn.Right(5) = ".json" then
		    redim header(-1)
		    c1 = "path"
		    c2 = "text"
		    header.AddRow c1
		    header.AddRow c2
		    
		    js = new JSONItem(value)
		    d2 = ImportFlatJSON(js,"")
		    d2 = d2
		    for each de as DictionaryEntry in d2
		      d = new Dictionary
		      d.Value(c1)=str(de.key).totext
		      d.Value(c2)= str(de.value).totext
		      tp = new tuple(d)
		      tuples.setvalue tp.Hash, tp
		    next
		    
		  elseif fn.Right(4) = ".xml" then
		    redim header(-1)
		    c1 = "path"
		    c2 = "text"
		    header.AddRow c1
		    header.AddRow c2
		    
		    xl = new XMLDocument(value)
		    d2 = ImportFlatXML(xl,"")
		    d2 = d2
		    for each de as DictionaryEntry in d2
		      d = new Dictionary
		      d.Value(c1)=str(de.key).totext
		      d.Value(c2)= str(de.value).totext
		      tp = new tuple(d)
		      tuples.setvalue tp.Hash, tp
		    next
		    
		  elseif fn.Right(5) = ".wiki" then
		    d2 = ImportWikifields(value)
		    
		    c = d2.keycount-1
		    
		    if c >=0 then
		      d = d2.Value(0)
		      redim header(-1)
		      for each key as text in d.keys
		        header.AddRow key
		      next
		      
		      for i = 0 to c
		        d = d2.value(i)
		        tp = new tuple(d)
		        tuples.setvalue tp.Hash, tp
		      next
		    end
		    
		  else
		    redim header(-1)
		    c1 = "line"
		    c2 = "text"
		    header.AddRow c1
		    header.AddRow c2
		    
		    s = value.ReplaceAll(TextExtensions.Ret+TextExtensions.Newline,TextExtensions.Newline)
		    s = s.ReplaceAll(TextExtensions.Ret,TextExtensions.Newline)
		    lines =s.Split(TextExtensions.Newline)
		    
		    i=0
		    for each line as text in lines
		      i = i+1
		      d = new Dictionary
		      d.Value(c1)=line
		      d.Value(c2)= i.ToText
		      tp = new tuple(d)
		      tuples.setvalue tp.Hash, tp
		    next
		  end if
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Insert(fields() as text)
		  dim t as tuple
		  dim i,c as int64
		  dim d as Dictionary
		  if fields.count <> header.count then
		    raise new RelationError("Insert Arity",101)
		  end
		  d = new Dictionary
		  c = header.count-1
		  for i = 0 to c
		    d.Value(header(i))=fields(i)
		  next
		  
		  t = new tuple(d)
		  tuples.setvalue(t.Hash, t)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Insert(t as text)
		  'dim pairs() as text
		  't.CSVSplit(pairs)
		  'insert pairs
		  
		  dim pairs() as text
		  dim xp as XPEvaluator
		  dim test as text
		  
		  'pairs = t.Split(" ")
		  
		  
		  xp = new XPEvaluator(functions)
		  xp.expectedreturn =  header.Count
		  xp.Compile(t)
		  test = xp.evaluate(globals,locals)
		  while xp.stack.count>0 
		    pairs.AddRow xp.stack(0)
		    xp.stack.RemoveRowAt 0
		  wend
		  
		  insert pairs
		  
		  
		  'if pairs.Count = 1 then
		  'limit val(pairs(0)), tuples.count
		  'else
		  'limit val(pairs(0)), val(pairs(1))
		  'end
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub intersection(r as relation)
		  dim e as tuple 
		  dim tp as tuple
		  dim i, c as int64
		  dim au as auto
		  
		  dim newtuples as new OrderedDictionary
		  
		  
		  e = EmptyTuple
		  
		  c = r.tuples.Count-1
		  for i = c downto 0
		    au = r.tuples.value(i)
		    if au isa Tuple then
		      tp= tuple(au)
		    else
		      raise new RelationError("Intersection invalid tuple",302)
		    end
		    
		    if tp.SameFamily(e) then
		      if tuples.hashash(tp.hash) then
		        newtuples.setValue tp.Hash, tp
		      end
		    else
		      raise new RelationError("Intersection different columns",301)
		    end
		  next
		  
		  tuples = newtuples
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Join(r as Relation, expression as Text)
		  dim r2,r3 as relation
		  dim commonheader(), leftheader(), rightheader(), newheader(), finalheader() as Text
		  dim newtuples as new OrderedDictionary
		  dim tp1, tp2, tp as tuple
		  dim d1, d10, d2, d as Dictionary
		  dim anti as boolean
		  dim xp as XPEvaluator
		  dim expression2 as text
		  dim leftfound as boolean
		  dim i, i2, c,c2 as int64
		  dim au as auto
		  dim tx as text
		  dim special as boolean
		  dim db as double
		  
		  
		  if expression = "cross" then 
		    Join(r,"1")
		    return
		  end
		  if expression = "rightsemi" then 
		    r.join(self,"leftsemi")
		    self.header = r.header
		    self.tuples =r.tuples
		    return
		  end
		  if expression = "rightanti" then 
		    r.join(self,"leftanti")
		    self.header = r.header
		    self.tuples =r.tuples
		    return
		  end
		  if expression = "right" then 
		    r.join(self,"left")
		    self.header = r.header
		    self.tuples =r.tuples
		    return
		  end
		  if expression = "outer" then 
		    r2 = clone
		    r3 = r.clone
		    join(r,"left")
		    r3.join(r2,"left")
		    union(r3)
		    return
		  end
		  
		  if expression="left" or expression = "leftsemi" or expression ="leftanti" or expression = "natural" then
		    JoinHash r,expression
		    return
		  end
		  
		  
		  for each f as Text in header
		    if r.header.indexof(f) > -1 then
		      commonheader.AddRow(f)
		    else
		      leftheader.AddRow(f)
		    end
		  next
		  for each f as Text in r.header
		    if header.indexof(f) =-1 then
		      rightheader.AddRow(f)
		      if r.formats.HasKey(f) then
		        formats.Value(f) = r.Formats.Value(f)
		      end
		      if r.labels.HasKey(f) then
		        labels.Value(f) = r.labels.Value(f)
		      end
		    end
		  next
		  
		  expression2 = ""
		  for each f as text in commonheader
		    if expression2 = "" then
		      expression2 = f+"_1 == "+ f+"_2"
		    else
		      expression2 = expression2 + " and " + f+"_1 == "+ f+"_2"
		    end
		  next
		  
		  select case expression
		  case "leftanti" 
		    special = true
		  case "natural"
		    for each f as Text in rightheader
		      AddColumn f
		    next
		    special = true
		  case "left"
		    for each f as Text in rightheader
		      AddColumn f
		    next
		    special = true
		  case "leftsemi"
		    special = true
		  else
		    header.RemoveAllRows
		    for each f as Text in leftheader
		      if commonheader.IndexOf(f) >= 0then
		        AddColumn f+"_1"
		      else
		        AddColumn f
		      end
		    next
		    for each f as Text in rightheader
		      if commonheader.IndexOf(f) >= 0 then
		        AddColumn f+"_2"
		      else
		        AddColumn f
		      end
		    next
		    expression2 = expression
		  end
		  
		  
		  xp = new XPEvaluator(functions)
		  xp.Compile(expression2)
		  
		  c = tuples.count-1
		  c2 = r.tuples.Count-1
		  for i = 0 to c
		    
		    
		    au = tuples.Value(i)
		    if au isa Tuple then
		      tp1 = tuple(au)
		    else
		      raise new RelationError("Join invalid tuple",671)
		    end
		    
		    d10 = tp1.Fields.Clone
		    for each cf as text in commonheader
		      d10.Value(cf+"_1") = d10.Value(cf)
		      d10.Remove(cf)
		    next
		    leftfound = false
		    
		    
		    for i2=0 to c2
		      
		      if Notifier <> nil then
		        if c*c2>4000 and (i*c2+i2) mod 4000 = 0 then
		          db = i*c2*100/(c*c2+1)
		          Notifier.SetMessage(dB)
		        end
		      end
		      
		      
		      d1 = d10.Clone
		      au = r.tuples.Value(i2)
		      if au isa Tuple then
		        tp2 = tuple(au)
		      else
		        raise new RelationError("Join invalid tuple",672)
		      end
		      
		      d2 = tp2.Fields.Clone
		      for each cf as text in commonheader
		        d1.Value(cf+"_2") = d2.Value(cf)
		      next
		      for each cf as text in rightheader
		        d1.Value(cf) = d2.Value(cf)
		      next
		      
		      if special then
		        tx = "1"
		        for each f as text in commonheader
		          if strcomp(d1.value(f+"_1"), d1.value(f+"_2"),1) <> 0 then
		            tx= "0"
		            exit for f
		          end
		        next
		      else
		        tx = xp.evaluate(d1,globals,locals)
		        for each cf as text in commonheader
		          d1.value(cf) = d1.value(cf+"_1")
		          d1.Remove(cf+"_1")
		          d1.Remove(cf+"_2")
		        next
		      end
		      if tx <> "0" then
		        select case expression
		        case "natural"
		          for each cf as text in commonheader
		            d1.value(cf) = d1.value(cf+"_1")
		            d1.Remove(cf+"_1")
		            d1.Remove(cf+"_2")
		          next
		        case "left"
		          for each cf as text in commonheader
		            d1.value(cf) = d1.value(cf+"_1")
		            d1.Remove(cf+"_1")
		            d1.Remove(cf+"_2")
		          next
		          leftfound = true
		        case "leftsemi"
		          for each cf as text in commonheader
		            d1.value(cf) = d1.value(cf+"_1")
		            d1.Remove(cf+"_1")
		            d1.Remove(cf+"_2")
		          next
		          for each cf as text in rightheader
		            d1.Remove(cf)
		          next
		        end
		        tp = new tuple(d1)
		        newtuples.setvalue tp.Hash, tp
		      else
		        leftfound = false
		        select case expression
		        case "leftanti"
		          for each cf as text in commonheader
		            d1.value(cf) = d1.value(cf+"_1")
		            d1.Remove(cf+"_1")
		            d1.Remove(cf+"_2")
		          next
		          for each cf as text in rightheader
		            d1.Remove(cf)
		          next
		          leftfound = true
		        end
		      end
		      if (expression="left" or expression="leftanti") and not leftfound then
		        for each cf as text in commonheader
		          d1.value(cf) = d1.value(cf+"_1")
		          d1.Remove(cf+"_1")
		          d1.Remove(cf+"_2")
		        next
		        if expression = "left" then
		          for each cf as text in rightheader 
		            tx = ""
		            d1.value(cf) = tx
		          next
		        end
		        tp = new tuple(d1)
		        newtuples.setvalue tp.Hash, tp
		      end
		    next
		    
		  next
		  
		  for each cf as text in commonheader
		    if header.IndexOf(cf) = -1 then
		      header.AddRowAt 0, cf
		    end
		  next
		  
		  tuples = newtuples
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub JoinHash(r as Relation, expression as Text)
		  dim r2,r3 as relation
		  dim commonheader(), leftheader(), rightheader(), newheader(), finalheader() as Text
		  dim newtuples as new OrderedDictionary
		  dim tp1, tp2, tp, tp10,tp11 as tuple
		  dim d1, d10, d11, d2, d as Dictionary
		  dim anti as boolean
		  dim xp as XPEvaluator
		  dim expression2 as text
		  dim leftfound as boolean
		  dim i, i2, c,c2 as int64
		  dim au as auto
		  dim tx as text
		  dim special as boolean
		  dim db as double
		  const emptytext as text = ""
		  
		  
		  for each f as Text in header
		    if r.header.indexof(f) > -1 then
		      commonheader.AddRow(f)
		    else
		      leftheader.AddRow(f)
		    end
		  next
		  for each f as Text in r.header
		    if header.indexof(f) =-1 then
		      rightheader.AddRow(f)
		      if r.formats.HasKey(f) then
		        formats.Value(f) = r.Formats.Value(f)
		      end
		      if r.labels.HasKey(f) then
		        labels.Value(f) = r.labels.Value(f)
		      end
		    end
		  next
		  
		  expression2 = ""
		  for each f as text in commonheader
		    if expression2 = "" then
		      expression2 = f+"_1 == "+ f+"_2"
		    else
		      expression2 = expression2 + " and " + f+"_1 == "+ f+"_2"
		    end
		  next
		  
		  select case expression
		  case "leftanti" 
		  case "natural"
		    for each f as Text in rightheader
		      AddColumn f
		    next
		  case "left"
		    for each f as Text in rightheader
		      AddColumn f
		    next
		  case "leftsemi"
		  else
		    raise new RelationError("Join Hash invalid expression",671)
		  end
		  
		  dim dhash as Dictionary
		  
		  c = tuples.count-1
		  c2 = r.tuples.Count-1
		  
		  dhash = new Dictionary
		  
		  
		  for i2 = 0 to c2
		    au = r.tuples.Value(i2)
		    if au isa Tuple then
		      tp2 = tuple(au)
		    else
		      raise new RelationError("Join invalid tuple",671)
		    end
		    
		    d10 = new Dictionary
		    for each cf as text in commonheader
		      d10.Value(cf) = tp2.Value(cf)
		    next
		    tp = new tuple(d10)
		    
		    dim tpstack as  TupleStack
		    if dhash.HasKey(tp.Hash) then
		      au = dhash.Value(tp.Hash)
		      if au isa TupleStack then
		        tpstack = TupleStack(au)
		      else
		        raise new RelationError("Join invalid stack",671)
		      end
		    else
		      tpstack = new TupleStack
		    end
		    tpstack.push tp2
		    dhash.Value(tp.Hash) = tpstack
		    
		  next
		  
		  i=i
		  
		  
		  for i = 0 to c
		    if Notifier <> nil then
		      if c>4000 and i mod 4000 = 0 then
		        db = i*100/(c+1)
		        Notifier.SetMessage(dB)
		      end
		    end
		    
		    au = tuples.Value(i)
		    if au isa Tuple then
		      tp1 = tuple(au)
		    else
		      raise new RelationError("Join invalid tuple",671)
		    end
		    
		    d1 = new Dictionary
		    for each cf as text in commonheader
		      d1.Value(cf) = tp1.Value(cf)
		    next
		    tp = new tuple(d1)
		    
		    dim tpstack as TupleStack
		    if dhash.HasKey(tp.Hash) then
		      au =  dhash.Value(tp.Hash)
		      if au isa TupleStack then
		        tpstack = TupleStack(au)
		      else
		        raise new RelationError("Join Hash invalid stack",671)
		      end
		      
		      tpstack = tpstack.Clone
		      
		      select case expression
		      case "left" , "natural"
		        d10 = tp1.Fields
		        while not tpstack.Empty
		          d11 = d10.Clone
		          tp2 = tpstack.shift
		          d2 = tp2.Fields
		          for each t as text in r.header
		            d11.Value(t) = d2.Value(t)
		          next
		          tp11 = new tuple(d11)
		          newtuples.setvalue tp11.Hash, tp11
		        wend
		      case "leftanti"
		        // nada
		      case "leftsemi"
		        newtuples.setvalue tp1.Hash, tp1
		      end
		    else
		      select case expression
		      case "left" 
		        d11 = tp1.Fields.Clone
		        for each t as text in rightheader
		          d11.Value(t) = emptytext
		        next
		        tp11 = new tuple(d11)
		        newtuples.setvalue tp11.Hash, tp11
		      case "leftanti"
		        newtuples.setvalue tp1.Hash, tp1
		      case "leftsemi"
		        // nada
		      case "natural"
		        // nada
		      end
		    end
		    
		  next
		  
		  
		  tuples = newtuples
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Label(pairs() as text)
		  dim p as text
		  dim i as int64
		  dim fields() as text
		  dim f0, f1 as text
		  
		  
		  for each p in pairs
		    fields = p.trim.split(" ")
		    if fields.Count<2 then
		      raise new RelationError("Invalid label",121)
		    end
		    f0 = fields(0).trim
		    fields.RemoveRowAt 0
		    f1 = Text.join(fields," ").trim.ReplaceAll("""","")
		    i = header.indexof(f0) 
		    if  i= -1 then
		      raise new RelationError("Unknown label " +f0,122)
		    end
		    
		    labels.Value(f0) = f1
		    
		  next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Label(t as text)
		  dim pairs() as text
		  pairs = t.Split(",") // commas inside label??
		  label pairs
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Limit(start as int64, length as int64)
		  dim i,c as int64
		  dim tuples2 as OrderedDictionary
		  dim d as Dictionary
		  dim au as auto
		  dim tp as tuple
		  
		  
		  if start = 0 then 
		    raise new XPError("Limit Start < 1",88)
		  end
		  
		  if start > tuples.count then 
		    raise new XPError("Limit Start > count "+str(start+1).totext+"<"+str(tuples.count).totext ,88)
		  end
		  
		  if length <0 then 
		    raise new XPError("Limit Length < 0",88)
		  end
		  
		  c = tuples.Count-1 
		  
		  if length < c/3 then
		    tuples2 = new OrderedDictionary
		    for i = start-1 to start-1+length-1
		      au= tuples.Value(i)
		      tp = tuple(au)
		      tuples2.setvalue(tp.Hash, tp)
		    next
		    tuples = tuples2
		    // add is more efficient than 
		    return
		  end
		  
		  for i = c downto start+length-1
		    tuples.Remove i
		  next
		  
		  for i = start-2 downto 0
		    tuples.Remove i
		  next
		  
		  i=i
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Limit(t as text)
		  dim pairs() as text
		  dim xp as XPEvaluator
		  
		  'pairs = t.Split(" ")
		  
		  
		  xp = new XPEvaluator(functions)
		  xp.expectedreturn = 2
		  xp.Compile(t)
		  pairs.addrow xp.evaluate(globals,locals)
		  if xp.stack.count>1 then
		    pairs.AddRow xp.stack(1)
		  end
		  
		  
		  if pairs.Count = 1 then
		    limit val(pairs(0)), tuples.count
		  else
		    limit val(pairs(0)), val(pairs(1))
		  end
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Order(pairs() as Text)
		  tuples.OrderPairs = pairs
		  tuples.Order
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Order(t as text)
		  dim pairs() as text
		  pairs = t.Split(",")
		  order pairs
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Parse(t as text)
		  dim list(), reg, field, rest as text
		  list = t.split(" ")
		  
		  field = list(0)
		  list.Remove 0
		  
		  reg = list(0)
		  list.Remove 0
		  
		  rest = Text.join(list," ")
		  
		  parse  field,reg,rest
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Parse(field as text, reg as text, labels as text)
		  dim xp as XPEvaluator
		  dim d as dictionary
		  dim v as Text
		  dim newtuples as new OrderedDictionary
		  dim tp as tuple
		  dim i,j, c,k,mc as int64
		  dim a as auto
		  dim r as RegEx
		  dim match as RegExMatch
		  dim list(-1), list2(-1) as text
		  dim db as text
		  db = "debug"
		  
		  //AddColumn(db)
		  list =  labels.Split(",")
		  for each l as text in list
		    list2.AddRow l.Trim
		    AddColumn(l.trim)
		  next
		  k = list2.Count-1
		  
		  r = new regex
		  r.SearchPattern=reg.mid(1,reg.Length-2)
		  
		  c = tuples.Count-1
		  
		  for i = 0 to c
		    tp = tuples.value(i)
		    d = tp.Fields
		    v = d.Value(field)
		    
		    match = r.Search(v)
		    if match<>nil then
		      mc = match.SubExpressionCount-1
		      //d.value(db) = match.SubExpressionString(0).ToText
		      for j = 0 to min(mc,k)
		        d.Value(list2(j))=match.SubExpressionString(j+1).ToText
		        
		      next
		    end
		    tp = new tuple(d)
		    newtuples.setvalue tp.hash, tp
		  next
		  
		  tuples = newtuples
		  
		  
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Print() As Text
		  dim widths() as double
		  dim i, c as integer
		  dim t as tuple
		  dim firstline, line, ch as text
		  dim lines(), fields() as text
		  
		  c = header.Ubound
		  
		  for i = 0 to c
		    widths.addrow header(i).Length 
		  next
		  
		  for each t in tuples
		    for i = 0 to c
		      widths(i) = max(widths(i), t.Value(header(i)).Length)
		    next
		  next
		  
		  ch = "─"
		  firstline = ch.Repeat(widths.sum+widths.count-1)
		  
		  lines.AddRow firstline
		  
		  for i = 0 to c
		    fields.AddRow header(i).pad(" ",widths(i))
		  next
		  
		  lines.AddRow Text.join(fields," ")
		  
		  for each t in tuples
		    fields.RemoveAllRows
		    for i = 0 to c
		      fields.AddRow t.value(header(i)).pad(" ",widths(i))
		    next
		    lines.AddRow Text.join(fields," ")
		  next
		  
		  return Text.join(lines,Encodings.UTF8.chr(10).toText)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Project(pairs() as text)
		  dim s as text
		  dim hasstats as boolean
		  dim newcolumns(), columns(), nc, fields() as text
		  dim stats() as text
		  dim newtuples as new Dictionary
		  dim d, d2 as Dictionary
		  dim tp,tp2 as tuple
		  dim au as auto
		  dim i,c,j,k as int64
		  dim h as text
		  dim a as Accumulator
		  
		  
		  
		  
		  if aggregators = nil then
		    aggregators = new Dictionary
		  end
		  
		  for each s in pairs
		    fields = s.trim.split(" ")
		    columns.AddRow fields(0).trim
		    if fields.Count >= 2 then
		      stats.AddRow  fields(1).trim
		      nc = fields(0).trim + "_" + fields(1).trim
		      //nc = text.join(fields,"_") // more than one column in accumulator
		      hasstats = true
		    else
		      stats.AddRow ""
		      nc = fields(0).trim
		    end
		    if newcolumns.IndexOf(nc) >= 0 then
		      raise new RelationError("Project duplicate column "+nc,141)
		    end
		    if formats.HasKey(fields(0).trim) then
		      formats.Value(nc) = formats.Value(fields(0).trim)
		    end
		    if labels.HasKey(fields(0).trim) then
		      labels.Value(nc) = labels.Value(fields(0).trim)
		    end
		    newcolumns.AddRow nc
		  next
		  
		  c = columns.count-1
		  
		  for each s in columns
		    if header.IndexOf(s) < 0  then
		      raise new RelationError("Project unknown column "+s,141)
		    end
		  next
		  
		  k = tuples.count-1
		  for j = 0 to k
		    if Notifier <> nil then
		      if k>4000 and i mod 4000 = 0 then
		        Notifier.SetMessage(i*100/(c+1))
		      end
		    end
		    au = tuples.Value(j)
		    if au isa tuple then
		      tp = tuple(au)
		    else
		      raise new XPError("Project illegal tuple",602)
		    end
		    d =  new dictionary 
		    for i = 0 to c
		      if stats(i) = "" then
		        d.Value(newcolumns(i)) = tp.value(columns(i))
		      end
		    next
		    tp2 = new tuple(d)
		    h = tp2.hash
		    if newtuples.haskey(h) then
		      d = dictionary(newtuples.value(h))
		    else
		      for i = 0 to c
		        if stats(i) <> "" then
		          if aggregators.HasKey(stats(i)) then
		            a = Accumulator(aggregators.Value(stats(i)))
		            a = a.Clone
		            a.functions = functions
		            d.Value(newcolumns(i)) = a
		          else
		            d.Value(newcolumns(i)) = new Accumulator(stats(i))
		          end
		        end
		      next
		    end
		    for i = 0 to c
		      if stats(i) <> "" then
		        a = Accumulator(d.Value(newcolumns(i)))
		        a.Add tp.value(columns(i))
		        d.Value(newcolumns(i)) = a
		      end
		    next
		    newtuples.Value(h) = d
		  next
		  
		  header = newcolumns
		  tuples.RemoveAll
		  
		  For Each t As dictionaryentry In newtuples
		    d  =  Dictionary(t.value)
		    d2 = new Dictionary
		    For Each u As dictionaryentry In d
		      if u.value Isa Accumulator then
		        d2.value(u.key) = Accumulator(u.value).Reduce
		      else
		        d2.value(u.key) = u.value
		      end
		    next
		    tp = new tuple(d2)
		    tuples.setvalue tp.Hash, tp
		  next
		  
		  ' special case no tuple, still have count and sum
		  
		  if tuples.Count = 0 then
		    d2 = new Dictionary
		    for i = 0 to c
		      select case stats(i)
		      case "count"
		        d2.Value(newcolumns(i)) = 0
		      case "sum"
		        d2.Value(newcolumns(i)) = 0
		      end
		    next
		    if d2.count > 0 then
		      tp = new tuple(d2)
		      tuples.setvalue tp.Hash, tp
		    end
		  end
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Project(t as text)
		  dim pairs(), fields() as text
		  dim t2,p,f,tr as text
		  dim i,c as integer
		  dim rollupfields(), rolluppairs(), rollup() as text
		  dim found as Boolean
		  
		  if t.Length >6 and t.Trim.left(6)="inline" then
		    dim r2 as Relation
		    r2 = self.Clone
		    r2.Project(t.mid(7))
		    self.join(r2, "natural")
		    return
		  end
		  
		  if t.Length >6 and t.Trim.left(6)="rollup" then
		    // check aggregators, there must be only one per field
		    t2 = t.mid(7)
		    pairs = t2.trim.Split(",")
		    c = pairs.Count-1
		    for i = 0 to c
		      p = pairs(i).trim
		      if p=""   then
		        raise new XPError("Project rollup empty pair",11)
		      end
		      fields = p.trim.Split(" ")
		      f = fields(0)
		      if rollupfields.IndexOf(f)>-1 then
		        raise new XPError("Project rollup duplicate column "+f,11)
		      end
		      rollupfields.AddRow f
		      tr = p.replace(" ","_")+" "+f
		      rolluppairs.AddRow tr
		      rollup.AddRow p
		    next
		    
		    dim r2 as Relation
		    r2 = self.Clone
		    
		    do
		      r2.Project(rollup)
		      for each h as text in header
		        if r2.header.IndexOf(h) =-1 then
		          r2.Extend h, ""
		        end 
		      next
		      r2.Rename(rolluppairs)
		      union r2
		      
		      // remove each time a column
		      found = false
		      c = rollup.Count-1
		      for i = c downto 0
		        p = rollup(i).Trim
		        if  p.IndexOf(" ")=-1 then
		          found = true
		          rollup.RemoveRowAt i
		          exit for i
		        end
		      next
		      
		    loop until not found
		    
		    return
		  end
		  
		  
		  
		  pairs = t.Split(",")
		  project pairs
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Rename(pairs() as text)
		  dim xp as XPEvaluator
		  dim d,dict as Dictionary
		  dim p as text
		  dim f, fields() as text
		  dim newtuples as  new OrderedDictionary
		  dim i,c as int64
		  dim v as text
		  dim tp as tuple
		  dim au as auto
		  
		  
		  dict = new  Dictionary
		  
		  for each p in pairs
		    fields = p.trim.split(" ")
		    if fields.Count<>2 then
		      raise new RelationError("Invalid rename",121)
		    end
		    fields(0) = fields(0).Trim
		    fields(1) = fields(1).Trim
		    i = header.indexof(fields(0)) 
		    if  i= -1 then
		      raise new RelationError("Unknown rename",122)
		    end
		    header.Remove i
		    if  header.indexof(fields(1)) > -1 then
		      raise new RelationError("Rename newname exists already ",122)
		    end
		    header.Insert i, fields(1)
		    dict.value(fields(0)) = fields(1)
		  next
		  
		  c = tuples.Count-1
		  for i =0  to c
		    au = tuples.value(i)
		    if au isa tuple then
		      d = tuple(au).Fields
		    else
		      raise new RelationError("Rename illegal tuple",603)
		    end
		    
		    for each u as DictionaryEntry In dict 
		      v = d.value(u.key)
		      d.Remove u.key
		      d.value(u.Value) = v
		    next
		    
		    tp = new tuple(d)
		    newtuples.setvalue tp.Hash, tp
		    
		  next
		  
		  tuples = newtuples
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Rename(t as text)
		  dim pairs() as text
		  pairs = t.Split(",")
		  rename pairs
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Select1(condition as text)
		  dim xp as XPEvaluator
		  dim d as Dictionary
		  dim a as auto
		  dim result as text
		  dim v as variant
		  dim i,c as int64
		  const rownumberlabel as text = "rownumber"
		  
		  xp = new XPEvaluator(functions)
		  xp.Compile(condition)
		  
		  c = tuples.Count-1
		  
		  for i = c downto 0
		    if Notifier <> nil then
		      if c>4000 and i mod 4000 = 0 then
		        Notifier.SetMessage(((c-i)*100)/(c+1))
		      end
		    end
		    a = tuples.value(i)
		    if a isa tuple then
		      d = tuple(a).Fields
		    else
		      raise new XPError("Select Illegal tuple",601)
		    end
		    locals.Value(rownumberlabel) = str(i+1).totext
		    result = xp.evaluate(d,globals,locals)
		    if result= "0" then
		      tuples.Remove i
		    end
		  next
		  Notifier = nil
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Serialize()
		  dim r as relation 
		  dim i,j,m,n as int64
		  dim list(-1),list2(2) as text
		  dim observationkey as text
		  dim tp, tp2 as tuple
		  dim d as Dictionary
		  dim au as auto
		  
		  if header.Count<2 then
		    raise new XPError("Serialize less than 2 properties",602)
		  end
		  
		  observationkey = header(0)
		  n = header.Count-1
		  for i = 1 to n
		    list.AddRow header(i)
		  next
		  
		  
		  
		  list2(0)= header(0)
		  list2(1)= TextExtensions.fromstring("key")
		  list2(2)= TextExtensions.fromstring("value")
		  if list2(0)="key" then list2(0) = "key0"
		  if list2(0)="value" then list2(0) = "value0"
		  
		  r = new relation(list2)
		  
		  m = tuples.count-1
		  for j = 0 to m
		    au = tuples.Value(j)
		    if au isa tuple then
		      tp = tuple(au)
		    else
		      raise new XPError("Serialize illegal tuple",602)
		    end
		    
		    
		    
		    for i = 0 to n-1
		      d = new Dictionary 
		      d.Value(list2(0)) = tp.Value(observationkey)
		      d.Value(list2(1))=list(i)
		      d.Value(list2(2))=tp.Value(list(i))
		      
		      tp2 = new tuple(d)
		      r.tuples.setvalue(tp2.Hash, tp2)
		    next
		    
		  next
		  
		  
		  self.header = list2
		  self.tuples = r.tuples
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetCSV(lines() as text, pad as boolean = false)
		  dim s as text
		  dim fields() as text
		  dim firstline as boolean
		  dim i, j,c,k as integer
		  dim tp as tuple
		  dim d as Dictionary
		  dim state, acc as text
		  dim quoted as boolean
		  dim quote as text = TextExtensions.Quote
		  dim ret as text = TextExtensions.ret
		  dim newline as text = TextExtensions.Newline
		  dim separator as text
		  dim line, peek as text
		  dim empty as text = ""
		  
		  header.RemoveAllRows
		  tuples.RemoveAll
		  
		  separator = ";"
		  
		  firstline = true
		  k = lines.Count-1
		  for j = 0 to k
		    line = lines(j)
		    if Notifier <> nil then
		      if k>5000 and j mod 5000 = 0 then
		        Notifier.SetMessage((j*100)/k)
		      end
		    end
		    if firstline and line.left(1) = "#" then Continue 
		    if firstline then
		      if line.IndexOf(separator) >= 0 then
		        separator = ";"
		      else
		        separator = ","
		      end
		      fields = line.Split(separator)
		      for each field as text in fields
		        AddColumn cleancolumn(field.Trim)
		      next
		      c = fields.Count-1
		      firstline = false
		    else
		      fields.RemoveAllRows
		      state = "start"
		      acc = ""
		      quoted = false
		      For Each ch As Text In line.Characters
		        select case state
		        case "start"
		          select case ch
		          case quote
		            state = "quote"
		            acc = ""
		            quoted = true
		          case separator
		            if quoted then 
		              fields.AddRow acc
		            else
		              fields.AddRow acc.trim
		            end
		            quoted = false
		            acc = ""
		          else
		            if not quoted then acc = acc + ch
		          end
		        case "quote"
		          select case ch
		          case quote
		            state = "quotesuspend"
		          else
		            acc = acc + ch
		          end
		        case "quotesuspend"
		          select case ch
		          case quote
		            acc = acc + quote
		            state = "quote"
		          case ","
		            if quoted then 
		              fields.AddRow acc
		            else
		              fields.AddRow acc.trim
		            end
		            quoted = false
		            acc = ""
		            state = "start"
		          else
		            state = "start"
		          end
		        end
		      next
		      
		      select case state
		      case "start"
		        fields.AddRow acc
		      case "quote"
		        'error missing end quote, we need to read the following line
		        if j< k then
		          lines(j) = lines(j) + " " + lines(j+1)
		          lines.RemoveRowAt j+1
		          k = k-1
		          j = j-2
		          continue for j
		        end
		        
		        
		        fields.AddRow acc
		      case "quotesuspend"
		        fields.AddRow acc
		      end
		      
		      d = new Dictionary
		      
		      c = fields.Count-1
		      
		      
		      if c +1 = header.Count then
		        for i = 0 to c
		          d.Value(header(i))=fields(i)
		        next
		      else
		        if not pad then
		          raise new XPError("Read CSV field count in row not header count (line: "+str(j+1).totext _
		          +", header: "+str(header.Count).totext+", fields:"+str(c+1).totext+")",99)
		        else
		          for i = 0 to header.Count-1
		            d.Value(header(i))=empty
		          next
		          for i = 0 to c
		            d.Value(header(i))=fields(i)
		          next
		        end
		      end
		      
		      
		      tp = new tuple(d)
		      tuples.setvalue tp.Hash, tp
		    end
		  next
		  Notifier = nil
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ToChart(options as Text) As Text
		  dim line, result as text
		  dim fields(), lines() as text
		  dim tp as tuple
		  dim i, c, j, k as integer
		  dim t as text
		  dim fm as text
		  dim f as text
		  dim charttype as text
		  dim chartid as text
		  dim labels(), values(), series() as text
		  dim eolt as text
		  dim seriestext as text
		  
		  eolt = Text.EndOfLine()
		  
		  charttype = "line"
		  fields = options.Trim.Split(" ")
		  if ubound(fields)>=0 then
		    charttype = fields(0)
		    fields.Remove 0
		    options = text.Join(fields," ")
		  end 
		  
		  chartid = "chart" + str(System.Random.InRange(0,999999)).ToText
		  result ="<div class=""ct-chart ct-minor-seventh"" id="""+chartid+"""></div>"+eolt _
		  + "<script>"+eolt
		  
		  result = result +  "var data = {"
		  
		  k = header.count-1
		  c = tuples.count-1
		  
		  if k<0 or c<0 then 
		    raise new RelationError("Chart no values",987)
		  end
		  
		  for i = 0 to c
		    tp = tuples.value(i)
		    labels.AddRow(tp.value(header(0)))
		  next
		  
		  for j= 1 to k
		    redim values(-1)
		    for i=0 to c 
		      tp = tuples.value(i)
		      values.addrow tp.value(header(j))
		    next
		    series.AddRow(text.join(values,","))
		  next
		  
		  result = result + "labels : ["""+text.join(labels,""",""")+"""], "+eolt
		  
		  seriestext = ""
		  if charttype = "pie" then
		    seriestext =  " ["+series(0)+"]"
		  else
		    
		    seriestext =  " ["
		    for j= 0 to k-1
		      seriestext = seriestext + " ["+series(j)+"]"
		      if j<k-1 then
		        seriestext = seriestext + ", "
		      end
		    next
		    seriestext = seriestext + "]"
		  end
		  result = result + " series: "+ seriestext
		  result = result + "}; "+eolt
		  
		  result = result + "var options = { "+options+ "}; "+eolt
		  
		  select case charttype
		    
		  case "bar"
		    result = result +  "new Chartist.Bar(""#"+chartid+""", data, options)"
		  case "line"  
		    result = result +  "new Chartist.Line(""#"+chartid+""", data, options)"
		  case "pie"  
		    result = result +  "new Chartist.Pie(""#"+chartid+""", data, options)"
		  end
		  
		  result = result + "</script>"+eolt
		  
		  result = result.ReplaceAll(eolt," ")+eolt
		  
		  
		  // <div class="ct-chart ct-minor-seventh" id="chart235692"></div>
		  // <script> var data = {labels : ["a 5","b 1"],  series: [ [5,1]] };  
		  // var options = { };  
		  // new Chartist.Line("#chart235692", data, options)
		  // </script> 
		  
		  
		  // {{chart
		  // | chart235692
		  // | line
		  // | ["a 5","b 1"]
		  // | [ [5,1]] 
		  // | options
		  // }}
		  
		  
		  result = "{{chart|"+chartid+"|"+charttype.Titlecase+"|["""+text.join(labels,""",""")+"""]|" + seriestext + "|" + options + "}}"
		  // | chart235692
		  // | ["a 5","b 1"]
		  // | [ [5,1]] 
		  // | options
		  // }}
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ToHTML(limit as text = "") As Text
		  dim line, result as text
		  dim fields(), lines() as text
		  dim tp as tuple
		  dim i, i0, c, j, k as integer
		  dim t as text
		  dim fm as text
		  dim f as text
		  dim td as text
		  
		  lines.AddRow("{| class=""datatable"" ")
		  
		  k = header.count-1
		  'for j= 0 to k
		  'fields.AddRow("<th>"+header(j)+"</th>")
		  'next
		  'line = "<tr>"+Text.join(fields," ")+"</tr>"
		  line = "! "
		  for j= 0 to k
		    if j > 0 then
		      line = line + " !! " 
		    end
		    f = header(j)
		    if labels.HasKey(f) then
		      line = line + labels.Value(f)
		    else
		      line = line + f
		    end
		  next
		  //line = "! "+Text.join(header," !! ")
		  lines.AddRow("|-")
		  lines.AddRow line
		  
		  c = tuples.count-1
		  
		  if val(limit) > 0 and val(limit)<c then
		    c = val(limit)-1
		    i0 = 0
		  elseif val(limit) < 0 and -val(limit)<c then
		    i0 = c+val(limit)+1
		  end
		  
		  
		  for i = i0 to c 
		    tp = tuples.value(i)
		    fields.RemoveAllRows
		    for j= 0 to k
		      f = header(j)
		      t = tp.Value(f)
		      'td = "<td>"
		      td = ""
		      if formats.HasKey(f) then
		        fm = formats.Value(f)
		        if fm<>"" then
		          t = format2(val(t),fm).ToText
		          'td = "<td align=""right"">"
		          td = "align=""right""  | "
		        end
		      end
		      'fields.AddRow(td+t+"</td>")
		      fields.AddRow(td + t)
		    next
		    '' line = "<tr>"+Text.join(fields," ")+"</tr>"
		    line = "| "+text.Join(fields," || ")
		    lines.AddRow("|-")
		    lines.AddRow line
		  next
		  
		  lines.AddRow("|}")
		  result = text.Join(lines,text.endofline)+text.endofline+text.endofline
		  
		  ''result = "<table class=""datatable"">"+Text.join(lines," ")+ "</table>"
		  
		  
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ToTemplate(tmp as text) As Text
		  dim line, result as text
		  dim fields(), lines() as text
		  dim tp as tuple
		  dim i, i2, lasti, lastvalidi, j, c,k,l,m as integer
		  dim t as text
		  dim fm as text
		  dim f as text
		  dim td as text
		  dim sel, sel2 as text
		  dim tmp2 as text
		  dim selt as text
		  dim selectors(-1) as text
		  dim templates(-1) as text
		  dim dicts(-1) as Dictionary
		  dim key as text
		  dim d, lastd, nextd as Dictionary
		  dim eachhashappened as boolean
		  
		  lasti = 0
		  lastvalidi = 0
		  do 
		    
		    i = tmp.IndexOf(lasti, "{{")
		    if i>=0 then
		      i2 = tmp.IndexOf(i, "}}")
		      if i2 >=0 then
		        sel = tmp.mid(i+2,i2-i-2)
		        if sel.length>=6 and sel.left(6) = "group " then 
		          sel2 = sel
		          sel = "group"
		        end
		        
		        select case sel
		        case "first", "each", "last","end"
		          selectors.AddRow(sel)
		          if ubound(selectors) > 0 then
		            templates.AddRow(tmp.mid(lastvalidi,i-lastvalidi).trim)
		          end
		          lastvalidi = i2+2
		        case "group"
		          selectors.AddRow(sel2)
		          if ubound(selectors) > 0 then
		            templates.AddRow(tmp.mid(lastvalidi,i-lastvalidi).trim)
		          end
		          lastvalidi = i2+2
		        else
		          // ignore
		        end
		        
		        lasti=i2+2
		        
		      end
		    end
		  loop until i < 0
		  
		  if ubound(selectors) > -1 then
		    templates.AddRow(tmp.mid(lastvalidi))
		  end
		  
		  c = tuples.count-1
		  k = header.Count-1
		  
		  
		  for i = 0 to c
		    if Notifier <> nil then
		      if c>4000 and i mod 4000 = 0 then
		        Notifier.SetMessage(((c-i)*50)/(c+1))
		      end
		    end
		    tp = tuples.value(i)
		    d = tp.Fields.clone
		    for j= 0 to k
		      f = header(j)
		      t = d.Value(f)
		      if formats.HasKey(f) then
		        fm = formats.Value(f)
		        d.Value(f) = format2(val(t),fm).ToText
		      end
		    next
		    dicts.AddRow d
		  next
		  
		  
		  
		  for i = 0 to c
		    if Notifier <> nil then
		      if c>4000 and i mod 4000 = 0 then
		        Notifier.SetMessage(((c-i)*50)/(c+1)+50)
		      end
		    end
		    
		    d = dicts(i)
		    if i>0 then
		      lastd = dicts(i-1)
		    else
		      lastd = nil
		    end
		    if i<c then
		      nextd = dicts(i+1)
		    else
		      nextd = nil
		    end
		    
		    eachhashappened = false
		    
		    m = selectors.Count-1
		    for l = 0 to m
		      selt = selectors(l)
		      line = templates(l)
		      select case selt
		      case "first"
		        if i = 0 then
		          for j = 0 to k
		            line = line.Replaceall("{{"+ header(j)+"}}",d.Value(header(j)))
		          next
		          result = result + line
		        end
		      case "each"
		        for j = 0 to k
		          line = line.Replaceall("{{"+ header(j)+"}}",d.Value(header(j)))
		        next
		        result = result + line
		        eachhashappened = true
		      case "last"
		        if i = c then
		          for j = 0 to k
		            line = line.Replaceall("{{"+ header(j)+"}}",d.Value(header(j)))
		          next
		          result = result + line
		        end
		      case "end"
		        // ignore
		      else
		        if selt.left(6)= "group " then
		          line = templates(l)
		          key = selt.mid(6).trim
		          if  d.HasKey(key) then
		            if not eachhashappened then // must be first
		              if i=0 or lastd.Value(key) <> d.Value(key) then
		                for j = 0 to k
		                  line = line.Replaceall("{{"+ header(j)+"}}",d.Value(header(j)))
		                next
		                result = result + line
		              end
		            else // must be last
		              if i=c or nextd.Value(key) <> d.Value(key) then
		                for j = 0 to k
		                  line = line.Replaceall("{{"+ header(j)+"}}",d.Value(header(j)))
		                next
		                result = result + line
		              end
		            end
		          end
		        end
		      end
		    next
		    
		  next
		  
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Union(r as relation)
		  dim e as tuple 
		  dim tp as tuple
		  dim i, c as int64
		  dim au as auto
		  
		  e = EmptyTuple
		  
		  c = r.tuples.Count-1
		  for i = 0 to c
		    au = r.tuples.value(i)
		    if au isa Tuple then
		      tp= tuple(au)
		    else
		      raise new RelationError("Union invalid tuple",302)
		    end
		    
		    if tp.SameFamily(e) then
		      tuples.setValue tp.Hash, tp
		    else
		      raise new RelationError("Union different columns",301)
		    end
		  next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Update(t as text)
		  dim list(), first, rest, eq, cond, silentquote as text
		  dim p as integer
		  
		  list = t.split(" ")
		  
		  first = list(0)
		  list.Remove 0
		  
		  rest = Text.join(list," ").trim
		  
		  list = rest.split(" ")
		  
		  eq = list(0)
		  
		  if eq <> "=" then
		    raise new XPError("Extend missing =",601)
		  end
		  
		  list.Remove 0
		  
		  rest = Text.join(list," ").trim
		  
		  p = rest.IndexOf(" where ")
		  
		  if p>-1 then
		    update rest.mid(p+len(" where ")), first, rest.left(p)
		  else
		    update "1", first, rest
		  end
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Update(condition as text, label as text, expression as text)
		  dim xp, xpc as XPEvaluator
		  dim d as dictionary
		  dim v as Text
		  dim newtuples as new OrderedDictionary
		  dim tp as tuple
		  dim i, c as int64
		  dim a as auto
		  const rownumberlabel as text = "rownumber"
		  
		  if header.IndexOf(label) = -1 then
		    raise new XPError("Update column does not exist",601)
		  end
		  
		  
		  xp = new XPEvaluator(functions)
		  xp.Compile(expression)
		  
		  xpc = new XPEvaluator(functions)
		  xpc.compile(condition)
		  
		  c = tuples.Count-1
		  
		  for i = 0 to c
		    if Notifier <> nil then
		      if c>4000 and i mod 4000 = 0 then
		        Notifier.SetMessage(i*100/(c+1))
		      end
		    end
		    a = tuples.value(i)
		    if a isa tuple then
		      d = tuple(a).Fields
		    else
		      raise new XPError("Update Illegal tuple",601)
		    end
		    if xpc.Evaluate(d,globals,locals) = "1" then
		      locals.Value(rownumberlabel) = str(i+1).totext
		      v = xp.evaluate(d,globals,locals)
		      d.value(label) = v
		      tp = new tuple(d)
		    else
		      tp = tuple(a)
		    end if
		    
		    newtuples.setvalue tp.hash, tp
		  next
		  
		  tuples = newtuples
		  Notifier = nil
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ValidName(t as text) As boolean
		  dim i,c as int64
		  dim ch as text
		  
		  select case t.left(1)
		  case "A" to "Z", "a" to "z","_"
		  else
		    return false
		  end
		  
		  c = t.Length-1
		  for i = 1 to c
		    ch = t.mid(i,1)
		    select case ch
		    case "A" to "Z", "a" to "z", "0" to "9", "_"
		    else
		      return false
		    end
		  next
		  
		  return true
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		aggregators As Dictionary
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  dim lines(),fields() as text
			  dim tp as tuple
			  dim test as text
			  dim i,c,j,k as integer
			  dim f,fm as text
			  dim a as auto
			  dim quote as text = TextExtensions.Quote
			  dim ret as text = TextExtensions.ret
			  dim newline as text = TextExtensions.Newline
			  dim line as text
			  
			  c = header.count-1
			  
			  lines.AddRow text.join(header,",")
			  
			  k = tuples.Count-1
			  
			  for j = 0 to k
			    a = tuples.Value(j)
			    if a isa tuple then
			      tp = tuple(a)
			    else
			      raise new RelationError("CSV no tuple",121)
			    end
			    fields.RemoveAllRows
			    for i = 0 to c
			      f = header(i)
			      test = tp.Value(f)
			      if formats.HasKey(f) then
			        fm = formats.Value(f)
			        if fm<>"" then
			          test = format2(val(test),fm).ToText
			        end
			      end
			      'if test.IsNumeric then
			      if IsNumeric(test) then
			        fields.AddRow test
			      else
			        fields.AddRow Quote + test.ReplaceAll(Quote,Quote+Quote) + Quote
			      end
			      
			    next
			    lines.AddRow Text.join(fields,",")
			  next
			  
			  return Text.join(lines,Newline)
			  
			  
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  dim s as text
			  dim lines(), fields() as text
			  dim firstline as boolean
			  dim i, c as integer
			  dim tp as tuple
			  dim d as Dictionary
			  dim state, acc as text
			  dim quoted as boolean
			  dim quote as text = TextExtensions.Quote
			  dim ret as text = TextExtensions.ret
			  dim newline as text = TextExtensions.Newline
			  dim separator as text
			  
			  
			  
			  s = value.ReplaceAll(Ret+Newline,Newline)
			  s = s.ReplaceAll(Ret,Newline)
			  
			  lines =s.Split(Newline)
			  
			  SetCSV(lines)
			  
			  
			  
			End Set
		#tag EndSetter
		CSV As Text
	#tag EndComputedProperty

	#tag Property, Flags = &h0
		formats As dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		functions As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		globals As dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		header() As text
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  dim js as new JSONItem
			  dim jline as JSONItem
			  dim i,j,m,n as integer
			  dim au as auto
			  dim tp as tuple
			  dim result as text
			  dim dlist(-1) as Dictionary
			  dim pairs(-1) as text
			  dim lines(-1) as text
			  dim f as text
			  
			  n = tuples.count-1
			  m = header.Count-1
			  
			  for i = 0 to n
			    au = tuples.Value(i)
			    if au isa tuple then
			      tp = tuple(au)
			    else
			      raise new RelationError("JSON no tuple",121)
			    end
			    redim pairs(-1)
			    for j=0 to m
			      pairs.addrow """"+header(j)+""": """+ TextExtensions.JSONescape(tp.Value(header(j)))+""""
			    next
			    lines.AddRow " { "+ text.join(pairs,", ")+ " } "
			  next
			  
			  result = "{ ""relation"": [ "+TextExtensions.Newline+  text.join(lines, ","+TextExtensions.Newline) + TextExtensions.Newline+"]}"
			  
			  return result
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  dim s as string
			  dim js, js2,js3 as JSONItem
			  dim i,n,j,m,k,l,p,q as integer
			  dim d as Dictionary
			  dim tp as Tuple
			  dim list(-1) as string
			  dim s2 as string
			  
			  s = value
			  js = new JSONItem(s)
			  
			  n = js.Count-1
			  
			  for i = 0 to n
			    if js.IsArray then
			      js2 = js.ChildAt(i)
			    else
			      list = js.names
			      js2 = js.Child(list(i))
			    end
			    if js2.IsArray then
			      m = js2.Count-1
			      for j=0 to m
			        js3 = js2.childat(j)
			        if not js3.IsArray then
			          l = js3.Count-1
			          for k = 0 to l
			            AddColumn(js3.nameAt(k).ToText)
			          next
			        end
			      next
			      q = header.Count-1
			      for j=0 to m
			        js3 = js2.childat(j)
			        if not js3.IsArray then
			          d = new Dictionary
			          for p = 0 to q
			            if js3.HasName(header(p)) then
			              s2 =  js3.Value(header(p))
			              d.Value(header(p)) =s2.totext
			            end
			          next
			          tp = new tuple(d)
			          tuples.SetValue(tp.hash,tp)
			        end
			      next
			    end
			    
			  next
			  
			End Set
		#tag EndSetter
		JSON As Text
	#tag EndComputedProperty

	#tag Property, Flags = &h0
		labels As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		locals As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		Notifier As RelationNotifier
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  dim line, result as text
			  dim fields(), lines() as text
			  dim tp as tuple
			  dim i,c,j,k as integer
			  dim f,test,fm as text
			  dim a as auto
			  dim quote as text = TextExtensions.Quote
			  dim ret as text = TextExtensions.ret
			  dim newline as text = TextExtensions.Newline
			  dim tab as text = TextExtensions.Tab
			  c = header.count-1
			  
			  for i = 0 to c
			    fields.AddRow(header(i))
			  next
			  
			  lines.AddRow text.join(header,",")
			  
			  k = tuples.Count-1
			  
			  for j = 0 to k
			    redim fields(-1)
			    a = tuples.Value(j)
			    if a isa tuple then
			      tp = tuple(a)
			    else
			      raise new RelationError("TXT no tuple",121)
			    end
			    for i = 0 to c
			      f = header(i)
			      test = tp.Value(f)
			      if formats.HasKey(f) then
			        fm = formats.Value(f)
			        if fm<>"" then
			          test = format2(val(test),fm).ToText
			        end
			      end
			      fields.AddRow(test)
			    next
			    line = Text.join(fields,Tab)
			    lines.AddRow line
			  next
			  
			  result = Text.join(lines,Newline)
			  
			  return result
			  
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  dim s as text
			  dim lines(), fields() as text
			  dim firstline as boolean
			  dim i, c as integer
			  dim t as tuple
			  dim d as Dictionary
			  dim ret as text = TextExtensions.ret
			  dim newline as text = TextExtensions.Newline
			  dim tab as text = TextExtensions.Tab
			  
			  s = value.ReplaceAll(Ret+Newline,Newline)
			  s = s.ReplaceAll(Ret,Newline)
			  
			  lines =s.Split(Newline)
			  
			  header.RemoveAllRows
			  tuples.RemoveAll
			  
			  firstline = true
			  for each line as text in lines
			    fields = line.Split(Tab)
			    if firstline then
			      for each field as text in fields
			        AddColumn CleanColumn(field)
			      next
			      c = fields.Count-1
			      firstline = false
			    else
			      d = new Dictionary
			      redim fields(c)
			      for i = 0 to c
			        d.Value(header(i))=fields(i)
			      next
			      t = new tuple(d)
			      tuples.setvalue t.Hash, t
			    end
			  next
			  
			End Set
		#tag EndSetter
		Tab As Text
	#tag EndComputedProperty

	#tag Property, Flags = &h0
		tuples As OrderedDictionary
	#tag EndProperty


	#tag Constant, Name = chartistcss, Type = Text, Dynamic = False, Default = \".ct-label {\n  fill: rgba(0\x2C 0\x2C 0\x2C 0.4);\n  color: rgba(0\x2C 0\x2C 0\x2C 0.4);\n  font-size: 0.75rem;\n  line-height: 1; }\n\n.ct-chart-line .ct-label\x2C\n.ct-chart-bar .ct-label {\n  display: block;\n  display: -webkit-box;\n  display: -moz-box;\n  display: -ms-flexbox;\n  display: -webkit-flex;\n  display: flex; }\n\n.ct-chart-pie .ct-label\x2C\n.ct-chart-donut .ct-label {\n  dominant-baseline: central; }\n\n.ct-label.ct-horizontal.ct-start {\n  -webkit-box-align: flex-end;\n  -webkit-align-items: flex-end;\n  -ms-flex-align: flex-end;\n  align-items: flex-end;\n  -webkit-box-pack: flex-start;\n  -webkit-justify-content: flex-start;\n  -ms-flex-pack: flex-start;\n  justify-content: flex-start;\n  text-align: left;\n  text-anchor: start; }\n\n.ct-label.ct-horizontal.ct-end {\n  -webkit-box-align: flex-start;\n  -webkit-align-items: flex-start;\n  -ms-flex-align: flex-start;\n  align-items: flex-start;\n  -webkit-box-pack: flex-start;\n  -webkit-justify-content: flex-start;\n  -ms-flex-pack: flex-start;\n  justify-content: flex-start;\n  text-align: left;\n  text-anchor: start; }\n\n.ct-label.ct-vertical.ct-start {\n  -webkit-box-align: flex-end;\n  -webkit-align-items: flex-end;\n  -ms-flex-align: flex-end;\n  align-items: flex-end;\n  -webkit-box-pack: flex-end;\n  -webkit-justify-content: flex-end;\n  -ms-flex-pack: flex-end;\n  justify-content: flex-end;\n  text-align: right;\n  text-anchor: end; }\n\n.ct-label.ct-vertical.ct-end {\n  -webkit-box-align: flex-end;\n  -webkit-align-items: flex-end;\n  -ms-flex-align: flex-end;\n  align-items: flex-end;\n  -webkit-box-pack: flex-start;\n  -webkit-justify-content: flex-start;\n  -ms-flex-pack: flex-start;\n  justify-content: flex-start;\n  text-align: left;\n  text-anchor: start; }\n\n.ct-chart-bar .ct-label.ct-horizontal.ct-start {\n  -webkit-box-align: flex-end;\n  -webkit-align-items: flex-end;\n  -ms-flex-align: flex-end;\n  align-items: flex-end;\n  -webkit-box-pack: center;\n  -webkit-justify-content: center;\n  -ms-flex-pack: center;\n  justify-content: center;\n  text-align: center;\n  text-anchor: start; }\n\n.ct-chart-bar .ct-label.ct-horizontal.ct-end {\n  -webkit-box-align: flex-start;\n  -webkit-align-items: flex-start;\n  -ms-flex-align: flex-start;\n  align-items: flex-start;\n  -webkit-box-pack: center;\n  -webkit-justify-content: center;\n  -ms-flex-pack: center;\n  justify-content: center;\n  text-align: center;\n  text-anchor: start; }\n\n.ct-chart-bar.ct-horizontal-bars .ct-label.ct-horizontal.ct-start {\n  -webkit-box-align: flex-end;\n  -webkit-align-items: flex-end;\n  -ms-flex-align: flex-end;\n  align-items: flex-end;\n  -webkit-box-pack: flex-start;\n  -webkit-justify-content: flex-start;\n  -ms-flex-pack: flex-start;\n  justify-content: flex-start;\n  text-align: left;\n  text-anchor: start; }\n\n.ct-chart-bar.ct-horizontal-bars .ct-label.ct-horizontal.ct-end {\n  -webkit-box-align: flex-start;\n  -webkit-align-items: flex-start;\n  -ms-flex-align: flex-start;\n  align-items: flex-start;\n  -webkit-box-pack: flex-start;\n  -webkit-justify-content: flex-start;\n  -ms-flex-pack: flex-start;\n  justify-content: flex-start;\n  text-align: left;\n  text-anchor: start; }\n\n.ct-chart-bar.ct-horizontal-bars .ct-label.ct-vertical.ct-start {\n  -webkit-box-align: center;\n  -webkit-align-items: center;\n  -ms-flex-align: center;\n  align-items: center;\n  -webkit-box-pack: flex-end;\n  -webkit-justify-content: flex-end;\n  -ms-flex-pack: flex-end;\n  justify-content: flex-end;\n  text-align: right;\n  text-anchor: end; }\n\n.ct-chart-bar.ct-horizontal-bars .ct-label.ct-vertical.ct-end {\n  -webkit-box-align: center;\n  -webkit-align-items: center;\n  -ms-flex-align: center;\n  align-items: center;\n  -webkit-box-pack: flex-start;\n  -webkit-justify-content: flex-start;\n  -ms-flex-pack: flex-start;\n  justify-content: flex-start;\n  text-align: left;\n  text-anchor: end; }\n\n.ct-grid {\n  stroke: rgba(0\x2C 0\x2C 0\x2C 0.2);\n  stroke-width: 1px;\n  stroke-dasharray: 2px; }\n\n.ct-grid-background {\n  fill: none; }\n\n.ct-point {\n  stroke-width: 10px;\n  stroke-linecap: round; }\n\n.ct-line {\n  fill: none;\n  stroke-width: 4px; }\n\n.ct-area {\n  stroke: none;\n  fill-opacity: 0.1; }\n\n.ct-bar {\n  fill: none;\n  stroke-width: 10px; }\n\n.ct-slice-donut {\n  fill: none;\n  stroke-width: 60px; }\n\n.ct-series-a .ct-point\x2C .ct-series-a .ct-line\x2C .ct-series-a .ct-bar\x2C .ct-series-a .ct-slice-donut {\n  stroke: #d70206; }\n\n.ct-series-a .ct-slice-pie\x2C .ct-series-a .ct-slice-donut-solid\x2C .ct-series-a .ct-area {\n  fill: #d70206; }\n\n.ct-series-b .ct-point\x2C .ct-series-b .ct-line\x2C .ct-series-b .ct-bar\x2C .ct-series-b .ct-slice-donut {\n  stroke: #f05b4f; }\n\n.ct-series-b .ct-slice-pie\x2C .ct-series-b .ct-slice-donut-solid\x2C .ct-series-b .ct-area {\n  fill: #f05b4f; }\n\n.ct-series-c .ct-point\x2C .ct-series-c .ct-line\x2C .ct-series-c .ct-bar\x2C .ct-series-c .ct-slice-donut {\n  stroke: #f4c63d; }\n\n.ct-series-c .ct-slice-pie\x2C .ct-series-c .ct-slice-donut-solid\x2C .ct-series-c .ct-area {\n  fill: #f4c63d; }\n\n.ct-series-d .ct-point\x2C .ct-series-d .ct-line\x2C .ct-series-d .ct-bar\x2C .ct-series-d .ct-slice-donut {\n  stroke: #d17905; }\n\n.ct-series-d .ct-slice-pie\x2C .ct-series-d .ct-slice-donut-solid\x2C .ct-series-d .ct-area {\n  fill: #d17905; }\n\n.ct-series-e .ct-point\x2C .ct-series-e .ct-line\x2C .ct-series-e .ct-bar\x2C .ct-series-e .ct-slice-donut {\n  stroke: #453d3f; }\n\n.ct-series-e .ct-slice-pie\x2C .ct-series-e .ct-slice-donut-solid\x2C .ct-series-e .ct-area {\n  fill: #453d3f; }\n\n.ct-series-f .ct-point\x2C .ct-series-f .ct-line\x2C .ct-series-f .ct-bar\x2C .ct-series-f .ct-slice-donut {\n  stroke: #59922b; }\n\n.ct-series-f .ct-slice-pie\x2C .ct-series-f .ct-slice-donut-solid\x2C .ct-series-f .ct-area {\n  fill: #59922b; }\n\n.ct-series-g .ct-point\x2C .ct-series-g .ct-line\x2C .ct-series-g .ct-bar\x2C .ct-series-g .ct-slice-donut {\n  stroke: #0544d3; }\n\n.ct-series-g .ct-slice-pie\x2C .ct-series-g .ct-slice-donut-solid\x2C .ct-series-g .ct-area {\n  fill: #0544d3; }\n\n.ct-series-h .ct-point\x2C .ct-series-h .ct-line\x2C .ct-series-h .ct-bar\x2C .ct-series-h .ct-slice-donut {\n  stroke: #6b0392; }\n\n.ct-series-h .ct-slice-pie\x2C .ct-series-h .ct-slice-donut-solid\x2C .ct-series-h .ct-area {\n  fill: #6b0392; }\n\n.ct-series-i .ct-point\x2C .ct-series-i .ct-line\x2C .ct-series-i .ct-bar\x2C .ct-series-i .ct-slice-donut {\n  stroke: #f05b4f; }\n\n.ct-series-i .ct-slice-pie\x2C .ct-series-i .ct-slice-donut-solid\x2C .ct-series-i .ct-area {\n  fill: #f05b4f; }\n\n.ct-series-j .ct-point\x2C .ct-series-j .ct-line\x2C .ct-series-j .ct-bar\x2C .ct-series-j .ct-slice-donut {\n  stroke: #dda458; }\n\n.ct-series-j .ct-slice-pie\x2C .ct-series-j .ct-slice-donut-solid\x2C .ct-series-j .ct-area {\n  fill: #dda458; }\n\n.ct-series-k .ct-point\x2C .ct-series-k .ct-line\x2C .ct-series-k .ct-bar\x2C .ct-series-k .ct-slice-donut {\n  stroke: #eacf7d; }\n\n.ct-series-k .ct-slice-pie\x2C .ct-series-k .ct-slice-donut-solid\x2C .ct-series-k .ct-area {\n  fill: #eacf7d; }\n\n.ct-series-l .ct-point\x2C .ct-series-l .ct-line\x2C .ct-series-l .ct-bar\x2C .ct-series-l .ct-slice-donut {\n  stroke: #86797d; }\n\n.ct-series-l .ct-slice-pie\x2C .ct-series-l .ct-slice-donut-solid\x2C .ct-series-l .ct-area {\n  fill: #86797d; }\n\n.ct-series-m .ct-point\x2C .ct-series-m .ct-line\x2C .ct-series-m .ct-bar\x2C .ct-series-m .ct-slice-donut {\n  stroke: #b2c326; }\n\n.ct-series-m .ct-slice-pie\x2C .ct-series-m .ct-slice-donut-solid\x2C .ct-series-m .ct-area {\n  fill: #b2c326; }\n\n.ct-series-n .ct-point\x2C .ct-series-n .ct-line\x2C .ct-series-n .ct-bar\x2C .ct-series-n .ct-slice-donut {\n  stroke: #6188e2; }\n\n.ct-series-n .ct-slice-pie\x2C .ct-series-n .ct-slice-donut-solid\x2C .ct-series-n .ct-area {\n  fill: #6188e2; }\n\n.ct-series-o .ct-point\x2C .ct-series-o .ct-line\x2C .ct-series-o .ct-bar\x2C .ct-series-o .ct-slice-donut {\n  stroke: #a748ca; }\n\n.ct-series-o .ct-slice-pie\x2C .ct-series-o .ct-slice-donut-solid\x2C .ct-series-o .ct-area {\n  fill: #a748ca; }\n\n.ct-square {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-square:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 100%; }\n  .ct-square:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-square > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-minor-second {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-minor-second:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 93.75%; }\n  .ct-minor-second:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-minor-second > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-major-second {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-major-second:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 88.8888888889%; }\n  .ct-major-second:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-major-second > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-minor-third {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-minor-third:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 83.3333333333%; }\n  .ct-minor-third:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-minor-third > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-major-third {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-major-third:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 80%; }\n  .ct-major-third:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-major-third > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-perfect-fourth {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-perfect-fourth:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 75%; }\n  .ct-perfect-fourth:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-perfect-fourth > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-perfect-fifth {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-perfect-fifth:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 66.6666666667%; }\n  .ct-perfect-fifth:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-perfect-fifth > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-minor-sixth {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-minor-sixth:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 62.5%; }\n  .ct-minor-sixth:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-minor-sixth > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-golden-section {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-golden-section:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 61.804697157%; }\n  .ct-golden-section:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-golden-section > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-major-sixth {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-major-sixth:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 60%; }\n  .ct-major-sixth:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-major-sixth > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-minor-seventh {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-minor-seventh:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 56.25%; }\n  .ct-minor-seventh:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-minor-seventh > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-major-seventh {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-major-seventh:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 53.3333333333%; }\n  .ct-major-seventh:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-major-seventh > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-octave {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-octave:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 50%; }\n  .ct-octave:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-octave > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-major-tenth {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-major-tenth:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 40%; }\n  .ct-major-tenth:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-major-tenth > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-major-eleventh {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-major-eleventh:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 37.5%; }\n  .ct-major-eleventh:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-major-eleventh > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-major-twelfth {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-major-twelfth:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 33.3333333333%; }\n  .ct-major-twelfth:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-major-twelfth > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n\n.ct-double-octave {\n  display: block;\n  position: relative;\n  width: 100%; }\n  .ct-double-octave:before {\n    display: block;\n    float: left;\n    content: \"\";\n    width: 0;\n    height: 0;\n    padding-bottom: 25%; }\n  .ct-double-octave:after {\n    content: \"\";\n    display: table;\n    clear: both; }\n  .ct-double-octave > svg {\n    display: block;\n    position: absolute;\n    top: 0;\n    left: 0; }\n", Scope = Public
	#tag EndConstant

	#tag Constant, Name = chartistcss2, Type = Text, Dynamic = False, Default = \"/* sofawiki modifications to chartist.css */\n\n.ct-label {\n  color: black;\n  font-size: 1rem; \n  font-family: serif; \n}\n\n.ct-label.ct-horizontal.ct-end {\n\tfont-size: 1rem;\n}\n\n.ct-chart-pie .ct-label\x2C .ct-chart-donut .ct-label {\n\tfont-size: 1rem;\n\tfill: white;\n\tfont-weight: bold;\n}\n\n.ct-grid-background {\n  fill: blue; }\n  \n.ct-series-a .ct-bar\x2C .ct-series-a .ct-line\x2C .ct-series-a .ct-point  {\nstroke: blue;\n\n}\n.ct-series-a .ct-slice-pie\x2C .ct-series-a .ct-slice-donut-solid\x2C .ct-series-a .ct-area {\n\tfill: blue;\n}\n\n.ct-series-b .ct-bar\x2C .ct-series-b .ct-line\x2C .ct-series-b .ct-point  {\nstroke: green;\n\n}\n\n.ct-series-b .ct-slice-pie\x2C .ct-series-b .ct-slice-donut-solid\x2C .ct-series-b .ct-area {\n\tfill: green;\n}\n\n.ct-series-c .ct-bar\x2C .ct-series-c .ct-line\x2C .ct-series-c .ct-point  {\nstroke: red;\n}\n\n.ct-series-c .ct-slice-pie\x2C .ct-series-c .ct-slice-donut-solid\x2C .ct-series-d .ct-area {\n\tfill: red;\n}\n\n.ct-series-d .ct-bar\x2C .ct-series-d .ct-line\x2C .ct-series-d .ct-point  {\nstroke: orange;\n}\n\n.ct-series-d .ct-slice-pie\x2C .ct-series-d .ct-slice-donut-solid\x2C .ct-series-d .ct-area {\n\tfill: orange;\n}\n\n.ct-series-e .ct-bar\x2C .ct-series-e .ct-line\x2C .ct-series-e .ct-point  {\nstroke: violet;\n}\n\n.ct-series-e .ct-slice-pie\x2C .ct-series-e .ct-slice-donut-solid\x2C .ct-series-e .ct-area {\n\tfill: violet;\n}\n\n.ct-series-f .ct-bar\x2C .ct-series-f .ct-line\x2C .ct-series-f .ct-point  {\nstroke: grey;\n}\n\n.ct-series-f .ct-slice-pie\x2C .ct-series-f .ct-slice-donut-solid\x2C .ct-series-f .ct-area {\n\tfill: grey;\n}\n\n\n\n", Scope = Public
	#tag EndConstant

	#tag Constant, Name = chartistjs, Type = Text, Dynamic = False, Default = \"/* Chartist.js 0.11.3\n * Copyright \xC2\xA9 2019 Gion Kunz\n * Free to use under either the WTFPL license or the MIT license.\n * https://raw.githubusercontent.com/gionkunz/chartist-js/master/LICENSE-WTFPL\n * https://raw.githubusercontent.com/gionkunz/chartist-js/master/LICENSE-MIT\n */\n\n!function(a\x2Cb){\"function\"\x3D\x3Dtypeof define&&define.amd\?define(\"Chartist\"\x2C[]\x2Cfunction(){return a.Chartist\x3Db()}):\"object\"\x3D\x3Dtypeof module&&module.exports\?module.exports\x3Db():a.Chartist\x3Db()}(this\x2Cfunction(){var a\x3D{version:\"0.11.3\"};return function(a\x2Cb){\"use strict\";var c\x3Da.window\x2Cd\x3Da.document;b.namespaces\x3D{svg:\"http://www.w3.org/2000/svg\"\x2Cxmlns:\"http://www.w3.org/2000/xmlns/\"\x2Cxhtml:\"http://www.w3.org/1999/xhtml\"\x2Cxlink:\"http://www.w3.org/1999/xlink\"\x2Cct:\"http://gionkunz.github.com/chartist-js/ct\"}\x2Cb.noop\x3Dfunction(a){return a}\x2Cb.alphaNumerate\x3Dfunction(a){return String.fromCharCode(97+a%26)}\x2Cb.extend\x3Dfunction(a){var c\x2Cd\x2Ce;for(a\x3Da||{}\x2Cc\x3D1;c<arguments.length;c++){d\x3Darguments[c];for(var f in d)e\x3Dd[f]\x2C\"object\"!\x3Dtypeof e||null\x3D\x3D\x3De||e instanceof Array\?a[f]\x3De:a[f]\x3Db.extend(a[f]\x2Ce)}return a}\x2Cb.replaceAll\x3Dfunction(a\x2Cb\x2Cc){return a.replace(new RegExp(b\x2C\"g\")\x2Cc)}\x2Cb.ensureUnit\x3Dfunction(a\x2Cb){return\"number\"\x3D\x3Dtypeof a&&(a+\x3Db)\x2Ca}\x2Cb.quantity\x3Dfunction(a){if(\"string\"\x3D\x3Dtypeof a){var b\x3D/^(\\d+)\\s*(.*)$/g.exec(a);return{value:+b[1]\x2Cunit:b[2]||void 0}}return{value:a}}\x2Cb.querySelector\x3Dfunction(a){return a instanceof Node\?a:d.querySelector(a)}\x2Cb.times\x3Dfunction(a){return Array.apply(null\x2Cnew Array(a))}\x2Cb.sum\x3Dfunction(a\x2Cb){return a+(b\?b:0)}\x2Cb.mapMultiply\x3Dfunction(a){return function(b){return b*a}}\x2Cb.mapAdd\x3Dfunction(a){return function(b){return b+a}}\x2Cb.serialMap\x3Dfunction(a\x2Cc){var d\x3D[]\x2Ce\x3DMath.max.apply(null\x2Ca.map(function(a){return a.length}));return b.times(e).forEach(function(b\x2Ce){var f\x3Da.map(function(a){return a[e]});d[e]\x3Dc.apply(null\x2Cf)})\x2Cd}\x2Cb.roundWithPrecision\x3Dfunction(a\x2Cc){var d\x3DMath.pow(10\x2Cc||b.precision);return Math.round(a*d)/d}\x2Cb.precision\x3D8\x2Cb.escapingMap\x3D{\"&\":\"&amp;\"\x2C\"<\":\"&lt;\"\x2C\">\":\"&gt;\"\x2C\'\"\':\"&quot;\"\x2C\"\'\":\"&#039;\"}\x2Cb.serialize\x3Dfunction(a){return null\x3D\x3D\x3Da||void 0\x3D\x3D\x3Da\?a:(\"number\"\x3D\x3Dtypeof a\?a\x3D\"\"+a:\"object\"\x3D\x3Dtypeof a&&(a\x3DJSON.stringify({data:a}))\x2CObject.keys(b.escapingMap).reduce(function(a\x2Cc){return b.replaceAll(a\x2Cc\x2Cb.escapingMap[c])}\x2Ca))}\x2Cb.deserialize\x3Dfunction(a){if(\"string\"!\x3Dtypeof a)return a;a\x3DObject.keys(b.escapingMap).reduce(function(a\x2Cc){return b.replaceAll(a\x2Cb.escapingMap[c]\x2Cc)}\x2Ca);try{a\x3DJSON.parse(a)\x2Ca\x3Dvoid 0!\x3D\x3Da.data\?a.data:a}catch(c){}return a}\x2Cb.createSvg\x3Dfunction(a\x2Cc\x2Cd\x2Ce){var f;return c\x3Dc||\"100%\"\x2Cd\x3Dd||\"100%\"\x2CArray.prototype.slice.call(a.querySelectorAll(\"svg\")).filter(function(a){return a.getAttributeNS(b.namespaces.xmlns\x2C\"ct\")}).forEach(function(b){a.removeChild(b)})\x2Cf\x3Dnew b.Svg(\"svg\").attr({width:c\x2Cheight:d}).addClass(e)\x2Cf._node.style.width\x3Dc\x2Cf._node.style.height\x3Dd\x2Ca.appendChild(f._node)\x2Cf}\x2Cb.normalizeData\x3Dfunction(a\x2Cc\x2Cd){var e\x2Cf\x3D{raw:a\x2Cnormalized:{}};return f.normalized.series\x3Db.getDataArray({series:a.series||[]}\x2Cc\x2Cd)\x2Ce\x3Df.normalized.series.every(function(a){return a instanceof Array})\?Math.max.apply(null\x2Cf.normalized.series.map(function(a){return a.length})):f.normalized.series.length\x2Cf.normalized.labels\x3D(a.labels||[]).slice()\x2CArray.prototype.push.apply(f.normalized.labels\x2Cb.times(Math.max(0\x2Ce-f.normalized.labels.length)).map(function(){return\"\"}))\x2Cc&&b.reverseData(f.normalized)\x2Cf}\x2Cb.safeHasProperty\x3Dfunction(a\x2Cb){return null!\x3D\x3Da&&\"object\"\x3D\x3Dtypeof a&&a.hasOwnProperty(b)}\x2Cb.isDataHoleValue\x3Dfunction(a){return null\x3D\x3D\x3Da||void 0\x3D\x3D\x3Da||\"number\"\x3D\x3Dtypeof a&&isNaN(a)}\x2Cb.reverseData\x3Dfunction(a){a.labels.reverse()\x2Ca.series.reverse();for(var b\x3D0;b<a.series.length;b++)\"object\"\x3D\x3Dtypeof a.series[b]&&void 0!\x3D\x3Da.series[b].data\?a.series[b].data.reverse():a.series[b]instanceof Array&&a.series[b].reverse()}\x2Cb.getDataArray\x3Dfunction(a\x2Cc\x2Cd){function e(a){if(b.safeHasProperty(a\x2C\"value\"))return e(a.value);if(b.safeHasProperty(a\x2C\"data\"))return e(a.data);if(a instanceof Array)return a.map(e);if(!b.isDataHoleValue(a)){if(d){var c\x3D{};return\"string\"\x3D\x3Dtypeof d\?c[d]\x3Db.getNumberOrUndefined(a):c.y\x3Db.getNumberOrUndefined(a)\x2Cc.x\x3Da.hasOwnProperty(\"x\")\?b.getNumberOrUndefined(a.x):c.x\x2Cc.y\x3Da.hasOwnProperty(\"y\")\?b.getNumberOrUndefined(a.y):c.y\x2Cc}return b.getNumberOrUndefined(a)}}return a.series.map(e)}\x2Cb.normalizePadding\x3Dfunction(a\x2Cb){return b\x3Db||0\x2C\"number\"\x3D\x3Dtypeof a\?{top:a\x2Cright:a\x2Cbottom:a\x2Cleft:a}:{top:\"number\"\x3D\x3Dtypeof a.top\?a.top:b\x2Cright:\"number\"\x3D\x3Dtypeof a.right\?a.right:b\x2Cbottom:\"number\"\x3D\x3Dtypeof a.bottom\?a.bottom:b\x2Cleft:\"number\"\x3D\x3Dtypeof a.left\?a.left:b}}\x2Cb.getMetaData\x3Dfunction(a\x2Cb){var c\x3Da.data\?a.data[b]:a[b];return c\?c.meta:void 0}\x2Cb.orderOfMagnitude\x3Dfunction(a){return Math.floor(Math.log(Math.abs(a))/Math.LN10)}\x2Cb.projectLength\x3Dfunction(a\x2Cb\x2Cc){return b/c.range*a}\x2Cb.getAvailableHeight\x3Dfunction(a\x2Cc){return Math.max((b.quantity(c.height).value||a.height())-(c.chartPadding.top+c.chartPadding.bottom)-c.axisX.offset\x2C0)}\x2Cb.getHighLow\x3Dfunction(a\x2Cc\x2Cd){function e(a){if(void 0!\x3D\x3Da)if(a instanceof Array)for(var b\x3D0;b<a.length;b++)e(a[b]);else{var c\x3Dd\?+a[d]:+a;g&&c>f.high&&(f.high\x3Dc)\x2Ch&&c<f.low&&(f.low\x3Dc)}}c\x3Db.extend({}\x2Cc\x2Cd\?c[\"axis\"+d.toUpperCase()]:{});var f\x3D{high:void 0\x3D\x3D\x3Dc.high\?-Number.MAX_VALUE:+c.high\x2Clow:void 0\x3D\x3D\x3Dc.low\?Number.MAX_VALUE:+c.low}\x2Cg\x3Dvoid 0\x3D\x3D\x3Dc.high\x2Ch\x3Dvoid 0\x3D\x3D\x3Dc.low;return(g||h)&&e(a)\x2C(c.referenceValue||0\x3D\x3D\x3Dc.referenceValue)&&(f.high\x3DMath.max(c.referenceValue\x2Cf.high)\x2Cf.low\x3DMath.min(c.referenceValue\x2Cf.low))\x2Cf.high<\x3Df.low&&(0\x3D\x3D\x3Df.low\?f.high\x3D1:f.low<0\?f.high\x3D0:f.high>0\?f.low\x3D0:(f.high\x3D1\x2Cf.low\x3D0))\x2Cf}\x2Cb.isNumeric\x3Dfunction(a){return null!\x3D\x3Da&&isFinite(a)}\x2Cb.isFalseyButZero\x3Dfunction(a){return!a&&0!\x3D\x3Da}\x2Cb.getNumberOrUndefined\x3Dfunction(a){return b.isNumeric(a)\?+a:void 0}\x2Cb.isMultiValue\x3Dfunction(a){return\"object\"\x3D\x3Dtypeof a&&(\"x\"in a||\"y\"in a)}\x2Cb.getMultiValue\x3Dfunction(a\x2Cc){return b.isMultiValue(a)\?b.getNumberOrUndefined(a[c||\"y\"]):b.getNumberOrUndefined(a)}\x2Cb.rho\x3Dfunction(a){function b(a\x2Cc){return a%c\x3D\x3D\x3D0\?c:b(c\x2Ca%c)}function c(a){return a*a+1}if(1\x3D\x3D\x3Da)return a;var d\x2Ce\x3D2\x2Cf\x3D2;if(a%2\x3D\x3D\x3D0)return 2;do e\x3Dc(e)%a\x2Cf\x3Dc(c(f))%a\x2Cd\x3Db(Math.abs(e-f)\x2Ca);while(1\x3D\x3D\x3Dd);return d}\x2Cb.getBounds\x3Dfunction(a\x2Cc\x2Cd\x2Ce){function f(a\x2Cb){return a\x3D\x3D\x3D(a+\x3Db)&&(a*\x3D1+(b>0\?o:-o))\x2Ca}var g\x2Ch\x2Ci\x2Cj\x3D0\x2Ck\x3D{high:c.high\x2Clow:c.low};k.valueRange\x3Dk.high-k.low\x2Ck.oom\x3Db.orderOfMagnitude(k.valueRange)\x2Ck.step\x3DMath.pow(10\x2Ck.oom)\x2Ck.min\x3DMath.floor(k.low/k.step)*k.step\x2Ck.max\x3DMath.ceil(k.high/k.step)*k.step\x2Ck.range\x3Dk.max-k.min\x2Ck.numberOfSteps\x3DMath.round(k.range/k.step);var l\x3Db.projectLength(a\x2Ck.step\x2Ck)\x2Cm\x3Dl<d\x2Cn\x3De\?b.rho(k.range):0;if(e&&b.projectLength(a\x2C1\x2Ck)>\x3Dd)k.step\x3D1;else if(e&&n<k.step&&b.projectLength(a\x2Cn\x2Ck)>\x3Dd)k.step\x3Dn;else for(;;){if(m&&b.projectLength(a\x2Ck.step\x2Ck)<\x3Dd)k.step*\x3D2;else{if(m||!(b.projectLength(a\x2Ck.step/2\x2Ck)>\x3Dd))break;if(k.step/\x3D2\x2Ce&&k.step%1!\x3D\x3D0){k.step*\x3D2;break}}if(j++>1e3)throw new Error(\"Exceeded maximum number of iterations while optimizing scale step!\")}var o\x3D2.221e-16;for(k.step\x3DMath.max(k.step\x2Co)\x2Ch\x3Dk.min\x2Ci\x3Dk.max;h+k.step<\x3Dk.low;)h\x3Df(h\x2Ck.step);for(;i-k.step>\x3Dk.high;)i\x3Df(i\x2C-k.step);k.min\x3Dh\x2Ck.max\x3Di\x2Ck.range\x3Dk.max-k.min;var p\x3D[];for(g\x3Dk.min;g<\x3Dk.max;g\x3Df(g\x2Ck.step)){var q\x3Db.roundWithPrecision(g);q!\x3D\x3Dp[p.length-1]&&p.push(q)}return k.values\x3Dp\x2Ck}\x2Cb.polarToCartesian\x3Dfunction(a\x2Cb\x2Cc\x2Cd){var e\x3D(d-90)*Math.PI/180;return{x:a+c*Math.cos(e)\x2Cy:b+c*Math.sin(e)}}\x2Cb.createChartRect\x3Dfunction(a\x2Cc\x2Cd){var e\x3D!(!c.axisX&&!c.axisY)\x2Cf\x3De\?c.axisY.offset:0\x2Cg\x3De\?c.axisX.offset:0\x2Ch\x3Da.width()||b.quantity(c.width).value||0\x2Ci\x3Da.height()||b.quantity(c.height).value||0\x2Cj\x3Db.normalizePadding(c.chartPadding\x2Cd);h\x3DMath.max(h\x2Cf+j.left+j.right)\x2Ci\x3DMath.max(i\x2Cg+j.top+j.bottom);var k\x3D{padding:j\x2Cwidth:function(){return this.x2-this.x1}\x2Cheight:function(){return this.y1-this.y2}};return e\?(\"start\"\x3D\x3D\x3Dc.axisX.position\?(k.y2\x3Dj.top+g\x2Ck.y1\x3DMath.max(i-j.bottom\x2Ck.y2+1)):(k.y2\x3Dj.top\x2Ck.y1\x3DMath.max(i-j.bottom-g\x2Ck.y2+1))\x2C\"start\"\x3D\x3D\x3Dc.axisY.position\?(k.x1\x3Dj.left+f\x2Ck.x2\x3DMath.max(h-j.right\x2Ck.x1+1)):(k.x1\x3Dj.left\x2Ck.x2\x3DMath.max(h-j.right-f\x2Ck.x1+1))):(k.x1\x3Dj.left\x2Ck.x2\x3DMath.max(h-j.right\x2Ck.x1+1)\x2Ck.y2\x3Dj.top\x2Ck.y1\x3DMath.max(i-j.bottom\x2Ck.y2+1))\x2Ck}\x2Cb.createGrid\x3Dfunction(a\x2Cc\x2Cd\x2Ce\x2Cf\x2Cg\x2Ch\x2Ci){var j\x3D{};j[d.units.pos+\"1\"]\x3Da\x2Cj[d.units.pos+\"2\"]\x3Da\x2Cj[d.counterUnits.pos+\"1\"]\x3De\x2Cj[d.counterUnits.pos+\"2\"]\x3De+f;var k\x3Dg.elem(\"line\"\x2Cj\x2Ch.join(\" \"));i.emit(\"draw\"\x2Cb.extend({type:\"grid\"\x2Caxis:d\x2Cindex:c\x2Cgroup:g\x2Celement:k}\x2Cj))}\x2Cb.createGridBackground\x3Dfunction(a\x2Cb\x2Cc\x2Cd){var e\x3Da.elem(\"rect\"\x2C{x:b.x1\x2Cy:b.y2\x2Cwidth:b.width()\x2Cheight:b.height()}\x2Cc\x2C!0);d.emit(\"draw\"\x2C{type:\"gridBackground\"\x2Cgroup:a\x2Celement:e})}\x2Cb.createLabel\x3Dfunction(a\x2Cc\x2Ce\x2Cf\x2Cg\x2Ch\x2Ci\x2Cj\x2Ck\x2Cl\x2Cm){var n\x2Co\x3D{};if(o[g.units.pos]\x3Da+i[g.units.pos]\x2Co[g.counterUnits.pos]\x3Di[g.counterUnits.pos]\x2Co[g.units.len]\x3Dc\x2Co[g.counterUnits.len]\x3DMath.max(0\x2Ch-10)\x2Cl){var p\x3Dd.createElement(\"span\");p.className\x3Dk.join(\" \")\x2Cp.setAttribute(\"xmlns\"\x2Cb.namespaces.xhtml)\x2Cp.innerText\x3Df[e]\x2Cp.style[g.units.len]\x3DMath.round(o[g.units.len])+\"px\"\x2Cp.style[g.counterUnits.len]\x3DMath.round(o[g.counterUnits.len])+\"px\"\x2Cn\x3Dj.foreignObject(p\x2Cb.extend({style:\"overflow: visible;\"}\x2Co))}else n\x3Dj.elem(\"text\"\x2Co\x2Ck.join(\" \")).text(f[e]);m.emit(\"draw\"\x2Cb.extend({type:\"label\"\x2Caxis:g\x2Cindex:e\x2Cgroup:j\x2Celement:n\x2Ctext:f[e]}\x2Co))}\x2Cb.getSeriesOption\x3Dfunction(a\x2Cb\x2Cc){if(a.name&&b.series&&b.series[a.name]){var d\x3Db.series[a.name];return d.hasOwnProperty(c)\?d[c]:b[c]}return b[c]}\x2Cb.optionsProvider\x3Dfunction(a\x2Cd\x2Ce){function f(a){var f\x3Dh;if(h\x3Db.extend({}\x2Cj)\x2Cd)for(i\x3D0;i<d.length;i++){var g\x3Dc.matchMedia(d[i][0]);g.matches&&(h\x3Db.extend(h\x2Cd[i][1]))}e&&a&&e.emit(\"optionsChanged\"\x2C{previousOptions:f\x2CcurrentOptions:h})}function g(){k.forEach(function(a){a.removeListener(f)})}var h\x2Ci\x2Cj\x3Db.extend({}\x2Ca)\x2Ck\x3D[];if(!c.matchMedia)throw\"window.matchMedia not found! Make sure you\'re using a polyfill.\";if(d)for(i\x3D0;i<d.length;i++){var l\x3Dc.matchMedia(d[i][0]);l.addListener(f)\x2Ck.push(l)}return f()\x2C{removeMediaQueryListeners:g\x2CgetCurrentOptions:function(){return b.extend({}\x2Ch)}}}\x2Cb.splitIntoSegments\x3Dfunction(a\x2Cc\x2Cd){var e\x3D{increasingX:!1\x2CfillHoles:!1};d\x3Db.extend({}\x2Ce\x2Cd);for(var f\x3D[]\x2Cg\x3D!0\x2Ch\x3D0;h<a.length;h+\x3D2)void 0\x3D\x3D\x3Db.getMultiValue(c[h/2].value)\?d.fillHoles||(g\x3D!0):(d.increasingX&&h>\x3D2&&a[h]<\x3Da[h-2]&&(g\x3D!0)\x2Cg&&(f.push({pathCoordinates:[]\x2CvalueData:[]})\x2Cg\x3D!1)\x2Cf[f.length-1].pathCoordinates.push(a[h]\x2Ca[h+1])\x2Cf[f.length-1].valueData.push(c[h/2]));return f}}(this\x2Ca)\x2Cfunction(a\x2Cb){\"use strict\";b.Interpolation\x3D{}\x2Cb.Interpolation.none\x3Dfunction(a){var c\x3D{fillHoles:!1};return a\x3Db.extend({}\x2Cc\x2Ca)\x2Cfunction(c\x2Cd){for(var e\x3Dnew b.Svg.Path\x2Cf\x3D!0\x2Cg\x3D0;g<c.length;g+\x3D2){var h\x3Dc[g]\x2Ci\x3Dc[g+1]\x2Cj\x3Dd[g/2];void 0!\x3D\x3Db.getMultiValue(j.value)\?(f\?e.move(h\x2Ci\x2C!1\x2Cj):e.line(h\x2Ci\x2C!1\x2Cj)\x2Cf\x3D!1):a.fillHoles||(f\x3D!0)}return e}}\x2Cb.Interpolation.simple\x3Dfunction(a){var c\x3D{divisor:2\x2CfillHoles:!1};a\x3Db.extend({}\x2Cc\x2Ca);var d\x3D1/Math.max(1\x2Ca.divisor);return function(c\x2Ce){for(var f\x2Cg\x2Ch\x2Ci\x3Dnew b.Svg.Path\x2Cj\x3D0;j<c.length;j+\x3D2){var k\x3Dc[j]\x2Cl\x3Dc[j+1]\x2Cm\x3D(k-f)*d\x2Cn\x3De[j/2];void 0!\x3D\x3Dn.value\?(void 0\x3D\x3D\x3Dh\?i.move(k\x2Cl\x2C!1\x2Cn):i.curve(f+m\x2Cg\x2Ck-m\x2Cl\x2Ck\x2Cl\x2C!1\x2Cn)\x2Cf\x3Dk\x2Cg\x3Dl\x2Ch\x3Dn):a.fillHoles||(f\x3Dk\x3Dh\x3Dvoid 0)}return i}}\x2Cb.Interpolation.cardinal\x3Dfunction(a){var c\x3D{tension:1\x2CfillHoles:!1};a\x3Db.extend({}\x2Cc\x2Ca);var d\x3DMath.min(1\x2CMath.max(0\x2Ca.tension))\x2Ce\x3D1-d;return function f(c\x2Cg){var h\x3Db.splitIntoSegments(c\x2Cg\x2C{fillHoles:a.fillHoles});if(h.length){if(h.length>1){var i\x3D[];return h.forEach(function(a){i.push(f(a.pathCoordinates\x2Ca.valueData))})\x2Cb.Svg.Path.join(i)}if(c\x3Dh[0].pathCoordinates\x2Cg\x3Dh[0].valueData\x2Cc.length<\x3D4)return b.Interpolation.none()(c\x2Cg);for(var j\x2Ck\x3D(new b.Svg.Path).move(c[0]\x2Cc[1]\x2C!1\x2Cg[0])\x2Cl\x3D0\x2Cm\x3Dc.length;m-2*!j>l;l+\x3D2){var n\x3D[{x:+c[l-2]\x2Cy:+c[l-1]}\x2C{x:+c[l]\x2Cy:+c[l+1]}\x2C{x:+c[l+2]\x2Cy:+c[l+3]}\x2C{x:+c[l+4]\x2Cy:+c[l+5]}];j\?l\?m-4\x3D\x3D\x3Dl\?n[3]\x3D{x:+c[0]\x2Cy:+c[1]}:m-2\x3D\x3D\x3Dl&&(n[2]\x3D{x:+c[0]\x2Cy:+c[1]}\x2Cn[3]\x3D{x:+c[2]\x2Cy:+c[3]}):n[0]\x3D{x:+c[m-2]\x2Cy:+c[m-1]}:m-4\x3D\x3D\x3Dl\?n[3]\x3Dn[2]:l||(n[0]\x3D{x:+c[l]\x2Cy:+c[l+1]})\x2Ck.curve(d*(-n[0].x+6*n[1].x+n[2].x)/6+e*n[2].x\x2Cd*(-n[0].y+6*n[1].y+n[2].y)/6+e*n[2].y\x2Cd*(n[1].x+6*n[2].x-n[3].x)/6+e*n[2].x\x2Cd*(n[1].y+6*n[2].y-n[3].y)/6+e*n[2].y\x2Cn[2].x\x2Cn[2].y\x2C!1\x2Cg[(l+2)/2])}return k}return b.Interpolation.none()([])}}\x2Cb.Interpolation.monotoneCubic\x3Dfunction(a){var c\x3D{fillHoles:!1};return a\x3Db.extend({}\x2Cc\x2Ca)\x2Cfunction d(c\x2Ce){var f\x3Db.splitIntoSegments(c\x2Ce\x2C{fillHoles:a.fillHoles\x2CincreasingX:!0});if(f.length){if(f.length>1){var g\x3D[];return f.forEach(function(a){g.push(d(a.pathCoordinates\x2Ca.valueData))})\x2Cb.Svg.Path.join(g)}if(c\x3Df[0].pathCoordinates\x2Ce\x3Df[0].valueData\x2Cc.length<\x3D4)return b.Interpolation.none()(c\x2Ce);var h\x2Ci\x2Cj\x3D[]\x2Ck\x3D[]\x2Cl\x3Dc.length/2\x2Cm\x3D[]\x2Cn\x3D[]\x2Co\x3D[]\x2Cp\x3D[];for(h\x3D0;h<l;h++)j[h]\x3Dc[2*h]\x2Ck[h]\x3Dc[2*h+1];for(h\x3D0;h<l-1;h++)o[h]\x3Dk[h+1]-k[h]\x2Cp[h]\x3Dj[h+1]-j[h]\x2Cn[h]\x3Do[h]/p[h];for(m[0]\x3Dn[0]\x2Cm[l-1]\x3Dn[l-2]\x2Ch\x3D1;h<l-1;h++)0\x3D\x3D\x3Dn[h]||0\x3D\x3D\x3Dn[h-1]||n[h-1]>0!\x3Dn[h]>0\?m[h]\x3D0:(m[h]\x3D3*(p[h-1]+p[h])/((2*p[h]+p[h-1])/n[h-1]+(p[h]+2*p[h-1])/n[h])\x2CisFinite(m[h])||(m[h]\x3D0));for(i\x3D(new b.Svg.Path).move(j[0]\x2Ck[0]\x2C!1\x2Ce[0])\x2Ch\x3D0;h<l-1;h++)i.curve(j[h]+p[h]/3\x2Ck[h]+m[h]*p[h]/3\x2Cj[h+1]-p[h]/3\x2Ck[h+1]-m[h+1]*p[h]/3\x2Cj[h+1]\x2Ck[h+1]\x2C!1\x2Ce[h+1]);return i}return b.Interpolation.none()([])}}\x2Cb.Interpolation.step\x3Dfunction(a){var c\x3D{postpone:!0\x2CfillHoles:!1};return a\x3Db.extend({}\x2Cc\x2Ca)\x2Cfunction(c\x2Cd){for(var e\x2Cf\x2Cg\x2Ch\x3Dnew b.Svg.Path\x2Ci\x3D0;i<c.length;i+\x3D2){var j\x3Dc[i]\x2Ck\x3Dc[i+1]\x2Cl\x3Dd[i/2];void 0!\x3D\x3Dl.value\?(void 0\x3D\x3D\x3Dg\?h.move(j\x2Ck\x2C!1\x2Cl):(a.postpone\?h.line(j\x2Cf\x2C!1\x2Cg):h.line(e\x2Ck\x2C!1\x2Cl)\x2Ch.line(j\x2Ck\x2C!1\x2Cl))\x2Ce\x3Dj\x2Cf\x3Dk\x2Cg\x3Dl):a.fillHoles||(e\x3Df\x3Dg\x3Dvoid 0)}return h}}}(this\x2Ca)\x2Cfunction(a\x2Cb){\"use strict\";b.EventEmitter\x3Dfunction(){function a(a\x2Cb){d[a]\x3Dd[a]||[]\x2Cd[a].push(b)}function b(a\x2Cb){d[a]&&(b\?(d[a].splice(d[a].indexOf(b)\x2C1)\x2C0\x3D\x3D\x3Dd[a].length&&delete d[a]):delete d[a])}function c(a\x2Cb){d[a]&&d[a].forEach(function(a){a(b)})\x2Cd[\"*\"]&&d[\"*\"].forEach(function(c){c(a\x2Cb)})}var d\x3D[];return{addEventHandler:a\x2CremoveEventHandler:b\x2Cemit:c}}}(this\x2Ca)\x2Cfunction(a\x2Cb){\"use strict\";function c(a){var b\x3D[];if(a.length)for(var c\x3D0;c<a.length;c++)b.push(a[c]);return b}function d(a\x2Cc){var d\x3Dc||this.prototype||b.Class\x2Ce\x3DObject.create(d);b.Class.cloneDefinitions(e\x2Ca);var f\x3Dfunction(){var a\x2Cc\x3De.constructor||function(){};return a\x3Dthis\x3D\x3D\x3Db\?Object.create(e):this\x2Cc.apply(a\x2CArray.prototype.slice.call(arguments\x2C0))\x2Ca};return f.prototype\x3De\x2Cf[\"super\"]\x3Dd\x2Cf.extend\x3Dthis.extend\x2Cf}function e(){var a\x3Dc(arguments)\x2Cb\x3Da[0];return a.splice(1\x2Ca.length-1).forEach(function(a){Object.getOwnPropertyNames(a).forEach(function(c){delete b[c]\x2CObject.defineProperty(b\x2Cc\x2CObject.getOwnPropertyDescriptor(a\x2Cc))})})\x2Cb}b.Class\x3D{extend:d\x2CcloneDefinitions:e}}(this\x2Ca)\x2Cfunction(a\x2Cb){\"use strict\";function c(a\x2Cc\x2Cd){return a&&(this.data\x3Da||{}\x2Cthis.data.labels\x3Dthis.data.labels||[]\x2Cthis.data.series\x3Dthis.data.series||[]\x2Cthis.eventEmitter.emit(\"data\"\x2C{type:\"update\"\x2Cdata:this.data}))\x2Cc&&(this.options\x3Db.extend({}\x2Cd\?this.options:this.defaultOptions\x2Cc)\x2Cthis.initializeTimeoutId||(this.optionsProvider.removeMediaQueryListeners()\x2Cthis.optionsProvider\x3Db.optionsProvider(this.options\x2Cthis.responsiveOptions\x2Cthis.eventEmitter)))\x2Cthis.initializeTimeoutId||this.createChart(this.optionsProvider.getCurrentOptions())\x2Cthis}function d(){return this.initializeTimeoutId\?i.clearTimeout(this.initializeTimeoutId):(i.removeEventListener(\"resize\"\x2Cthis.resizeListener)\x2Cthis.optionsProvider.removeMediaQueryListeners())\x2Cthis}function e(a\x2Cb){return this.eventEmitter.addEventHandler(a\x2Cb)\x2Cthis}function f(a\x2Cb){return this.eventEmitter.removeEventHandler(a\x2Cb)\x2Cthis}function g(){i.addEventListener(\"resize\"\x2Cthis.resizeListener)\x2Cthis.optionsProvider\x3Db.optionsProvider(this.options\x2Cthis.responsiveOptions\x2Cthis.eventEmitter)\x2Cthis.eventEmitter.addEventHandler(\"optionsChanged\"\x2Cfunction(){this.update()}.bind(this))\x2Cthis.options.plugins&&this.options.plugins.forEach(function(a){a instanceof Array\?a[0](this\x2Ca[1]):a(this)}.bind(this))\x2Cthis.eventEmitter.emit(\"data\"\x2C{type:\"initial\"\x2Cdata:this.data})\x2Cthis.createChart(this.optionsProvider.getCurrentOptions())\x2Cthis.initializeTimeoutId\x3Dvoid 0}function h(a\x2Cc\x2Cd\x2Ce\x2Cf){this.container\x3Db.querySelector(a)\x2Cthis.data\x3Dc||{}\x2Cthis.data.labels\x3Dthis.data.labels||[]\x2Cthis.data.series\x3Dthis.data.series||[]\x2Cthis.defaultOptions\x3Dd\x2Cthis.options\x3De\x2Cthis.responsiveOptions\x3Df\x2Cthis.eventEmitter\x3Db.EventEmitter()\x2Cthis.supportsForeignObject\x3Db.Svg.isSupported(\"Extensibility\")\x2Cthis.supportsAnimations\x3Db.Svg.isSupported(\"AnimationEventsAttribute\")\x2Cthis.resizeListener\x3Dfunction(){this.update()}.bind(this)\x2Cthis.container&&(this.container.__chartist__&&this.container.__chartist__.detach()\x2Cthis.container.__chartist__\x3Dthis)\x2Cthis.initializeTimeoutId\x3DsetTimeout(g.bind(this)\x2C0)}var i\x3Da.window;b.Base\x3Db.Class.extend({constructor:h\x2CoptionsProvider:void 0\x2Ccontainer:void 0\x2Csvg:void 0\x2CeventEmitter:void 0\x2CcreateChart:function(){throw new Error(\"Base chart type can\'t be instantiated!\")}\x2Cupdate:c\x2Cdetach:d\x2Con:e\x2Coff:f\x2Cversion:b.version\x2CsupportsForeignObject:!1})}(this\x2Ca)\x2Cfunction(a\x2Cb){\"use strict\";function c(a\x2Cc\x2Cd\x2Ce\x2Cf){a instanceof Element\?this._node\x3Da:(this._node\x3Dy.createElementNS(b.namespaces.svg\x2Ca)\x2C\"svg\"\x3D\x3D\x3Da&&this.attr({\"xmlns:ct\":b.namespaces.ct}))\x2Cc&&this.attr(c)\x2Cd&&this.addClass(d)\x2Ce&&(f&&e._node.firstChild\?e._node.insertBefore(this._node\x2Ce._node.firstChild):e._node.appendChild(this._node))}function d(a\x2Cc){return\"string\"\x3D\x3Dtypeof a\?c\?this._node.getAttributeNS(c\x2Ca):this._node.getAttribute(a):(Object.keys(a).forEach(function(c){if(void 0!\x3D\x3Da[c])if(c.indexOf(\":\")!\x3D\x3D-1){var d\x3Dc.split(\":\");this._node.setAttributeNS(b.namespaces[d[0]]\x2Cc\x2Ca[c])}else this._node.setAttribute(c\x2Ca[c])}.bind(this))\x2Cthis)}function e(a\x2Cc\x2Cd\x2Ce){return new b.Svg(a\x2Cc\x2Cd\x2Cthis\x2Ce)}function f(){return this._node.parentNode instanceof SVGElement\?new b.Svg(this._node.parentNode):null}function g(){for(var a\x3Dthis._node;\"svg\"!\x3D\x3Da.nodeName;)a\x3Da.parentNode;return new b.Svg(a)}function h(a){var c\x3Dthis._node.querySelector(a);return c\?new b.Svg(c):null}function i(a){var c\x3Dthis._node.querySelectorAll(a);return c.length\?new b.Svg.List(c):null}function j(){return this._node}function k(a\x2Cc\x2Cd\x2Ce){if(\"string\"\x3D\x3Dtypeof a){var f\x3Dy.createElement(\"div\");f.innerHTML\x3Da\x2Ca\x3Df.firstChild}a.setAttribute(\"xmlns\"\x2Cb.namespaces.xmlns);var g\x3Dthis.elem(\"foreignObject\"\x2Cc\x2Cd\x2Ce);return g._node.appendChild(a)\x2Cg}function l(a){return this._node.appendChild(y.createTextNode(a))\x2Cthis}function m(){for(;this._node.firstChild;)this._node.removeChild(this._node.firstChild);return this}function n(){return this._node.parentNode.removeChild(this._node)\x2Cthis.parent()}function o(a){return this._node.parentNode.replaceChild(a._node\x2Cthis._node)\x2Ca}function p(a\x2Cb){return b&&this._node.firstChild\?this._node.insertBefore(a._node\x2Cthis._node.firstChild):this._node.appendChild(a._node)\x2Cthis}function q(){return this._node.getAttribute(\"class\")\?this._node.getAttribute(\"class\").trim().split(/\\s+/):[]}function r(a){return this._node.setAttribute(\"class\"\x2Cthis.classes(this._node).concat(a.trim().split(/\\s+/)).filter(function(a\x2Cb\x2Cc){return c.indexOf(a)\x3D\x3D\x3Db}).join(\" \"))\x2Cthis}function s(a){var b\x3Da.trim().split(/\\s+/);return this._node.setAttribute(\"class\"\x2Cthis.classes(this._node).filter(function(a){return b.indexOf(a)\x3D\x3D\x3D-1}).join(\" \"))\x2Cthis}function t(){return this._node.setAttribute(\"class\"\x2C\"\")\x2Cthis}function u(){return this._node.getBoundingClientRect().height}function v(){return this._node.getBoundingClientRect().width}function w(a\x2Cc\x2Cd){return void 0\x3D\x3D\x3Dc&&(c\x3D!0)\x2CObject.keys(a).forEach(function(e){function f(a\x2Cc){var f\x2Cg\x2Ch\x2Ci\x3D{};a.easing&&(h\x3Da.easing instanceof Array\?a.easing:b.Svg.Easing[a.easing]\x2Cdelete a.easing)\x2Ca.begin\x3Db.ensureUnit(a.begin\x2C\"ms\")\x2Ca.dur\x3Db.ensureUnit(a.dur\x2C\"ms\")\x2Ch&&(a.calcMode\x3D\"spline\"\x2Ca.keySplines\x3Dh.join(\" \")\x2Ca.keyTimes\x3D\"0;1\")\x2Cc&&(a.fill\x3D\"freeze\"\x2Ci[e]\x3Da.from\x2Cthis.attr(i)\x2Cg\x3Db.quantity(a.begin||0).value\x2Ca.begin\x3D\"indefinite\")\x2Cf\x3Dthis.elem(\"animate\"\x2Cb.extend({attributeName:e}\x2Ca))\x2Cc&&setTimeout(function(){try{f._node.beginElement()}catch(b){i[e]\x3Da.to\x2Cthis.attr(i)\x2Cf.remove()}}.bind(this)\x2Cg)\x2Cd&&f._node.addEventListener(\"beginEvent\"\x2Cfunction(){d.emit(\"animationBegin\"\x2C{element:this\x2Canimate:f._node\x2Cparams:a})}.bind(this))\x2Cf._node.addEventListener(\"endEvent\"\x2Cfunction(){d&&d.emit(\"animationEnd\"\x2C{element:this\x2Canimate:f._node\x2Cparams:a})\x2Cc&&(i[e]\x3Da.to\x2Cthis.attr(i)\x2Cf.remove())}.bind(this))}a[e]instanceof Array\?a[e].forEach(function(a){f.bind(this)(a\x2C!1)}.bind(this)):f.bind(this)(a[e]\x2Cc)}.bind(this))\x2Cthis}function x(a){var c\x3Dthis;this.svgElements\x3D[];for(var d\x3D0;d<a.length;d++)this.svgElements.push(new b.Svg(a[d]));Object.keys(b.Svg.prototype).filter(function(a){return[\"constructor\"\x2C\"parent\"\x2C\"querySelector\"\x2C\"querySelectorAll\"\x2C\"replace\"\x2C\"append\"\x2C\"classes\"\x2C\"height\"\x2C\"width\"].indexOf(a)\x3D\x3D\x3D-1}).forEach(function(a){c[a]\x3Dfunction(){var d\x3DArray.prototype.slice.call(arguments\x2C0);return c.svgElements.forEach(function(c){b.Svg.prototype[a].apply(c\x2Cd)})\x2Cc}})}var y\x3Da.document;b.Svg\x3Db.Class.extend({constructor:c\x2Cattr:d\x2Celem:e\x2Cparent:f\x2Croot:g\x2CquerySelector:h\x2CquerySelectorAll:i\x2CgetNode:j\x2CforeignObject:k\x2Ctext:l\x2Cempty:m\x2Cremove:n\x2Creplace:o\x2Cappend:p\x2Cclasses:q\x2CaddClass:r\x2CremoveClass:s\x2CremoveAllClasses:t\x2Cheight:u\x2Cwidth:v\x2Canimate:w})\x2Cb.Svg.isSupported\x3Dfunction(a){return y.implementation.hasFeature(\"http://www.w3.org/TR/SVG11/feature#\"+a\x2C\"1.1\")};var z\x3D{easeInSine:[.47\x2C0\x2C.745\x2C.715]\x2CeaseOutSine:[.39\x2C.575\x2C.565\x2C1]\x2CeaseInOutSine:[.445\x2C.05\x2C.55\x2C.95]\x2CeaseInQuad:[.55\x2C.085\x2C.68\x2C.53]\x2CeaseOutQuad:[.25\x2C.46\x2C.45\x2C.94]\x2CeaseInOutQuad:[.455\x2C.03\x2C.515\x2C.955]\x2CeaseInCubic:[.55\x2C.055\x2C.675\x2C.19]\x2CeaseOutCubic:[.215\x2C.61\x2C.355\x2C1]\x2CeaseInOutCubic:[.645\x2C.045\x2C.355\x2C1]\x2CeaseInQuart:[.895\x2C.03\x2C.685\x2C.22]\x2CeaseOutQuart:[.165\x2C.84\x2C.44\x2C1]\x2CeaseInOutQuart:[.77\x2C0\x2C.175\x2C1]\x2CeaseInQuint:[.755\x2C.05\x2C.855\x2C.06]\x2CeaseOutQuint:[.23\x2C1\x2C.32\x2C1]\x2CeaseInOutQuint:[.86\x2C0\x2C.07\x2C1]\x2CeaseInExpo:[.95\x2C.05\x2C.795\x2C.035]\x2CeaseOutExpo:[.19\x2C1\x2C.22\x2C1]\x2CeaseInOutExpo:[1\x2C0\x2C0\x2C1]\x2CeaseInCirc:[.6\x2C.04\x2C.98\x2C.335]\x2CeaseOutCirc:[.075\x2C.82\x2C.165\x2C1]\x2CeaseInOutCirc:[.785\x2C.135\x2C.15\x2C.86]\x2CeaseInBack:[.6\x2C-.28\x2C.735\x2C.045]\x2CeaseOutBack:[.175\x2C.885\x2C.32\x2C1.275]\x2CeaseInOutBack:[.68\x2C-.55\x2C.265\x2C1.55]};b.Svg.Easing\x3Dz\x2Cb.Svg.List\x3Db.Class.extend({constructor:x})}(this\x2Ca)\x2Cfunction(a\x2Cb){\"use strict\";function c(a\x2Cc\x2Cd\x2Ce\x2Cf\x2Cg){var h\x3Db.extend({command:f\?a.toLowerCase():a.toUpperCase()}\x2Cc\x2Cg\?{data:g}:{});d.splice(e\x2C0\x2Ch)}function d(a\x2Cb){a.forEach(function(c\x2Cd){t[c.command.toLowerCase()].forEach(function(e\x2Cf){b(c\x2Ce\x2Cd\x2Cf\x2Ca)})})}function e(a\x2Cc){this.pathElements\x3D[]\x2Cthis.pos\x3D0\x2Cthis.close\x3Da\x2Cthis.options\x3Db.extend({}\x2Cu\x2Cc)}function f(a){return void 0!\x3D\x3Da\?(this.pos\x3DMath.max(0\x2CMath.min(this.pathElements.length\x2Ca))\x2Cthis):this.pos}function g(a){return this.pathElements.splice(this.pos\x2Ca)\x2Cthis}function h(a\x2Cb\x2Cd\x2Ce){return c(\"M\"\x2C{x:+a\x2Cy:+b}\x2Cthis.pathElements\x2Cthis.pos++\x2Cd\x2Ce)\x2Cthis}function i(a\x2Cb\x2Cd\x2Ce){return c(\"L\"\x2C{x:+a\x2Cy:+b}\x2Cthis.pathElements\x2Cthis.pos++\x2Cd\x2Ce)\x2Cthis}function j(a\x2Cb\x2Cd\x2Ce\x2Cf\x2Cg\x2Ch\x2Ci){return c(\"C\"\x2C{x1:+a\x2Cy1:+b\x2Cx2:+d\x2Cy2:+e\x2Cx:+f\x2Cy:+g}\x2Cthis.pathElements\x2Cthis.pos++\x2Ch\x2Ci)\x2Cthis}function k(a\x2Cb\x2Cd\x2Ce\x2Cf\x2Cg\x2Ch\x2Ci\x2Cj){return c(\"A\"\x2C{rx:+a\x2Cry:+b\x2CxAr:+d\x2ClAf:+e\x2Csf:+f\x2Cx:+g\x2Cy:+h}\x2Cthis.pathElements\x2Cthis.pos++\x2Ci\x2Cj)\x2Cthis}function l(a){var c\x3Da.replace(/([A-Za-z])([0-9])/g\x2C\"$1 $2\").replace(/([0-9])([A-Za-z])/g\x2C\"$1 $2\").split(/[\\s\x2C]+/).reduce(function(a\x2Cb){return b.match(/[A-Za-z]/)&&a.push([])\x2Ca[a.length-1].push(b)\x2Ca}\x2C[]);\"Z\"\x3D\x3D\x3Dc[c.length-1][0].toUpperCase()&&c.pop();var d\x3Dc.map(function(a){var c\x3Da.shift()\x2Cd\x3Dt[c.toLowerCase()];return b.extend({command:c}\x2Cd.reduce(function(b\x2Cc\x2Cd){return b[c]\x3D+a[d]\x2Cb}\x2C{}))})\x2Ce\x3D[this.pos\x2C0];return Array.prototype.push.apply(e\x2Cd)\x2CArray.prototype.splice.apply(this.pathElements\x2Ce)\x2Cthis.pos+\x3Dd.length\x2Cthis}function m(){var a\x3DMath.pow(10\x2Cthis.options.accuracy);return this.pathElements.reduce(function(b\x2Cc){var d\x3Dt[c.command.toLowerCase()].map(function(b){return this.options.accuracy\?Math.round(c[b]*a)/a:c[b]}.bind(this));return b+c.command+d.join(\"\x2C\")}.bind(this)\x2C\"\")+(this.close\?\"Z\":\"\")}function n(a\x2Cb){return d(this.pathElements\x2Cfunction(c\x2Cd){c[d]*\x3D\"x\"\x3D\x3D\x3Dd[0]\?a:b})\x2Cthis}function o(a\x2Cb){return d(this.pathElements\x2Cfunction(c\x2Cd){c[d]+\x3D\"x\"\x3D\x3D\x3Dd[0]\?a:b})\x2Cthis}function p(a){return d(this.pathElements\x2Cfunction(b\x2Cc\x2Cd\x2Ce\x2Cf){var g\x3Da(b\x2Cc\x2Cd\x2Ce\x2Cf);(g||0\x3D\x3D\x3Dg)&&(b[c]\x3Dg)})\x2Cthis}function q(a){var c\x3Dnew b.Svg.Path(a||this.close);return c.pos\x3Dthis.pos\x2Cc.pathElements\x3Dthis.pathElements.slice().map(function(a){return b.extend({}\x2Ca)})\x2Cc.options\x3Db.extend({}\x2Cthis.options)\x2Cc}function r(a){var c\x3D[new b.Svg.Path];return this.pathElements.forEach(function(d){d.command\x3D\x3D\x3Da.toUpperCase()&&0!\x3D\x3Dc[c.length-1].pathElements.length&&c.push(new b.Svg.Path)\x2Cc[c.length-1].pathElements.push(d)})\x2Cc}function s(a\x2Cc\x2Cd){for(var e\x3Dnew b.Svg.Path(c\x2Cd)\x2Cf\x3D0;f<a.length;f++)for(var g\x3Da[f]\x2Ch\x3D0;h<g.pathElements.length;h++)e.pathElements.push(g.pathElements[h]);return e}var t\x3D{m:[\"x\"\x2C\"y\"]\x2Cl:[\"x\"\x2C\"y\"]\x2Cc:[\"x1\"\x2C\"y1\"\x2C\"x2\"\x2C\"y2\"\x2C\"x\"\x2C\"y\"]\x2Ca:[\"rx\"\x2C\"ry\"\x2C\"xAr\"\x2C\"lAf\"\x2C\"sf\"\x2C\"x\"\x2C\"y\"]}\x2Cu\x3D{accuracy:3};b.Svg.Path\x3Db.Class.extend({constructor:e\x2Cposition:f\x2Cremove:g\x2Cmove:h\x2Cline:i\x2Ccurve:j\x2Carc:k\x2Cscale:n\x2Ctranslate:o\x2Ctransform:p\x2Cparse:l\x2Cstringify:m\x2Cclone:q\x2CsplitByCommand:r})\x2Cb.Svg.Path.elementDescriptions\x3Dt\x2Cb.Svg.Path.join\x3Ds}(this\x2Ca)\x2Cfunction(a\x2Cb){\"use strict\";function c(a\x2Cb\x2Cc\x2Cd){this.units\x3Da\x2Cthis.counterUnits\x3Da\x3D\x3D\x3De.x\?e.y:e.x\x2Cthis.chartRect\x3Db\x2Cthis.axisLength\x3Db[a.rectEnd]-b[a.rectStart]\x2Cthis.gridOffset\x3Db[a.rectOffset]\x2Cthis.ticks\x3Dc\x2Cthis.options\x3Dd}function d(a\x2Cc\x2Cd\x2Ce\x2Cf){var g\x3De[\"axis\"+this.units.pos.toUpperCase()]\x2Ch\x3Dthis.ticks.map(this.projectValue.bind(this))\x2Ci\x3Dthis.ticks.map(g.labelInterpolationFnc);h.forEach(function(j\x2Ck){var l\x2Cm\x3D{x:0\x2Cy:0};l\x3Dh[k+1]\?h[k+1]-j:Math.max(this.axisLength-j\x2C30)\x2Cb.isFalseyButZero(i[k])&&\"\"!\x3D\x3Di[k]||(\"x\"\x3D\x3D\x3Dthis.units.pos\?(j\x3Dthis.chartRect.x1+j\x2Cm.x\x3De.axisX.labelOffset.x\x2C\"start\"\x3D\x3D\x3De.axisX.position\?m.y\x3Dthis.chartRect.padding.top+e.axisX.labelOffset.y+(d\?5:20):m.y\x3Dthis.chartRect.y1+e.axisX.labelOffset.y+(d\?5:20)):(j\x3Dthis.chartRect.y1-j\x2Cm.y\x3De.axisY.labelOffset.y-(d\?l:0)\x2C\"start\"\x3D\x3D\x3De.axisY.position\?m.x\x3Dd\?this.chartRect.padding.left+e.axisY.labelOffset.x:this.chartRect.x1-10:m.x\x3Dthis.chartRect.x2+e.axisY.labelOffset.x+10)\x2Cg.showGrid&&b.createGrid(j\x2Ck\x2Cthis\x2Cthis.gridOffset\x2Cthis.chartRect[this.counterUnits.len]()\x2Ca\x2C[e.classNames.grid\x2Ce.classNames[this.units.dir]]\x2Cf)\x2Cg.showLabel&&b.createLabel(j\x2Cl\x2Ck\x2Ci\x2Cthis\x2Cg.offset\x2Cm\x2Cc\x2C[e.classNames.label\x2Ce.classNames[this.units.dir]\x2C\"start\"\x3D\x3D\x3Dg.position\?e.classNames[g.position]:e.classNames.end]\x2Cd\x2Cf))}.bind(this))}var e\x3D(a.window\x2Ca.document\x2C{x:{pos:\"x\"\x2Clen:\"width\"\x2Cdir:\"horizontal\"\x2CrectStart:\"x1\"\x2CrectEnd:\"x2\"\x2CrectOffset:\"y2\"}\x2Cy:{pos:\"y\"\x2Clen:\"height\"\x2Cdir:\"vertical\"\x2CrectStart:\"y2\"\x2CrectEnd:\"y1\"\x2CrectOffset:\"x1\"}});b.Axis\x3Db.Class.extend({constructor:c\x2CcreateGridAndLabels:d\x2CprojectValue:function(a\x2Cb\x2Cc){throw new Error(\"Base axis can\'t be instantiated!\")}})\x2Cb.Axis.units\x3De}(this\x2Ca)\x2Cfunction(a\x2Cb){\"use strict\";function c(a\x2Cc\x2Cd\x2Ce){var f\x3De.highLow||b.getHighLow(c\x2Ce\x2Ca.pos);this.bounds\x3Db.getBounds(d[a.rectEnd]-d[a.rectStart]\x2Cf\x2Ce.scaleMinSpace||20\x2Ce.onlyInteger)\x2Cthis.range\x3D{min:this.bounds.min\x2Cmax:this.bounds.max}\x2Cb.AutoScaleAxis[\"super\"].constructor.call(this\x2Ca\x2Cd\x2Cthis.bounds.values\x2Ce)}function d(a){return this.axisLength*(+b.getMultiValue(a\x2Cthis.units.pos)-this.bounds.min)/this.bounds.range}a.window\x2Ca.document;b.AutoScaleAxis\x3Db.Axis.extend({constructor:c\x2CprojectValue:d})}(this\x2Ca)\x2Cfunction(a\x2Cb){\"use strict\";function c(a\x2Cc\x2Cd\x2Ce){var f\x3De.highLow||b.getHighLow(c\x2Ce\x2Ca.pos);this.divisor\x3De.divisor||1\x2Cthis.ticks\x3De.ticks||b.times(this.divisor).map(function(a\x2Cb){return f.low+(f.high-f.low)/this.divisor*b}.bind(this))\x2Cthis.ticks.sort(function(a\x2Cb){return a-b})\x2Cthis.range\x3D{min:f.low\x2Cmax:f.high}\x2Cb.FixedScaleAxis[\"super\"].constructor.call(this\x2Ca\x2Cd\x2Cthis.ticks\x2Ce)\x2Cthis.stepLength\x3Dthis.axisLength/this.divisor}function d(a){return this.axisLength*(+b.getMultiValue(a\x2Cthis.units.pos)-this.range.min)/(this.range.max-this.range.min)}a.window\x2Ca.document;b.FixedScaleAxis\x3Db.Axis.extend({constructor:c\x2CprojectValue:d})}(this\x2Ca)\x2Cfunction(a\x2Cb){\"use strict\";function c(a\x2Cc\x2Cd\x2Ce){b.StepAxis[\"super\"].constructor.call(this\x2Ca\x2Cd\x2Ce.ticks\x2Ce);var f\x3DMath.max(1\x2Ce.ticks.length-(e.stretch\?1:0));this.stepLength\x3Dthis.axisLength/f}function d(a\x2Cb){return this.stepLength*b}a.window\x2Ca.document;b.StepAxis\x3Db.Axis.extend({constructor:c\x2CprojectValue:d})}(this\x2Ca)\x2Cfunction(a\x2Cb){\"use strict\";function c(a){var c\x3Db.normalizeData(this.data\x2Ca.reverseData\x2C!0);this.svg\x3Db.createSvg(this.container\x2Ca.width\x2Ca.height\x2Ca.classNames.chart);var d\x2Cf\x2Cg\x3Dthis.svg.elem(\"g\").addClass(a.classNames.gridGroup)\x2Ch\x3Dthis.svg.elem(\"g\")\x2Ci\x3Dthis.svg.elem(\"g\").addClass(a.classNames.labelGroup)\x2Cj\x3Db.createChartRect(this.svg\x2Ca\x2Ce.padding);d\x3Dvoid 0\x3D\x3D\x3Da.axisX.type\?new b.StepAxis(b.Axis.units.x\x2Cc.normalized.series\x2Cj\x2Cb.extend({}\x2Ca.axisX\x2C{ticks:c.normalized.labels\x2Cstretch:a.fullWidth})):a.axisX.type.call(b\x2Cb.Axis.units.x\x2Cc.normalized.series\x2Cj\x2Ca.axisX)\x2Cf\x3Dvoid 0\x3D\x3D\x3Da.axisY.type\?new b.AutoScaleAxis(b.Axis.units.y\x2Cc.normalized.series\x2Cj\x2Cb.extend({}\x2Ca.axisY\x2C{high:b.isNumeric(a.high)\?a.high:a.axisY.high\x2Clow:b.isNumeric(a.low)\?a.low:a.axisY.low})):a.axisY.type.call(b\x2Cb.Axis.units.y\x2Cc.normalized.series\x2Cj\x2Ca.axisY)\x2Cd.createGridAndLabels(g\x2Ci\x2Cthis.supportsForeignObject\x2Ca\x2Cthis.eventEmitter)\x2Cf.createGridAndLabels(g\x2Ci\x2Cthis.supportsForeignObject\x2Ca\x2Cthis.eventEmitter)\x2Ca.showGridBackground&&b.createGridBackground(g\x2Cj\x2Ca.classNames.gridBackground\x2Cthis.eventEmitter)\x2Cc.raw.series.forEach(function(e\x2Cg){var i\x3Dh.elem(\"g\");i.attr({\"ct:series-name\":e.name\x2C\"ct:meta\":b.serialize(e.meta)})\x2Ci.addClass([a.classNames.series\x2Ce.className||a.classNames.series+\"-\"+b.alphaNumerate(g)].join(\" \"));var k\x3D[]\x2Cl\x3D[];c.normalized.series[g].forEach(function(a\x2Ch){var i\x3D{x:j.x1+d.projectValue(a\x2Ch\x2Cc.normalized.series[g])\x2Cy:j.y1-f.projectValue(a\x2Ch\x2Cc.normalized.series[g])};k.push(i.x\x2Ci.y)\x2Cl.push({value:a\x2CvalueIndex:h\x2Cmeta:b.getMetaData(e\x2Ch)})}.bind(this));var m\x3D{lineSmooth:b.getSeriesOption(e\x2Ca\x2C\"lineSmooth\")\x2CshowPoint:b.getSeriesOption(e\x2Ca\x2C\"showPoint\")\x2CshowLine:b.getSeriesOption(e\x2Ca\x2C\"showLine\")\x2CshowArea:b.getSeriesOption(e\x2Ca\x2C\"showArea\")\x2CareaBase:b.getSeriesOption(e\x2Ca\x2C\"areaBase\")}\x2Cn\x3D\"function\"\x3D\x3Dtypeof m.lineSmooth\?m.lineSmooth:m.lineSmooth\?b.Interpolation.monotoneCubic():b.Interpolation.none()\x2Co\x3Dn(k\x2Cl);if(m.showPoint&&o.pathElements.forEach(function(c){var h\x3Di.elem(\"line\"\x2C{x1:c.x\x2Cy1:c.y\x2Cx2:c.x+.01\x2Cy2:c.y}\x2Ca.classNames.point).attr({\"ct:value\":[c.data.value.x\x2Cc.data.value.y].filter(b.isNumeric).join(\"\x2C\")\x2C\"ct:meta\":b.serialize(c.data.meta)});this.eventEmitter.emit(\"draw\"\x2C{type:\"point\"\x2Cvalue:c.data.value\x2Cindex:c.data.valueIndex\x2Cmeta:c.data.meta\x2Cseries:e\x2CseriesIndex:g\x2CaxisX:d\x2CaxisY:f\x2Cgroup:i\x2Celement:h\x2Cx:c.x\x2Cy:c.y})}.bind(this))\x2Cm.showLine){var p\x3Di.elem(\"path\"\x2C{d:o.stringify()}\x2Ca.classNames.line\x2C!0);this.eventEmitter.emit(\"draw\"\x2C{type:\"line\"\x2Cvalues:c.normalized.series[g]\x2Cpath:o.clone()\x2CchartRect:j\x2Cindex:g\x2Cseries:e\x2CseriesIndex:g\x2CseriesMeta:e.meta\x2CaxisX:d\x2CaxisY:f\x2Cgroup:i\x2Celement:p})}if(m.showArea&&f.range){var q\x3DMath.max(Math.min(m.areaBase\x2Cf.range.max)\x2Cf.range.min)\x2Cr\x3Dj.y1-f.projectValue(q);o.splitByCommand(\"M\").filter(function(a){return a.pathElements.length>1}).map(function(a){var b\x3Da.pathElements[0]\x2Cc\x3Da.pathElements[a.pathElements.length-1];return a.clone(!0).position(0).remove(1).move(b.x\x2Cr).line(b.x\x2Cb.y).position(a.pathElements.length+1).line(c.x\x2Cr)}).forEach(function(b){var h\x3Di.elem(\"path\"\x2C{d:b.stringify()}\x2Ca.classNames.area\x2C!0);this.eventEmitter.emit(\"draw\"\x2C{type:\"area\"\x2Cvalues:c.normalized.series[g]\x2Cpath:b.clone()\x2Cseries:e\x2CseriesIndex:g\x2CaxisX:d\x2CaxisY:f\x2CchartRect:j\x2Cindex:g\x2Cgroup:i\x2Celement:h})}.bind(this))}}.bind(this))\x2Cthis.eventEmitter.emit(\"created\"\x2C{bounds:f.bounds\x2CchartRect:j\x2CaxisX:d\x2CaxisY:f\x2Csvg:this.svg\x2Coptions:a})}function d(a\x2Cc\x2Cd\x2Cf){b.Line[\"super\"].constructor.call(this\x2Ca\x2Cc\x2Ce\x2Cb.extend({}\x2Ce\x2Cd)\x2Cf)}var e\x3D(a.window\x2Ca.document\x2C{axisX:{offset:30\x2Cposition:\"end\"\x2ClabelOffset:{x:0\x2Cy:0}\x2CshowLabel:!0\x2CshowGrid:!0\x2ClabelInterpolationFnc:b.noop\x2Ctype:void 0}\x2CaxisY:{offset:40\x2Cposition:\"start\"\x2ClabelOffset:{x:0\x2Cy:0}\x2CshowLabel:!0\x2CshowGrid:!0\x2ClabelInterpolationFnc:b.noop\x2Ctype:void 0\x2CscaleMinSpace:20\x2ConlyInteger:!1}\x2Cwidth:void 0\x2Cheight:void 0\x2CshowLine:!0\x2CshowPoint:!0\x2CshowArea:!1\x2CareaBase:0\x2ClineSmooth:!0\x2CshowGridBackground:!1\x2Clow:void 0\x2Chigh:void 0\x2CchartPadding:{top:15\x2Cright:15\x2Cbottom:5\x2Cleft:10}\x2CfullWidth:!1\x2CreverseData:!1\x2CclassNames:{chart:\"ct-chart-line\"\x2Clabel:\"ct-label\"\x2ClabelGroup:\"ct-labels\"\x2Cseries:\"ct-series\"\x2Cline:\"ct-line\"\x2Cpoint:\"ct-point\"\x2Carea:\"ct-area\"\x2Cgrid:\"ct-grid\"\x2CgridGroup:\"ct-grids\"\x2CgridBackground:\"ct-grid-background\"\x2Cvertical:\"ct-vertical\"\x2Chorizontal:\"ct-horizontal\"\x2Cstart:\"ct-start\"\x2Cend:\"ct-end\"}});b.Line\x3Db.Base.extend({constructor:d\x2CcreateChart:c})}(this\x2Ca)\x2Cfunction(a\x2Cb){\"use strict\";function c(a){var c\x2Cd;a.distributeSeries\?(c\x3Db.normalizeData(this.data\x2Ca.reverseData\x2Ca.horizontalBars\?\"x\":\"y\")\x2Cc.normalized.series\x3Dc.normalized.series.map(function(a){return[a]})):c\x3Db.normalizeData(this.data\x2Ca.reverseData\x2Ca.horizontalBars\?\"x\":\"y\")\x2Cthis.svg\x3Db.createSvg(this.container\x2Ca.width\x2Ca.height\x2Ca.classNames.chart+(a.horizontalBars\?\" \"+a.classNames.horizontalBars:\"\"));var f\x3Dthis.svg.elem(\"g\").addClass(a.classNames.gridGroup)\x2Cg\x3Dthis.svg.elem(\"g\")\x2Ch\x3Dthis.svg.elem(\"g\").addClass(a.classNames.labelGroup);if(a.stackBars&&0!\x3D\x3Dc.normalized.series.length){var i\x3Db.serialMap(c.normalized.series\x2Cfunction(){\nreturn Array.prototype.slice.call(arguments).map(function(a){return a}).reduce(function(a\x2Cb){return{x:a.x+(b&&b.x)||0\x2Cy:a.y+(b&&b.y)||0}}\x2C{x:0\x2Cy:0})});d\x3Db.getHighLow([i]\x2Ca\x2Ca.horizontalBars\?\"x\":\"y\")}else d\x3Db.getHighLow(c.normalized.series\x2Ca\x2Ca.horizontalBars\?\"x\":\"y\");d.high\x3D+a.high||(0\x3D\x3D\x3Da.high\?0:d.high)\x2Cd.low\x3D+a.low||(0\x3D\x3D\x3Da.low\?0:d.low);var j\x2Ck\x2Cl\x2Cm\x2Cn\x2Co\x3Db.createChartRect(this.svg\x2Ca\x2Ce.padding);k\x3Da.distributeSeries&&a.stackBars\?c.normalized.labels.slice(0\x2C1):c.normalized.labels\x2Ca.horizontalBars\?(j\x3Dm\x3Dvoid 0\x3D\x3D\x3Da.axisX.type\?new b.AutoScaleAxis(b.Axis.units.x\x2Cc.normalized.series\x2Co\x2Cb.extend({}\x2Ca.axisX\x2C{highLow:d\x2CreferenceValue:0})):a.axisX.type.call(b\x2Cb.Axis.units.x\x2Cc.normalized.series\x2Co\x2Cb.extend({}\x2Ca.axisX\x2C{highLow:d\x2CreferenceValue:0}))\x2Cl\x3Dn\x3Dvoid 0\x3D\x3D\x3Da.axisY.type\?new b.StepAxis(b.Axis.units.y\x2Cc.normalized.series\x2Co\x2C{ticks:k}):a.axisY.type.call(b\x2Cb.Axis.units.y\x2Cc.normalized.series\x2Co\x2Ca.axisY)):(l\x3Dm\x3Dvoid 0\x3D\x3D\x3Da.axisX.type\?new b.StepAxis(b.Axis.units.x\x2Cc.normalized.series\x2Co\x2C{ticks:k}):a.axisX.type.call(b\x2Cb.Axis.units.x\x2Cc.normalized.series\x2Co\x2Ca.axisX)\x2Cj\x3Dn\x3Dvoid 0\x3D\x3D\x3Da.axisY.type\?new b.AutoScaleAxis(b.Axis.units.y\x2Cc.normalized.series\x2Co\x2Cb.extend({}\x2Ca.axisY\x2C{highLow:d\x2CreferenceValue:0})):a.axisY.type.call(b\x2Cb.Axis.units.y\x2Cc.normalized.series\x2Co\x2Cb.extend({}\x2Ca.axisY\x2C{highLow:d\x2CreferenceValue:0})));var p\x3Da.horizontalBars\?o.x1+j.projectValue(0):o.y1-j.projectValue(0)\x2Cq\x3D[];l.createGridAndLabels(f\x2Ch\x2Cthis.supportsForeignObject\x2Ca\x2Cthis.eventEmitter)\x2Cj.createGridAndLabels(f\x2Ch\x2Cthis.supportsForeignObject\x2Ca\x2Cthis.eventEmitter)\x2Ca.showGridBackground&&b.createGridBackground(f\x2Co\x2Ca.classNames.gridBackground\x2Cthis.eventEmitter)\x2Cc.raw.series.forEach(function(d\x2Ce){var f\x2Ch\x2Ci\x3De-(c.raw.series.length-1)/2;f\x3Da.distributeSeries&&!a.stackBars\?l.axisLength/c.normalized.series.length/2:a.distributeSeries&&a.stackBars\?l.axisLength/2:l.axisLength/c.normalized.series[e].length/2\x2Ch\x3Dg.elem(\"g\")\x2Ch.attr({\"ct:series-name\":d.name\x2C\"ct:meta\":b.serialize(d.meta)})\x2Ch.addClass([a.classNames.series\x2Cd.className||a.classNames.series+\"-\"+b.alphaNumerate(e)].join(\" \"))\x2Cc.normalized.series[e].forEach(function(g\x2Ck){var r\x2Cs\x2Ct\x2Cu;if(u\x3Da.distributeSeries&&!a.stackBars\?e:a.distributeSeries&&a.stackBars\?0:k\x2Cr\x3Da.horizontalBars\?{x:o.x1+j.projectValue(g&&g.x\?g.x:0\x2Ck\x2Cc.normalized.series[e])\x2Cy:o.y1-l.projectValue(g&&g.y\?g.y:0\x2Cu\x2Cc.normalized.series[e])}:{x:o.x1+l.projectValue(g&&g.x\?g.x:0\x2Cu\x2Cc.normalized.series[e])\x2Cy:o.y1-j.projectValue(g&&g.y\?g.y:0\x2Ck\x2Cc.normalized.series[e])}\x2Cl instanceof b.StepAxis&&(l.options.stretch||(r[l.units.pos]+\x3Df*(a.horizontalBars\?-1:1))\x2Cr[l.units.pos]+\x3Da.stackBars||a.distributeSeries\?0:i*a.seriesBarDistance*(a.horizontalBars\?-1:1))\x2Ct\x3Dq[k]||p\x2Cq[k]\x3Dt-(p-r[l.counterUnits.pos])\x2Cvoid 0!\x3D\x3Dg){var v\x3D{};v[l.units.pos+\"1\"]\x3Dr[l.units.pos]\x2Cv[l.units.pos+\"2\"]\x3Dr[l.units.pos]\x2C!a.stackBars||\"accumulate\"!\x3D\x3Da.stackMode&&a.stackMode\?(v[l.counterUnits.pos+\"1\"]\x3Dp\x2Cv[l.counterUnits.pos+\"2\"]\x3Dr[l.counterUnits.pos]):(v[l.counterUnits.pos+\"1\"]\x3Dt\x2Cv[l.counterUnits.pos+\"2\"]\x3Dq[k])\x2Cv.x1\x3DMath.min(Math.max(v.x1\x2Co.x1)\x2Co.x2)\x2Cv.x2\x3DMath.min(Math.max(v.x2\x2Co.x1)\x2Co.x2)\x2Cv.y1\x3DMath.min(Math.max(v.y1\x2Co.y2)\x2Co.y1)\x2Cv.y2\x3DMath.min(Math.max(v.y2\x2Co.y2)\x2Co.y1);var w\x3Db.getMetaData(d\x2Ck);s\x3Dh.elem(\"line\"\x2Cv\x2Ca.classNames.bar).attr({\"ct:value\":[g.x\x2Cg.y].filter(b.isNumeric).join(\"\x2C\")\x2C\"ct:meta\":b.serialize(w)})\x2Cthis.eventEmitter.emit(\"draw\"\x2Cb.extend({type:\"bar\"\x2Cvalue:g\x2Cindex:k\x2Cmeta:w\x2Cseries:d\x2CseriesIndex:e\x2CaxisX:m\x2CaxisY:n\x2CchartRect:o\x2Cgroup:h\x2Celement:s}\x2Cv))}}.bind(this))}.bind(this))\x2Cthis.eventEmitter.emit(\"created\"\x2C{bounds:j.bounds\x2CchartRect:o\x2CaxisX:m\x2CaxisY:n\x2Csvg:this.svg\x2Coptions:a})}function d(a\x2Cc\x2Cd\x2Cf){b.Bar[\"super\"].constructor.call(this\x2Ca\x2Cc\x2Ce\x2Cb.extend({}\x2Ce\x2Cd)\x2Cf)}var e\x3D(a.window\x2Ca.document\x2C{axisX:{offset:30\x2Cposition:\"end\"\x2ClabelOffset:{x:0\x2Cy:0}\x2CshowLabel:!0\x2CshowGrid:!0\x2ClabelInterpolationFnc:b.noop\x2CscaleMinSpace:30\x2ConlyInteger:!1}\x2CaxisY:{offset:40\x2Cposition:\"start\"\x2ClabelOffset:{x:0\x2Cy:0}\x2CshowLabel:!0\x2CshowGrid:!0\x2ClabelInterpolationFnc:b.noop\x2CscaleMinSpace:20\x2ConlyInteger:!1}\x2Cwidth:void 0\x2Cheight:void 0\x2Chigh:void 0\x2Clow:void 0\x2CreferenceValue:0\x2CchartPadding:{top:15\x2Cright:15\x2Cbottom:5\x2Cleft:10}\x2CseriesBarDistance:15\x2CstackBars:!1\x2CstackMode:\"accumulate\"\x2ChorizontalBars:!1\x2CdistributeSeries:!1\x2CreverseData:!1\x2CshowGridBackground:!1\x2CclassNames:{chart:\"ct-chart-bar\"\x2ChorizontalBars:\"ct-horizontal-bars\"\x2Clabel:\"ct-label\"\x2ClabelGroup:\"ct-labels\"\x2Cseries:\"ct-series\"\x2Cbar:\"ct-bar\"\x2Cgrid:\"ct-grid\"\x2CgridGroup:\"ct-grids\"\x2CgridBackground:\"ct-grid-background\"\x2Cvertical:\"ct-vertical\"\x2Chorizontal:\"ct-horizontal\"\x2Cstart:\"ct-start\"\x2Cend:\"ct-end\"}});b.Bar\x3Db.Base.extend({constructor:d\x2CcreateChart:c})}(this\x2Ca)\x2Cfunction(a\x2Cb){\"use strict\";function c(a\x2Cb\x2Cc){var d\x3Db.x>a.x;return d&&\"explode\"\x3D\x3D\x3Dc||!d&&\"implode\"\x3D\x3D\x3Dc\?\"start\":d&&\"implode\"\x3D\x3D\x3Dc||!d&&\"explode\"\x3D\x3D\x3Dc\?\"end\":\"middle\"}function d(a){var d\x2Ce\x2Cg\x2Ch\x2Ci\x2Cj\x3Db.normalizeData(this.data)\x2Ck\x3D[]\x2Cl\x3Da.startAngle;this.svg\x3Db.createSvg(this.container\x2Ca.width\x2Ca.height\x2Ca.donut\?a.classNames.chartDonut:a.classNames.chartPie)\x2Ce\x3Db.createChartRect(this.svg\x2Ca\x2Cf.padding)\x2Cg\x3DMath.min(e.width()/2\x2Ce.height()/2)\x2Ci\x3Da.total||j.normalized.series.reduce(function(a\x2Cb){return a+b}\x2C0);var m\x3Db.quantity(a.donutWidth);\"%\"\x3D\x3D\x3Dm.unit&&(m.value*\x3Dg/100)\x2Cg-\x3Da.donut&&!a.donutSolid\?m.value/2:0\x2Ch\x3D\"outside\"\x3D\x3D\x3Da.labelPosition||a.donut&&!a.donutSolid\?g:\"center\"\x3D\x3D\x3Da.labelPosition\?0:a.donutSolid\?g-m.value/2:g/2\x2Ch+\x3Da.labelOffset;var n\x3D{x:e.x1+e.width()/2\x2Cy:e.y2+e.height()/2}\x2Co\x3D1\x3D\x3D\x3Dj.raw.series.filter(function(a){return a.hasOwnProperty(\"value\")\?0!\x3D\x3Da.value:0!\x3D\x3Da}).length;j.raw.series.forEach(function(a\x2Cb){k[b]\x3Dthis.svg.elem(\"g\"\x2Cnull\x2Cnull)}.bind(this))\x2Ca.showLabel&&(d\x3Dthis.svg.elem(\"g\"\x2Cnull\x2Cnull))\x2Cj.raw.series.forEach(function(e\x2Cf){if(0!\x3D\x3Dj.normalized.series[f]||!a.ignoreEmptyValues){k[f].attr({\"ct:series-name\":e.name})\x2Ck[f].addClass([a.classNames.series\x2Ce.className||a.classNames.series+\"-\"+b.alphaNumerate(f)].join(\" \"));var p\x3Di>0\?l+j.normalized.series[f]/i*360:0\x2Cq\x3DMath.max(0\x2Cl-(0\x3D\x3D\x3Df||o\?0:.2));p-q>\x3D359.99&&(p\x3Dq+359.99);var r\x2Cs\x2Ct\x2Cu\x3Db.polarToCartesian(n.x\x2Cn.y\x2Cg\x2Cq)\x2Cv\x3Db.polarToCartesian(n.x\x2Cn.y\x2Cg\x2Cp)\x2Cw\x3Dnew b.Svg.Path(!a.donut||a.donutSolid).move(v.x\x2Cv.y).arc(g\x2Cg\x2C0\x2Cp-l>180\x2C0\x2Cu.x\x2Cu.y);a.donut\?a.donutSolid&&(t\x3Dg-m.value\x2Cr\x3Db.polarToCartesian(n.x\x2Cn.y\x2Ct\x2Cl-(0\x3D\x3D\x3Df||o\?0:.2))\x2Cs\x3Db.polarToCartesian(n.x\x2Cn.y\x2Ct\x2Cp)\x2Cw.line(r.x\x2Cr.y)\x2Cw.arc(t\x2Ct\x2C0\x2Cp-l>180\x2C1\x2Cs.x\x2Cs.y)):w.line(n.x\x2Cn.y);var x\x3Da.classNames.slicePie;a.donut&&(x\x3Da.classNames.sliceDonut\x2Ca.donutSolid&&(x\x3Da.classNames.sliceDonutSolid));var y\x3Dk[f].elem(\"path\"\x2C{d:w.stringify()}\x2Cx);if(y.attr({\"ct:value\":j.normalized.series[f]\x2C\"ct:meta\":b.serialize(e.meta)})\x2Ca.donut&&!a.donutSolid&&(y._node.style.strokeWidth\x3Dm.value+\"px\")\x2Cthis.eventEmitter.emit(\"draw\"\x2C{type:\"slice\"\x2Cvalue:j.normalized.series[f]\x2CtotalDataSum:i\x2Cindex:f\x2Cmeta:e.meta\x2Cseries:e\x2Cgroup:k[f]\x2Celement:y\x2Cpath:w.clone()\x2Ccenter:n\x2Cradius:g\x2CstartAngle:l\x2CendAngle:p})\x2Ca.showLabel){var z;z\x3D1\x3D\x3D\x3Dj.raw.series.length\?{x:n.x\x2Cy:n.y}:b.polarToCartesian(n.x\x2Cn.y\x2Ch\x2Cl+(p-l)/2);var A;A\x3Dj.normalized.labels&&!b.isFalseyButZero(j.normalized.labels[f])\?j.normalized.labels[f]:j.normalized.series[f];var B\x3Da.labelInterpolationFnc(A\x2Cf);if(B||0\x3D\x3D\x3DB){var C\x3Dd.elem(\"text\"\x2C{dx:z.x\x2Cdy:z.y\x2C\"text-anchor\":c(n\x2Cz\x2Ca.labelDirection)}\x2Ca.classNames.label).text(\"\"+B);this.eventEmitter.emit(\"draw\"\x2C{type:\"label\"\x2Cindex:f\x2Cgroup:d\x2Celement:C\x2Ctext:\"\"+B\x2Cx:z.x\x2Cy:z.y})}}l\x3Dp}}.bind(this))\x2Cthis.eventEmitter.emit(\"created\"\x2C{chartRect:e\x2Csvg:this.svg\x2Coptions:a})}function e(a\x2Cc\x2Cd\x2Ce){b.Pie[\"super\"].constructor.call(this\x2Ca\x2Cc\x2Cf\x2Cb.extend({}\x2Cf\x2Cd)\x2Ce)}var f\x3D(a.window\x2Ca.document\x2C{width:void 0\x2Cheight:void 0\x2CchartPadding:5\x2CclassNames:{chartPie:\"ct-chart-pie\"\x2CchartDonut:\"ct-chart-donut\"\x2Cseries:\"ct-series\"\x2CslicePie:\"ct-slice-pie\"\x2CsliceDonut:\"ct-slice-donut\"\x2CsliceDonutSolid:\"ct-slice-donut-solid\"\x2Clabel:\"ct-label\"}\x2CstartAngle:0\x2Ctotal:void 0\x2Cdonut:!1\x2CdonutSolid:!1\x2CdonutWidth:60\x2CshowLabel:!0\x2ClabelOffset:0\x2ClabelPosition:\"inside\"\x2ClabelInterpolationFnc:b.noop\x2ClabelDirection:\"neutral\"\x2CreverseData:!1\x2CignoreEmptyValues:!1});b.Pie\x3Db.Base.extend({constructor:e\x2CcreateChart:d\x2CdetermineAnchorPosition:c})}(this\x2Ca)\x2Ca});\n", Scope = Public
	#tag EndConstant


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
			Name="Tab"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Text"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="CSV"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Text"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="JSON"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Text"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
