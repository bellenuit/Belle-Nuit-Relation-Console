#tag Class
Protected Class LineHandler
Implements RelationNotifier
	#tag Method, Flags = &h0
		Sub Constructor(m as text = "HTML")
		  mode = m
		  context = new Dictionary
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Run(t as text, internal as text = "", internalbody as text = "", d as dictionary = nil) As text
		  'StartProfiling
		  
		  dim lines(-1) as text
		  dim readlines(-1) as text
		  dim vlist(-1) as variant
		  dim tlist(-1) as text
		  dim fields(-1) as text
		  dim commafields(-1) as text
		  dim i, il, c,j,k as Int64
		  dim dict as new Dictionary
		  
		  dim internals(-1) as text
		  dim file,file2 as FolderItem
		  
		  dim r,r2 as relation
		  dim command as text
		  dim line, mline as text
		  dim key as text
		  dim body as text
		  dim found as Boolean
		  dim xp as XPEvaluator
		  dim tx,ti,tn,tnkey as text
		  dim fn as XPCompiledFunction
		  dim a as Accumulator
		  dim firstchart as Boolean
		  dim consoleprogress as int64
		  firstchart = true
		  dim whilestack(-1) as integer
		  dim ifstack(-1) as integer
		  dim loopcounter as integer
		  
		  dim au as Auto
		  dim tp as tuple
		  
		  dim ptag, ptagerror, ptag2 as text
		  if mode = "html" and false then
		    ptag = "<p>"
		    ptag2 = "</p>"
		    ptagerror = "<p class=""error"">"
		    if internal = "" then
		      result = "<!DOCTYPE html><meta charset=""UTF-8""><html><body><!-- code here -->"
		    end
		  else
		    ptag = ""
		    ptagerror ="{{error}}"
		    ptag2 = text.EndOfLine
		    if internal = "" then
		      result = ""
		    end
		  end
		  
		  
		  dim plines(-1) as text
		  dim poffset as int64
		  
		  if internal = "" then
		    programs = new Dictionary
		    globalrelations = new Dictionary
		    functions = new Dictionary
		    aggregators = new Dictionary
		    offsets = new Dictionary
		    transactiondict = new Dictionary
		    transactionprefix = ""
		    transactionerror = ""
		    redim stack(-1)
		    redim errors(-1)
		  else
		    if internalbody <> "" then
		      internals = internalbody.split(",")
		    end
		  end
		  
		  if d<>nil then 
		    dict = d
		  end
		  
		  if t.IndexOf(chr(13).ToText)>-1 and t.IndexOf(chr(10).ToText)=-1 then
		    t = t.replaceall(chr(13).ToText,chr(10).ToText)
		  end
		  
		  lines = t.split(chr(10).ToText)
		  c = ubound(lines)
		  
		  if mode = "html" and false then
		    if internal = "" then
		      result = result + "<style>.error {color:red; } table.datatable, table.datatable th, table.datatable td { margin-bottom: 5px; padding: 5px; border: 1px solid black; border-collapse: collapse;}"+_
		      " ul {margin-block-start: 0em; margin-block-end: 0em;}</style>"
		    end
		  end
		  
		  #if TargetConsole 
		    if not interactive then
		      for i=0 to 31
		        stdout.write("-")
		      next
		      stdout.write(TextExtensions.newline)
		    end
		  #EndIf
		  
		  dim rtime as double
		  rtime =system.Microseconds
		  
		  for i = 0 to c
		    
		    if internal <>"" then
		      if offsets.Value(internal).Int64Value > 0 then
		        il = offsets.Value(internal).Int64Value + i
		      else
		        il = -offsets.Value(internal).Int64Value //include 
		      end
		    else
		      #if  TargetDesktop
		        if mythread <> nil then
		          if system.Microseconds - rtime > 1000 then
		            mythread.Progress i
		            mythread.Sleep(10)
		            rtime =system.Microseconds
		          end
		        end
		      #else 
		        if not interactive then
		          while  32*i/c -consoleprogress > 0
		            stdout.write("*")
		            consoleprogress = consoleprogress + 1
		            
		          wend
		        end
		      #endif
		      il = i // offset of program?
		    end
		    ti = str(il+1).ToText
		    line = trim(lines(i)).toText
		    // remove comments
		    if line.IndexOf("// ") > -1 then
		      line = line.left(line.IndexOf("// "))  // should be valid within strings
		    end
		    if line = "" or line ="//" then continue
		    if left(line,1) = "'" then
		      if line.Length>3 then
		        result = result + line.mid(2) + ptag2
		      end
		      Continue
		    end
		    fields = line.split(" ")
		    command = fields(0)
		    fields.RemoveRowAt 0
		    body = Text.Join(fields," ")
		    select case command
		      'case "aggregator"
		      'redim plines(-1)
		      'if offsets.HasKey(body.trim) then
		      'result = result + ptagerror+ti+" Warning : Symbol overwritten "+body.trim+ptag2
		      'errors.Append il
		      'end
		      'offsets.Value(body.trim) = i+1
		      'found = false
		      'while i<c and not found
		      'i = i + 1
		      'line = lines(i).trim
		      'if line <> "end aggregator" then
		      'plines.AddRow line
		      'else
		      '
		      'found = true 
		      'end
		      'wend
		      'if found then
		      'a = new Accumulator(body.trim, Text.join(plines,chr(10).ToText),offsets.value(body.trim))
		      'aggregators.value(body.trim)=a
		      'else
		      'result = result + ptagerror+ti+" Error : Aggregator missing end"+ptag2
		      'errors.Append il
		      'end
		    case "assert"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.globals = dict
		        if not r.assert(body) then
		          // nothing
		          result = result + ptagerror+ti+" Assertion error : " + body +ptag2
		          errors.Append il
		          if transactionprefix <>"" then
		            transactionerror = ti+" Assertion error : " + body
		          end
		        end if
		      end
		      
		    case "beep"
		      SetBeep body
		    case "compile"
		      xp = new XPEvaluator(functions)
		      xp.Compile(body)
		      result = result + ptag + text.join(xp.rpn," ") + ""+ptag2
		      xp = nil
		    case "data"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        found = false
		        r = stack(UBound(stack))
		        r.globals = dict
		        r.functions = functions
		        while i<c and not found
		          i = i + 1
		          il = il+1
		          ti = str(il+1).ToText
		          line = trim(lines(i)).toText
		          if line <> "end data" then
		            r.Insert(line)
		          else
		            found = true 
		          end
		        wend
		      end
		    case "deserialize"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.Deserialize
		      end
		    case "difference"
		      if ubound(stack) < 1 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack.Pop
		        r2 = stack.pop
		        r2.difference(r)
		        stack.AddRow r2
		      end
		    case "dup"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack)).Clone
		        stack.AddRow r
		      end
		    case "echo"
		      'locals  = new Dictionary
		      if ubound(stack) >=0 then
		        r = stack(UBound(stack))
		        if r.tuples.Count > 0 then
		          au = r.tuples.value(0)
		          if au isa Tuple then
		            tp= tuple(au)
		            for each e as DictionaryEntry in tp.Fields
		              if not dict.HasKey(e.key) then
		                dict.Value(e.key) = e.Value
		              end
		            next
		          end
		        end
		      end
		      xp = new XPEvaluator(functions)
		      xp.Compile(body)
		      tx =  xp.evaluate(dict)
		      result = result + tx + ptag2 
		      if tx.Length>0 then
		        select case tx.left(1)
		        case "=", "*", "{","|"
		        else
		          result = result + ptag2
		        end
		      end
		      xp = nil
		    case "else"
		      loopcounter = 1
		      if ifstack.Count > 0 then 
		        while i<c and loopcounter > 0
		          i = i + 1
		          mline = trim(lines(i)).toText
		          if left(mline,2) = "if" then
		            loopcounter = loopcounter + 1
		          elseif left(mline,4) = "else" then  // depends, if after if or end if 
		            if loopcounter = 1 then
		              loopcounter = loopcounter - 1
		            end
		          elseif left(mline,6) = "end if" then
		            loopcounter = loopcounter - 1
		          end
		        wend
		      else
		        result = result + ptagerror+ti+" Error : End not possible here"+ptag2
		        errors.Append il
		      end
		    case "end"
		      if line = "end while" and whilestack.Count>0 then
		        i = whilestack.pop -1
		      elseif line = "end if"  then
		        if ifstack.Count > 0 then
		          ifstack.RemoveRowAt ubound(ifstack)
		        else
		          result = result + ptagerror+ti+" Error : Endif not possible here"+ptag2
		          errors.Append il
		        end
		      elseif line = "end transaction" then
		        // clean up transaction
		        if transactionerror<>"" then
		          result = result + ptagerror+ti+" Transaction errror: "+transactionerror+ptag2
		          // delete all 
		          if currentpath<>"" then
		            for each e as DictionaryEntry in transactiondict
		              file = new FolderItem(currentpath+e.value,folderitem.PathModes.URL,true)
		              if file.Exists then
		                file.Delete
		              end
		            next
		          end
		        else
		          // copy 
		          if currentpath<>"" then
		            for each e as DictionaryEntry in transactiondict
		              file = new FolderItem(currentpath+e.value,folderitem.PathModes.URL,true)
		              if file.Exists then
		                file2 = new FolderItem(currentpath+e.key,folderitem.PathModes.URL,true)
		                if file2.Exists then
		                  file2.Delete
		                end
		                file.moveto(file2)
		              else
		                result = result + ptagerror+ti+" Transaction errror: missing file "+e.value+ptag2
		              end
		            next
		          end
		        end
		        transactionprefix = ""
		        transactionerror =""
		        transactiondict.RemoveAll
		      else
		        result = result + ptagerror+ti+" Error : End not possible here"+ptag2
		        errors.Append il
		      end
		    case "extend"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.globals = dict
		        r.functions = functions
		        r.Notifier = me
		        r.extend(body)
		        r.Notifier = nil
		      end
		    case "format"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.format1(body)
		      end
		    case "formatdump"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        vlist = r.Formats.keys
		        redim tlist(-1)
		        for each velem as variant in vlist
		          tlist.AddRow velem.textvalue+" "+r.Formats.value(velem).TextValue
		        next
		        result = result  + ptag+ "Formats: "+Text.join(tlist,", ")+ ptag2
		      end
		    case "function"
		      redim plines(-1)
		      plines.AddRow line
		      fields = line.split("(")
		      if ubound(fields) = 0 then
		        result = result + ptagerror+ti+" Error : Missing paranthesis"+ptag2
		        errors.Append il
		      end
		      key = fields(0).trim
		      key = key.mid(len("function ")).trim
		      fields.RemoveRowAt 0
		      body = "(" + text.join(fields,"(")
		      if offsets.HasKey(body.trim) then
		        result = result + ptagerror+ti+" Warning : Symbol overwritten"+ptag2
		        errors.Append il
		      end
		      offsets.Value(key) = i
		      found = false
		      while i<c and not found
		        i = i + 1
		        line = lines(i).trim
		        plines.AddRow line
		        if line = "end function" then
		          found = true 
		        end
		      wend
		      if found then
		        fn = new XPCompiledFunction(key, Text.join(plines,chr(10).ToText),offsets.value(key),false,Functions)
		        functions.value(key)= fn
		      else
		        result = result + ptagerror+ti+" Error : Function missing end"+ptag2
		        errors.Append il
		      end
		    case "if"
		      xp = new XPEvaluator(functions)
		      xp.Compile(body)
		      ifstack.AddRow i
		      if val(xp.evaluate(dict)) <> 0 then
		      else
		        loopcounter = 1
		        while i<c and loopcounter > 0
		          i = i + 1
		          mline = trim(lines(i)).toText
		          if left(mline,2) = "if" then
		            loopcounter = loopcounter + 1
		          elseif left(mline,4) = "else" then
		            if loopcounter = 1 then
		              loopcounter = loopcounter - 1
		            end
		          elseif left(mline,6) = "end if" then
		            loopcounter = loopcounter - 1
		          end
		        wend
		      end
		    case "import"
		      r = new Relation("a", dict)
		      xp = new XPEvaluator(functions)
		      xp.Compile(body)
		      tn = xp.evaluate(dict)
		      xp = nil
		      if tn="" then
		        result = result + ptagerror+ti+" Error : Empty filename"+ptag2
		        errors.Append il
		      elseif currentpath<>"" then
		        file = new FolderItem(currentpath+tn,folderitem.PathModes.URL,true)
		        if not file.Exists then
		          result = result + ptagerror+ti+" Error : File does not exist"+ptag2
		          errors.Append il
		        else
		          dim tip as TextInputStream
		          tip = TextInputStream.Open(file)
		          tip.Encoding = Encodings.UTF8
		          tx = tip.ReadAll.ToText
		          if tx<>"" then
		            r = new Relation("",dict)
		            r.import(tx,tn)
		            stack.AddRow r
		          else
		            result = result + ptagerror+ti+" Error : Empty file "+tn+ptag2
		            errors.Append il
		          end
		        end
		      else
		        result = result + ptagerror+ti+" Error : File not imported (no currentpath)"+ptag2
		        errors.Append il
		      end
		    case "include"
		      
		      r = new Relation("",dict)
		      xp = new XPEvaluator(functions)
		      xp.Compile(body)
		      tn = xp.evaluate(dict)
		      xp = nil
		      if tn="" then tn=" "
		      if not ValidFileName(tn.Replace(".rel","")) then
		        result = result + ptagerror+ti+" Error : Invalid filename "+tn+ptag2
		        errors.Append il
		      elseif not (".rel"=tn.right(4)) then
		        result = result + ptagerror+ti+" Error : Invalid filename "+tn+ptag2
		        errors.Append il
		      else
		        if tn="" then
		          result = result + ptagerror+ti+" Error : Empty filename"+ptag2
		          errors.Append il
		        elseif currentpath<>"" then
		          
		          file = new FolderItem(currentpath+tn,folderitem.PathModes.URL,true)
		          if not file.Exists then
		            result = result + ptagerror+ti+" Error : File does not exist "+tn+ptag2
		            errors.Append il
		          else
		            dim tip as TextInputStream
		            tip = TextInputStream.Open(file)
		            
		            if file.Length>0 then 
		              dim tmp as text
		              tmp = tip.ReadAll.ToText
		              
		              dim mlines(-1) as text
		              offsets.Value(tn) = -i // give only fixed number on error include
		              result = result + run(tmp,tn,"")
		              
		            end if
		            tip = nil
		          end if
		        else
		          result = result + ptagerror+ti+" Error : Empty file "+tn+ptag2
		          errors.Append il
		        end
		        
		      end
		      
		    case "insert"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.globals = dict
		        r.functions = functions
		        r.Insert(body)
		      end
		    case "intersection"
		      if ubound(stack) < 1 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack.Pop
		        r2 = stack.pop
		        r2.intersection(r)
		        stack.AddRow r2
		      end
		    case "join"
		      if ubound(stack) < 1 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack.Pop
		        r.globals = dict
		        r.functions = functions
		        r.Notifier = me
		        r2 = stack.pop
		        r2.Notifier = me
		        r2.join(r, body)
		        r.Notifier = nil
		        r2.Notifier = nil
		        r = nil
		        stack.AddRow r2
		        
		      end
		    case "label"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.label(body)
		      end
		    case "limit"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.globals = dict
		        r.functions = functions
		        r.limit(body)
		      end
		    case "memory"
		      result = result + ptag + "Objects : "+ Str(Runtime.ObjectCount).ToText +". Memory used : "+ format(Runtime.MemoryUsed/1024/1024,"0").ToText+" MB " +ptag2
		    case "order"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.order(body)
		      end
		    case "parameter"
		      if ubound(internals) < 0 then
		        result = result + ptagerror+ti+" Error : Parameter stack empty"+ptag2
		        errors.Append il
		      else
		        tx = internals(0).trim
		        xp = new XPEvaluator(functions)
		        xp.Compile(tx)
		        dict.value(body.Trim) = xp.evaluate(dict)
		        xp = nil
		        internals.RemoveRowAt 0
		      end
		    case "parse"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.globals = dict
		        r.functions = functions
		        r.parse(body)
		      end
		    case "pop"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack.Pop
		        r = nil
		      end
		    case "print"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        if mode = "HTML" and true then
		          result = result + r.ToHTML(body)
		        else
		          result =  result + ptag+ r.ToCSV(body)+text.EndOfLine
		        end
		      end
		    case "program"
		      redim plines(-1)
		      fields = body.split("(")
		      if ubound(fields) = 0 then
		        result = result + ptagerror+ti+" Error: Missing paranthesis"+ptag2
		        errors.Append il
		      end
		      key = fields(0).trim
		      fields.RemoveRowAt 0
		      body = "("+Text.Join(fields,")").trim
		      if left(body,1) <> "(" or right(body,1) <> ")" then
		        result = result + ptagerror+ti+" Error: Missing paranthesis"+ptag2
		        errors.Append il
		      end
		      body = body.mid(1,len(body)-2) 
		      if body<>"" then
		        commafields = body.split(",")
		        k = commafields.Count-1
		        for j=0 to k
		          plines.AddRow "parameter "+commafields(j).Trim
		        next
		      end
		      if offsets.HasKey(key) then
		        result = result + ptagerror+ti+" Warning : Symbol overwritten"+ptag2
		        errors.Append il
		      end
		      offsets.Value(key) = i+1
		      found = false
		      while i<c and not found
		        i = i + 1
		        line = lines(i).trim
		        if line <> "end program" then
		          plines.AddRow line
		        else
		          
		          found = true 
		        end
		      wend
		      if found then
		        programs.value(key)=Text.join(plines,chr(10).ToText)
		      else
		        result = result + ptagerror+ti+" Error : Program missing end"+ptag2
		        errors.Append il
		      end
		    case "project"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.aggregators = aggregators
		        r.functions = functions
		        r.globals = dict
		        r.Notifier = me
		        r.project(body)
		        r.Notifier = nil
		      end
		    case "read"
		      r = new Relation("",dict)
		      dim enc as text
		      enc = "utf8"
		      
		      dim csvpad as Boolean = false
		      
		      'if body.trim.length > 4 and body.trim.left(3)= "pad" then
		      'csvpad = true
		      'body =body.trim.mid(4)
		      'fields = body.trim.Split(" ")
		      'end
		      
		      if body.trim.length > 7 and body.trim.left(8)= "encoding" then
		        body =body.trim.mid(9)
		        fields = body.trim.Split(" ")
		        enc = fields(0)
		        fields.RemoveRowAt 0
		        if fields.count = 0 then
		          result = result + ptagerror+ti+" Error : Missing filename"+ptag2
		          errors.Append il
		          body = ""
		        else
		          body = text.join(fields," ")
		        end
		        
		      end
		      
		      xp = new XPEvaluator(functions)
		      xp.Compile(body)
		      tn = xp.evaluate(dict)
		      xp = nil
		      if tn="" then tn=" "
		      if not ValidFileName(tn.Replace(".csv","").Replace(".txt","").Replace(".json","")) then
		        result = result + ptagerror+ti+" Error : Invalid filename "+tn+ptag2
		        errors.Append il
		      else
		        if tn="" then
		          result = result + ptagerror+ti+" Error : Empty filename"+ptag2
		          errors.Append il
		        elseif  tn.IndexOf(".") = -1 then
		          if globalrelations.HasKey(tn) then
		            r = globalrelations.value(tn)
		          else
		            result = result + ptagerror+ti+" Error : Relation does not exist "+tn+ptag2
		            errors.Append il
		            r = new Relation("",dict)
		          end
		          stack.AddRow r.clone
		        elseif currentpath<>"" then
		          
		          if transactiondict.HasKey(tn) then
		            tn = transactiondict.value(tn)
		          end
		          file = new FolderItem(currentpath+tn,folderitem.PathModes.URL,true)
		          
		          if not file.Exists then
		            result = result + ptagerror+ti+" Error : File does not exist"+ptag2
		            errors.Append il
		          else
		            tnkey = tn + "|" + file.ModificationDate.TotalSeconds.ToText
		            #if TargetDesktop
		              if readcache.HasKey(tnkey) then
		                au = readcache.value(tnkey) 
		                if au isa Relation then
		                  r = relation(au)
		                  stack.AddRow r.Clone
		                  continue
		                end
		              else
		                // there may be an obsolete version we need to remove from cache
		                for each key in readcache.keys
		                  if key.Length>= tn.length and tn = key.left(tn.Length) then
		                    readcache.Remove(key)
		                  end
		                next
		              end
		            #endif
		            
		            dim tip as TextInputStream
		            tip = TextInputStream.Open(file)
		            select case enc
		            case "macroman"
		              tip.Encoding = Encodings.MacRoman
		            case "windowslatin1"
		              tip.Encoding = Encodings.WindowsLatin1
		            case "latin1"
		              tip.Encoding = Encodings.ISOLatin1
		            case "utf8"
		              tip.Encoding = Encodings.UTF8
		            else
		              tip.Encoding = Encodings.UTF8
		            end
		            
		            
		            
		            
		            if file.Length>0 then 
		              select case right(tn,4)
		              case ".csv"
		                redim readlines(-1) 
		                while not tip.EndOfFile
		                  readlines.AddRow tip.ReadLine.ToText
		                wend
		                #if  TargetDesktop
		                  if mythread <> nil then
		                    mythread.Progress i
		                    mythread.Sleep(10)
		                  end
		                #EndIf
		                'tx = tip.ReadAll.ToText
		                r = new Relation("",dict)
		                r.Notifier = me
		                r.SetCSV readlines, csvpad
		                stack.AddRow r
		                r.Notifier = nil
		                #if TargetDesktop
		                  readcache.value(tnkey) = r.Clone
		                #endif
		                redim readlines(-1) 
		              case ".txt"
		                tx = tip.ReadAll.ToText
		                r = new Relation("",dict)
		                r.tab = tx
		                stack.AddRow r
		                #if TargetDesktop
		                  readcache.value(tnkey) = r.Clone
		                #endif
		              else
		                tx = tip.ReadAll.ToText
		                if right(tn,5)=".json" then
		                  r = new Relation("",dict)
		                  r.JSON = tx
		                  stack.AddRow r
		                  #if TargetDesktop
		                    readcache.value(tnkey) = r.Clone
		                  #endif
		                else
		                  result = result + ptagerror+ti+" Error : Invalid filename "+tn+ptag2
		                  errors.Append il
		                end
		              end
		            else
		              result = result + ptagerror+ti+" Error : Empty file "+tn+ptag2
		              errors.Append il
		            end
		            tip = nil
		          end
		        else
		          result = result + ptagerror+ti+" Error : File not read (no currentpath)"+ptag2
		          errors.Append il
		        end
		        'end
		      end
		    case "relation"
		      r = new relation(body,dict)
		      stack.AddRow r
		    case "rename"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.globals = dict
		        r.rename(body)
		      end
		    case "run"
		      fields = body.split("(")
		      if ubound(fields) = 0 then
		        result = result + ptagerror+ti+" Error : Missing arguments"+ptag2
		        errors.Append il
		      else
		        key = fields(0).trim
		        fields.RemoveRowAt 0
		        body = "(" + Text.Join(fields,"(")
		        if body.left(1) <> "(" or body.right(1) <> ")" then
		          result = result + ptagerror+ti+" Error : Missing paranthesis"+ptag2
		          errors.Append il
		        else
		          body = body.mid(1,len(body)-2)
		          if programs.HasKey(key) then
		            tx = programs.value(key)
		            
		            dim dict2 as new Dictionary
		            dim dict0 as new Dictionary
		            if dict <> nil then
		              for each e as DictionaryEntry in dict
		                dict2.value(e.key) = e.Value
		              next
		            end
		            dict0 = dict
		            result = run(tx,key,body,dict2) // result is property 
		            dict = dict0
		          else
		            result = result + ptagerror+ti+" Error : Program not defined "+key+ptag2
		            errors.Append il
		          end
		        end
		      end
		    case "select"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.globals = dict
		        r.functions = functions
		        r.Notifier = me
		        r.Select1(body)
		        r.Notifier = nil
		      end
		    case "serialize"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.Serialize
		      end
		    case "set"
		      'locals  = new Dictionary
		      if ubound(stack) >=0 then
		        r = stack(UBound(stack))
		        if r.tuples.Count > 0 then
		          au = r.tuples.value(0)
		          if au isa Tuple then
		            tp= tuple(au)
		            for each e as DictionaryEntry in tp.Fields
		              if not dict.HasKey(e.key) then
		                dict.Value(e.key) = e.Value
		              end
		            next
		          end
		        end
		      end
		      fields = body.split(" ")
		      key = fields(0)
		      fields.RemoveRowAt 0
		      body = Text.join(fields," ").trim
		      fields = body.split(" ")
		      if fields(0) <> "=" then
		        raise new XPError("Set missing =",601)
		      end
		      fields.Remove 0
		      body = Text.join(fields," ").trim
		      xp = new XPEvaluator(functions)
		      xp.Compile(body)
		      dict.value(key) = xp.evaluate(dict)
		      xp = nil
		    case "stack"
		      r = new relation("stack, cardinality, column", dict)
		      dim sti, stc, ci as int32
		      for sti = 0 to ubound(stack)
		        r2 = stack(sti)
		        for each th as text in r2.header
		          dim ins as text
		          ins = str(sti+1).totext+","+str(r2.Cardinality).totext+","""+th+""""
		          r.insert(ins)
		        next
		      next
		      stack.AddRow(r)
		    case "stop"
		      i = c
		    case "swap"
		      if ubound(stack) < 1 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack.Pop
		        r2 = stack.pop
		        stack.AddRow r
		        stack.AddRow r2
		      end
		    case "template"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack.Pop
		        xp = new XPEvaluator(functions)
		        xp.Compile(body)
		        tn = xp.evaluate(dict)
		        xp = nil
		        if tn="" then tn=" "
		        if not ValidFileName(tn.Replace(".txt","")) then
		          result = result + ptagerror+ti+" Error : Invalid filename "+tn+ptag2
		          errors.Append il
		        else
		          if tn="" then
		            result = result + ptagerror+ti+" Error : Empty filename "+tn+ptag2
		            errors.Append il
		          elseif currentpath<>"" then
		            
		            file = new FolderItem(currentpath+tn,folderitem.PathModes.URL,true)
		            if not file.Exists then
		              result = result + ptagerror+ti+" Error : File does not exist "+tn+ptag2
		              errors.Append il
		            else
		              dim tip as TextInputStream
		              tip = TextInputStream.Open(file)
		              
		              if file.Length>0 then 
		                dim tmp as text
		                tmp = tip.ReadAll.ToText
		                result = result + r.ToTemplate(tmp)
		              end if
		              tip = nil
		            end if
		          else
		            result = result + ptagerror+ti+" Error : Empty file "+tn+ptag2
		            errors.Append il
		          end
		          
		        end
		        stack.AddRow r
		      end
		    case "transaction"
		      transactionprefix = "tmp-"
		      transactionerror = ""
		    case "union"
		      if ubound(stack) < 1 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack.Pop
		        r2 = stack.pop
		        r2.union(r)
		        stack.AddRow r2
		      end
		    case "update"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        r.globals = dict
		        r.functions = functions
		        r.Notifier = me
		        r.update(body)
		        r.Notifier = nil
		      end
		    case "while"
		      xp = new XPEvaluator(functions)
		      xp.Compile(body)
		      if val(xp.evaluate(dict)) <> 0 then
		        whilestack.AddRow i
		      else
		        //whilestack.RemoveRowAt(UBound(whilestack))
		        loopcounter = 1
		        while i<c and loopcounter > 0
		          i = i + 1
		          mline = trim(lines(i)).toText
		          if left(mline,5) = "while" then
		            loopcounter = loopcounter + 1
		          elseif left(mline,9) = "end while" then
		            loopcounter = loopcounter - 1
		          end
		        wend
		      end
		    case "write"
		      if ubound(stack) < 0 then
		        result = result + ptagerror+ti+" Error : Stack empty"+ptag2
		        errors.Append il
		      else
		        r = stack(UBound(stack))
		        xp = new XPEvaluator(functions)
		        xp.Compile(body)
		        tn = xp.evaluate(dict)
		        if transactionprefix <> "" then
		          transactiondict.Value(tn) = transactionprefix+tn
		          tn = transactionprefix+tn
		        end
		        
		        xp = nil
		        if not ValidFileName(tn.trim.Replace(".csv","").Replace(".txt","").Replace(".json","")) then
		          result = result + ptagerror+ti+" Error : Invalid filename "+tn+ptag2
		          errors.Append il
		        else
		          if tn="" then
		            result = result + ptagerror+ti+" Error : Empty filename"+ptag2
		            errors.Append il
		          elseif currentpath<>"" or tn.IndexOf(".") = -1 then
		            file = new FolderItem(currentpath+tn,folderitem.PathModes.URL,true)
		            dim tout as TextOutputStream
		            select case right(tn,4)
		            case ".csv"
		              r = stack(UBound(stack))
		              tout = TextOutputStream.Create(file)
		              tout.Encoding = Encodings.UTF8
		              tout.Write(r.csv)
		            case ".txt"
		              r = stack(UBound(stack))
		              tout = TextOutputStream.Create(file)
		              tout.Encoding = Encodings.UTF8
		              tout.Write(r.tab)
		            else
		              if right(tn,5)=".json" then
		                r = stack(UBound(stack))
		                tout = TextOutputStream.Create(file)
		                tout.Encoding = Encodings.UTF8
		                tout.Write(r.json)
		              elseif tn.IndexOf(".") = -1 then
		                globalrelations.Value(tn) = r.Clone
		              else
		                result = result + ptagerror+ti+" Error : Invalid filename "+tn+ptag2
		                errors.Append il
		              end
		            end
		            tout = nil
		          else
		            result = result + ptagerror+ti+" Error : File not saved "+tn+ptag2
		            errors.Append il
		          end
		        end
		        'end
		      end
		      
		    else
		      
		      result = result + ptagerror+ti+" Error : " + lines(i) + ""+ptag2
		      errors.Append il
		      
		    end
		  next
		  'StopProfiling
		  
		  #if TargetConsole 
		    if not interactive then
		      stdout.write(TextExtensions.newline)
		    end
		  #EndIf
		  
		  if mode = "html" and false then
		    if internal ="" then
		      result = result + "</body></html>"
		    end
		  end
		  
		  return result
		  
		  Exception err as RelationError
		    result = result + ptagerror+ti+" Error : " + err.Message.totext + ""+ptag2
		    errors.Append il
		    return result
		  Exception err as XPError
		    result = result + ptagerror+ti+" Error : " + err.Message.totext + ""+ptag2
		    errors.Append il
		    return result
		  Exception err as RegExException
		    result = result + ptagerror+ti+" Error : " + err.Message.totext + ""+ptag2
		    errors.Append il
		    return result
		  Exception err as RuntimeException
		    result = result + ptagerror+ti+" Error : " + err.Message.totext + ""+ptag2
		    errors.Append il
		    return result
		  Exception err as OutOfBoundsException
		    result = result + ptagerror+ti+" Error : OutOfBoundsException"
		    return result
		    
		    
		    
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetBeep(notes as text)
		  #if  TargetDesktop
		    if mythread <> nil then
		      if notes.trim <> "" then
		        mythread.Beep(notes)
		      else
		        mythread.Beep("C")
		      end
		      mythread.Sleep(50)
		    end
		  #EndIf
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetMessage(i as integer)
		  #if  TargetDesktop
		    if mythread <> nil then
		      mythread.RelationProgress(i)
		      mythread.Sleep(10)
		    end
		  #EndIf
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ValidFileName(t as text) As boolean
		  dim i,c as int64
		  dim ch as text
		  
		  if t.length < 1 then return false
		  if t.left(1) = "." then return false
		  if t.IndexOf(":")>-1 then return false
		  if t.IndexOf("/")>-1 then return false
		  
		  return true
		End Function
	#tag EndMethod


	#tag Hook, Flags = &h0
		Event SetProgress(i as int64)
	#tag EndHook


	#tag Property, Flags = &h0
		aggregators As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		context As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		currentpath As text
	#tag EndProperty

	#tag Property, Flags = &h0
		errors() As int64
	#tag EndProperty

	#tag Property, Flags = &h0
		functions As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		globalrelations As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		interactive As boolean
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mode As text
	#tag EndProperty

	#tag Property, Flags = &h0
		mythread As LineHandlerThread
	#tag EndProperty

	#tag Property, Flags = &h0
		offsets As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		programs As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		readcache As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		Result As text
	#tag EndProperty

	#tag Property, Flags = &h0
		stack() As Relation
	#tag EndProperty

	#tag Property, Flags = &h0
		transactiondict As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		transactionerror As text
	#tag EndProperty

	#tag Property, Flags = &h0
		transactionprefix As text
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
			Name="currentpath"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="text"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="interactive"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Result"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="text"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="transactionprefix"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="text"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="transactionerror"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="text"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
