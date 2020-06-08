#tag Class
Protected Class XPCompiledFunction
Inherits XPFunction
	#tag Event
		Sub pRun(byref stack() as text)
		  dorun stack
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h21
		Private Sub Compile()
		  dim i,c,il,j,k as int64
		  dim ti, line, command, command2, body,f,rest,eq as text
		  dim fields(-1) as text
		  dim fl as text
		  dim conditioncount as int64
		  dim found as boolean
		  dim elsefound as int64
		  
		  c = ubound(lines)
		  
		  redim compiledlines(-1)
		  redim compiledinitlines(-1)
		  
		  arity = 0
		  
		  if isAggregator then
		    compiledlines.append "1"
		    compiledlines.append "/aggregatorfirstrun"
		    compiledlines.append ":set"
		    
		    compiledlines.append "#190000"
		    compiledlines.append ":stackcount"
		    compiledlines.append "190001"
		    compiledlines.append ":gotoifn"
		    compiledlines.append "/elem"
		    compiledlines.append ":set"
		  end
		  
		  for i = 0 to c
		    il = offset + i
		    ti = str(il+1).ToText
		    line = trim(lines(i)).toText
		    compiledlines.append "#"+ti
		    // remove comments
		    if line.IndexOf("//") > -1 then
		      line = line.left(line.IndexOf("//"))
		    end
		    if line = "" then continue
		    fields = line.split(" ")
		    command = fields(0)
		    fields.RemoveRowAt 0
		    body = Text.Join(fields," ")
		    
		    select case command
		    case "function"
		      if  isAggregator or i > 0 then
		        raise new XPError("Invalid instruction #"+ti,66)
		      end
		      body = Text.Join(fields," ")
		      fields = body.Split("(")
		      if ubound(fields) = 0 then
		        raise new XPError("Missing paranthesis in function definition #"+ti,66)
		      end
		      fields.RemoveRowAt 0
		      body = "("+Text.Join(fields,"(")
		      body = body.trim
		      if body.left(1) <> "(" or body.right(1) <> ")" then
		        raise new XPError("Missing paranthesis in function definition #"+ti,66)
		      end
		      body = body.mid(1,len(body)-2)
		      fields = body.Split(",")
		      arity = fields.Count
		      for j = arity-1 downto 0
		        fl = fields(j).trim
		        args.addrow fl
		      next
		    case "init"
		      if not isAggregator then
		        raise new XPError("Invalid instruction #"+ti,66)
		      end
		      fields = body.split(" ")
		      f = fields(0).trim
		      if f = "" then
		        raise new XPError("Set empty field #"+ti,321)
		      end
		      fields.RemoveRowAt 0
		      rest = Text.join(fields," ").trim
		      fields = rest.split(" ")
		      eq = fields(0)
		      if eq <> "=" then
		        raise new XPError("Init missing =",601)
		      end
		      fields.RemoveRowAt 0
		      body = Text.Join(fields," ")
		      xp = new XPEvaluator
		      xp.Compile(body)
		      for each ti in xp.rpn
		        compiledlines.AddRow ti
		      next
		      compiledlines.AddRow "/"+f
		      compiledlines.AddRow ":init"
		    case "set"
		      fields = body.split(" ")
		      f = fields(0).trim
		      if f = "" then
		        raise new XPError("Set empty field #"+ti,321)
		      end
		      fields.RemoveRowAt 0
		      rest = Text.join(fields," ").trim
		      fields = rest.split(" ")
		      eq = fields(0)
		      if eq <> "=" then
		        raise new XPError("Set missing =",601)
		      end
		      fields.RemoveRowAt 0
		      body = Text.join(fields," ").trim
		      if body <> "" then
		        xp = new XPEvaluator
		        xp.Compile(body)
		        for each ti in xp.rpn
		          compiledlines.AddRow ti
		        next
		      else
		        compiledlines.AddRow "0"
		      end
		      compiledlines.AddRow "/"+f
		      compiledlines.AddRow ":set"
		    case "if"
		      conditioncount = 1
		      elsefound = -1
		      found = false
		      j=i
		      do
		        j=j+1
		        line = trim(lines(j)).toText
		        fields = line.split(" ")
		        command2 = fields(0)
		        select case command2
		        case "if"
		          conditioncount = conditioncount + 1
		        case "else"
		          if conditioncount=1 then
		            if elsefound <0 then
		              'xp = new XPEvaluator
		              'xp.Compile(body)
		              'for each ti in xp.rpn
		              'compiledlines.AddRow ti
		              'next
		              'compiledlines.append str(j+offset+1).ToText
		              'compiledlines.append ":gotoifn"
		              lines(j) = ""
		              elsefound = j
		            else
		              raise new XPError("Duplicate else #"+ti,801)
		            end
		          end
		        case "end"
		          if line = "end if" then
		            if conditioncount=1 then
		              if elsefound < 0 then
		                xp = new XPEvaluator
		                xp.Compile(body)
		                for each ti in xp.rpn
		                  compiledlines.AddRow ti
		                next
		                compiledlines.append str(j+offset+1).ToText
		                compiledlines.append ":gotoifn"
		              else
		                xp = new XPEvaluator
		                xp.Compile(body)
		                for each ti in xp.rpn
		                  compiledlines.AddRow ti
		                next
		                compiledlines.append str(elsefound+offset+2).ToText
		                compiledlines.append ":gotoifn"
		                lines(elsefound) = "goto "+str(j+offset+1).ToText
		              end
		              lines(j) = ""
		              found = true
		            else
		              conditioncount = conditioncount -1
		            end
		          end
		        else
		          // do nothing
		        end select
		      loop until found or j = c
		      if not found then
		        raise new XPError("Missing end if #"+ti,802)
		      end
		    case "while"
		      conditioncount = 1
		      j=i
		      do
		        j=j+1
		        line = trim(lines(j)).toText
		        fields = line.split(" ")
		        command2 = fields(0)
		        select case command2
		        case "while"
		          conditioncount = conditioncount + 1
		        case "end"
		          if line = "end while" then
		            if conditioncount = 1 then
		              xp = new XPEvaluator
		              xp.Compile(body)
		              for each ti in xp.rpn
		                compiledlines.AddRow ti
		              next
		              compiledlines.append str(j+2+offset).ToText
		              compiledlines.append ":gotoifn"
		              lines(j) = "goto "+str(i+1+offset).ToText
		              found = true
		            else
		              conditioncount = conditioncount -1
		            end
		          end
		        end
		      loop until found or j = c
		    case "else"
		      raise new XPError("Duplicate else #"+ti,801)
		    case "end" //if //while
		      if line <> "end function" then
		        raise new XPError("Duplicate end #"+ti,801)
		      end
		    case "goto" //if //while
		      compiledlines.append body
		      compiledlines.append ":goto"
		    else
		      
		      raise new XPError("Invalid instruction #"+ti,66)
		    end
		    
		  next
		  
		  if isAggregator then
		    compiledlines.append "0"
		    compiledlines.append "/aggregatorfirstrun"
		    compiledlines.append ":set"
		    
		    compiledlines.append "190000"
		    compiledlines.append ":goto"
		    
		    compiledlines.append "#190001"
		  end
		  
		  compiledlines.append "result"
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(lb as text, source as text, off as int64, ia as boolean = false)
		  isAggregator = ia
		  
		  lines = source.split(chr(10).ToText)
		  offset = off 
		  
		  label = ":"+lb
		  
		  xp = new XPEvaluator
		  
		  compile
		  
		  offset = offset
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoAggregatorInit()
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoRun(byref stack() as text)
		  
		  dim localvalues as new Dictionary
		  dim t as text
		  
		  if xp = nil then xp = new XPEvaluator
		  
		  xp.rpn = me.compiledlines
		  
		  for each arg as text in args
		    localvalues.Value(arg) = stack.pop
		  next
		  
		  t = xp.Evaluate(localvalues)
		  
		  stack.AddRow t
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DoRunAggregator(list() as text) As text
		  dim localvalues as new Dictionary
		  dim t as text
		  
		  if xp = nil then xp = new XPEvaluator
		  
		  redim xp.rpn(-1)
		  
		  
		  while list.Count>0
		    xp.rpn.AddRow list.pop
		  wend
		  
		  
		  
		  for each t in me.compiledlines
		    xp.rpn.AddRow t
		  next
		  
		  
		  t = xp.Evaluate(localvalues)
		  
		  return t
		  
		  
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		aggregatelist() As Text
	#tag EndProperty

	#tag Property, Flags = &h21
		Private args() As Text
	#tag EndProperty

	#tag Property, Flags = &h21
		Private compiledinitlines() As text
	#tag EndProperty

	#tag Property, Flags = &h0
		compiledlines() As text
	#tag EndProperty

	#tag Property, Flags = &h0
		fndict As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h21
		Private isAggregator As boolean
	#tag EndProperty

	#tag Property, Flags = &h21
		Private lines() As text
	#tag EndProperty

	#tag Property, Flags = &h21
		Private offset As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		vars As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h21
		Private xp As XPEvaluator
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="label"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="text"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="arity"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="int64"
			EditorType=""
		#tag EndViewProperty
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
	#tag EndViewBehavior
End Class
#tag EndClass
