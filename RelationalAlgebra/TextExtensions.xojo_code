#tag Module
Protected Module TextExtensions
	#tag Method, Flags = &h1
		Protected Function CaseSensitiveOrder(t1 as text, t2 as text) As Integer
		  return t1.Compare(t2,1)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CSVSplit(extends line as text, byref fields() as text)
		  dim state, acc as text
		  dim quoted as boolean
		  
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
		      case ","
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
		    'error missing end quote, accept silently
		    fields.AddRow acc
		  case "quotesuspend"
		    fields.AddRow acc
		  end
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function FromString(s as string) As text
		  dim s2 as string
		  dim t as text
		  s2 = s.DefineEncoding(encodings.UTF8)
		  t = s2.totext
		  return t
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsNumeric(extends t as Text) As boolean
		  dim r as RegEx
		  
		  dim start, hasperiod as boolean
		  if t="" then return true
		  if t.left(1) = "-" then return t.mid(1).isnumeric
		  
		  start = true
		  
		  r = new regex
		  r.SearchPattern = "^[+-]?\d+(.\d+)?([eE][+-]?\d+)?$"
		  
		  if r.Search(t) <> nil then
		    return true
		  else
		    return false
		  end
		  
		  
		  
		  return true
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function JSONescape(t as text) As text
		  '\b  Backspace (ascii code 08)
		  '\f  Form feed (ascii code 0C)
		  '\n  New line
		  '\r  Carriage return
		  '\t  Tab
		  '\"  Double quote
		  '\\  Backslash character
		  
		  dim result as text
		  result = t
		  result = result.replaceall("\","\\")
		  result = result.replaceall("""","\""")
		  result = result.replaceall(TextExtensions.Ret,"\r")
		  result = result.replaceall(TextExtensions.Tab,"\t")
		  result = result.replaceall(TextExtensions.Newline,"\n")
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Newline() As Text
		  return Encodings.UTF8.chr(10).ToText
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Pad(extends t as Text, ch as text, n as int64) As text
		  dim list() as text
		  dim i,d as integer
		  
		  d = n-t.Length
		  
		  for i = 1 to n
		    list.AddRow ch
		  next
		  
		  return t+Text.join(list,"")
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Quote() As Text
		  return Encodings.UTF8.chr(34).ToText
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Repeat(extends ch as text, n as int64) As text
		  dim list() as text
		  dim i as integer
		  
		  for i = 1 to n
		    list.AddRow ch
		  next
		  
		  return Text.join(list,"")
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Ret() As Text
		  return Encodings.UTF8.chr(13).ToText
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Tab() As Text
		  return Encodings.UTF8.chr(9).ToText
		End Function
	#tag EndMethod


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
	#tag EndViewBehavior
End Module
#tag EndModule
