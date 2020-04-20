#tag Class
Protected Class XPurltext
Inherits XPFunction
	#tag Event
		Sub pRun(byref stack() as text)
		  dim a,k as text
		  dim i, c as integer
		  dim list as text
		  
		  list = "01234567890abcdefghijklmnopqrstuvwxyz-"
		  
		  if ubound(stack)<0 then
		    raise new xperror("stack <1",102)
		  end
		  
		  a =stack.Pop
		  
		  a = a.Lowercase
		  
		  c = a.Length
		  
		  for i =0 to c-1
		    k = a.mid(i,1)
		    if list.IndexOf(k) <0 then
		      a = a.ReplaceAll(k,"-")
		    end
		  next
		  while a.IndexOf("--")>-1 
		    a = a.ReplaceAll("--","-")
		  wend
		  if right(a,1)="-" then
		    a = a.left(a.length-1)
		  end if
		  
		  stack.AddRow a
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor()
		  arity = 1
		  label = "_urltext"
		End Sub
	#tag EndMethod


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
