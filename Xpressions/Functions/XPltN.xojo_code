#tag Class
Protected Class XPltN
Inherits XPFunction
	#tag Event
		Sub pRun(byref stack() as text)
		  dim a, b as text
		  
		  if ubound(stack)<1 then
		    raise new xperror("stack <2",102)
		  end
		  
		  a = stack.Pop
		  b = stack.Pop
		  
		  if val(b) < val(a) then
		    stack.AddRow("1")
		  else
		    stack.AddRow("0")
		  end
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor()
		  arity = 2
		  label = ":ltn"
		End Sub
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
End Class
#tag EndClass
