#tag Class
Protected Class XPAbs
Inherits XPFunction
	#tag Event
		Sub pRun(byref stack() as text)
		  dim a as double
		  
		  if ubound(stack)<0 then
		    raise new xperror("stack <1",102)
		  end
		  
		  a = val(stack.Pop)
		  
		  stack.AddRow cText(abs(a))
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor()
		  arity = 1
		  label = ":abs"
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
