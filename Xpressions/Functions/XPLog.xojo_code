#tag Class
Protected Class XPLog
Inherits XPFunction
	#tag Event
		Sub pRun(byref stack() as text)
		  dim a as double
		  
		  if ubound(stack)<0 then
		    raise new xperror("stack <1",102)
		  end
		  
		  a = val(stack.Pop)
		  
		  if a<0 then
		    raise new xperror("x<0",103)
		  end
		  
		  
		  
		  stack.AddRow cText(log(a)/log(10))
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor()
		  arity = 1
		  label = ":log"
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
