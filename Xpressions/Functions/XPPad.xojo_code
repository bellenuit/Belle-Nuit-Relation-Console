#tag Class
Protected Class XPPad
Inherits XPFunction
	#tag Event
		Sub pRun(byref stack() as text)
		  dim a, sp as text
		  dim bi, ci, li as integer
		  
		  if ubound(stack)<1 then
		    raise new xperror("stack <2",103)
		  end
		  
		  bi = val(stack.Pop)
		  a = stack.Pop
		  
		  sp = " "
		  
		  while a.Length<bi
		    a = a + sp
		  wend
		  
		  
		  stack.AddRow a
		  
		  
		  Exception e as OutOfBoundsException
		    stack.AddRow ""
		    
		    
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor()
		  arity = 2
		  label = ":pad"
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
