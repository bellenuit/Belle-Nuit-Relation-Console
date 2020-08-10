#tag Class
Protected Class XPRegex
Inherits XPFunction
	#tag Event
		Sub pRun(byref stack() as text)
		  dim a, b as text
		  
		  if ubound(stack)<1 then
		    raise new xperror("stack <2",102)
		  end
		  
		  a = stack.Pop
		  b = stack.Pop
		  
		  
		  dim rg as new RegEx
		  dim myMatch As RegExMatch
		  
		  rg.SearchPattern = a
		  myMatch = rg.Search(b)
		  If myMatch <> Nil Then
		    stack.AddRow "1"
		  Else
		    stack.AddRow "0"
		  End If
		  
		  Exception err As RegExException
		    raise new XPError(err.Message,111)
		    
		    
		    
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor()
		  arity = 2
		  label = ":regex"
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
