#tag Class
Protected Class TupleStack
	#tag Method, Flags = &h0
		Function Clone() As TupleStack
		  dim t2 as new tuplestack
		  dim i,c as integer
		  c = list.Count -1 
		  for i = 0 to c
		    t2.Push list(i)
		  next
		  return t2
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Empty() As boolean
		  return list.Count = 0
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Pop() As tuple
		  if empty then raise new XPError("stack pop empty",44)
		  return list.Pop
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Push(au as tuple)
		  list.AddRow au
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Shift() As tuple
		  if empty then raise new XPError("stack pop empty",44)
		  dim au as tuple
		  au = list(0)
		  list.RemoveRowAt 0
		  return au
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		List(-1) As tuple
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
	#tag EndViewBehavior
End Class
#tag EndClass
