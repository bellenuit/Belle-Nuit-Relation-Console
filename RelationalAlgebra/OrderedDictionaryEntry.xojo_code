#tag Class
Protected Class OrderedDictionaryEntry
Implements Xojo.Core.Iterator
	#tag Method, Flags = &h0
		Sub Constructor(p as OrderedDictionary)
		  parent = p
		  cursor = 0
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IntrinsicValue() As auto
		  return parent.Value(cursor)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Key() As Text
		  return parent.key(cursor)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function MoveNext() As Boolean
		  // Part of the Xojo.Core.Iterator interface.
		  
		  if cursor < parent.count -1 then
		    cursor = cursor + 1
		    return true
		  else
		    return false
		  end
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Remove()
		  parent.Remove cursor
		  cursor = cursor -1
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Value() As Auto
		  // Part of the Xojo.Core.Iterator interface.
		  
		  'return parent.Value(cursor)
		  
		  return self
		End Function
	#tag EndMethod


	#tag Property, Flags = &h21
		Private cursor As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		Private parent As OrderedDictionary
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
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
			Name="Name"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
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
