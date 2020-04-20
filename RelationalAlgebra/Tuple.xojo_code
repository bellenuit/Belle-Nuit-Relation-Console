#tag Class
Protected Class Tuple
	#tag Method, Flags = &h0
		Function Arity() As int64
		  return pfields.Count
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(list as Dictionary)
		  dim keys(), values() as text
		  dim au as text
		  
		  pfields = list.Clone
		  
		  for each f as DictionaryEntry in list
		    keys.AddRow(f.key)
		  next
		  keys.Sort
		  
		  for each k as Text in keys
		    au = list.Value(k)
		    
		    values.AddRow au
		    
		  next
		  
		  pHash = EncodeHex(md5(Text.join(values,""))).totext
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Fields() As Dictionary
		  return pfields.clone
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Hash() As text
		  return pHash
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function HasKey(k as text) As boolean
		  return pfields.HasKey(k)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function SameFamily(t as tuple) As boolean
		  if arity <> t.Arity then return false
		  
		  for each e as DictionaryEntry in pfields
		    if not t.haskey(e.key) then return false
		  next
		  
		  return true
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Value(s as text) As text
		  'for each d as dictionaryEntry in pfields
		  'if d.key = s then
		  'return d.value
		  'end
		  'raise new RelationError("Tuple unknown column",201)
		  'next
		  '
		  '
		  dim result as text
		  dim tx as text = ""
		  result = pfields.Lookup(s,tx)
		  return result
		  
		  // gives Framework failed assertion at ObjectGlue.cpp:167
		  if pfields.HasKey(s) then
		    return pfields.Value(s)
		  else
		    raise new RelationError("Tuple unknown column",201)
		  end
		End Function
	#tag EndMethod


	#tag Property, Flags = &h21
		Private pfields As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h21
		Private pHash As text
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
