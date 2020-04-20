#tag Class
Protected Class OrderedDictionary
Implements Xojo.Core.Iterable
	#tag Method, Flags = &h0
		Function Clone() As OrderedDictionary
		  dim od as new OrderedDictionary
		  od.dict = dict.clone
		  
		  
		  for each t as Text in arr
		    od.arr.AddRow t
		  next
		  
		  return od
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor()
		  dict = new dictionary
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Count() As Integer
		  return arr.count
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DictCompare(a as text, b as text) As Integer
		  dim adict, bdict as dictionary
		  dim fields() as text
		  dim anum, bnum as double
		  dim atext, btext as text
		  dim test as integer
		  dim aua, aub as auto
		  
		  aua = dict.value(a)
		  aub = dict.value(b)
		  
		  if aua isa tuple then
		    adict = tuple(aua).Fields
		  else
		    raise new XPError("Dict Compare illegal value",609)
		  end
		  if aub isa tuple then
		    bdict = tuple(aub).Fields
		  else
		    raise new XPError("Dict Compare illegal value",609)
		  end
		  
		  for each p as text in OrderPairs
		    fields = p.trim.split(" ")
		    if fields.Count<2 then fields.AddRow "A"
		    
		    if not adict.HasKey(fields(0)) or not bdict.HasKey(fields(0)) then
		      raise new XPError("Dict Compare missing field "+ fields(0),609)
		    end
		    
		    select case fields(1)
		    case "A"
		      atext = adict.Value(fields(0))
		      btext = bdict.Value(fields(0))
		      test = atext.Compare(btext,1) 
		      if test <> 0 then return test
		    case "Z"
		      atext = adict.Value(fields(0))
		      btext = bdict.Value(fields(0))
		      test = atext.Compare(btext,1) 
		      if test <> 0 then return -test
		    case "a"
		      atext = adict.Value(fields(0))
		      btext = bdict.Value(fields(0))
		      test = atext.Compare(btext,0) 
		      if test <> 0 then return test
		    case "z"
		      atext = adict.Value(fields(0))
		      btext = bdict.Value(fields(0))
		      test = atext.Compare(btext,0) 
		      if test <> 0 then return -test
		    case "1"
		      anum = val(adict.Value(fields(0)))
		      bnum = val(bdict.Value(fields(0)))
		      if anum > bnum then return 1
		      if anum < bnum then return -1
		    case "9"
		      anum = val(adict.Value(fields(0)))
		      bnum = val(bdict.Value(fields(0)))
		      if anum > bnum then return -1
		      if anum < bnum then return 1
		    else
		      raise new RelationError("invalid order parameter",501)
		    end
		    
		    
		    
		    
		  next
		  
		  return 0
		  
		  Exception err as KeyNotFoundException
		    raise new RelationError("invalid order field",501)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetIterator() As OrderedDictionaryEntry
		  // Part of the Xojo.Core.Iterable interface.
		  
		  return new OrderedDictionaryEntry(self)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function HasHash(t as Text) As Boolean
		  return dict.HasKey(t)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Key(i as integer) As Text
		  if i<0 then
		    raise new OutOfBoundsException
		  elseif i>=arr.Count then
		    raise new OutOfBoundsException
		  else
		    return arr(i)
		  end
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Lookup(key as Text, default as Auto) As Auto
		  return dict.Lookup(key,default)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Order()
		  if arr.Count < 2 then return
		  
		  dim aua as auto
		  dim adict as Dictionary
		  dim fields(-1) as text
		  
		  aua = dict.value(arr(0))
		  if aua isa tuple then
		    adict = tuple(aua).Fields
		  else
		    raise new XPError("Dict Compare illegal value",609)
		  end
		  
		  for each p as text in OrderPairs
		    fields = p.trim.split(" ")
		    if not adict.HasKey(fields(0)) then
		      raise new XPError("Dict Compare missing field "+ fields(0),609)
		    end
		  next
		  
		  arr.sort(addressof DictCompare)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Remove(i as integer)
		  
		  
		  dict.Remove(key(i))
		  
		  arr.Remove i
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Remove(k as Text)
		  dim i as integer
		  
		  dict.Remove(k)
		  
		  i = arr.IndexOf(k) 
		  arr.Remove i
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub RemoveAll()
		  arr.RemoveAllRows
		  dict.RemoveAll
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetValue(key as Text, v as Auto)
		  if not dict.HasKey(key) then
		    arr.AddRow key
		  end
		  dict.value(key) = v
		  
		  'if arr.IndexOf(key)=-1 then // complexity n-square!
		  '
		  'end
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Value(i as integer) As Auto
		  dim a as auto
		  
		  if i<0 then 
		    raise new OutOfBoundsException
		  elseif i>=arr.Count then
		    raise new OutOfBoundsException
		  else
		    return dict.value(key(i))
		  end
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Value(t as text) As Auto
		  return dict.value(t)
		End Function
	#tag EndMethod


	#tag Property, Flags = &h1
		Protected arr() As text
	#tag EndProperty

	#tag Property, Flags = &h0
		dict As dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		OrderPairs() As Text
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
