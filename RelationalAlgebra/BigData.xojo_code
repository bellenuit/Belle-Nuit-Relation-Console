#tag Class
Protected Class BigData
	#tag Method, Flags = &h0
		Sub Constructor(w as Int64, h as int64 = 1)
		  if w<1 or h<1 then raise new OutOfBoundsException
		  data = new MemoryBlock(32*w*h)
		  if longtext = nil then
		    longtext = new Dictionary
		  end
		  width = w
		  height = h
		  nulltext = data.StringValue(0,32)
		  maxheight=-1
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoubleHeight()
		  // A B
		  // C D
		  // _ _
		  // _ _
		  
		  // A B C D _ _ _ _
		  
		  
		  dim data2 as new MemoryBlock(width*2*height*32)
		  
		  data2.StringValue(0,width*height*32) = data.StringValue(0,width*height*32)
		  
		  data = data2
		  height = 2 * height
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DoubleWidth()
		  // A B _ _
		  // C D _ _
		  
		  // A B _ _ C D _ _ 
		  
		  dim data2 as new MemoryBlock(2*width*height*32)
		  dim i as integer
		  
		  for i = 0 to height-1
		    data2.StringValue(2*i*width*32,width*32) = data.StringValue(i*width*32,width*32)
		  next
		  
		  data = data2
		  width = 2 * width
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Offset(row as int64, column as Int64) As int64
		  return (row * width + column) * 32
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Point(row as int64, column as Int64) As Text
		  dim c as String
		  dim b as byte
		  dim o as integer
		  if row <0 then raise new OutOfBoundsException
		  if column<0 then raise new OutOfBoundsException
		  if row<height and column<width then
		    o = offset(row,column)
		    if data.byte(o) = 32 then
		      c = data.CString(o+1)
		      return c.ToText
		    else
		      c = data.StringValue(o,32)
		      if longtext.HasKey(c) then
		        return longtext.Value(c)
		      else
		        raise new KeyNotFoundException
		      end
		    end
		    
		  else
		    raise new OutOfBoundsException
		  end
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetPoint(row as int64, column as Int64, t as text)
		  dim c,key as String
		  dim o as integer
		  if row <0 then raise new OutOfBoundsException
		  if column<0 then raise new OutOfBoundsException
		  if row<height and column<width then
		    o = offset(row,column)
		    c = t
		    if c.bytes < 31 then
		      data.StringValue(o,32) = nulltext
		      data.byte(o) = 32
		      data.CString(o+1) = t.ToCString(xojo.core.TextEncoding.UTF8)
		    else
		      key = encodehex(md5(c))
		      data.StringValue(o,32) = nulltext
		      data.StringValue(o,32) = key
		      longtext.Value(key)=t
		    end
		    maxheight = max(row,maxheight)
		  elseif  row>=height  then
		    DoubleHeight
		    SetPoint(row,column,t)
		  else 
		    DoubleWidth
		    SetPoint(row,column,t)
		  end
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SetUniqueRow(t() as text)
		  if t.Count<=width then
		    dim m as new MemoryBlock(width*32)
		    dim i as integer
		    dim c, c2,key as string
		    dim o as integer
		    for i = 0 to t.Count-1
		      o = offset(0,i)
		      c = t(i)
		      if c.bytes < 31 then
		        data.StringValue(o,32) = nulltext
		        data.byte(o) = 32
		        data.CString(o+1) = t(i).ToCString(xojo.core.TextEncoding.UTF8)
		      else
		        key = encodehex(md5(c))
		        data.StringValue(o,32) = nulltext
		        data.StringValue(o,32) = key
		        longtext.Value(key)=t
		      end
		    next
		    
		    c = m.StringValue(0,width*32)
		    for i = 0 to maxheight
		      c2 = data.StringValue(i*width*32,width*32)
		      if StrComp(c,c2,1)=0 then return // exists already
		    next
		    maxheight = maxheight +1
		    data.StringValue(maxheight*width*32,width*32)=c
		  else
		    DoubleWidth
		    SetUniqueRow(t)
		  end
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UnitTest()
		  SetUniqueRow(Array("John", "Sally", "Fred", "Nancy"))
		  SetUniqueRow(Array("John", "Sad", "Fred", "Nancy"))
		  SetUniqueRow(Array("John", "Sally", "Fred", "Nancy"))
		  SetUniqueRow(Array("John", "Sally", "Fred", "Nan"))
		  
		  
		  SetPoint(5,10,"hallo hier bin ich")
		  SetPoint(789,8,"Zürich Genève")
		  SetPoint(1014,32,"Fribourg")
		  SetPoint(1014,32,"Frib")
		  SetPoint(5,9,"This method gets the current user’s email addresses. It uses the Count and Value methods of the AddressBookData class to do so.")
		  
		  if  point(1014,32) <> "Frib" then
		    raise new OutOfBoundsException
		  end
		  if  point(789,8) <> "Zürich Genève" then
		    raise new OutOfBoundsException
		  end
		  if  point(5,10) <> "hallo hier bin ich" then
		    raise new OutOfBoundsException
		  end
		  if  point(5,9) <> "This method gets the current user’s email addresses. It uses the Count and Value methods of the AddressBookData class to do so." then
		    raise new OutOfBoundsException
		  end
		  
		  if  point(0,0) <> "John" then
		    raise new OutOfBoundsException
		  end
		  
		  if  point(1,1) <> "Sad" then
		    raise new OutOfBoundsException
		  end
		  
		  if  point(2,3) <> "Nan" then
		    raise new OutOfBoundsException
		  end
		  
		  height = height
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h21
		Private data As MemoryBlock
	#tag EndProperty

	#tag Property, Flags = &h0
		height As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		Shared longtext As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		maxheight As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		nulltext As string
	#tag EndProperty

	#tag Property, Flags = &h21
		Private width As Integer
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
		#tag ViewProperty
			Name="height"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="nulltext"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="string"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="maxheight"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
