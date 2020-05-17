#tag Module
Protected Module ImportModule
	#tag Method, Flags = &h0
		Function ImportFlatJSON(js as JSONItem, path as text = "") As Dictionary
		  dim d as new Dictionary
		  dim child as variant
		  dim d2 as Dictionary
		  dim i,c,j as integer
		  dim currentpath as text
		  dim path2 as text
		  dim n as string
		  
		  if path <>"" then
		    path2 = path+":"
		  else
		    path2 = ""
		  end
		  
		  c = js.count-1
		  
		  for i = 0 to c
		    j = i+1
		    if js.IsArray then
		      currentpath = path2 + j.totext
		      child = js.valueat(i)
		    else
		      n = js.nameat(i)
		      currentpath = path2+ n.totext
		      child = js.value(n)
		    end
		    if child isa JSONitem then
		      d2 = ImportFlatJSON(JSONitem(child),currentpath)
		      For Each de As DictionaryEntry In d2
		        d.value(de.key) = de.value
		      next
		    else
		      d.value(currentpath) = str(child).ToText
		    end
		  next
		  
		  return d
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ImportFlatXML(xl as XMLNode, path as text = "") As Dictionary
		  dim d as new Dictionary
		  dim d2 as Dictionary
		  dim i,c,j as integer
		  dim currentpath as text
		  dim path2 as text
		  dim n as string
		  dim child as xmlnode
		  dim attr as XmlAttribute
		  dim s as string
		  
		  if path <>"" then
		    path2 = path+":"
		  else
		    path2 = ""
		  end
		  if xl isa XmlAttribute then
		    currentpath = path2 + "0:"+xl.name.ToText
		  elseif xl isa XmlDocument then
		    currentpath =""
		  elseif xl isa XmlTextNode then
		    currentpath = path
		  else
		    currentpath = path2 + xl.name.ToText
		  end
		  
		  select case xl.type
		  case 1, 9 // element
		    c = xl.AttributeCount-1
		    for i = 0 to c
		      j = i+1
		      attr = xl.GetAttributeNode(i)
		      if attr <> nil then
		        d2 = ImportflatXML(attr,currentpath)
		        For Each de As DictionaryEntry In d2
		          d.value(de.key) = de.value
		        next
		      end
		    next
		    c = xl.childcount-1
		    for i = 0 to c
		      j = i+1
		      child = xl.Child(i)
		      if child <> nil then
		        if c>0 then
		          d2 = ImportflatXML(child,currentpath+":"+str(j).totext)
		        else
		          d2 = ImportflatXML(child,currentpath)
		        end
		        For Each de As DictionaryEntry In d2
		          d.value(de.key) = de.value
		        next
		      end
		    next
		  case 2 // attribute
		    d.value(currentpath) = xl.value
		  case 3 // text
		    s = xl.value
		    Var re As New RegEx
		    re.SearchPattern = "\s+"
		    re.ReplacementPattern = " "
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    d.value(currentpath) = trim(s)
		  end
		  
		  return d
		  
		End Function
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
End Module
#tag EndModule
