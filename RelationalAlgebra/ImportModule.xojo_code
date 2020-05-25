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

	#tag Method, Flags = &h0
		Function ImportWikiFields(t as text) As Dictionary
		  Var re As New RegEx
		  Var match As RegExMatch
		  var dict as Dictionary
		  var dict2 as Dictionary
		  dim maxfields, i,c as integer = 0
		  
		  re.SearchPattern = "\[\[([^\]\|]*)([\|]?)(.*?)\]\]"
		  match = re.Search(t)
		  
		  Var val As text
		  var fieldstring as text
		  var key as text
		  var fields(-1) as text
		  
		  dict = new Dictionary
		  Do
		    If match <> Nil Then
		      val = match.SubExpressionString(1).totext
		      
		      var delim as integer
		      delim = val.indexof("::")
		      if delim > -1 then
		        val = match.SubExpressionString(1).totext+match.SubExpressionString(2).totext+match.SubExpressionString(3).totext
		        fieldstring = val.mid(delim+2)
		        fields = fieldstring.Split("::")
		        
		        key = val.left(delim)
		        var arr(-1) as text
		        if dict.HasKey(key) then
		          arr = dict.Value(key)
		        end
		        for each f as text in fields
		          arr.AddRow(f)
		        next
		        maxfields = max(maxfields,arr.Count)
		        dict.Value(key) = arr
		      End If
		      
		      match = re.Search
		    end
		  Loop Until match Is Nil
		  
		  c = maxfields-1
		  dict2 = new Dictionary
		  var d as Dictionary
		  var arr2(-1) as text
		  for i = 0 to c
		    d = new Dictionary
		    for each key in dict.keys
		      arr2 = dict.Value(key)
		      val = arr2(min(i,arr2.Count-1))
		      d.value(key) = val
		    next
		    dict2.Value(i) = d
		  next
		  
		  
		  
		  
		  return dict2
		End Function
	#tag EndMethod


	#tag Note, Name = Untitled
		
		function swGetAllFields($s,$allowinternalvariables=false)
		{
		
		preg_match_all("@\[\[([^\]\|]*)([\|]?)(.*?)\]\]@", $s, $matches, PREG_SET_ORDER);
		
		$result=array();
		
		foreach ($matches as $v)
		{
		
		$val = $v[1]; // link
		//echotime('field0 '.$val);
		// handle fields
		
		if (!$allowinternalvariables)
		if (substr($val,0,1)=='_' && substr($val,0,strlen('_description')) != '_description')
		continue;
		
		
		//echotime('field '.$val);
		if ($delim = stripos($val,'::'))    // we show only values        
		{ 
		$val = $v[1].$v[2].$v[3]; // we use everything 
		
		$fieldstring = substr($val,$delim+2);  
		$key = substr($val,0,$delim);
		
		$fields = explode('::',$fieldstring);
		
		$t = '';
		foreach ($fields as $f)
		{
		$result[$key][]=$f;
		}
		
		
		}
		elseif(substr(strtolower($val),0,strlen('category:')) == 'category:')
		{
		$result['_category'][]=substr($v[1],strlen('category:'));
		}
		else
		{
		$result['_link'][]=$v[1];
		}
		
		}
		
		preg_match_all("@\{\{(.*?)\}\}@", $s, $matches, PREG_SET_ORDER);
		foreach ($matches as $v)
		{
		if (substr($v[1],0,1) != '{') // not use args
		{
		$fields = explode('|',$v[1]);
		$result['_template'][]=$fields[0];
		}
		
		}
		return $result;
		
		}
	#tag EndNote


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
