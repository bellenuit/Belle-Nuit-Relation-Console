#tag Module
Protected Module WikitextModule
	#tag Method, Flags = &h0
		Function urltext(t as text) As text
		  dim result, last as text
		  dim list(-1),list2(-1) as text
		  dim good as text = "abcdefghijklmnopqrstuvwxyz0123456789"
		  
		  if t = "" then return t
		  
		  result = t.Lowercase
		  
		  result = result.ReplaceAll("ä","ae")
		  result = result.ReplaceAll("ö","oe")
		  result = result.ReplaceAll("ü","ue")
		  
		  result = result.ReplaceAll("à","a")
		  result = result.ReplaceAll("â","a")
		  result = result.ReplaceAll("ã","a")
		  result = result.ReplaceAll("é","e")
		  result = result.ReplaceAll("è","e")
		  result = result.ReplaceAll("ê","e")
		  result = result.ReplaceAll("ë","e")
		  result = result.ReplaceAll("í","i")
		  result = result.ReplaceAll("ì","i")
		  result = result.ReplaceAll("î","i")
		  result = result.ReplaceAll("ï","i")
		  result = result.ReplaceAll("ó","o")
		  result = result.ReplaceAll("ò","o")
		  result = result.ReplaceAll("ô","o")
		  result = result.ReplaceAll("õ","o")
		  result = result.ReplaceAll("ú","u")
		  result = result.ReplaceAll("ù","u")
		  result = result.ReplaceAll("û","u")
		  result = result.ReplaceAll("ÿ","y")
		  result = result.ReplaceAll("ñ","n")
		  
		  list = result.Split
		  for each elem as text in list
		    if good.IndexOf(elem)>=0 then
		      list2.AddRow(elem)
		    elseif last <> "-" then
		      list2.AddRow " -"
		      last = "-"
		    end
		    last = elem
		  next
		  
		  result = text.join(list2,"")
		  result = result.replaceall(" ","").ReplaceAll("-"," ").Trim.replaceall(" ","_")
		  
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Wiki2HTML(t as text, checknowiki as boolean = true) As text
		  dim re as new RegEx
		  dim multiline as boolean
		  
		  if checknowiki then
		    dim n1, n2 as integer
		    dim t1, t2, t3 as text
		    n1 = t.IndexOf("<nowiki>")
		    if n1 > -1 then
		      n2 = t.IndexOf(n1, "</nowiki>")
		      
		      t1 = t.left(n1)
		      if n2>-1 then
		        t2 = t.mid(n1+len("<nowiki>"),n2-n1-len("<nowiki>"))
		        t3 = t.mid(n2+len("</nowiki>"))
		      else
		        t2 = t.mid(n1+len("<nowiki>"))
		        t3 = ""
		      end
		      t1 = Wiki2HTML(t1,false)
		      t3 = Wiki2HTML(t3,true)
		      return t1 + t2 + t3
		    end
		    
		  end
		  
		  
		  
		  
		  
		  re.Options.ReplaceAllMatches = True
		  
		  
		  
		  
		  dim s as string
		  
		  
		  s = t
		  
		  dim lines(-1), newlines(-1), newlines2(-1), fields(-1) as string
		  dim state as string
		  dim key as string
		  dim i as integer
		  
		  
		  lines = s.Split(EndOfLine)
		  
		  if ubound(lines) > 0 then
		    multiline = true
		  end
		  
		  // table
		  
		  state = ""
		  
		  for each line as string in lines
		    key = left(line+"  ",2)
		    select case state 
		    case ""
		      select case key
		      case "{|"
		        newlines.Append "<table "+mid(line+"  ",4)+">"
		        state = "table"
		      case "|}", "! ", "| ", "|-"
		        newlines.Append "<span class=""error"">"+line+"</span>"
		      else
		        newlines.Append line
		      end
		    case "table"
		      select case key
		      case "|-"
		        newlines.Append "<tr "+mid(line+"  ",4)+">"
		        state = "tablerow"
		      case "! "
		        newlines.Append "<tr>"
		        fields = split(mid(line+"  ",3)," !! ")
		        for each field as string in fields
		          i = field.IndexOf(" | ")
		          if i >= 0 then
		            newlines.append "<th "+trim(left(field,i))+">" +trim(mid(field,i+2))+ "</th>"
		          else
		            newlines.append "<th>" +field + "</th>"
		          end
		        next
		        state = "tablerow"
		      case "| "
		        newlines.Append "<tr>"
		        fields = split(mid(line+"  ",3)," || ")
		        for each field as string in fields
		          i = field.IndexOf(" | ")
		          if i >= 0 then
		            newlines.append "<td "+trim(left(field,i))+">" +trim(mid(field,i+2))+ "</td>"
		          else
		            newlines.append "<td>" +field + "</td>"
		          end
		        next
		        state = "tablerow"
		      case "|}"
		        newlines.Append "</table>"
		        state = ""
		      else
		        newlines.Append "<span class=""error"">"+line+"</span>"
		      end
		    case "tablerow"
		      select case key
		      case "|-"
		        newlines.Append "</tr>"
		        newlines.Append "<tr "+mid(line+"  ",4)+">"
		        state = "tablerow"
		      case "! "
		        fields = split(mid(line+"  ",3)," !! ")
		        for each field as string in fields
		          i = field.IndexOf(" | ")
		          if i >= 0 then
		            newlines.append "<th "+trim(left(field,i))+">" +trim(mid(field,i+3))+ "</th>"
		          else
		            newlines.append "<th>" +field + "</th>"
		          end
		        next
		      case "| "
		        fields = split(mid(line+"  ",3)," || ")
		        for each field as string in fields
		          i = field.IndexOf(" | ")
		          if i >= 0 then
		            newlines.append "<td "+trim(left(field,i))+">" +trim(mid(field,i+3))+ "</td>"
		          else
		            newlines.append "<td>" +field + "</td>"
		          end
		        next
		        state = "tablerow"
		      case "|}"
		        newlines.Append "</tr>"
		        newlines.Append "</table>"
		        state = ""
		      else
		        newlines.Append "<span class=""error"">"+line+"</span>"
		      end
		    end
		  next
		  
		  redim lines(-1)
		  
		  for each s in newlines
		    if s = "" then continue
		    
		    // bold italic
		    re.SearchPattern = "'''''(.*?)'''''"
		    re.ReplacementPattern ="<b><i>$1</i></b>"
		    s = re.Replace(s)
		    
		    // bold
		    re.SearchPattern = "'''(.*?)'''"
		    re.ReplacementPattern ="<b>$1</b>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    // italic
		    re.SearchPattern = "''(.*?)''"
		    re.ReplacementPattern ="<i>$1</i>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    // hr
		    re.SearchPattern = "^----"
		    re.ReplacementPattern ="<hr>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    // h4
		    re.SearchPattern = "^====(.*?)===="
		    re.ReplacementPattern ="<h4>$1</h4>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    // h3
		    re.SearchPattern = "^===(.*?)==="
		    re.ReplacementPattern ="<h3>$1</h3>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    // h2
		    re.SearchPattern = "^==(.*?)=="
		    re.ReplacementPattern ="<h2>$1</h2>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    // simple list
		    re.SearchPattern = "^\*\*(.*)"
		    re.ReplacementPattern ="<ul><li><ul><li>$1</li></ul></li></ul>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    // simple list
		    re.SearchPattern = "^\*(.*)"
		    re.ReplacementPattern ="<ul><li>$1</li></ul>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    // plain links first
		    re.SearchPattern = "^https?://\S*"
		    re.ReplacementPattern ="<a href=""$0"">$0</a>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    '
		    // plain links not [
		    re.SearchPattern = "\s(https?://\S*)"
		    re.ReplacementPattern ="<a href=""$1"">$1</a>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    // named links
		    re.SearchPattern = "[[](https?://\S*) (.*?)[]]"
		    re.ReplacementPattern ="<a href=""$1"">$2</a>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    // named links simple
		    re.SearchPattern = "[[](\S*\.html) (.*?)[]]"
		    re.ReplacementPattern ="<a href=""$1"">$2</a>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    //  links csv simple
		    re.SearchPattern = "[[][[]Media:(\S*\.csv)[]][]]"
		    re.ReplacementPattern ="<a href=""$1"">$1</a>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    //  links txt simple
		    re.SearchPattern = "[[][[]Media:(\S*\.txt)[]][]]"
		    re.ReplacementPattern ="<a href=""$1"">$1</a>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    //  links png simple
		    re.SearchPattern = "[[][[]Image:(\S*\.png)[]][]]"
		    re.ReplacementPattern ="<img src=""$1"" width=""100%"">"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    //  links jpg simple
		    re.SearchPattern = "[[][[]Image:(\S*\.jpg)[]][]]"
		    re.ReplacementPattern ="<img src=""$1"" width=""100%"">"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    //  links  wiki
		    re.SearchPattern = "[[][[](.*?)[]][]]"
		    re.ReplacementPattern ="<a href=""$1.html"">$1</a>"
		    
		    dim match As RegExMatch
		    dim res0, res1,res1url as string
		    match = re.Search(s)
		    do
		      If match <> Nil Then
		        res0 = match.SubExpressionString(0)
		        res1 = match.SubExpressionString(1)
		        res1url = urltext(res1.totext)
		        s = s.ReplaceAll(res0,"<a href="""+res1url+".html"">"+res1+"</a>")
		      End If
		      match = re.Search
		    loop until match=nil
		    s = re.Replace(s)
		    
		    //  errors
		    re.SearchPattern = "{{error}}(.*)"
		    re.ReplacementPattern ="<p class=""error"">$1</p>"
		    re.Options.ReplaceAllMatches = True
		    s = re.Replace(s)
		    
		    if s.IndexOf("<h2>") = -1 and s.IndexOf("<h3>") = -1 and s.IndexOf("<h4>") = -1 _
		      and s.IndexOf("<ul>") = -1 and s.IndexOf("<li>") = -1  and s.IndexOf("<t") = -1 and s.IndexOf("<p") = -1  _
		      and multiline then
		      s = "<p>"+s+"</p>"
		    end
		    
		    
		    newlines2.AddRow s
		    
		  next
		  redim newlines(-1)
		  s = join(newlines2,endofline)
		  
		  
		  
		  return s.ToText
		  
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
