#tag Class
Protected Class Accumulator
	#tag Method, Flags = &h0
		Sub Add(t as text)
		  list.AddRow t
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Clone() As Accumulator
		  
		  dim a as Accumulator
		  
		  if method = "custom" then
		    a = new Accumulator(label, source, offset)
		    return a
		  else
		    a = new Accumulator(method)
		    return a
		  end
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(m as Text)
		  method = m
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(lb as text, src as text, off as int64)
		  method = "custom"
		  label = lb
		  source = src
		  offset = off
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function cText12(d as double) As text
		  
		  
		  dim a as double
		  dim t as text
		  dim s as string
		  
		  a = abs(d)
		  
		  
		  if a > 10e12 then
		    return format(d,"-0.000000000000e").ToText
		  elseif a < 10e-12 then
		    return format(d,"-0.000000000000e").ToText
		  end
		  
		  s = format(d,"-0.############")
		  if len(s)>12 then
		    s = format(round(d*10000000000000)/10000000000000,"-0.############")
		  end
		  if right(s,1)="." then
		    s = mid(s,1,len(s)-1)
		  end
		  return s.ToText
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pAvg() As Text
		  dim acc, v as double
		  
		  if list.Count = 0 then return ""
		  
		  acc = 0
		  for each t as Text in list
		    v = val(t)
		    acc = acc + v
		  next
		  
		  v = acc / list.count
		  
		  return cText12(v)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pConcat() As Text
		  return Text.Join(list,"")
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pCount() As Text
		  return list.count.toText
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function pCustom() As Text
		  dim xp as new XPCompiledFunction(label,source,offset,true)
		  dim result as text = "result"
		  dim elem as text = "elem"
		  
		  xp.fndict = functions
		  
		  return xp.DoRunAggregator(list)
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pMax() As Text
		  dim acc, v as double
		  
		  if list.count = 0 then return ""
		  
		  acc = val(list(0))
		  
		  
		  for each t as Text in list
		    v = val(t)
		    if v > acc then acc = v
		  next
		  
		  return cText12(acc)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pMaxS() As Text
		  dim acc as text
		  
		  if list.count = 0 then return ""
		  
		  acc = list(0)
		  
		  for each t as Text in list
		    if t.Compare(acc,1) > 0 then acc = t
		  next
		  
		  return acc
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pMedian() As Text
		  dim acc(), v as double
		  
		  if list.Count = 0 then return ""
		  
		  for each t as Text in list
		    acc.AddRow val(t)
		  next
		  
		  acc.Sort
		  
		  if acc.count mod 2 <> 0 then
		    v = acc((acc.count-1)/2)
		  else
		    v = (acc(acc.count/2) +  acc(acc.count/2-2))/2
		  end
		  
		  return cText12(v)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pMedianS() As Text
		  dim v as text
		  
		  if list.Count = 0 then return ""
		  
		  
		  list.Sort(addressof TextExtensions.CaseSensitiveOrder)
		  
		  if list.count mod 2 <> 0 then
		    v = list((list.count-1)/2)
		  else
		    v = list(list.count/2-2)
		  end
		  
		  return v
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pMin() As Text
		  dim acc, v as double
		  
		  if list.count = 0 then return ""
		  
		  acc = val(list(0))
		  
		  
		  for each t as Text in list
		    v = val(t)
		    if v < acc then acc = v
		  next
		  
		  return cText12(acc)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pMinS() As Text
		  dim acc as text
		  
		  if list.count = 0 then return ""
		  
		  acc = list(0)
		  
		  for each t as Text in list
		    if t.Compare(acc,1) > 0 then acc = t
		  next
		  
		  return acc
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pStdDev() As Text
		  dim acc, acc2, v as double
		  
		  if list.Count = 0 then return ""
		  
		  acc = 0
		  acc2 = 0
		  for each t as Text in list
		    v = val(t)
		    acc = acc + v
		    acc2 = acc2 + v*v
		  next
		  
		  v = sqrt(acc2/list.Count - acc/list.Count * acc/list.Count)
		  
		  return cText12(v)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pSum() As Text
		  dim acc, v as double
		  
		  acc = 0
		  for each t as Text in list
		    v = val(t)
		    acc = acc + v
		  next
		  
		  return cText12(acc)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pSum4() As Text
		  dim acc, v as double
		  
		  acc = 0
		  for each t as Text in list
		    v = val(t)
		    acc = acc + v
		  next
		  
		  return cText12(acc)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Reduce() As Text
		  select case method
		  case "count"
		    return pCount
		  case "sum"
		    return pSum
		  case "avg"
		    return pAvg
		  case "min"
		    return pMin
		  case "max"
		    return pMax
		  case "median"
		    return pMedian
		  case "mins"
		    return pMins
		  case "maxs"
		    return pMaxs
		  case "medians"
		    return pMedians
		  case "stddev"
		    return pStdDev
		  case "concat"
		    return pConcat
		  case "custom"
		    return pCustom
		  end
		  
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		functions As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h21
		Private label As text
	#tag EndProperty

	#tag Property, Flags = &h21
		Private list() As Text
	#tag EndProperty

	#tag Property, Flags = &h21
		Private method As Text
	#tag EndProperty

	#tag Property, Flags = &h21
		Private offset As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		Private source As Text
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
