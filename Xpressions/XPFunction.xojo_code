#tag Class
Protected Class XPFunction
	#tag Method, Flags = &h1
		Protected Function cText(d as Double) As text
		  // double is guranteed 15 decimal digits
		  // show 15 first digits and cut 0 at the end
		  // move to scientific only if the number cannot be represented (bigger > 10^15, smaller 10^15)
		  
		  dim a as double
		  dim t as text
		  dim s as string
		  
		  a = abs(d)
		  
		  
		  
		  if a > 10e14 then
		    return format(d,"-0.00000000000000e").ToText
		  elseif a < 10e-300 then
		    return "0"
		  elseif a < 10e-14 then
		    return format(d,"-0.00000000000000e").ToText
		  end
		  
		  s = format(d,"-0.##############")
		  if len(s)>15 then
		    s = format(round(d*10000000000000000)/10000000000000000,"-0.##############")
		  end
		  if right(s,1)="." then
		    s = mid(s,1,len(s)-1)
		  end
		  return s.ToText
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub run(byref stack() as text)
		  prun(stack)
		End Sub
	#tag EndMethod


	#tag Hook, Flags = &h0
		Event pRun(byref stack() as text)
	#tag EndHook


	#tag Property, Flags = &h0
		arity As int64
	#tag EndProperty

	#tag Property, Flags = &h0
		functions() As XPFunction
	#tag EndProperty

	#tag Property, Flags = &h0
		label As text
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
			Name="label"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="text"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="arity"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="int64"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
