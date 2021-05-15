#tag Class
Protected Class XPNumberFormat
Inherits XPFunction
	#tag Event
		Sub pRun(byref stack() as text)
		  dim a, decimals, decpoint, thousandssep as text
		  dim f as double
		  dim neg as text
		  dim mantissa, intpart, s, intpart2 as text
		  dim chunks(-1) as string
		  dim i, c as integer
		  
		  if ubound(stack)<3 then
		    raise new xperror("stack <4",102)
		  end
		  
		  thousandssep = stack.pop
		  decpoint = stack.pop
		  decimals = stack.pop
		  a = stack.pop
		  
		  f = val(a)
		  
		  neg = ""
		  if f < 0 then
		    neg = "-"
		    f = -f
		  end
		  
		  
		  mantissa = StringUtils.sprintf("%0."+decimals+"f", f).totext
		  mantissa = replace(mantissa,".",decpoint).totext
		  
		  intpart = StringUtils.sprintf("%1d", f).totext
		  
		  if intpart.length > 12 then
		    intpart = intpart.mid(0,intpart.length-12) + thousandssep +  intpart.mid(intpart.length-12,3) + thousandssep +  intpart.mid(intpart.length-9,3) + thousandssep +  intpart.mid(intpart.length-6,3) + thousandssep + intpart.right(3)
		  elseif intpart.length > 9 then
		    intpart = intpart.mid(0,intpart.length-9) + thousandssep +  intpart.mid(intpart.length-9,3) + thousandssep +  intpart.mid(intpart.length-6,3) + thousandssep + intpart.right(3)
		  elseif intpart.length > 6 then
		    intpart = intpart.mid(0,intpart.length-6) + thousandssep +  intpart.mid(intpart.length-6,3) + thousandssep + intpart.right(3)
		  elseif intpart.length > 3 then
		    intpart = intpart.mid(0,intpart.length-3) + thousandssep + intpart.right(3)
		  end
		  
		  
		  s = neg+intpart+mantissa
		  
		  stack.AddRow s
		  
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor()
		  arity = 4
		  label = ":numberformat"
		End Sub
	#tag EndMethod


	#tag ViewBehavior
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
