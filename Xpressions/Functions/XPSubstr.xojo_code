#tag Class
Protected Class XPSubstr
Inherits XPFunction
	#tag Event
		Sub pRun(byref stack() as text)
		  dim a, b, c as text
		  dim bi, ci, li as integer
		  
		  if ubound(stack)<2 then
		    raise new xperror("stack <3",103)
		  end
		  
		  ci = val(stack.Pop)
		  bi = val(stack.Pop)-1
		  a = stack.Pop
		  
		  li = a.Length
		  
		  if bi>li then
		    stack.AddRow ""
		    return
		  end
		  if bi=-1 then
		    stack.AddRow ""
		    return
		  end
		  if bi < 0 then
		    bi = li+bi+1
		    if bi<0 then
		      ci = ci+bi
		      bi=0
		      if ci<1 then
		        stack.AddRow ""
		        return
		      end
		    end
		  end
		  
		  if bi+ci>li then
		    ci = li-bi
		  end
		  
		  stack.AddRow a.mid(bi,ci)
		  
		  
		  Exception e as OutOfBoundsException
		    stack.AddRow ""
		    
		    
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub Constructor()
		  arity = 3
		  label = ":substr"
		End Sub
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
End Class
#tag EndClass
