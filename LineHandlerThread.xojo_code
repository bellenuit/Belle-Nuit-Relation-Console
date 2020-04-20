#tag Class
Protected Class LineHandlerThread
Inherits thread
	#tag Method, Flags = &h0
		Sub Progress(i as integer)
		  dim d as new Dictionary
		  
		  d.Value("value") = i
		  
		  AddUserInterfaceUpdate(d)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub RelationProgress(i as integer)
		  dim d as new Dictionary
		  
		  d.Value("relationvalue") = i
		  
		  AddUserInterfaceUpdate(d)
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		lh As LineHandler
	#tag EndProperty

	#tag Property, Flags = &h0
		starttime As double
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
			InitialValue=""
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
			Name="Priority"
			Visible=true
			Group="Behavior"
			InitialValue="5"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="StackSize"
			Visible=true
			Group="Behavior"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="starttime"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="double"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
