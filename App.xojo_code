#tag Class
Protected Class App
Inherits ConsoleApplication
	#tag Event
		Function Run(args() as String) As Integer
		  dim state, t, ch,result as text
		  dim filename as text
		  dim file as FolderItem
		  dim tip as TextInputstream
		  dim lh as LineHandler
		  dim history(-1) as text
		  dim lines(-1) as text
		  dim found, done as boolean
		  
		  StdOut.WriteLine("Belle Nuit Relation console")
		  
		  lh = new LineHandler("TEXT")
		  
		  for each arg as string in args
		    select case arg
		    case "-f"
		      state = "f"
		    else
		      if state = "f" then
		        filename = arg.ToText
		      elseif left(arg,1) = "-" then
		        state = arg.ToText.mid(1)
		      elseif state <> "" then
		        lh.globals.Value(state) = arg.ToText
		      end
		    end
		  next
		  
		  file =  SpecialFolder.CurrentWorkingDirectory
		  lh.currentpath = file.URLPath.ToText
		  
		  if filename<>"" then
		    lh.interactive = false
		    file = SpecialFolder.CurrentWorkingDirectory.Child(filename)
		    if file.exists then
		      tip = TextInputStream.Open(file)
		      tip.Encoding = Encodings.UTF8
		      t = tip.ReadAll.ToText
		      result = lh.run(t)
		      
		      StdOut.WriteLine(result)
		    else
		      StdOut.WriteLine("Error: file does not exist")
		    end
		  else
		    lh.interactive = true
		    // interactive 
		    // hack to get lh first run
		    result = lh.run(t)
		    t ="_int"
		    lh.offsets.value(t) = 0
		    while not done
		      StdOut.Write("> ")
		      t = ""
		      found = false
		      do
		        t = t + Input.ToText
		        if t="" then 
		          found = true
		        elseif right(t,1) <> "_" then
		          found = true
		          
		        else
		          t = t.mid(0,t.Length-1)
		        end
		        
		      loop until found
		      
		      if t="q" or t="quit" then 
		        done = true
		      elseif t="history" then
		        StdOut.Write(text.join(history,text.EndOfLine)+text.EndOfLine)
		      elseif t="" then
		        // nothing
		      else
		        history.AddRow t
		        t = t.ReplaceAll(" ; ",text.EndOfLine)
		        lh.Result=""
		        result = lh.run(t,"_int")
		        if left(t,4)="echo" and result.right(2) =text.EndOfLine+text.EndOfLine then
		          result = result.mid(0,result.length-1)
		        end
		        StdOut.Write(result)
		      end
		    wend
		  end
		  
		  
		  
		End Function
	#tag EndEvent

	#tag Event
		Function UnhandledException(error As RuntimeException) As Boolean
		  
		End Function
	#tag EndEvent


	#tag ViewBehavior
	#tag EndViewBehavior
End Class
#tag EndClass
