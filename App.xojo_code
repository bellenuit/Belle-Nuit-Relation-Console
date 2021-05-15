#tag Class
Protected Class App
Inherits ConsoleApplication
	#tag Event
		Function Run(args() as String) As Integer
		  dim state, t, ch,result,s as text
		  dim filename as text
		  dim file as FolderItem
		  dim tip as TextInputstream
		  dim lh as LineHandler
		  dim history(-1) as text
		  dim lines(-1) as text
		  dim found, done as boolean
		  dim dict as new Dictionary
		  
		  StdOut.WriteLine("Belle Nuit Relation console")
		  
		  lh = new LineHandler("TEXT")
		  
		  // possible
		  // -i run interactively
		  // -h or empty usage 
		  // or read the entire line and convert -<word> to <newline><word>
		  
		  dim path as text
		  file =  SpecialFolder.CurrentWorkingDirectory
		  lh.currentpath = file.URLPath.ToText
		  
		  dim args2() as text
		  dim i as Int32
		  path = args(0).ToText
		  
		  if ubound(args)=0 then
		    state = "-i"
		    lh.interactive = true
		    'StdOut.WriteLine(usage)
		    'return 0
		  else
		    select case args(1)
		    case "-h"
		      state = "-h"
		      lh.interactive = false
		      StdOut.WriteLine(usage)
		      return 0
		    case "-i"
		      state = "-i"
		      lh.interactive = true
		      
		    else
		      state = "-b"
		      lh.interactive = false
		      'redim args2(ubound(args)-1)
		      'for i = 0 to UBound(args2)
		      'if args(i+1).IndexOf(" ")>-1 then
		      's = args(i+1).ToText
		      'args2(i) = """"+s+""""
		      'else
		      'args2(i) = args(i+1).ToText
		      'end
		      'next
		      't = text.Join(args2," ")
		      'StdOut.WriteLine(system.CommandLine)
		      t = system.CommandLine.totext.mid(path.Length)
		      var rg as new regex
		      
		      rg.SearchPattern=" \["
		      rg.ReplacementPattern = " """
		      rg.Options.ReplaceAllMatches = True
		      t = rg.Replace(t).totext
		      t = t + " "
		      t = rg.Replace(t).totext
		      rg.SearchPattern="] "
		      rg.ReplacementPattern = """ "
		      rg.Options.ReplaceAllMatches = True
		      t = rg.Replace(t).totext
		      rg.SearchPattern=" -(\w)"
		      rg.ReplacementPattern = EndOfLine+"$1"
		      rg.Options.ReplaceAllMatches = True
		      t = rg.Replace(t).totext
		      
		      StdOut.WriteLine(t)
		      
		      result = lh.run(t,"","",dict)
		      
		      StdOut.WriteLine(result)
		      return 0
		    end select
		  end if
		  
		  'for each arg as string in args
		  'select case arg
		  'case "-f"
		  'state = "f"
		  'else
		  'if state = "f" then
		  'filename = arg.ToText
		  'elseif left(arg,1) = "-" then
		  'state = arg.ToText.mid(1)
		  'elseif state <> "" then
		  'dict.Value(state) = arg.ToText
		  'end
		  'end
		  'next
		  
		  
		  
		  'if filename<>"" then
		  if lh.interactive = false then
		    'lh.interactive = false
		    'file = SpecialFolder.CurrentWorkingDirectory.Child(filename)
		    'if file.exists then
		    'tip = TextInputStream.Open(file)
		    'tip.Encoding = Encodings.UTF8
		    't = tip.ReadAll.ToText
		    'result = lh.run(t,"","",dict)
		    
		    'StdOut.WriteLine(result)
		    'else
		    'StdOut.WriteLine("Error: file does not exist")
		    'end
		  else
		    lh.interactive = true
		    // interactive 
		    // hack to get lh first run
		    result =  lh.run(t,"","",dict)
		    t ="_int"
		    lh.offsets.value(t) = 0
		    while not done
		      StdOut.Write("> ")
		      t = ""
		      found = false
		      'do
		      'ch = stdin.read(1).ToText
		      'select case ch
		      'case chr(38) // up
		      'StdOut.Write  chr(27).ToText+"\e[3~"
		      'StdOut.Write history(ubound(history))
		      '
		      'case chr(40) // down
		      'StdOut.Write chr(27).ToText+"\e[3~"
		      'case EndOfLine
		      'found = true
		      'else
		      't = t + ch
		      'end
		      '
		      'loop until found
		      
		      t = input.ToText
		      
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
		        result = lh.run(t,"_int","",dict)
		        if left(t,4)="echo" and result.right(2) =text.EndOfLine+text.EndOfLine then
		          result = result.mid(0,result.length-1)
		        end
		        if result.length>9 and result.Left(9) = "{{error}}" then
		          result = chr(27).totext+"[1;31m" + result.mid(9)+chr(27).totext+"[0m"
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


	#tag Method, Flags = &h0
		Function Usage() As text
		  dim s as string
		  s = "Usage"+EndOfLine+ _
		  "-h help"+EndOfLine+ _
		  "-i interactive"+EndOfLine+_
		  "-<instruction> <options> [-<instruction> <options>...]"
		  
		  return s.totext
		  
		End Function
	#tag EndMethod


	#tag ViewBehavior
	#tag EndViewBehavior
End Class
#tag EndClass
