REM  *****  BASIC  *****

Dim Interactive As Boolean
Dim WaitFor

Function Convert(Optional inFileURL, Optional filterSpec, Optional outFileURL)
	Dim inDoc, inDocType, openParams, closeInDoc, presentationDoc

	' Set Interactivity i.e., LogMessage pops up a message.
	Interactive = False

	WaitFor = 10

	' Init dependencies
	BasicLibraries.LoadLibrary("Tools")
	' BasicLibraries.LoadLibrary("XrayTool")

	' Setup Export filters
 	InitExportFilters

	' Export to doc format by default
 	If IsMissing(filterSpec) Then
		If Interactive Then
			filterSpec = InputBox("Export to: ")
		Else
			filterSpec = "doc"
		End If
	End If
	filterSpec = Trim(filterSpec)

	closeInDoc = False
	If IsMissing(inFileURL) Then
		' Most likely, the Macro is run interactively. Act on
		' the current document
 		If Not ThisComponent.HasLocation() Then
			LogMessage("Document doesn't have a location")
			Goto Failure
		End If

		inDoc = ThisComponent
		inFileURL = inDoc.GetLocation()
		closeInDoc = False

	Else
		' Load the document
		On Error Goto Failure
		openParams = Array(MakePropertyValue("Hidden", True),MakePropertyValue("ReadOnly", True),)

		'openParams = Array()
		inDoc = StarDesktop.loadComponentFromURL(inFileURL, "_blank", 0, OpenParams())
		closeInDoc = True
	End If

	If IsMissing(outFileURL) Then
		outFileURL = GetURLWithoutExtension(inFileURL)
	End If

	If ExportDocument(inDoc, filterSpec, outFileURL) Then
		Goto Success
	End If

	LogMessage("filterSpec1 is " & filterSpec)

	' Export didn't go through. Maybe didn't find a valid filter.

	' Check whether the request is to convert a Text or a Web
	' Document to a Presentation Document

	inDocType = GetDocumentType(inDoc)
	If (inDocType = "com.sun.star.text.TextDocument" Or _
	    inDocType = "com.sun.star.text.WebDocument") Then
		LogMessage("Filterspec2 is " & filterSpec)
		filter = GetFilter("com.sun.star.presentation.PresentationDocument", filterSpec)
		If IsNull(filter) Then
			LogMessage("We tried our best. Nothing more to do"
			Goto Failure
		Else
			LogMessage("Trying to create presentation document. Found valid filter for " & filterSpec)
		End If
	Else
		Goto Failure
	End If

	' Export Outline to Presentation
	dispatcher = createUnoService("com.sun.star.frame.DispatchHelper")
	dispatcher.executeDispatch(inDoc.CurrentController.Frame, ".uno:SendOutlineToStarImpress", "", 0, Array())

	' Dispatch event above is aynchronous. Wait for a few seconds for the above event to finish
	Wait(WaitFor * 1000)

	' After the dispatch, the current component is a presentation
	' document. Note that it doesn't have a location

	presentationDoc = ThisComponent
	If IsNull(ExportDocument(presentationDoc, filter, outFileURL)) Then
		Goto Failure
	Else
		presentationDoc.Close(True)
	End If

Success:
	LogMessage("Successfully exported to " & outFileURL )
	Goto Done

Failure:
	LogMessage("Export failed " & outFileURL )
	Goto Done

Done:
	If closeInDoc Then
		inDoc.Close(True)
	End If
End Function

' http://codesnippets.services.openoffice.org/Writer/Writer.MergeDocs.snip
' http://user.services.openoffice.org/en/forum/viewtopic.php?f=20&t=39983
' http://user.services.openoffice.org/en/forum/viewtopic.php?f=21&t=23531

' http://wiki.services.openoffice.org/wiki/Documentation/BASIC_Guide/Files_and_Directories_%28Runtime_Library%29


Function ExportDocument(inputDoc, filterSpec, outFileURL) As Boolean
	Dim inputDocType, filter
	ExportDocument = False

	On Error Goto Failure
	inputDocType = GetDocumentType(inputDoc)

	If IsArray(filterSpec) Then
		' Filter is fully specified
		filter = filterSpec
	Else
		' Filter is specified by it's name
		filter = GetFilter(inputDocType, filterSpec)
	End If

	If InStr(outFileURL, ".") = 0 Then
		outFileURL = outFileURL & "." & FilterSaveExtension(filter)
	End If

	LogMessage("outFileURL is " & outFileURL)

	inputDoc.storeToUrl(outFileURL, Array(MakePropertyValue("FilterName", FilterHandler(filter))))

 	ExportDocument = True
	LogMessage("Export to " & outFileURL & " succeeded")
Done:
 	Exit Function

Failure:
	LogMessage("Export to " & outFileURL & " failed")
	Resume Done
End Function


Function GetURLWithoutExtension(s As String)
	Dim pos
	pos = Instr(s, ".")
	If pos = 0 Then
		GetURLWithoutExtension = s
	Else
		GetURLWithoutExtension = Left(s, pos - 1)
	End If
End Function

Function GetDocumentType(oDoc)
	For Each docType in DocTypes
		If (oDoc.supportsService(docType)) Then
			GetDocumentType = docType
			Exit Function
		End If
	Next docType
	GetDocumentType = Nothing
End Function

Function MakePropertyValue(Optional sName As String, Optional sValue) As com.sun.star.beans.PropertyValue
	Dim oPropertyValue As New com.sun.star.beans.PropertyValue

 	If Not IsMissing(sName) Then
		oPropertyValue.Name = sName
	EndIf

	If Not IsMissing(sValue) Then
		oPropertyValue.Value = sValue
	EndIf

	MakePropertyValue() = oPropertyValue

End Function


Sub LogMessage(message)
	If Interactive Then
		If Err <> 0 Then
			Print "Error " & Err & ": " & Error$ & " (line : " & Erl & ")"
		End If
		Print message
	End If
End Sub


