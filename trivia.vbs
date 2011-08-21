'Option Explicit

Script("Name") = "TJOCTriviaAnswerScript"
Script("Author") = "scottrick49"
Script("Major") = 1
Script("Minor") = 0
Script("Revision") = 0

Dim questions(2000)
Dim answers(2000)

Dim admins(3)

Dim answersFile
Dim triviaBotName
Dim triviaQuestionPrefix
Dim triviaNoAnswerPrefix
Dim triviaRealAnswerPrefix
Dim triviaRealAnswerEnd

Dim currentQuestion

Sub Event_Load()
	admins(0) = "RUSSIA2020AD"
	admins(1) = "scottrick49"
	admins(2) = "regularblack"
	
	answersFile = "TJOC_Answers.txt"
	triviaBotName = "TJOC.Trivia.Inf"
	triviaQuestionPrefix = "(TJOC trivia.txt) $1.00:"
	triviaNoAnswerPrefix = "The answer(s):"
	triviaRealAnswerPrefix = "("
	triviaRealAnswerEnd = ")Well done."
	call ReadTriviaFile
End Sub

Sub Event_Close()
	call WriteTriviaFile
End Sub

Sub Event_LoggedOn(Username, Product)
    AddChat vbWhite, "Loaded FatBot" & Chr(153)
End Sub

Sub Event_UserTalk(Username, Flags, Message, Ping)
	For Each item in admins
		If StrComp(UCase(Username), UCase(item)) = 0 Then
			If StrComp(UCase(Message), UCase("!info")) = 0 Then
				call PrintInfo
			End If
			
			Exit Sub
		End If
	Next
End Sub

Sub Event_UserEmote(Username, Flags, Message)
	Message = Trim(Message)
	
	If StrComp(Username, triviaBotName) Then
		questionPrefix = Left(Message, Len(triviaQuestionPrefix))
		
		'First check to see if this message was a question
		If StrComp(questionPrefix, triviaQuestionPrefix) = 0 Then
			question = Mid(Message, Len(triviaQuestionPrefix) + 1)
			question = Trim(question)
			currentQuestion = question
			
			answerIndex = FindAnswer(question)
			
			If answerIndex >= 0 Then
				AddQ answers(answerIndex)
			Else
				AddChat vbRed, "No answer"
			End If
		Else
			'Not a question, so see if its an answer
			
			'No answer variables
			noAnswerPrefix = Left(Message, Len(triviaNoAnswerPrefix))
			
			'Real answer variables
			realAnswerPrefixLength = Len(triviaRealAnswerPrefix)
			realAnswerPrefix = Left(Message, realAnswerPrefixLength)
			
			If StrComp(noAnswerPrefix, triviaNoAnswerPrefix) = 0 Then
				answer = Mid(Message, Len(triviaNoAnswerPrefix) + 1)
				answer = Trim(answer)
				AddAnswer(answer)
				
			ElseIf StrComp(realAnswerPrefix, triviaRealAnswerPrefix) = 0 Then
				
				endAnswer = Mid(triviaRealAnswerEnd, 1, 1)
				endAnswerIndex = InStr(Message, endAnswer)
				afterAnswer = Mid(Message, endAnswerIndex, Len(triviaRealAnswerEnd))
				
				'AddChat vbWhite, "endAnswer='" + endAnswer + "'  index=" & endAnswerIndex & "  afterAnswer='" + afterAnswer +"'"
				
				If StrComp(afterAnswer, triviaRealAnswerEnd) = 0 Then
					answer = Mid(Message, realAnswerPrefixLength + 1, endAnswerIndex - realAnswerPrefixLength - 1)
					answer = Trim(answer)
					AddAnswer(answer)
				Else
					AddChat &Hdd44aa, "  --> started with real answer prefix, but the part after didn't work :("
				End If
			End If
		End If
	End If
End Sub

Sub AddAnswer(answer)
	If Len(currentQuestion) <= 0 Then
		AddChat &HFF7F00, "Not cataloging, no question!"
		Exit Sub
	End If
	
	AddChat &HFF7F00, "Cataloging [" & currentQuestion & "], [" & answer & "]"
	
	index = 0
	
	For Each item In questions
		If StrComp(currentQuestion, item) = 0 Then
			answers(index) = answer
			Exit Sub
		ElseIf Len(item) <= 0 Then
			questions(index) = currentQuestion
			answers(index) = answer
			Exit Sub
		End If
		
		index = index + 1
	Next
End Sub

Function FindAnswer(question)
	index = 0
	
	For Each item In questions
		If StrComp(question, item) = 0 Then
			FindAnswer = index
			AddChat &H7D26CD, " --> Found answer at index " & index
			Exit Function
		End If
		
		index = index + 1
	Next

	FindAnswer = -1
End Function

Sub PrintInfo
	count = 0
	For Each item in questions
		If Len(item) <= 0 Then Exit For
	
		count = count + 1
	Next
	
	AddQ "FatBot" & Chr(153) & " --- " & count & " questions cataloged."
End Sub

Sub ReadTriviaFile
	AddChat vbYellow, "ReadTriviaFile"
	
	Set fso = CreateObject("Scripting.FileSystemObject")
	Set file = fso.OpenTextFile(answersFile, 1)
	index = 0

	' Read from the file and display the results.
	Do While file.AtEndOfStream <> True
		questions(index) = file.ReadLine
		answers(index) = file.ReadLine
		'AddChat vbYellow, "Reading [" & questions(index) & "] [" & answers(index) & "]"
		index = index + 1
	Loop
	
	file.Close
End Sub

Sub WriteTriviaFile
	AddChat vbYellow, "WriteTriviaFile"

	Set fso = CreateObject("Scripting.FileSystemObject")
	Set file = fso.CreateTextFile(answersFile, True)
	index = 0
	
	For Each item In questions
		If Len(item) <= 0 Then
			Exit Sub
		End If
	
		'AddChat vbYellow, "Writing [" & questions(index) & "] [" & answers(index) & "]"
		file.WriteLine(questions(index))
		file.WriteLine(answers(index))
		
		index = index + 1
	Next
	
	file.Close
End Sub