' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

'-----------------------------------------------------------------------------
' Contains the definition of the Scanner, which produces tokens from text 
'-----------------------------------------------------------------------------

Imports Microsoft.CodeAnalysis.VisualBasic.SyntaxFacts
Imports CoreInternalSyntax = Microsoft.CodeAnalysis.Syntax.InternalSyntax

Namespace Microsoft.CodeAnalysis.VisualBasic.Syntax.InternalSyntax
    Partial Friend Class Scanner

        Private Function ScanJson(Optional leadingTrivia As CoreInternalSyntax.SyntaxList(Of VisualBasicSyntaxNode) = Nothing) As SyntaxToken
            If Not CanGet() Then
                Return MakeEofToken()
            End If

            Dim kind As SyntaxKind

            Dim leadingTriviaLength As Integer
            Dim offset As Integer
            Dim length As Integer

            If leadingTrivia.Node Is Nothing Then
                leadingTriviaLength = GetWhitespaceLength(0)
                offset = leadingTriviaLength
            Else
                offset = 0
            End If

            If Not CanGet(offset) Then
                Return MakeEofToken()
            End If

            Dim c = Peek(offset)

            Select Case c
                Case "["c
                    kind = SyntaxKind.OpenBracketToken
                    length = 1
                Case "]"c
                    kind = SyntaxKind.CloseBracketToken
                    length = 1
                Case """"c
                    Return ScanJsonString(leadingTriviaLength)
                Case Else
                    Dim token = ScanNextToken(allowLeadingMultilineTrivia:=False)

                    ' For the purposes of this prototype these keywords aren't considered true keywords.
                    ' At the moment this is mostly to stop the pretty-lister from case-correcting them.
                    If (token.Kind = SyntaxKind.TrueKeyword AndAlso token.ValueText = "true") OrElse
                       (token.Kind = SyntaxKind.FalseKeyword AndAlso token.ValueText = "false") _
                    Then
                        token = MakeIdentifier(DirectCast(token, KeywordSyntax))
                    End If

                    Return token
            End Select

            If leadingTrivia.Node Is Nothing Then
                leadingTrivia = ScanWhitespace(leadingTriviaLength)
            End If

            Dim text = GetText(length)

            Dim trailingTrivia As CoreInternalSyntax.SyntaxList(Of VisualBasicSyntaxNode) = ScanSingleLineTrivia()

            Return MakePunctuationToken(kind, text, leadingTrivia, trailingTrivia.Node)
        End Function

        Private Function ScanJsonString(
                             leadingTriviaLength As Integer
                         ) As SyntaxToken

            Dim leadingTrivia = ScanWhitespace(leadingTriviaLength)

            Debug.Assert(CanGet() AndAlso Peek() = """"c)

            ' Skip the opening quote.
            Dim length = 1

            ' Compute the value of the string, accounting for escape sequences, e.g. \r, \u1234.
            Dim valueBuffer As System.Text.StringBuilder = GetScratch()

            Dim isNotTerminated = False,
                hasInvalidEscape = False,
                hasInvalidHexDigit = False

            Do While CanGet(length)
                Dim c = Peek(length)

                Select Case c
                    Case """"c
                        ' Terminate string.
                        length += 1
                        Exit Do

                    Case "\"c
                        ' Escaped characters.
                        If Not CanGet(length + 1) Then
                            hasInvalidEscape = True
                            Exit Do
                        End If

                        Select Case Peek(length + 1)
                            Case """"c, "\"c, "/"c
                                valueBuffer.Append(Peek(length + 1))

                            Case "b"c
                                valueBuffer.Append(ChrW(8))

                            Case "f"c
                                valueBuffer.Append(ChrW(12))

                            Case "n"c
                                valueBuffer.Append(ChrW(10))

                            Case "r"c
                                valueBuffer.Append(ChrW(13))

                            Case "t"c
                                valueBuffer.Append(ChrW(9))

                            Case "u"c
                                Dim charCode = ""

                                For i = 2 To 5
                                    Dim digit = GetChar(length + i)

                                    Select Case digit
                                        Case "0"c To "9"c, "A"c To "F"c, "a"c To "f"c
                                            charCode &= digit
                                        Case Else
                                            hasInvalidHexDigit = True
                                            ' Consume all digits so far.
                                            length += i
                                            Exit For
                                    End Select
                                Next

                                If Not hasInvalidHexDigit Then
                                    valueBuffer.Append(ChrW(CInt("&H" & charCode)))
                                    length += 4
                                End If
                            Case Else
                                hasInvalidEscape = True
                        End Select

                        length += 2

                    Case ChrW(0) To ChrW(31), ChrW(127)
                        ' Control characters are disallowed.
                        isNotTerminated = True
                        Exit Do

                    Case Else
                        ' Legal character.

                        valueBuffer.Append(c)
                        length += 1
                End Select
            Loop

            Dim text = GetText(length)

            Dim trailingTrivia As CoreInternalSyntax.SyntaxList(Of VisualBasicSyntaxNode) = ScanSingleLineTrivia()

            Dim result = SyntaxFactory.StringLiteralToken(text, GetScratchText(valueBuffer), leadingTrivia, trailingTrivia.Node)

            ' TODO: Rename SetDiagnostics to WithDiagnostics as it's not a mutating method.
            If isNotTerminated Then
                result = CType(result.SetDiagnostics({ErrorFactory.ErrorInfo(ERRID.ERR_UnterminatedStringLiteral)}), StringLiteralTokenSyntax)
            ElseIf hasInvalidEscape Then
                result = CType(result.SetDiagnostics({ErrorFactory.ErrorInfo(ERRID.ERR_InvalidEscapeSequenceInJsonString)}), StringLiteralTokenSyntax)
            ElseIf hasInvalidHexDigit Then
                result = CType(result.SetDiagnostics({ErrorFactory.ErrorInfo(ERRID.ERR_HexDigitExpected)}), StringLiteralTokenSyntax)
            End If

            Return result
        End Function

    End Class
End Namespace
