' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

'-----------------------------------------------------------------------------
' Contains the definition of the Scanner, which produces tokens from text 
'-----------------------------------------------------------------------------

Imports Microsoft.CodeAnalysis.VisualBasic.SyntaxFacts
Imports CoreInternalSyntax = Microsoft.CodeAnalysis.Syntax.InternalSyntax

Namespace Microsoft.CodeAnalysis.VisualBasic.Syntax.InternalSyntax
    Partial Friend Class Scanner

        Private Function ScanJson() As SyntaxToken
            If Not CanGet() Then
                Return MakeEofToken()
            End If

            Dim kind As SyntaxKind

            Dim leadingTriviaLength = GetWhitespaceLength(0)
            Dim offset = leadingTriviaLength
            Dim length As Integer

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

            Dim leadingTrivia = ScanWhitespace(leadingTriviaLength)

            Dim text = GetText(length)

            Dim trailingTrivia As CoreInternalSyntax.SyntaxList(Of VisualBasicSyntaxNode) = ScanSingleLineTrivia()

            Return MakePunctuationToken(kind, text, leadingTrivia, trailingTrivia.Node)
        End Function

    End Class
End Namespace
