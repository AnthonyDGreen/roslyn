' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices
Imports CoreInternalSyntax = Microsoft.CodeAnalysis.Syntax.InternalSyntax
Imports InternalSyntaxFactory = Microsoft.CodeAnalysis.VisualBasic.Syntax.InternalSyntax.SyntaxFactory

'
'============ Methods for parsing portions of executable statements ==
'

Namespace Microsoft.CodeAnalysis.VisualBasic.Syntax.InternalSyntax

    Partial Friend Class Parser

        Private Function ParseJsonObject(
                             Optional openBrace As PunctuationSyntax = Nothing,
                             Optional expressions As CoreInternalSyntax.SeparatedSyntaxListBuilder(Of ExpressionSyntax) = Nothing,
                             Optional expression As ExpressionSyntax = Nothing
                         ) As JsonObjectExpressionSyntax

            If expression IsNot Nothing Then
                Debug.Assert(Not expressions.IsNull)
                GoTo AfterFirstExpression
            End If
            If openBrace IsNot Nothing Then GoTo AfterOpenBrace
            Debug.Assert(expressions.IsNull)

            openBrace = Nothing
            If Not TryGetTokenAndEatNewLine(SyntaxKind.OpenBraceToken, openBrace, createIfMissing:=True) Then
                Return SyntaxFactory.JsonObjectExpression(openBrace, Nothing, InternalSyntaxFactory.MissingPunctuation(SyntaxKind.CloseBraceToken))
            End If

AfterOpenBrace:

            Dim members As CoreInternalSyntax.SeparatedSyntaxList(Of ExpressionSyntax) = Nothing

            If CurrentToken.Kind <> SyntaxKind.CloseBraceToken Then

                expressions = _pool.AllocateSeparated(Of ExpressionSyntax)()

                Do
                    expression = ParseJsonExpression()

AfterFirstExpression:
                    If expression.ContainsDiagnostics Then
                        expression = ResyncAt(expression, SyntaxKind.CommaToken, SyntaxKind.CloseBraceToken)
                    End If

                    expressions.Add(expression)

                    Dim comma As PunctuationSyntax = Nothing
                    If TryGetTokenAndEatNewLine(SyntaxKind.CommaToken, comma) Then
                        expressions.AddSeparator(comma)
                    Else
                        Exit Do
                    End If

                Loop

                members = expressions.ToList()
                _pool.Free(expressions)

            End If

            Dim closeBrace As PunctuationSyntax = Nothing

            TryEatNewLineAndGetToken(SyntaxKind.CloseBraceToken, closeBrace, createIfMissing:=True, state:=ScannerState.Json)

            Return SyntaxFactory.JsonObjectExpression(openBrace, members, closeBrace)

        End Function

        Private Function ParseJsonNameValuePairExpression(
                             nameExpression As ExpressionSyntax
                         ) As JsonNameValuePairExpressionSyntax

            Debug.Assert(CurrentToken.Kind = SyntaxKind.ColonToken)

            Dim colonToken As PunctuationSyntax = Nothing

            nameExpression = DirectCast(RemoveTrailingColonTriviaAndConvertToColonToken(nameExpression, colonToken, Nothing), ExpressionSyntax)

            If colonToken Is Nothing Then
                ' Recovery failed.
                colonToken = InternalSyntaxFactory.MissingPunctuation(SyntaxKind.ColonToken)
            End If

            GetNextToken(ScannerState.Json)

            Dim valueExpression = ParseJsonExpression()

            Return SyntaxFactory.JsonNameValuePairExpression(nameExpression, colonToken, valueExpression)
        End Function

        Private Function ParseJsonExpression() As ExpressionSyntax

            Select Case CurrentToken.Kind
                Case SyntaxKind.OpenBraceToken
                    Return ParseJsonObject()

                Case SyntaxKind.OpenBracketToken
                    Return ParseJsonArray()

                Case SyntaxKind.IdentifierToken

                    Select Case CurrentToken.ValueText
                        Case "true", "false", "null"
                            Return ParseJsonConstantExpression()
                    End Select

            End Select

            Dim expression = ParseExpressionCore(OperatorPrecedence.PrecedenceNone)

            ' This is a name-value pair. Parse as JSON object.
            If CurrentToken.Kind = SyntaxKind.ColonToken Then
                Return ParseJsonNameValuePairExpression(expression)
            Else
                Return expression
            End If

        End Function

        Private Function ParseJsonArray() As JsonArrayExpressionSyntax

            Debug.Assert(CurrentToken.Kind = SyntaxKind.OpenBracketToken)

            Dim openBracket As PunctuationSyntax = Nothing
            If Not TryGetTokenAndEatNewLine(SyntaxKind.OpenBracketToken, openBracket, createIfMissing:=True, state:=ScannerState.Json) Then
                Return SyntaxFactory.JsonArrayExpression(openBracket, Nothing, InternalSyntaxFactory.MissingPunctuation(SyntaxKind.CloseBracketToken))
            End If

            Dim elements As CoreInternalSyntax.SeparatedSyntaxList(Of ExpressionSyntax) = Nothing

            If CurrentToken.Kind <> SyntaxKind.CloseBracketToken Then

                Dim expressions = _pool.AllocateSeparated(Of ExpressionSyntax)()

                Do
                    Dim expression = ParseJsonExpression()

                    If expression.ContainsDiagnostics Then
                        expression = ResyncAt(expression, ScannerState.Json, SyntaxKind.CommaToken, SyntaxKind.CloseBracketToken)
                    End If

                    expressions.Add(expression)

                    Dim comma As PunctuationSyntax = Nothing
                    If TryGetTokenAndEatNewLine(SyntaxKind.CommaToken, comma, state:=ScannerState.Json) Then
                        expressions.AddSeparator(comma)
                    Else
                        Exit Do
                    End If

                Loop

                elements = expressions.ToList()
                _pool.Free(expressions)

            End If

            Dim closeBracket As PunctuationSyntax = Nothing

            If CurrentToken.IsEndOfLine AndAlso PeekNextToken(ScannerState.Json).Kind = SyntaxKind.CloseBracketToken Then
                TryEatNewLine(ScannerState.Json)
            End If

            ResetCurrentToken(ScannerState.Json)

            If CurrentToken.Kind = SyntaxKind.CloseBracketToken Then
                closeBracket = DirectCast(CurrentToken, PunctuationSyntax)
                GetNextToken(ScannerState.Json)
            Else
                closeBracket = DirectCast(HandleUnexpectedToken(SyntaxKind.CloseBracketToken), PunctuationSyntax)
            End If

            Return SyntaxFactory.JsonArrayExpression(openBracket, elements, closeBracket)

        End Function

        Private Function ParseJsonConstantExpression() As JsonConstantExpressionSyntax
            Debug.Assert(CurrentToken.Kind = SyntaxKind.IdentifierToken)

            Dim valueToken = DirectCast(CurrentToken, IdentifierTokenSyntax)

            GetNextToken(ScannerState.Json)

            Return SyntaxFactory.JsonConstantExpression(valueToken)

        End Function

    End Class

End Namespace
