' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports System.Collections.Generic
Imports System.Collections.Immutable
Imports System.Runtime.InteropServices
Imports Microsoft.CodeAnalysis.Collections
Imports Microsoft.CodeAnalysis.VisualBasic.Symbols
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace Microsoft.CodeAnalysis.VisualBasic
    Partial Friend Class Binder

        Private Function TryLookupTypeBySimpleName(name As String, arity As Integer, diagnostics As DiagnosticBag) As TypeSymbol

            Dim result = LookupResult.GetInstance()

            Dim useSiteDiagnostics As HashSet(Of DiagnosticInfo) = Nothing
            Lookup(result, name, arity, LookupOptions.NamespacesOrTypesOnly, useSiteDiagnostics:=Nothing)

            Dim type As TypeSymbol
            If result.IsGood AndAlso result.HasSingleSymbol AndAlso result.SingleSymbol.UnwrapAlias().Kind = SymbolKind.NamedType Then
                type = DirectCast(result.SingleSymbol.UnwrapAlias(), NamedTypeSymbol)
            Else
                type = DirectCast(Compilation.CreateErrorTypeSymbol(Compilation.GlobalNamespace, name, arity), TypeSymbol)
            End If

            result.Free()
            Return type

        End Function

        Private Function BindJsonObjectExpression(syntax As JsonObjectExpressionSyntax, diagnostics As DiagnosticBag) As BoundExpression

            Dim jsonObjectType = TryLookupTypeBySimpleName("JObject", 0, diagnostics)

            Dim membersBuilder = ArrayBuilder(Of BoundNode).GetInstance()

            For Each member In syntax.Members
                membersBuilder.Add(BindValue(member, diagnostics))
                'If member.Kind = SyntaxKind.JsonNameValuePairExpression Then
                '    membersBuilder.Add(BindJsonNameValuePairExpression(DirectCast(member, JsonNameValuePairExpressionSyntax), diagnostics))
                'Else
                '    membersBuilder.Add(BindValue(member, diagnostics))
                'End If
            Next

            Return New BoundJsonObject(syntax, membersBuilder.ToImmutableAndFree(), Me, jsonObjectType)

        End Function

        Private Function BindJsonArrayExpression(syntax As JsonArrayExpressionSyntax, diagnostics As DiagnosticBag) As BoundExpression

            Dim jsonArrayType = TryLookupTypeBySimpleName("JArray", 0, diagnostics)

            Dim elementsBuilder = ArrayBuilder(Of BoundNode).GetInstance()

            For Each element In syntax.Elements
                elementsBuilder.Add(BindValue(element, diagnostics))
            Next

            Return New BoundJsonArray(syntax, elementsBuilder.ToImmutableAndFree(), Me, jsonArrayType)

        End Function

        Private Function BindJsonNameValuePairExpression(syntax As JsonNameValuePairExpressionSyntax, diagnostics As DiagnosticBag) As BoundExpression

            Dim jsonPairType = TryLookupTypeBySimpleName("JPair", 0, diagnostics)

            Dim boundName = BindValue(syntax.NameExpression, diagnostics)

            Dim boundValue = BindValue(syntax.ValueExpression, diagnostics)

            Return New BoundJsonNameValuePair(syntax, boundName, boundValue, Me, jsonPairType)

        End Function

        Private Function BindJsonConstantExpression(syntax As JsonConstantExpressionSyntax, diagnostics As DiagnosticBag) As BoundExpression

            Dim boundValue As BoundExpression

            Select Case syntax.ValueToken.ValueText
                Case "true"
                    boundValue = New BoundLiteral(syntax, ConstantValue.True, Compilation.GetSpecialType(SpecialType.System_Boolean))

                Case "false"
                    boundValue = New BoundLiteral(syntax, ConstantValue.False, Compilation.GetSpecialType(SpecialType.System_Boolean))

                Case "null"
                    boundValue = New BoundLiteral(syntax, ConstantValue.Null, type:=Nothing)

                Case Else
                    Throw ExceptionUtilities.Unreachable

            End Select

            Return boundValue

            'Return New BoundJsonConstant(syntax, boundValue, Me, boundValue.Type)

        End Function

    End Class
End Namespace