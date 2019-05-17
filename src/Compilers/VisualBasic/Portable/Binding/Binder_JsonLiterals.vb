' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports System.Collections.Generic
Imports System.Collections.Immutable
Imports System.Runtime.InteropServices
Imports Microsoft.CodeAnalysis.InternalUtilities
Imports Microsoft.CodeAnalysis.Collections
Imports Microsoft.CodeAnalysis.VisualBasic.Symbols
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Microsoft.CodeAnalysis.PooledObjects

Namespace Microsoft.CodeAnalysis.VisualBasic
    Partial Friend Class Binder

        Private Function BindJsonObjectExpression(syntax As JsonObjectExpressionSyntax, diagnostics As DiagnosticBag) As BoundExpression

            diagnostics = CheckJsonFeaturesAllowed(syntax, diagnostics)

            Dim jsonObjectType = GetWellKnownType(WellKnownType.Newtonsoft_Json_Linq_JObject, syntax, diagnostics)

            Dim membersBuilder = ArrayBuilder(Of BoundNode).GetInstance()

            For Each member In syntax.Members
                membersBuilder.Add(BindValue(member, diagnostics))
            Next

            Return New BoundJsonObject(syntax, membersBuilder.ToImmutableAndFree(), Me, jsonObjectType)

        End Function

        Private Function BindJsonArrayExpression(syntax As JsonArrayExpressionSyntax, diagnostics As DiagnosticBag) As BoundExpression

            diagnostics = CheckJsonFeaturesAllowed(syntax, diagnostics)

            Dim jsonArrayType = GetWellKnownType(WellKnownType.Newtonsoft_Json_Linq_JArray, syntax, diagnostics)

            Dim elementsBuilder = ArrayBuilder(Of BoundNode).GetInstance()

            For Each element In syntax.Elements
                elementsBuilder.Add(BindValue(element, diagnostics))
            Next

            Return New BoundJsonArray(syntax, elementsBuilder.ToImmutableAndFree(), Me, jsonArrayType)

        End Function

        Private Function BindJsonNameValuePairExpression(syntax As JsonNameValuePairExpressionSyntax, diagnostics As DiagnosticBag) As BoundExpression

            diagnostics = CheckJsonFeaturesAllowed(syntax, diagnostics)

            Dim jsonPairType = GetWellKnownType(WellKnownType.Newtonsoft_Json_Linq_JProperty, syntax, diagnostics)

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
                    boundValue = New BoundLiteral(syntax, ConstantValue.Create(syntax.ValueToken.ValueText), Compilation.GetSpecialType(SpecialType.System_String))

            End Select

            Return boundValue

            'Return New BoundJsonConstant(syntax, boundValue, Me, boundValue.Type)

        End Function

        ''' <summary>
        ''' Check if JSON features are allowed. If not, report an error and return a
        ''' separate DiagnosticBag that can be used for binding sub-expressions.
        ''' </summary>
        Private Function CheckJsonFeaturesAllowed(syntax As VisualBasicSyntaxNode, diagnostics As DiagnosticBag) As DiagnosticBag
            ' Check if XObject is available, which matches the native compiler.
            Dim type = Compilation.GetWellKnownType(WellKnownType.Newtonsoft_Json_Linq_JObject)
            If type.IsErrorType() Then
                ' "JSON literals are not available. Add a references to Newtonsoft.Json.dll."
                ReportDiagnostic(diagnostics, syntax, ERRID.ERR_JsonFeaturesNotAvailable)
                ' DiagnosticBag does not need to be created from the pool
                ' since this is an error recovery scenario only.
                Return New DiagnosticBag()
            Else
                Return diagnostics
            End If
        End Function

    End Class
End Namespace
