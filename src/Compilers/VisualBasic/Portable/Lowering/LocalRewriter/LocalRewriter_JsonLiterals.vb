' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports System.Collections.Immutable
Imports System.Runtime.InteropServices
Imports System.Text
Imports Microsoft.CodeAnalysis.Collections
Imports Microsoft.CodeAnalysis.VisualBasic.Symbols
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace Microsoft.CodeAnalysis.VisualBasic

    Partial Friend Class LocalRewriter

        Private Function TryLookupMethod(node As BoundNode, binder As Binder, receiver As BoundExpression, name As String, arity As Integer, ByRef hasErrors As Boolean) As BoundMethodGroup
            Dim lookup = LookupResult.GetInstance()
            Dim useSiteDiagnostics As HashSet(Of DiagnosticInfo) = Nothing

            binder.LookupMember(lookup, receiver.Type, name, 0, LookupOptions.MethodsOnly Or LookupOptions.AllMethodsOfAnyArity, useSiteDiagnostics)

            If Not lookup.IsGood Then
                lookup.Clear()
                binder.LookupExtensionMethods(lookup, receiver.Type, name, 0, LookupOptions.MethodsOnly Or LookupOptions.MustBeInstance Or LookupOptions.AllMethodsOfAnyArity, useSiteDiagnostics)
            End If

            _diagnostics.Add(node, useSiteDiagnostics)

            If lookup.Kind = LookupResultKind.Inaccessible Then
                hasErrors = True
            ElseIf Not lookup.IsGood Then
                hasErrors = True
                lookup.Free()
                Return Nothing
            End If

            Dim methodGroup = New BoundMethodGroup(node.Syntax, Nothing, lookup.Symbols.ToDowncastedImmutable(Of MethodSymbol), lookup.Kind, receiver, QualificationKind.QualifiedViaValue).MakeCompilerGenerated()
            lookup.Free()

            Return methodGroup

        End Function

        Public Overrides Function VisitJsonObject(node As BoundJsonObject) As BoundNode
            If node.Members.Length = 0 Then
                Return node.Binder.BindObjectCreationExpression(node.Syntax, node.Type, ImmutableArray(Of BoundExpression).Empty, _diagnostics)
            End If

            Dim factory = New SyntheticBoundNodeFactory(_topMethod, _currentMethodOrLambda, node.Syntax, _compilationState, _diagnostics)
            Dim binder = node.Binder

            Dim temp = factory.SynthesizedLocal(node.Type)
            Dim boundTemp = factory.Local(temp, isLValue:=True)

            Dim locals = ImmutableArray.Create(Of LocalSymbol)(temp)

            Dim sequenceExpressions = New BoundExpression(0 To node.Members.Length) {}

            sequenceExpressions(0) = factory.AssignmentExpression(boundTemp, binder.BindObjectCreationExpression(node.Syntax, node.Type, ImmutableArray(Of BoundExpression).Empty, _diagnostics))

            Dim addMethod = TryLookupMethod(node, binder, boundTemp, "AddEx", 0, hasErrors:=Nothing)

            If addMethod Is Nothing Then
                addMethod = TryLookupMethod(node, binder, boundTemp, "Add", 0, hasErrors:=Nothing)
            End If

            Dim i = 1
            For Each member As BoundExpression In node.Members

                Dim arguments As ImmutableArray(Of BoundExpression)

                If member.Kind = BoundKind.JsonNameValuePair Then
                    Dim pair = DirectCast(member, BoundJsonNameValuePair)

                    arguments = ImmutableArray.Create(pair.Name, pair.Value)
                Else
                    arguments = ImmutableArray.Create(member)
                End If

                sequenceExpressions(i) = binder.BindInvocationExpression(node.Syntax,
                                                                         node.Syntax,
                                                                         TypeCharacter.None,
                                                                         addMethod,
                                                                         arguments,
                                                                         Nothing,
                                                                         _diagnostics,
                                                                         callerInfoOpt:=Nothing,
                                                                         forceExpandedForm:=True).MakeCompilerGenerated()

                If sequenceExpressions(i).HasErrors Then
                    Return factory.BadExpression(DirectCast(MyBase.VisitJsonObject(node), BoundExpression))
                End If

                i += 1
            Next

            Dim result = New BoundSequence(node.Syntax, locals, ImmutableArray.Create(sequenceExpressions), factory.Local(temp, isLValue:=False), node.Type)

            Return MyBase.Visit(result)
        End Function

        Public Overrides Function VisitJsonArray(node As BoundJsonArray) As BoundNode
            If node.Elements.Length = 0 Then
                Return node.Binder.BindObjectCreationExpression(node.Syntax, node.Type, ImmutableArray(Of BoundExpression).Empty, _diagnostics)
            End If

            Dim factory = New SyntheticBoundNodeFactory(_topMethod, _currentMethodOrLambda, node.Syntax, _compilationState, _diagnostics)
            Dim binder = node.Binder

            Dim temp = factory.SynthesizedLocal(node.Type)
            Dim boundTemp = factory.Local(temp, isLValue:=True)

            Dim locals = ImmutableArray.Create(Of LocalSymbol)(temp)

            Dim sequenceExpressions = New BoundExpression(0 To node.Elements.Length) {}

            sequenceExpressions(0) = factory.AssignmentExpression(boundTemp, binder.BindObjectCreationExpression(node.Syntax, node.Type, ImmutableArray(Of BoundExpression).Empty, _diagnostics))

            Dim addMethod = TryLookupMethod(node, binder, boundTemp, "Add", 0, hasErrors:=Nothing)

            Dim i = 1
            For Each element As BoundExpression In node.Elements

                sequenceExpressions(i) = binder.BindInvocationExpression(node.Syntax,
                                                                         node.Syntax,
                                                                         TypeCharacter.None,
                                                                         addMethod,
                                                                         ImmutableArray.Create(element),
                                                                         Nothing,
                                                                         _diagnostics,
                                                                         callerInfoOpt:=Nothing,
                                                                         forceExpandedForm:=True).MakeCompilerGenerated()
                i += 1
            Next

            Dim result = New BoundSequence(node.Syntax, locals, ImmutableArray.Create(sequenceExpressions), factory.Local(temp, isLValue:=False), node.Type)

            Return MyBase.Visit(result)
        End Function

        Public Overrides Function VisitJsonNameValuePair(node As BoundJsonNameValuePair) As BoundNode
            Dim binder = node.Binder

            Return MyBase.Visit(binder.BindObjectCreationExpression(node.Syntax, node.Type, ImmutableArray.Create(node.Name, node.Value), _diagnostics))
        End Function

        Public Overrides Function VisitJsonConstant(node As BoundJsonConstant) As BoundNode
            Return DirectCast(MyBase.VisitJsonConstant(node), BoundJsonConstant).Value
        End Function

    End Class

End Namespace
