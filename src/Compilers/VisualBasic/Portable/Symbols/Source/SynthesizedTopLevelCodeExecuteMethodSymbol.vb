' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports System.Collections.Generic
Imports System.Collections.Immutable
Imports System.Diagnostics
Imports System.Runtime.InteropServices
Imports Microsoft.CodeAnalysis.PooledObjects
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic.Symbols
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports TypeKind = Microsoft.CodeAnalysis.TypeKind

Namespace Microsoft.CodeAnalysis.VisualBasic.Symbols

    ''' <summary>
    ''' Represents a compiler generated "Execute" method for top-level code.
    ''' </summary>
    Friend Class TopLevelCodeContainerMethodSymbol
        Inherits SourceMethodSymbol

        Public Overrides ReadOnly Property Name As String
        Private ReadOnly Statements As IEnumerable(Of ExecutableStatementSyntax)
        Private ReadOnly BaseMethod As MethodSymbol

        Public Sub New(container As SourceNamedTypeSymbol, name As String, syntax As VisualBasicSyntaxNode, binder As Binder, statements As IEnumerable(Of ExecutableStatementSyntax))
            MyBase.New(container, SourceMemberFlags.AccessibilityPublic Or SourceMemberFlags.Public Or SourceMemberFlags.MethodKindOrdinary, binder.GetSyntaxReference(syntax))

            Me.Name = name
            Me.Statements = statements
        End Sub

        Public Sub New(container As SourceNamedTypeSymbol, syntax As VisualBasicSyntaxNode, binder As Binder, baseMethod As MethodSymbol)
            MyBase.New(container, GetMemberFlags(baseMethod), binder.GetSyntaxReference(syntax))

            Me.Name = baseMethod.Name
            Me.BaseMethod = baseMethod
        End Sub

        Private Shared Function GetMemberFlags(baseMethod As MethodSymbol) As SourceMemberFlags
            Dim flags As SourceMemberFlags = SourceMemberFlags.Overrides

            flags = flags Or CType(baseMethod.DeclaredAccessibility, SourceMemberFlags)

            If baseMethod.MethodKind = MethodKind.Ordinary Then
                flags = flags Or SourceMemberFlags.MethodKindOrdinary
            Else
                Throw ExceptionUtilities.Unreachable
            End If

            If baseMethod.IsSub Then
                flags = flags Or SourceMemberFlags.MethodIsSub
            End If

            Return flags
        End Function

        Friend Overrides Function GetBoundMethodBody(compilationState As TypeCompilationState, diagnostics As DiagnosticBag, <Out()> Optional ByRef methodBodyBinder As Binder = Nothing) As BoundBlock

            'Dim syntaxTree As SyntaxTree = Me.SyntaxTree

            '' All source method symbols must have block syntax.
            'Dim methodBlock As MethodBlockBaseSyntax = Me.BlockSyntax
            'Debug.Assert(methodBlock IsNot Nothing)

            Dim root = DirectCast(Syntax, CompilationUnitSyntax)

            Dim typeBinder = BinderBuilder.CreateBinderForType(DirectCast(Me.ContainingModule, SourceModuleSymbol), Me.Syntax.SyntaxTree, Me.ContainingType)

            ' Bind the method block
            methodBodyBinder = BinderBuilder.CreateBinderForMethodBody(Me, Me.Syntax, typeBinder)

#If DEBUG Then
            ' Enable DEBUG check for ordering of simple name binding.
            methodBodyBinder.EnableSimpleNameBindingOrderChecks(True)
#End If
            Dim boundBlock = methodBodyBinder.BindCompilationUnitStatements(root, diagnostics)
#If DEBUG Then
            methodBodyBinder.EnableSimpleNameBindingOrderChecks(False)
#End If
            ' The binder just ignores any non-executable statements and represents them as no-op statements.
            ' But these no-ops cause the first offset of user-code to not be at 0 which throws assertions
            ' later in the emitter. Removing these no-ops here makes the most sense, for now.
            Dim rewrittenStatements = ArrayBuilder(Of BoundStatement).GetInstance(boundBlock.Statements.Length)
            For Each statement In boundBlock.Statements
                If statement.Kind = BoundKind.NoOpStatement AndAlso TypeOf statement.Syntax IsNot ExecutableStatementSyntax Then Continue For

                rewrittenStatements.Add(statement)
            Next
            boundBlock = boundBlock.Update(boundBlock.StatementListSyntax, boundBlock.Locals, rewrittenStatements.ToImmutableAndFree())

            If Me.IsSub Then Return boundBlock

            ' Find the last expression statement, if any, and try to use that as the return value.
            ' Can't predict exactly which statement this will be because if the CompilationUnit has
            ' any member declarations the binder will represent them as no-ops in the BoundBlock.
            Dim expressionStatement As BoundExpressionStatement = Nothing

            For i = boundBlock.Statements.Count - 1 To 0 Step -1
                If boundBlock.Statements(i).Kind = BoundKind.ExpressionStatement Then
                    expressionStatement = DirectCast(boundBlock.Statements(i), BoundExpressionStatement)
                    Exit For
                End If
            Next

            If expressionStatement Is Nothing Then Return boundBlock

            Select Case expressionStatement.Expression.Kind
                Case BoundKind.XmlElement,
                     BoundKind.XmlDocument,
                     BoundKind.JsonObject,
                     BoundKind.JsonArray

                    Dim newStatements = ArrayBuilder(Of BoundStatement).GetInstance(boundBlock.Statements.Length)
                    For Each statement In boundBlock.Statements
                        newStatements.Add(statement)
                    Next

                    Dim syntax = expressionStatement.Syntax

                    Dim functionReturnLocal = methodBodyBinder.GetLocalForFunctionValue()
                    Dim assignment = New BoundAssignmentOperator(syntax,
                                                                 New BoundLocal(syntax, functionReturnLocal, functionReturnLocal.Type),
                                                                 methodBodyBinder.ApplyImplicitConversion(expressionStatement.Expression.Syntax, functionReturnLocal.Type, expressionStatement.Expression, diagnostics),
                                                                 suppressObjectClone:=True)

                    For i = newStatements.Count - 1 To 0 Step -1
                        If newStatements(i) Is expressionStatement Then
                            newStatements(i) = expressionStatement.Update(assignment)
                            Exit For
                        End If
                    Next

                    boundBlock = boundBlock.Update(boundBlock.StatementListSyntax, boundBlock.Locals, newStatements.ToImmutableAndFree())
            End Select

            Return boundBlock

        End Function

        Protected Overrides Function GetAttributesBag() As CustomAttributesBag(Of VisualBasicAttributeData)
            Return CustomAttributesBag(Of VisualBasicAttributeData).Empty
        End Function

        Protected Overrides Function GetReturnTypeAttributesBag() As CustomAttributesBag(Of VisualBasicAttributeData)
            Return CustomAttributesBag(Of VisualBasicAttributeData).Empty
        End Function

        Friend Overrides ReadOnly Property MayBeReducibleExtensionMethod As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides ReadOnly Property TypeParameters As ImmutableArray(Of TypeParameterSymbol)
            Get
                Return ImmutableArray(Of TypeParameterSymbol).Empty
            End Get
        End Property

        Private _Parameters As ImmutableArray(Of ParameterSymbol)

        Public Overrides ReadOnly Property Parameters As ImmutableArray(Of ParameterSymbol)
            Get

                If BaseMethod Is Nothing Then
                    Return ImmutableArray(Of ParameterSymbol).Empty
                Else
                    If _Parameters.IsDefault Then

                        Dim builder = ArrayBuilder(Of ParameterSymbol).GetInstance(BaseMethod.ParameterCount)

                        For Each param In BaseMethod.Parameters
                            Dim flags As SourceParameterFlags

                            If param.IsByRef Then
                                flags = flags Or SourceParameterFlags.ByRef
                            Else
                                flags = flags Or SourceParameterFlags.ByVal
                            End If

                            If param.IsParamArray Then
                                flags = flags Or SourceParameterFlags.ParamArray
                            End If

                            Dim defaultValue As ConstantValue = Nothing
                            If param.IsOptional Then
                                flags = flags Or SourceParameterFlags.Optional

                                If param.HasExplicitDefaultValue Then
                                    defaultValue = param.ExplicitDefaultConstantValue()
                                End If
                            End If

                            builder.Add(SourceComplexParameterSymbol.Create(Me, param.Name, param.Ordinal, param.Type, Location.None, Nothing, flags, defaultValue))
                        Next

                        _Parameters = builder.ToImmutableAndFree()
                    End If

                    Return _Parameters
                End If
            End Get
        End Property

        Public Overrides ReadOnly Property ReturnType As TypeSymbol
            Get
                If BaseMethod Is Nothing Then
                    Return ContainingAssembly.GetSpecialType(SpecialType.System_Object)
                Else
                    Return BaseMethod.ReturnType
                End If
            End Get
        End Property

        Friend Overrides ReadOnly Property OverriddenMembers As OverriddenMembersResult(Of MethodSymbol)
            Get
                If BaseMethod Is Nothing Then
                    Return OverriddenMembersResult(Of MethodSymbol).Empty
                Else
                    Return OverriddenMembersResult(Of MethodSymbol).Create(ImmutableArray.Create(BaseMethod),
                                                                           ImmutableArray(Of MethodSymbol).Empty,
                                                                           ImmutableArray(Of MethodSymbol).Empty)
                End If
            End Get
        End Property
    End Class
End Namespace
