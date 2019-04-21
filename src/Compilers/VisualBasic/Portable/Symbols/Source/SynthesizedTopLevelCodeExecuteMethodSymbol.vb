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

        Public Sub New(container As SourceNamedTypeSymbol, name As String, syntax As VisualBasicSyntaxNode, binder As Binder, statements As IEnumerable(Of ExecutableStatementSyntax))
            MyBase.New(container, SourceMemberFlags.AccessibilityPublic Or SourceMemberFlags.Public Or SourceMemberFlags.MethodKindOrdinary, binder.GetSyntaxReference(syntax))

            Me.Name = name
            Me.Statements = statements
        End Sub

        Public Overrides ReadOnly Property ReturnType As TypeSymbol
            Get
                Return ContainingAssembly.GetSpecialType(SpecialType.System_Object)
                'Return DeclaringCompilation.GetWellKnownType(WellKnownType.System_Threading_Tasks_Task_T).Construct(ContainingAssembly.GetSpecialType(SpecialType.System_Object))
            End Get
        End Property

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
            Dim boundStatement = methodBodyBinder.BindCompilationUnitStatements(root, diagnostics)
#If DEBUG Then
            methodBodyBinder.EnableSimpleNameBindingOrderChecks(False)
#End If

            Return DirectCast(boundStatement, BoundBlock)

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

        Public Overrides ReadOnly Property Parameters As ImmutableArray(Of ParameterSymbol)
            Get
                Return ImmutableArray(Of ParameterSymbol).Empty
            End Get
        End Property

        Friend Overrides ReadOnly Property OverriddenMembers As OverriddenMembersResult(Of MethodSymbol)
            Get
                Return OverriddenMembersResult(Of MethodSymbol).Empty
            End Get
        End Property
    End Class
End Namespace
