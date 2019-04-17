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
    Friend Class SynthesizedTopLevelCodeExecuteMethodSymbol
        Inherits SynthesizedRegularMethodBase

        Private ReadOnly Statements As IEnumerable(Of ExecutableStatementSyntax)

        Public Sub New(syntaxNode As VisualBasicSyntaxNode, name As String, container As SourceNamedTypeSymbol, statements As IEnumerable(Of ExecutableStatementSyntax))
            MyBase.New(syntaxNode, container, name, isShared:=False)

            Me.Statements = statements
        End Sub

        Public Overrides ReadOnly Property DeclaredAccessibility As Accessibility
            Get
                ' TODO: This should match the overridden method once overriding is supported.
                Return Accessibility.Public
            End Get
        End Property

        Public Overrides ReadOnly Property IsAsync As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides ReadOnly Property IsSub As Boolean
            Get
                Return False
            End Get
        End Property

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

        Friend Overrides Sub AddSynthesizedAttributes(compilationState As ModuleCompilationState, ByRef attributes As ArrayBuilder(Of SynthesizedAttributeData))
            MyBase.AddSynthesizedAttributes(compilationState, attributes)
        End Sub

        Friend Overrides ReadOnly Property GenerateDebugInfoImpl As Boolean
            Get
                Return False
            End Get
        End Property

        Friend Overrides Function CalculateLocalSyntaxOffset(localPosition As Integer, localTree As SyntaxTree) As Integer
            Throw ExceptionUtilities.Unreachable
        End Function
    End Class
End Namespace
