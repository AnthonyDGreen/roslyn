' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Namespace Microsoft.CodeAnalysis.VisualBasic.Syntax

    Partial Public NotInheritable Class CompilationUnitSyntax
        Inherits VisualBasicSyntaxNode
        Implements ICompilationUnitSyntax

        Private ReadOnly Property ICompilationUnitSyntax_EndOfFileToken As SyntaxToken Implements ICompilationUnitSyntax.EndOfFileToken
            Get
                Return EndOfFileToken
            End Get
        End Property

        ''' <summary> 
        ''' Returns #r directives specified in the compilation. 
        ''' </summary>       
        Public Function GetReferenceDirectives() As IList(Of ReferenceDirectiveTriviaSyntax)
            Return GetReferenceDirectives(Nothing)
        End Function

        Friend Function GetReferenceDirectives(filter As Func(Of ReferenceDirectiveTriviaSyntax, Boolean)) As IList(Of ReferenceDirectiveTriviaSyntax)
            ' #r directives are always on the first token of the compilation unit.
            Dim firstToken = CType(Me.GetFirstToken(includeZeroWidth:=True), SyntaxNodeOrToken)
            Return firstToken.GetDirectives(Of ReferenceDirectiveTriviaSyntax)(filter)
        End Function

        ''' <summary>
        ''' Returns true if this compilation unit contains top-level executable statements, expressions, or type-member declarations (e.g. methods, fields, properties). 
        ''' </summary>
        Friend ReadOnly Property HasTopLevelCode As Boolean
            Get
                For Each node In Me.Members
                    Select Case node.Kind
                        Case SyntaxKind.NamespaceBlock,
                             SyntaxKind.ClassBlock,
                             SyntaxKind.StructureBlock,
                             SyntaxKind.EnumBlock,
                             SyntaxKind.DelegateSubStatement,
                             SyntaxKind.DelegateFunctionStatement,
                             SyntaxKind.InterfaceBlock,
                             SyntaxKind.ModuleBlock

                            Continue For

                        Case Else

                            ' TODO: This should also return true if the file contains a file-level attribute with a `<Type:` or `<Method:` target, or
                            ' an `Inherits` or `Implements` clause.

                            Return True

                    End Select
                Next

                Return False
            End Get
        End Property

        Friend ReadOnly Property HasTopLevelExecutableStatements As Boolean
            Get
                For Each node In Members
                    If TypeOf node Is ExecutableStatementSyntax Then
                        Return True
                    End If
                Next

                Return False
            End Get
        End Property

        Friend Function GetTopLevelExecutableStatements() As IEnumerable(Of ExecutableStatementSyntax)
            Return Members.OfType(Of ExecutableStatementSyntax)
        End Function

        Friend Function GetImplicitTypeName() As String
            If Not HasTopLevelCode Then Return String.Empty

            Return IO.Path.GetFileName(SyntaxTree.FilePath).Split({"."c}).First()
        End Function

        Friend Function GetInheritsStatement() As InheritsStatementSyntax
            Return Members.OfType(Of InheritsStatementSyntax).FirstOrDefault()
        End Function

    End Class
End Namespace

