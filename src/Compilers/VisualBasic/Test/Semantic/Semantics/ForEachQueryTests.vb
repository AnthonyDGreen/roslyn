' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Microsoft.CodeAnalysis.VisualBasic.SyntaxFacts
Imports Roslyn.Test.Utilities

Namespace Microsoft.CodeAnalysis.VisualBasic.UnitTests.Semantics

    Public Class ForEachQueryTests
        Inherits BasicTestBase

        <Fact>
        Public Sub SimpleCaseWithWhereClause()
            Dim compilationDef =
<compilation name="QueryExpressions">
    <file name="a.vb">
Option Strict Off
Option Infer On

Imports System
Imports System.Collections
Imports System.Linq

Module Program
    Sub Main()
        Dim digits = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

        For Each i In digits Where i Mod 2 = 0
            Console.Write(i)
        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            Dim verifier = CompileAndVerify(compilation, expectedOutput:="02468")

        End Sub

    End Class
End Namespace
