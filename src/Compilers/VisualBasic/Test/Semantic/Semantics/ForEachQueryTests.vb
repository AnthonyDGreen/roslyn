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

        <Fact>
        Public Sub SimpleCaseWithSelectClause()
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

        For Each i In digits Where i &lt; 4 Select square = i * i
            Console.Write(square)
        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            Dim verifier = CompileAndVerify(compilation, expectedOutput:="0149")

        End Sub

        <Fact>
        Public Sub SelectClauseHidesRangeVariables()
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

        For Each i In digits Where i &lt; 4 Select square = i * i
            Console.Write(i)
        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            AssertTheseCompileDiagnostics(compilation,
<expected>
BC30451: 'i' is not declared. It may be inaccessible due to its protection level.
            Console.Write(i)
                          ~
</expected>)

        End Sub

        <Fact>
        Public Sub MultiSelect()
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
        Dim digits = {1, 2, 3, 4, 5}

        For Each i In digits Select square = i * i, cube = i * i * i
            Console.Write(square &amp; cube)
        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            Dim verifier = CompileAndVerify(compilation, expectedOutput:="1148927166425125")

        End Sub

        <Fact>
        Public Sub EmptySelectClauseRecoversGracefully()
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

        For Each i In digits Select

        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            AssertTheseCompileDiagnostics(compilation,
<expected>
BC30201: Expression expected.
        For Each i In digits Select
                                   ~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectClauseWithoutInferableNameRecoversGracefully()
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

        For Each i In digits Select
            Console.Write(i)
        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            AssertTheseCompileDiagnostics(compilation,
<expected>
BC30491: Expression does not produce a value.
            Console.Write(i)
            ~~~~~~~~~~~~~~~~
BC36599: Range variable name can be inferred only from a simple or qualified name with no arguments.
            Console.Write(i)
            ~~~~~~~~~~~~~~~~
</expected>)

        End Sub

    End Class
End Namespace
