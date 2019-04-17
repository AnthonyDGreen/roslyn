' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Microsoft.CodeAnalysis.VisualBasic.SyntaxFacts
Imports Roslyn.Test.Utilities

Namespace Microsoft.CodeAnalysis.VisualBasic.UnitTests.Semantics

    Public Class TopLevelTests
        Inherits BasicTestBase

        <Fact>
        Public Sub TopLevelDeclarations()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="TopLevelExplicitMain.vb">
Public Shared Sub Main()
    System.Console.Write("Hello, World!")    
End Sub
    </file>
</compilation>, expectedOutput:="Hello, World!")

        End Sub

        <Fact>
        Public Sub TopLevelStatements()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="TopLevelExplicitMainCallingExecute.vb">
Public Shared Sub Main()
    Call New TopLevelExplicitMainCallingExecute().Execute()
End Sub

System.Console.Write("Hello, World!")
    </file>
</compilation>, expectedOutput:="Hello, World!")

        End Sub

        <Fact>
        Public Sub HelloWorld()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="SynthesizedMainCallingExecute.vb">
System.Console.Write("Hello, World!")
    </file>
</compilation>, expectedOutput:="Hello, World!")

        End Sub

    End Class
End Namespace
