' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Microsoft.CodeAnalysis.VisualBasic.SyntaxFacts
Imports Microsoft.CodeAnalysis.Test.Utilities
Imports Roslyn.Test.Utilities

#Const TestRegressions = False

Namespace Microsoft.CodeAnalysis.VisualBasic.UnitTests.Semantics

    Public Class TopLevelTests
        Inherits BasicTestBase

#If TestRegressions Then


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

        <Fact>
        Public Sub EmitsPdbInfo()
            Dim source =
<compilation>
    <file name="GeneratesPdbInfo.vb"><![CDATA[
System.Console.Write("Hello, World!")
]]></file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(source, TestOptions.DebugExe)

            compilation.VerifyPdb(
<symbols>
    <files>
        <file id="1" name="GeneratesPdbInfo.vb" language="VB" checksumAlgorithm="SHA1" checksum="A4-41-F6-DE-84-67-3F-E8-E3-EF-2C-B3-BC-5A-7E-7B-9A-D3-99-9A"/>
    </files>
    <entryPoint declaringType="GeneratesPdbInfo" methodName="Main"/>
    <methods>
        <method containingType="GeneratesPdbInfo" name="Execute">
            <customDebugInfo>
                <encLocalSlotMap>
                    <slot kind="21" offset="0"/>
                </encLocalSlotMap>
            </customDebugInfo>
            <sequencePoints>
                <entry offset="0x0" startLine="1" startColumn="1" endLine="1" endColumn="38" document="1"/>
                <entry offset="0xb" startLine="1" startColumn="1" endLine="2" endColumn="1" document="1"/>
            </sequencePoints>
            <scope startOffset="0x0" endOffset="0xd">
                <currentnamespace name=""/>
            </scope>
        </method>
        <method containingType="GeneratesPdbInfo" name="Main">
            <scope startOffset="0x0" endOffset="0xc">
                <importsforward declaringType="GeneratesPdbInfo" methodName="Execute"/>
            </scope>
        </method>
    </methods>
</symbols>, options:=PdbValidationOptions.SkipConversionValidation)
        End Sub

        <Fact>
        Public Sub GetDeclaredSymbolForCompilationUnitWithNoExecuteMethod()
            Dim source =
<compilation>
    <file name=<%= NameOf(GetDeclaredSymbolForCompilationUnitWithNoExecuteMethod) & ".vb" %>>
Public Shared Sub Main()
    System.Console.Write("Hello, World!")    
End Sub
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(source, TestOptions.DebugExe)

            Dim tree = compilation.SyntaxTrees.Single()
            Dim model = compilation.GetSemanticModel(tree)
            Dim symbol = model.GetDeclaredSymbol(tree.GetRoot())

            Assert.Equal(NameOf(GetDeclaredSymbolForCompilationUnitWithNoExecuteMethod), symbol.Name)

        End Sub

        <Fact>
        Public Sub GetDeclaredSymbolForCompilationUnitWithExecuteMethod()
            Dim source =
<compilation>
    <file name=<%= NameOf(GetDeclaredSymbolForCompilationUnitWithExecuteMethod) & ".vb" %>>
Public Shared Sub Main()
    Call New GetDeclaredSymbolForCompilationUnitWithExecuteMethod().Execute()
End Sub

System.Console.Write("Hello, World!")
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(source, TestOptions.DebugExe)

            Dim tree = compilation.SyntaxTrees.Single()
            Dim model = compilation.GetSemanticModel(tree)
            Dim symbol = model.GetDeclaredSymbol(tree.GetRoot())

            Assert.Equal("Execute", symbol.Name)

        End Sub

        <Fact>
        Public Sub ExecuteOverridesBaseMethod()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="ExecuteOverridesBaseMethod.vb">
writer.Write("Hello, World!")
    </file>
    <file name="ExecuteOverridesBaseMethod_OtherPart.vb">
Partial Class ExecuteOverridesBaseMethod
    Inherits JustCallsExecute

    Public Shared Sub Main()
        JustCallsExecute.Run(New ExecuteOverridesBaseMethod)
    End Sub
End Class
    </file>
    <file name="JustCallsExecute.vb">
MustInherit Class JustCallsExecute

    Protected MustOverride Sub Execute(writer As System.IO.TextWriter)

    Public Shared Sub Run(derived As JustCallsExecute)
        derived.Execute(System.Console.Out)
    End Sub
End Class
    </file>
</compilation>, expectedOutput:="Hello, World!")

        End Sub

        <Fact>
        Public Sub DefaultBaseClass()

            Dim parseOptions = New VisualBasicParseOptions(preprocessorSymbols:={New KeyValuePair(Of String, Object)("DefaultTopLevelBaseClass", "JustCallsExecute")})

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="DefaultBaseClass.vb">
writer.Write("Hello, World!")
    </file>
    <file name="Program.vb">
Module Program
    Public Sub Main()
        JustCallsExecute.Run(New DefaultBaseClass)
    End Sub
End Module
    </file>
    <file name="JustCallsExecute.vb">
MustInherit Class JustCallsExecute

    Protected MustOverride Sub Execute(writer As System.IO.TextWriter)

    Public Shared Sub Run(derived As JustCallsExecute)
        derived.Execute(System.Console.Out)
    End Sub
End Class
    </file>
</compilation>, options:=TestOptions.ReleaseDebugExe.WithParseOptions(parseOptions).WithMainTypeName("Program"), expectedOutput:="Hello, World!")

        End Sub

#End If

        <Fact>
        Public Sub ReturnExpression()

            Dim xmlReferences As MetadataReference() = {MscorlibRef, MsvbRef, SystemRef, SystemCoreRef, SystemXmlRef, SystemXmlLinqRef}
            Dim parseOptions = New VisualBasicParseOptions(preprocessorSymbols:={New KeyValuePair(Of String, Object)("DefaultTopLevelBaseClass", "JustCallsExecute")})

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="ReturnExpression.vb">
&lt;html/&gt;
    </file>
    <file name="Program.vb">
Module Program
    Public Sub Main()
        JustCallsExecute.Run(New ReturnExpression)
    End Sub
End Module
    </file>
    <file name="JustCallsExecute.vb">
MustInherit Class JustCallsExecute

    Protected MustOverride Function Execute() As System.Xml.Linq.XElement

    Public Shared Sub Run(derived As JustCallsExecute)
        System.Console.Write(derived.Execute())
    End Sub
End Class
    </file>
</compilation>,
options:=TestOptions.ReleaseDebugExe.WithParseOptions(parseOptions).WithMainTypeName("Program"),
references:=xmlReferences,
expectedOutput:="<html />")

        End Sub

    End Class
End Namespace
