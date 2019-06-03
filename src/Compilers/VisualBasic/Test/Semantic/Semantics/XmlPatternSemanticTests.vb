' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic.Symbols
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Roslyn.Test.Utilities

Namespace Microsoft.CodeAnalysis.VisualBasic.UnitTests.Semantics
    Public Class XmlPatternSemanticTests
        Inherits BasicTestBase

        Private ReadOnly XmlReferences As MetadataReference() = {SystemRef, SystemCoreRef, SystemXmlRef, SystemXmlLinqRef}
        Private ReadOnly TestTypeDefinitions As Xml.Linq.XElement =
<file name="MockWpf.vb">
Namespace Global.System.Windows

    Public Class Window
        Property Title As String
    End Class

    Namespace Controls

        Public Class Grid

        End Class

    End Namespace
End Namespace
</file>

        <Fact>
        Public Sub SimpleElement()

            Dim verifier = CompileAndVerify(
<compilation>
    <%= TestTypeDefinitions %>
    <file name="a.vb">
Imports System.Console
Imports &lt;xmlns="clr-namespace:System.Windows"&gt;
Imports System.Windows,
        System.Windows.Controls

Module Program
    Sub Main()
        Dim obj = &lt;Window&gt;&lt;/Window&gt;
        Write(obj.GetType().Name)
    End Sub
End Module
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="Window")

        End Sub

        <Fact>
        Public Sub SimpleElementWithAttribute()

            Dim verifier = CompileAndVerify(
<compilation>
    <%= TestTypeDefinitions %>
    <file name="a.vb">
Imports System.Console
Imports &lt;xmlns="clr-namespace:System.Windows"&gt;
Imports System.Windows,
        System.Windows.Controls

Module Program
    Sub Main()
        Dim obj = &lt;Window Title="MainWindow"&gt;&lt;/Window&gt;
        Write(obj.Title)
    End Sub
End Module
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="MainWindow")

        End Sub

    End Class
End Namespace
