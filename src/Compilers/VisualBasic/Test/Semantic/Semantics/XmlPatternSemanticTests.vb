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
Imports System
Imports System.Collections.Generic,
        System.Collections.ObjectModel

Namespace Global.System.Windows

    Public Class Window
        Property Title As String = ""
        Property Width As Integer
        Property Height As Integer
        Property WindowState As WindowState
        Property Content As Object
        Property HeaderContent As Object

        Event Loaded As EventHandler(Of EventArgs)

        Sub Show()
            RaiseEvent Loaded(Me, EventArgs.Empty)
        End Sub

    End Class

    Public Enum WindowState
        Normal
        Minimized
        Maximized
    End Enum

    Namespace Controls

        Public Class Button
            Property Content As Object
        End Class

        Public Class Panel
            Property Children As New Collection(Of Object)
        End Class

        Public Class Grid
            Inherits Panel
        End Class

    End Namespace
End Namespace
</file>

        Private ReadOnly TestTypeExtensions As Xml.Linq.XElement =
<file name="WpfExtensions.vb">
Imports System
Imports System.Collections.Generic
Imports System.Windows,
        System.Windows.Controls

Namespace Global.XmlPatternHelpers

    Public Module WpfExtensions
       
        &lt;Runtime.CompilerServices.Extension&gt;
        Sub SetChildContent(instance As Window, content As Object)
            instance.Content = content
        End Sub
       
        &lt;Runtime.CompilerServices.Extension&gt;
        Sub SetChildContent(instance As Button, content As Object)
            instance.Content = content
        End Sub
        
        &lt;Runtime.CompilerServices.Extension&gt;
        Sub AddChildContent(instance As Panel, content As Object)
            instance.Children.Add(content)
        End Sub

    End Module

End Namespace
</file>

#Const TEST_REGRESSIONS = True

#If TEST_REGRESSIONS Then

        <Fact>
        Public Sub SimpleElement()

            Dim verifier = CompileAndVerify(
<compilation>
    <%= TestTypeDefinitions %>
    <file name=<%= GenerateFilename() %>><![CDATA[
Imports System.Console
Imports <xmlns="clr-namespace:System.Windows">
Imports System.Windows,
        System.Windows.Controls

Module Program
    Sub Main()
        Dim obj = <Window></Window>
        Write(obj.GetType().Name)
    End Sub
End Module
]]>
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="Window")

        End Sub

        <Fact>
        Public Sub SimpleElementWithStringAttribute()

            Dim verifier = CompileAndVerify(
<compilation>
    <%= TestTypeDefinitions %>
    <file name=<%= GenerateFilename() %>><![CDATA[
Imports System.Console
Imports <xmlns="clr-namespace:System.Windows">
Imports System.Windows,
        System.Windows.Controls

Module Program
    Sub Main()
        Dim obj = <Window Title="MainWindow"></Window>
        Write(obj.Title)
    End Sub
End Module
]]>
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="MainWindow")

        End Sub

        <Fact>
        Public Sub SimpleElementWithIntegerAttributes()

            Dim verifier = CompileAndVerify(
<compilation>
    <%= TestTypeDefinitions %>
    <file name=<%= GenerateFilename() %>><![CDATA[
Imports System.Console
Imports <xmlns="clr-namespace:System.Windows">
Imports System.Windows,
        System.Windows.Controls

Module Program
    Sub Main()
        Dim obj As Window = <Window Title="MainWindow" Width="1920" Height="1080"></Window>
        Write(obj.Width + obj.Height)
    End Sub
End Module
]]>
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="3000")

        End Sub

        <Fact>
        Public Sub SimpleElementWithEmbeddedAttributeValue()

            Dim verifier = CompileAndVerify(
<compilation>
    <%= TestTypeDefinitions %>
    <file name=<%= GenerateFilename() %>><![CDATA[
Imports System
Imports System.Console
Imports <xmlns="clr-namespace:System.Windows">
Imports System.Windows,
        System.Windows.Controls

Module Program
    Sub Main()
        Dim obj As Window = <Window Title=<%= Guid.NewGuid().ToString() %>></Window>
        Write(Guid.TryParse(obj.Title, result:=Nothing))
    End Sub
End Module
]]>
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="True")

        End Sub

        <Fact>
        Public Sub SimpleElementWithEmbeddedAttributeValueNoConversion()

            Dim compilation = CreateCompilationWithMscorlib40AndVBRuntimeAndReferences(
<compilation>
    <%= TestTypeDefinitions %>
    <file name=<%= GenerateFilename() %>><![CDATA[
Imports System
Imports System.Console
Imports <xmlns="clr-namespace:System.Windows">
Imports System.Windows,
        System.Windows.Controls

Module Program
    Sub Main()
        Dim obj As Window = <Window Title=<%= Guid.NewGuid() %>></Window>
        Write(Guid.TryParse(obj.Title, result:=Nothing))
    End Sub
End Module
]]>
    </file>
</compilation>, references:=XmlReferences)

            AssertTheseCompileDiagnostics(compilation,
<expected><![CDATA[
BC30311: Value of type 'Guid' cannot be converted to 'String'.
        Dim obj As Window = <Window Title=<%= Guid.NewGuid() %>></Window>
                                              ~~~~~~~~~~~~~~]]>
</expected>)

        End Sub

        <Fact>
        Public Sub EventAttributeWithMethodNameValue()

            Dim verifier = CompileAndVerify(
<compilation>
    <%= TestTypeDefinitions %>
    <file name=<%= GenerateFilename() %>><![CDATA[
Imports System
Imports System.Console
Imports <xmlns="clr-namespace:System.Windows">
Imports System.Windows,
        System.Windows.Controls

Module Program
    Sub Main()
        Dim obj As Window = <Window Loaded="Window_Loaded"></Window>
        obj.Show()
    End Sub

    Sub Window_Loaded(sender As Object, e As EventArgs)
        Write("Loaded")
    End Sub
End Module
]]>
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="Loaded")

        End Sub

        <Fact>
        Public Sub EnumAttributeWithNamedValue()

            Dim verifier = CompileAndVerify(
<compilation>
    <%= TestTypeDefinitions %>
    <file name=<%= GenerateFilename() %>><![CDATA[
Imports System
Imports System.Console
Imports <xmlns="clr-namespace:System.Windows">
Imports System.Windows,
        System.Windows.Controls

Module Program
    Sub Main()
        Dim obj As Window = <Window WindowState="Minimized"></Window>
        Write(CInt(obj.WindowState))
    End Sub
End Module
]]>
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="1")

        End Sub

        <Fact>
        Public Sub SingleContent()

            Dim verifier = CompileAndVerify(
<compilation>
    <%= TestTypeDefinitions %>
    <%= TestTypeExtensions %>
    <file name=<%= GenerateFilename() %>><![CDATA[
Imports System
Imports System.Console
Imports <xmlns="clr-namespace:System.Windows">
Imports System.Windows,
        System.Windows.Controls
Imports XmlPatternHelpers

Module Program
    Sub Main()
        Dim obj As Window = <Window>
                                <Grid>
                                </Grid>
                            </Window>
        Write(obj.Content.GetType().Name)
    End Sub
End Module
]]>
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="Grid")

        End Sub

        <Fact>
        Public Sub MultiContent()

            Dim verifier = CompileAndVerify(
<compilation>
    <%= TestTypeDefinitions %>
    <%= TestTypeExtensions %>
    <file name=<%= GenerateFilename() %>><![CDATA[
Imports System
Imports System.Console
Imports <xmlns="clr-namespace:System.Windows">
Imports System.Windows,
        System.Windows.Controls
Imports XmlPatternHelpers

Module Program
    Sub Main()
        Dim obj As Window = <Window>
                                <Grid>
                                    <Button></Button>
                                    <Button></Button>
                                    <Button></Button>
                                </Grid>
                            </Window>
        Write(CType(obj.Content, Grid).Children.Count)
    End Sub
End Module
]]>
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="3")

        End Sub

#End If

        <Fact>
        Public Sub ComplexScalarSetter()

            Dim verifier = CompileAndVerify(
<compilation>
    <%= TestTypeDefinitions %>
    <%= TestTypeExtensions %>
    <file name=<%= GenerateFilename() %>><![CDATA[
Imports System
Imports System.Console
Imports <xmlns="clr-namespace:System.Windows">
Imports System.Windows,
        System.Windows.Controls
Imports XmlPatternHelpers

Module Program
    Sub Main()
        Dim obj As Window = <Window>
                                <Window.HeaderContent>
                                    <Button/>
                                </Window.HeaderContent>
                            </Window>

        Write((obj.Content Is Nothing) & obj.HeaderContent.GetType().Name)
    End Sub
End Module
]]>
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="TrueButton")

        End Sub

        Shared Function GenerateFilename(<System.Runtime.CompilerServices.CallerMemberName> Optional memberName As String = "") As String
            Return memberName & ".vb"
        End Function

    End Class
End Namespace
