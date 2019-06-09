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

        Public Class Control

        End Class

        Public Class Button
            Inherits Control

            Property Content As Object
        End Class

        Public Class Panel
            Inherits Control

            Property Children As New Collection(Of Object)

        End Class

        Public Class Grid
            Inherits Panel

            ReadOnly Property RowDefinitions As New RowDefinitionCollection

        End Class

        Public Class Label
            Inherits Control

            Property Text As String
        End Class

        Public Class RowDefinitionCollection
            Inherits Collection(Of RowDefinition)

        End Class

        Public Class RowDefinition
            Property Height As New GridLength(1.0, GridUnitType.Star)
        End Class

        Public Structure GridLength

            Shared ReadOnly Auto As New GridLength(1.0, GridUnitType.Auto)

            ReadOnly Property Value As Double
            ReadOnly Property GridUnitType As GridUnitType

            Sub New(value As Double, gridUnitType As GridUnitType)
                Me.Value = value
                Me.GridUnitType = gridUnitType
            End Sub

            Overrides Function ToString() As String
                Select Case GridUnitType
                    Case GridUnitType.Auto
                        Return "auto"
                    Case GridUnitType.Star
                        Return If(Value = 1.0, "*", Value &amp; "*")
                    Case Else
                        Return Value.ToString()
                End Select
            End Function
        End Structure

        Public Enum GridUnitType
            Auto
            Pixel
            Star
        End Enum

    End Namespace
End Namespace
</file>

        Private ReadOnly TestTypeExtensions As Xml.Linq.XElement =
<file name="WpfExtensions.vb"><![CDATA[
Imports System
Imports System.Collections.Generic
Imports System.Windows,
        System.Windows.Controls

Namespace Global.XmlPatternHelpers

    Public Module WpfExtensions
       
        <Runtime.CompilerServices.Extension>
        Sub SetChildContent(instance As Window, content As Object)
            instance.Content = content
        End Sub
       
        <Runtime.CompilerServices.Extension>
        Sub SetChildContent(instance As Button, content As Object)
            instance.Content = content
        End Sub
       
        <Runtime.CompilerServices.Extension>
        Sub SetChildContent(instance As Label, content As String)
            instance.Text = content
        End Sub
        
        <Runtime.CompilerServices.Extension>
        Sub AddChildContent(instance As Panel, content As Object)
            instance.Children.Add(content)
        End Sub
        
        <Runtime.CompilerServices.Extension>
        Sub AddChildContent(instance As RowDefinitionCollection, content As RowDefinition)
            instance.Add(content)
        End Sub
                       
        <Runtime.CompilerServices.Extension>
        Sub SetName(Of T As Control)(instance As T, ByRef value As T, valueText As String, otherValue As Object)
            value = instance
        End Sub
       
        <Runtime.CompilerServices.Extension>
        Sub SetHeight(instance As RowDefinition, value As GridLength, valueText As String, otherValue As Object)

            If value.Value <> 0.0 OrElse value.GridUnitType <> GridUnitType.Auto Then
                instance.Height = value
                Return
            End If

            valueText = If(valueText?.Trim().ToLower(), "")
            Dim result As Double = Nothing

            If otherValue IsNot Nothing Then
                If TypeOf otherValue Is GridLength Then
                    instance.Height = CType(otherValue, GridLength)
                Else
                    Throw New NotSupportedException()
                End If

            ElseIf valueText.StartsWith("{") Then
                Throw New NotSupportedException()

            ElseIf valueText = "auto" Then
                instance.Height = GridLength.Auto

            ElseIf valueText = "*" Then
                instance.Height = New GridLength(1.0, GridUnitType.Star)

            ElseIf valueText.EndsWith("*") Then
                valueText = valueText.SubString(0, valueText.Length - 1).Trim()

                If Double.TryParse(valueText, result) Then
                    instance.Height = New GridLength(result, GridUnitType.Star)
                Else
                    Throw New FormatException()
                End If

            ElseIf valueText.EndsWith("px") Then
                valueText = valueText.SubString(0, valueText.Length - 2).Trim()

                If Double.TryParse(valueText, result) Then
                    instance.Height = New GridLength(result, GridUnitType.Pixel)
                Else
                    Throw New FormatException()
                End If

            ElseIf valueText.EndsWith("in") Then
                valueText = valueText.SubString(0, valueText.Length - 2).Trim()

                If Double.TryParse(valueText, result) Then
                    instance.Height = New GridLength(result * 96.0, GridUnitType.Pixel)
                Else
                    Throw New FormatException()
                End If

            ElseIf valueText.EndsWith("cm") Then
                valueText = valueText.SubString(0, valueText.Length - 2).Trim()

                If Double.TryParse(valueText, result) Then
                    instance.Height = New GridLength(result * 96.0 / 2.54, GridUnitType.Pixel)
                Else
                    Throw New FormatException()
                End If

            ElseIf valueText.EndsWith("pt") Then
                valueText = valueText.SubString(0, valueText.Length - 2).Trim()

                If Double.TryParse(valueText, result) Then
                    instance.Height = New GridLength(result * 96.0 / 72.0, GridUnitType.Pixel)
                Else
                    Throw New FormatException()
                End If

            Else
                If Double.TryParse(valueText, result) Then
                    instance.Height = New GridLength(result, GridUnitType.Pixel)
                Else
                    Throw New FormatException()
                End If

            End If
        End Sub

    End Module

End Namespace
]]>
</file>

#Const TEST_REGRESSIONS = False

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

        <Fact>
        Public Sub ComplexCollectionSetter()

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
                                    <Grid.RowDefinitions>
                                        <RowDefinition Height="auto" />
                                        <RowDefinition Height="*" />
                                        <RowDefinition Height="5*" />
                                        <RowDefinition Height="1in" />
                                        <RowDefinition Height="75" />
                                    </Grid.RowDefinitions>
                                </Grid>
                            </Window>

        For Each rd In CType(obj.Content, Grid).RowDefinitions
            Write(rd.Height.ToString())
        Next
    End Sub
End Module
]]>
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="auto*5*9675")

        End Sub

#End If

        <Fact>
        Public Sub ChildTextContent()

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
    WithEvents Button1 As Button

    Sub Main()
        Dim obj As Window = <Window>
                                <Label>+</Label>
                            </Window>
        Write(CType(obj.Content, Label).Text)
    End Sub
End Module
]]>
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="+")

        End Sub

        <Fact>
        Public Sub AttributeWithByRefValue()

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
    WithEvents Button1 As Button

    Sub Main()
        Dim obj As Window = <Window>
                                <Grid>
                                    <Button Name="Button1" />
                                </Grid>
                            </Window>
        Write(Button1 Is Nothing)
    End Sub
End Module
]]>
    </file>
</compilation>, references:=XmlReferences, expectedOutput:="False")

        End Sub


        Shared Function GenerateFilename(<System.Runtime.CompilerServices.CallerMemberName> Optional memberName As String = "") As String
            Return memberName & ".vb"
        End Function

    End Class
End Namespace
