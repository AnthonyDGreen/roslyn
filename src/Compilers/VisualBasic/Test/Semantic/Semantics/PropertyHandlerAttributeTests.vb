' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Microsoft.CodeAnalysis.VisualBasic.SyntaxFacts
Imports Roslyn.Test.Utilities

Namespace Microsoft.CodeAnalysis.VisualBasic.UnitTests.Semantics

    Public Class PropertyHandlerAttributeTests
        Inherits BasicTestBase

        <Fact>
        Public Sub SimpleNotifyHandler()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="a.vb">
Imports System
Imports System.Diagnostics
Imports System.Console

Namespace Global.System.Runtime.CompilerServices
    Public MustInherit Class PropertyHandlerAttribute
        Inherits Attribute
    End Class
End Namespace

Public Class NotifyAttribute
    Inherits Runtime.CompilerServices.PropertyHandlerAttribute
End Class

Class ListingInfo
    &lt;Notify&gt;
    Property Title As String = "New Listing"

    &lt;Notify&gt;
    Property Description As String = "No description."

    Property Silent As Boolean

    #Disable Warning BC42353 ' Intentionally letting the function return 'False' by default.
    Protected Function NotifyOnPropertySet(propertyName As String, ByRef backingField As String, value As String) As Boolean
        If value = backingField Then Return True

        RaiseEvent PropertyChanged(propertyName)
    End Function

    Event PropertyChanged(propertyName As String)    
End Class

Module Program
    Sub Main()
        Dim listing = New ListingInfo
        AddHandler listing.PropertyChanged, Sub(pName) Write($"Property '{pName}' changed.")

        listing.Title = listing.Title

        listing.Title = "NEW! Vibranium Earrings!"
        listing.Description = "The latest in Wakandan fashion. Get it while supplies last!"
        listing.Silent = True

        Write(listing.Title = "New Listing")
    End Sub
End Module
    </file>
</compilation>, expectedOutput:="Property 'Title' changed.Property 'Description' changed.False")

        End Sub

        <Fact>
        Public Sub WrapperHandler()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="a.vb">
Imports System
Imports System.Collections.Generic
Imports System.Diagnostics
Imports System.Console

MustInherit Class PropertyHandlerAttribute
    Inherits Attribute
End Class

Class WrapperAttribute
    Inherits PropertyHandlerAttribute
End Class

Class ListingInfo
    Public ReadOnly ViewState As IDictionary(Of String, Object) = New Dynamic.ExpandoObject()

    Protected Sub WrapperOnPropertyGet(Of T)(propertyName As String, ByRef backingField As T, ByRef value As T)
        value = ViewState(propertyName)
    End Sub

    Protected Function WrapperOnPropertySet(Of T)(propertyName As String, ByRef backingField As T, ByRef value As T) As Boolean
        ViewState(propertyName) = value

        ' Never store values in the backing field.
        Return True
    End Function

    &lt;Wrapper&gt;
    Property Rating As Integer = 3

    &lt;Wrapper&gt;
    Property Category As String = "General"

    &lt;Wrapper&gt;
    Property Subcategory As String = "&lt;None&gt;"

    Function BackingFieldsToString() As String
        Return _Rating &amp; _Category &amp; _Subcategory
    End Function

End Class

Module Program
    Sub Main()
        Dim listing = New ListingInfo
        Write(listing.BackingFieldsToString())
        Write(listing.Rating &amp; listing.Category &amp; listing.Subcategory)
        Write(listing.ViewState!Rating &amp; listing.ViewState!Category &amp; listing.ViewState!Subcategory)
    End Sub
End Module
    </file>
</compilation>, expectedOutput:="03General<None>3General<None>")

        End Sub
    End Class
End Namespace
