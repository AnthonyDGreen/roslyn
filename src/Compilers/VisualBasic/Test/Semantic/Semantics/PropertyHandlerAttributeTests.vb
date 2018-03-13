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
    Protected Function OnNotifyPropertySet(propertyName As String, ByRef backingField As String, value As String) As Boolean
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



    End Class
End Namespace
