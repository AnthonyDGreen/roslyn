' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports Microsoft.CodeAnalysis.Text
Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Symbols
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace Microsoft.CodeAnalysis.VisualBasic

    ''' <summary>
    ''' Holds additional information needed to rewrite a bound for each node with query extensions.
    ''' </summary>
    Friend NotInheritable Class ForEachRangeInfo

        ''' <summary>
        ''' A list of local "range" variables.
        ''' </summary>
        Public ReadOnly Variables As ImmutableArray(Of LocalSymbol)

        ''' <summary>
        ''' A list of anonymous type fields.
        ''' </summary>
        Public ReadOnly Fields As ImmutableArray(Of BoundExpression)

        ''' <summary>
        ''' Initializes a new instance of the <see cref="ForEachRangeInfo" /> class.
        ''' </summary>
        ''' <param name="variables">A list of local "range" variables.</param>
        ''' <param name="fields">A list of anonymous type fields.</param>
        Public Sub New(
            variables As ImmutableArray(Of LocalSymbol),
            fields As ImmutableArray(Of BoundExpression)
        )
            Me.Variables = variables
            Me.Fields = fields

            Debug.Assert(Me.Variables.Length = Me.Fields.Length)
        End Sub
    End Class
End Namespace
