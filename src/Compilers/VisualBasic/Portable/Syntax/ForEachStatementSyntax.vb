' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace Microsoft.CodeAnalysis.VisualBasic.Syntax
    Partial Public Class ForEachStatementSyntax

        ' This method included for back-compat.
        ''' <summary>
        ''' Returns a copy of this with the specified changes. Returns this instance if
        ''' there are no actual changes.
        ''' </summary>
        ''' <param name="forKeyword">
        ''' The value for the ForKeyword property.
        ''' </param>
        ''' <param name="eachKeyword">
        ''' The value for the EachKeyword property.
        ''' </param>
        ''' <param name="controlVariable">
        ''' The value for the ControlVariable property.
        ''' </param>
        ''' <param name="inKeyword">
        ''' The value for the InKeyword property.
        ''' </param>
        ''' <param name="expression">
        ''' The value for the Expression property.
        ''' </param>
        Public Function Update(forKeyword As SyntaxToken, eachKeyword As SyntaxToken, controlVariable As VisualBasicSyntaxNode, inKeyword As SyntaxToken, expression As ExpressionSyntax) As ForEachStatementSyntax
            Return Update(forKeyword, eachKeyword, controlVariable, inKeyword, expression, CommaToken, AdditionalVariables, QueryClauses)
        End Function

        Friend ReadOnly Property HasQueryExtensions As Boolean
            Get
                Return QueryClauses.Count > 0 OrElse AdditionalVariables.Count > 0 OrElse CommaToken.Node IsNot Nothing
            End Get
        End Property

    End Class
End Namespace

Namespace Microsoft.CodeAnalysis.VisualBasic
    Partial Public Class SyntaxFactory

        ' This method included for back-compat.
        ''' <summary>
        ''' The For Each statement that begins a For Each-Next block. This statement always
        ''' occurs as the Begin of a ForBlock, and the body of the For Each-Next is the
        ''' Body of that ForBlock. Most of the time, the End of that ForBlock is the
        ''' corresponding Next statement. However, multiple nested For statements are ended
        ''' by a single Next statement with multiple variables, then the inner For
        ''' statements will have End set to Nothing, and the Next statement is the End of
        ''' the outermost For statement that is being ended.
        ''' </summary>
        ''' <param name="forKeyword">
        ''' The "For" keyword.
        ''' </param>
        ''' <param name="eachKeyword">
        ''' The "Each" keyword.
        ''' </param>
        ''' <param name="controlVariable">
        ''' If the For or For Each statement is of a form that does not declare a new loop
        ''' control variable, this is the expression that denotes the loop control
        ''' variable. If this loop is of a form that does declare a new control variable,
        ''' this is a VariableDeclarator that has the variable being declared.
        ''' </param>
        ''' <param name="inKeyword">
        ''' The "In" keyword.
        ''' </param>
        ''' <param name="expression">
        ''' The expression denoting the collection to iterate over.
        ''' </param>
        Public Shared Function ForEachStatement(forKeyword As SyntaxToken, eachKeyword As SyntaxToken, controlVariable As VisualBasicSyntaxNode, inKeyword As SyntaxToken, expression As ExpressionSyntax) As ForEachStatementSyntax
            Return ForEachStatement(forKeyword, eachKeyword, controlVariable, inKeyword, expression, commaToken:=Nothing, additionalVariables:=Nothing, queryClauses:=Nothing)
        End Function

        ' This method included for back-compat.
        ''' <summary>
        ''' The For Each statement that begins a For Each-Next block. This statement always
        ''' occurs as the Begin of a ForBlock, and the body of the For Each-Next is the
        ''' Body of that ForBlock. Most of the time, the End of that ForBlock is the
        ''' corresponding Next statement. However, multiple nested For statements are ended
        ''' by a single Next statement with multiple variables, then the inner For
        ''' statements will have End set to Nothing, and the Next statement is the End of
        ''' the outermost For statement that is being ended.
        ''' </summary>
        ''' <param name="controlVariable">
        ''' If the For or For Each statement is of a form that does not declare a new loop
        ''' control variable, this is the expression that denotes the loop control
        ''' variable. If this loop is of a form that does declare a new control variable,
        ''' this is a VariableDeclarator that has the variable being declared.
        ''' </param>
        ''' <param name="expression">
        ''' The expression denoting the collection to iterate over.
        ''' </param>
        Public Shared Function ForEachStatement(controlVariable As VisualBasicSyntaxNode, expression As ExpressionSyntax) As ForEachStatementSyntax
            Return ForEachStatement(controlVariable, expression, additionalVariables:=Nothing, queryClauses:=Nothing)
        End Function

    End Class
End Namespace
