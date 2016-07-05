' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Roslyn.Test.Utilities

Namespace Microsoft.CodeAnalysis.VisualBasic.UnitTests.Semantics
    Public Class SelectCaseTests
        Inherits BasicTestBase
        <Fact()>
        Public Sub SelectCaseExpression_NothingLiteral()
            Dim compilation = CreateCompilationWithMscorlib(
        <compilation>
            <file name="a.vb"><![CDATA[
Public Module M
    Sub SelectCaseExpression()
        Select Case Nothing'BIND:"Nothing"
        End Select
    End Sub
End Module
    ]]></file>
        </compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of LiteralExpressionSyntax)(compilation, "a.vb")

            Assert.Null(semanticSummary.Type)
            Assert.Equal("System.Object", semanticSummary.ConvertedType.ToTestDisplayString())
            Assert.Equal(TypeKind.Class, semanticSummary.ConvertedType.TypeKind)
            Assert.Equal(ConversionKind.WideningNothingLiteral, semanticSummary.ImplicitConversion.Kind)

            Assert.Null(semanticSummary.Symbol)
            Assert.Equal(CandidateReason.None, semanticSummary.CandidateReason)
            Assert.Equal(0, semanticSummary.CandidateSymbols.Length)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(0, semanticSummary.MemberGroup.Length)

            Assert.True(semanticSummary.ConstantValue.HasValue)
            Assert.Null(semanticSummary.ConstantValue.Value)
        End Sub

        <Fact()>
        Public Sub SelectCaseExpression_Literal()
            Dim compilation = CreateCompilationWithMscorlib(
        <compilation>
            <file name="a.vb"><![CDATA[
Public Module M
    Sub SelectCaseExpression()
        Select Case 1.1'BIND:"1.1"
        End Select
    End Sub
End Module
    ]]></file>
        </compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of LiteralExpressionSyntax)(compilation, "a.vb")

            Assert.Equal("System.Double", semanticSummary.Type.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.Type.TypeKind)
            Assert.Equal("System.Double", semanticSummary.ConvertedType.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.ConvertedType.TypeKind)
            Assert.Equal(ConversionKind.Identity, semanticSummary.ImplicitConversion.Kind)

            Assert.Null(semanticSummary.Symbol)
            Assert.Equal(CandidateReason.None, semanticSummary.CandidateReason)
            Assert.Equal(0, semanticSummary.CandidateSymbols.Length)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(0, semanticSummary.MemberGroup.Length)

            Assert.True(semanticSummary.ConstantValue.HasValue)
            Assert.Equal(1.1, semanticSummary.ConstantValue.Value)
        End Sub

        <Fact()>
        Public Sub SelectCaseExpression_Local_IdentifierNameSyntax()
            Dim compilation = CreateCompilationWithMscorlib(
        <compilation>
            <file name="a.vb"><![CDATA[
Imports System
Module M1
    Sub SelectCaseExpression()
        Dim number As Integer = 0
        Select Case number'BIND:"number"
        End Select
    End Sub
Ehd Module
    ]]></file>
        </compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of IdentifierNameSyntax)(compilation, "a.vb")

            Assert.Equal("System.Int32", semanticSummary.Type.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.Type.TypeKind)
            Assert.Equal("System.Int32", semanticSummary.ConvertedType.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.ConvertedType.TypeKind)
            Assert.Equal(ConversionKind.Identity, semanticSummary.ImplicitConversion.Kind)

            Assert.Equal("number As System.Int32", semanticSummary.Symbol.ToTestDisplayString())
            Assert.Equal(SymbolKind.Local, semanticSummary.Symbol.Kind)
            Assert.Equal(0, semanticSummary.CandidateSymbols.Length)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(0, semanticSummary.MemberGroup.Length)

            Assert.False(semanticSummary.ConstantValue.HasValue)
        End Sub

        <Fact()>
        Public Sub SelectCaseExpression_MethodCall_InvocationExpressionSyntax()
            Dim compilation = CreateCompilationWithMscorlib(
        <compilation>
            <file name="a.vb"><![CDATA[
Imports System
Module M1
    Function Foo() As Integer
        Console.WriteLine("Foo")
        Return 0
    End Function

    Sub SelectCaseExpression()
        Select Case Foo()'BIND:"Foo()"
        End Select
    End Sub
End Module
    ]]></file>
        </compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of InvocationExpressionSyntax)(compilation, "a.vb")

            Assert.Equal("System.Int32", semanticSummary.Type.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.Type.TypeKind)
            Assert.Equal("System.Int32", semanticSummary.ConvertedType.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.ConvertedType.TypeKind)
            Assert.Equal(ConversionKind.Identity, semanticSummary.ImplicitConversion.Kind)

            Assert.Equal("Function M1.Foo() As System.Int32", semanticSummary.Symbol.ToTestDisplayString())
            Assert.Equal(SymbolKind.Method, semanticSummary.Symbol.Kind)
            Assert.Equal(0, semanticSummary.CandidateSymbols.Length)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(0, semanticSummary.MemberGroup.Length)

            Assert.False(semanticSummary.ConstantValue.HasValue)
        End Sub

        <Fact()>
        Public Sub SelectCaseExpression_MethodCall_IdentifierNameSyntax()
            Dim compilation = CreateCompilationWithMscorlib(
        <compilation>
            <file name="a.vb"><![CDATA[
Imports System
Module M1
    Function Foo() As Integer
        Console.WriteLine("Foo")
        Return 0
    End Function

    Sub SelectCaseExpression()
        Select Case Foo()'BIND:"Foo"
        End Select
    End Sub
End Module
    ]]></file>
        </compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of IdentifierNameSyntax)(compilation, "a.vb")

            Assert.Null(semanticSummary.Type)
            Assert.Null(semanticSummary.ConvertedType)
            Assert.Equal(ConversionKind.Identity, semanticSummary.ImplicitConversion.Kind)

            Assert.Equal("Function M1.Foo() As System.Int32", semanticSummary.Symbol.ToTestDisplayString())
            Assert.Equal(SymbolKind.Method, semanticSummary.Symbol.Kind)
            Assert.Equal(0, semanticSummary.CandidateSymbols.Length)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(1, semanticSummary.MemberGroup.Length)
            Dim sortedMethodGroup = semanticSummary.MemberGroup.AsEnumerable().OrderBy(Function(s) s.ToTestDisplayString()).ToArray()
            Assert.Equal("Function M1.Foo() As System.Int32", sortedMethodGroup(0).ToTestDisplayString())

            Assert.False(semanticSummary.ConstantValue.HasValue)
        End Sub

        <Fact()>
        Public Sub SelectCaseExpression_Lambda()
            Dim compilation = CreateCompilationWithMscorlib(
        <compilation>
            <file name="a.vb"><![CDATA[
Public Module M
    Sub SelectCaseExpression()
        Select Case (Function(arg) arg Is Nothing)'BIND:"Function(arg) arg Is Nothing"
        End Select
    End Sub
End Module
    ]]></file>
        </compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of SingleLineLambdaExpressionSyntax)(compilation, "a.vb")

            Assert.Null(semanticSummary.Type)
            Assert.Equal("Function <generated method>(arg As System.Object) As System.Boolean", semanticSummary.ConvertedType.ToTestDisplayString())
            Assert.Equal(TypeKind.Delegate, semanticSummary.ConvertedType.TypeKind)
            Assert.Equal(ConversionKind.Widening Or ConversionKind.Lambda, semanticSummary.ImplicitConversion.Kind)

            Assert.Equal("Function (arg As System.Object) As System.Boolean", semanticSummary.Symbol.ToTestDisplayString())
            Assert.Equal(SymbolKind.Method, semanticSummary.Symbol.Kind)
            Assert.Equal(0, semanticSummary.CandidateSymbols.Length)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(0, semanticSummary.MemberGroup.Length)

            Assert.False(semanticSummary.ConstantValue.HasValue)
        End Sub

        <Fact()>
        Public Sub SelectCaseExpression_ParenthesizedLambda()
            Dim compilation = CreateCompilationWithMscorlib(
        <compilation>
            <file name="a.vb"><![CDATA[
Public Module M
    Sub SelectCaseExpression()
        Select Case (Function(arg) arg Is Nothing)'BIND:"(Function(arg) arg Is Nothing)"
        End Select
    End Sub
End Module
    ]]></file>
        </compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of ParenthesizedExpressionSyntax)(compilation, "a.vb")

            Assert.Equal("Function <generated method>(arg As System.Object) As System.Boolean", semanticSummary.Type.ToTestDisplayString())
            Assert.Equal(TypeKind.Delegate, semanticSummary.Type.TypeKind)
            Assert.Equal("Function <generated method>(arg As System.Object) As System.Boolean", semanticSummary.ConvertedType.ToTestDisplayString())
            Assert.Equal(TypeKind.Delegate, semanticSummary.ConvertedType.TypeKind)
            Assert.Equal(ConversionKind.Identity, semanticSummary.ImplicitConversion.Kind)

            Assert.Null(semanticSummary.Symbol)
            Assert.Equal(CandidateReason.None, semanticSummary.CandidateReason)
            Assert.Equal(0, semanticSummary.CandidateSymbols.Length)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(0, semanticSummary.MemberGroup.Length)

            Assert.False(semanticSummary.ConstantValue.HasValue)
        End Sub

        <Fact()>
        Public Sub SelectCaseExpression_Error_NotAValue_InvocationExpressionSyntax()
            Dim compilation = CreateCompilationWithMscorlib(
        <compilation>
            <file name="a.vb"><![CDATA[
Imports System
Module M1
    Sub Foo()
    End Sub

    Sub SelectCaseExpression(number As Integer)
        Select Case Foo()'BIND:"Foo()"
        End Select
    End Sub
End Module
    ]]></file>
        </compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of InvocationExpressionSyntax)(compilation, "a.vb")

            Assert.Equal("System.Void", semanticSummary.Type.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.Type.TypeKind)
            Assert.Equal("System.Void", semanticSummary.ConvertedType.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.ConvertedType.TypeKind)
            Assert.Equal(ConversionKind.Identity, semanticSummary.ImplicitConversion.Kind)

            Assert.Null(semanticSummary.Symbol)
            Assert.Equal(CandidateReason.NotAValue, semanticSummary.CandidateReason)
            Assert.Equal(1, semanticSummary.CandidateSymbols.Length)
            Dim sortedCandidates = semanticSummary.CandidateSymbols.AsEnumerable().OrderBy(Function(s) s.ToTestDisplayString()).ToArray()
            Assert.Equal("Sub M1.Foo()", sortedCandidates(0).ToTestDisplayString())
            Assert.Equal(SymbolKind.Method, sortedCandidates(0).Kind)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(0, semanticSummary.MemberGroup.Length)

            Assert.False(semanticSummary.ConstantValue.HasValue)
        End Sub

        <Fact()>
        Public Sub SelectCaseExpression_Error_NotAValue_IdentifierNameSyntax()
            Dim compilation = CreateCompilationWithMscorlib(
        <compilation>
            <file name="a.vb"><![CDATA[
Imports System
Module M1
    Sub Foo()
    End Sub

    Sub SelectCaseExpression(number As Integer)
        Select Case Foo'BIND:"Foo"
        End Select
    End Sub
End Module
    ]]></file>
        </compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of IdentifierNameSyntax)(compilation, "a.vb")

            Assert.Equal("System.Void", semanticSummary.Type.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.Type.TypeKind)
            Assert.Equal("System.Void", semanticSummary.ConvertedType.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.ConvertedType.TypeKind)
            Assert.Equal(ConversionKind.Identity, semanticSummary.ImplicitConversion.Kind)

            Assert.Null(semanticSummary.Symbol)
            Assert.Equal(CandidateReason.NotAValue, semanticSummary.CandidateReason)
            Assert.Equal(1, semanticSummary.CandidateSymbols.Length)
            Dim sortedCandidates = semanticSummary.CandidateSymbols.AsEnumerable().OrderBy(Function(s) s.ToTestDisplayString()).ToArray()
            Assert.Equal("Sub M1.Foo()", sortedCandidates(0).ToTestDisplayString())
            Assert.Equal(SymbolKind.Method, sortedCandidates(0).Kind)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(0, semanticSummary.MemberGroup.Length)

            Assert.False(semanticSummary.ConstantValue.HasValue)
        End Sub

        <Fact()>
        Public Sub SelectCaseExpression_Error_OverloadResolutionFailure()
            Dim compilation = CreateCompilationWithMscorlib(
        <compilation>
            <file name="a.vb"><![CDATA[
Imports System
Module M1
    Sub Foo(i As Integer)
    End Sub

    Sub SelectCaseExpression(number As Integer)
        Select Case Foo'BIND:"Foo"
        End Select
    End Sub
End Module
    ]]></file>
        </compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of IdentifierNameSyntax)(compilation, "a.vb")

            Assert.Null(semanticSummary.Type)
            Assert.Equal("System.Void", semanticSummary.ConvertedType.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.ConvertedType.TypeKind)
            Assert.Equal(ConversionKind.Identity, semanticSummary.ImplicitConversion.Kind)

            Assert.Null(semanticSummary.Symbol)
            Assert.Equal(CandidateReason.OverloadResolutionFailure, semanticSummary.CandidateReason)
            Assert.Equal(1, semanticSummary.CandidateSymbols.Length)
            Dim sortedCandidates = semanticSummary.CandidateSymbols.AsEnumerable().OrderBy(Function(s) s.ToTestDisplayString()).ToArray()
            Assert.Equal("Sub M1.Foo(i As System.Int32)", sortedCandidates(0).ToTestDisplayString())
            Assert.Equal(SymbolKind.Method, sortedCandidates(0).Kind)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(1, semanticSummary.MemberGroup.Length)
            Dim sortedMethodGroup = semanticSummary.MemberGroup.AsEnumerable().OrderBy(Function(s) s.ToTestDisplayString()).ToArray()
            Assert.Equal("Sub M1.Foo(i As System.Int32)", sortedMethodGroup(0).ToTestDisplayString())

            Assert.False(semanticSummary.ConstantValue.HasValue)
        End Sub

        <Fact()>
        Public Sub SelectCase_RelationalCaseClauseExpression_Literal()
            Dim compilation = CreateCompilationWithMscorlib(
        <compilation>
            <file name="a.vb"><![CDATA[
Imports System
Module M1
    Sub RelationalCaseClauseExpression(number As Integer)
        Select Case number
            Case Is < 1'BIND:"1"
        End Select
    End Sub
End Module
    ]]></file>
        </compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of LiteralExpressionSyntax)(compilation, "a.vb")

            Assert.Equal("System.Int32", semanticSummary.Type.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.Type.TypeKind)
            Assert.Equal("System.Int32", semanticSummary.ConvertedType.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.ConvertedType.TypeKind)
            Assert.Equal(ConversionKind.Identity, semanticSummary.ImplicitConversion.Kind)

            Assert.Null(semanticSummary.Symbol)
            Assert.Equal(CandidateReason.None, semanticSummary.CandidateReason)
            Assert.Equal(0, semanticSummary.CandidateSymbols.Length)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(0, semanticSummary.MemberGroup.Length)

            Assert.True(semanticSummary.ConstantValue.HasValue)
            Assert.Equal(1, semanticSummary.ConstantValue.Value)
        End Sub

        <Fact()>
        Public Sub SelectCase_RangeCaseClauseExpression_MethodCall()
            Dim compilation = CreateCompilationWithMscorlib(
        <compilation>
            <file name="a.vb"><![CDATA[
Imports System
Module M1
    Function Foo() As Integer
        Return 0
    End Function

    Sub RangeCaseClauseExpression(number As Integer)
        Select Case number
            Case Foo() To 1'BIND:"Foo()"
        End Select
    End Sub
End Module
    ]]></file>
        </compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of InvocationExpressionSyntax)(compilation, "a.vb")

            Assert.Equal("System.Int32", semanticSummary.Type.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.Type.TypeKind)
            Assert.Equal("System.Int32", semanticSummary.ConvertedType.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.ConvertedType.TypeKind)
            Assert.Equal(ConversionKind.Identity, semanticSummary.ImplicitConversion.Kind)

            Assert.Equal("Function M1.Foo() As System.Int32", semanticSummary.Symbol.ToTestDisplayString())
            Assert.Equal(SymbolKind.Method, semanticSummary.Symbol.Kind)
            Assert.Equal(0, semanticSummary.CandidateSymbols.Length)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(0, semanticSummary.MemberGroup.Length)

            Assert.False(semanticSummary.ConstantValue.HasValue)
        End Sub

        <Fact()>
        Public Sub SelectCase_SimpleCaseClauseExpression_DateTime()
            Dim compilation = CreateCompilationWithMscorlib(
        <compilation>
            <file name="a.vb"><![CDATA[
Imports System
Module M1
    Function Foo() As Integer
        Return 0
    End Function

    Sub SimpleCaseClauseExpression(number As Integer)
        Select Case number
            Case #8/13/2002 12:14 PM#'BIND:"#8/13/2002 12:14 PM#"
        End Select
    End Sub
End Module
    ]]></file>
        </compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of LiteralExpressionSyntax)(compilation, "a.vb")

            Assert.Equal("System.DateTime", semanticSummary.Type.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.Type.TypeKind)
            Assert.Equal("System.Int32", semanticSummary.ConvertedType.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.ConvertedType.TypeKind)
            Assert.Equal(ConversionKind.DelegateRelaxationLevelNone, semanticSummary.ImplicitConversion.Kind)

            Assert.Null(semanticSummary.Symbol)
            Assert.Equal(CandidateReason.None, semanticSummary.CandidateReason)
            Assert.Equal(0, semanticSummary.CandidateSymbols.Length)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(0, semanticSummary.MemberGroup.Length)

            Assert.True(semanticSummary.ConstantValue.HasValue)
            Assert.Equal(#8/13/2002 12:14:00 PM#, semanticSummary.ConstantValue.Value)
        End Sub

        <WorkItem(543098, "http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/543098")>
        <Fact()>
        Public Sub SelectCase_BoundLocal()
            Dim compilation = CreateCompilationWithMscorlib(
<compilation>
    <file name="a.vb"><![CDATA[
Imports System

Class Program
    Sub Test()
        Dim i As Integer = 10
        Select Case i'BIND:"i"
            Case NewMethod()
                Console.Write(5)
        End Select
    End Sub

    Private Shared Function NewMethod() As Integer
        Return 5
    End Function
End Class
    ]]></file>
</compilation>)

            Dim semanticSummary = CompilationUtils.GetSemanticInfoSummary(Of IdentifierNameSyntax)(compilation, "a.vb")

            Assert.Equal("System.Int32", semanticSummary.Type.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.Type.TypeKind)
            Assert.Equal("System.Int32", semanticSummary.ConvertedType.ToTestDisplayString())
            Assert.Equal(TypeKind.Structure, semanticSummary.ConvertedType.TypeKind)
            Assert.Equal(ConversionKind.Identity, semanticSummary.ImplicitConversion.Kind)

            Assert.Equal("i As System.Int32", semanticSummary.Symbol.ToTestDisplayString())
            Assert.Equal(SymbolKind.Local, semanticSummary.Symbol.Kind)
            Assert.Equal(0, semanticSummary.CandidateSymbols.Length)

            Assert.Null(semanticSummary.Alias)

            Assert.Equal(0, semanticSummary.MemberGroup.Length)

            Assert.False(semanticSummary.ConstantValue.HasValue)
        End Sub

        <WorkItem(543387, "http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/543387")>
        <Fact()>
        Public Sub SelectCase_AnonymousLambda()
            Dim compilation = CreateCompilationWithMscorlibAndVBRuntime(
<compilation>
    <file name="a.vb"><![CDATA[
Module Program
    Sub Main()
        Select Case Nothing
            Case Function() 5
                System.Console.WriteLine("Failed")
            Case Else
                System.Console.WriteLine("Succeeded")
        End Select
    End Sub
End Module
    ]]></file>
</compilation>, options:=TestOptions.ReleaseExe.WithOptionStrict(OptionStrict.Custom))

            CompileAndVerify(compilation, expectedOutput:=
            <![CDATA[
Succeeded
]]>)

            AssertTheseDiagnostics(compilation,
<expected>
BC42036: Operands of type Object used in expressions for 'Select', 'Case' statements; runtime errors could occur.
        Select Case Nothing
                    ~~~~~~~
BC42016: Implicit conversion from 'Object' to 'Boolean'.
            Case Function() 5
                 ~~~~~~~~~~~~
</expected>)
        End Sub

        <WorkItem(948019, "http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/948019")>
        <Fact()>
        Public Sub Bug948019_01()
            Dim compilation = CreateCompilationWithMscorlib(
<compilation>
    <file name="a.vb"><![CDATA[
Class C
    Public Sub M(day As DayOfWeek)
        Dim day2 = day
        Select Case day 'BIND:"day"
            Case DayOfWeek.A
            Case 
        End Select
    End Sub
    Enum DayOfWeek
        A
        B
    End Enum
End Class
    ]]></file>
</compilation>)

            Dim node = CompilationUtils.FindBindingText(Of IdentifierNameSyntax)(compilation, "a.vb")
            Dim semanticModel = compilation.GetSemanticModel(node.SyntaxTree)

            Dim typeInfo = semanticModel.GetTypeInfo(node)

            Assert.Equal("C.DayOfWeek", typeInfo.Type.ToTestDisplayString())
            Assert.Equal("C.DayOfWeek", typeInfo.ConvertedType.ToTestDisplayString())
            Assert.Equal(ConversionKind.Identity, semanticModel.GetConversion(node).Kind)

            Dim symbolInfo = semanticModel.GetSymbolInfo(node)

            Assert.Equal("day As C.DayOfWeek", symbolInfo.Symbol.ToTestDisplayString())
            Assert.Equal(SymbolKind.Parameter, symbolInfo.Symbol.Kind)
        End Sub

        <WorkItem(948019, "http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/948019")>
        <Fact()>
        Public Sub Bug948019_02()
            Dim compilation = CreateCompilationWithMscorlib(
<compilation>
    <file name="a.vb"><![CDATA[
Class C
    Public Sub M(day As DayOfWeek)
        Dim day2 = day
        Select Case day 'BIND:"day"
            Case DayOfWeek.A
            Case 2
        End Select
    End Sub
    Enum DayOfWeek
        A
        B
    End Enum
End Class
    ]]></file>
</compilation>)

            Dim node = CompilationUtils.FindBindingText(Of IdentifierNameSyntax)(compilation, "a.vb")
            Dim semanticModel = compilation.GetSemanticModel(node.SyntaxTree)

            Dim typeInfo = semanticModel.GetTypeInfo(node)

            Assert.Equal("C.DayOfWeek", typeInfo.Type.ToTestDisplayString())
            Assert.Equal("C.DayOfWeek", typeInfo.ConvertedType.ToTestDisplayString())
            Assert.Equal(ConversionKind.Identity, semanticModel.GetConversion(node).Kind)

            Dim symbolInfo = semanticModel.GetSymbolInfo(node)

            Assert.Equal("day As C.DayOfWeek", symbolInfo.Symbol.ToTestDisplayString())
            Assert.Equal(SymbolKind.Parameter, symbolInfo.Symbol.Kind)
        End Sub

        ' Here are the cases we have to deal with.
        ' The narrowing conversions are most interesting to users.
        ' We don't report a warning for either widening or identity reference conversions because
        ' such patterns are valid for null-checking(+).
        ' Reference type -> value type (narrowing value (unboxing))
        ' * Reference type -> nullable value type
        ' * Reference type -> non-nullable value type
        ' Reference type -> reference type (narrowing reference)
        ' Reference type -> reference type (widening reference)
        ' Reference type -> reference type (identity)
        ' Value type -> reference type (widening value (boxing))
        ' Value type -> value type
        ' * Value type -> nullable of that value type (widening nullable)
        ' * Value type -> value type (identity)
        ' * Nullable value type -> underlying value type (narrowing nullable)
        ' * Nullable value type -> nullable value type (identity) (+)
        ' Type parameter type without class-constraint -> any type (error)
        ' Any type -> unconstrained type parameter type (error)
        ' (+) We report a warning in this case since it's not even useful for null-checking.

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ReferenceTypeToReferenceType()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="Program.vb">
Imports System
Imports System.Console
Imports System.Collections
Imports System.Collections.Generic
Imports System.Linq

Module Program
    Sub Main()
        '?70121
        M({Nothing, New String("*", 7), New ArrayList(), New List(Of String) From {"Hi"}, ({"Hello", "Goodbye"}), 1})
    End Sub

    Sub M(ParamArray args As Object())
        For Each obj In args
            Select Case obj
                Case i As String
                    Write(i.Length)
                Case j As String()
                    Write(j.Length)
                Case k As List(Of String)
                    Write(k.Count)
                Case l As IEnumerable
                    Write(l.Cast(Of Object).Count())
                Case m As Object
                    Write(m)
                Case Else
                    Write("?")
            End Select
        Next
    End Sub
End Module
    </file>
</compilation>,
expectedOutput:="?70121")

            AssertNoDiagnostics(CType(verifier.Compilation, VisualBasicCompilation))

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ERR_TypeCaseUnnecessaryNullableType()

            Dim compilation = CreateCompilationWithMscorlibAndVBRuntime(
<compilation>
    <file name="Program.vb">
Imports System
Imports System.Console
Imports System.Collections
Imports System.Collections.Generic

Module Program
    Sub Main()
        '???S2???I1Monday
        M(Nothing, New String("*", 7), New ArrayList(), 2S, 3L, New List(Of String) From {"Hi"}, ({"Hello", "Goodbye"}), 1, DayOfWeek.Monday)
    End Sub

    Sub M(ParamArray args As Object())
        For Each obj In args
            Select Case obj
                Case i As Integer?
                    Write("I" &amp; i.Value)
                Case j As Short?
                    Write("S" &amp; j.Value)
                Case k As DayOfWeek?
                    Write(k.Value.ToString())
                Case Else
                    Write("?")
            End Select
        Next
    End Sub
End Module
    </file>
</compilation>)

            AssertTheseDiagnostics(compilation,
<expected>
BC37251: Type must be a reference type, type parameter type, or non-nullable value type.
                Case i As Integer?
                          ~~~~~~~~
BC37251: Type must be a reference type, type parameter type, or non-nullable value type.
                Case j As Short?
                          ~~~~~~
BC37251: Type must be a reference type, type parameter type, or non-nullable value type.
                Case k As DayOfWeek?
                          ~~~~~~~~~~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ERR_TypeCaseUnnecessaryNullableType_TypeParameter()

            Dim compilation = CreateCompilationWithMscorlibAndVBRuntime(
<compilation>
    <file name="Program.vb">
Imports System
Imports System.Console

Module Program
    Sub Main()
        '?T1??S6
        M(Of Integer, Short)(DayOfWeek.Monday, 1, 4L, Date.Today, 6S)
    End Sub

    Sub M(Of T As Structure, S As Structure)(ParamArray args As ValueType())
        For Each obj In args
            Select Case obj
                Case i As T?
                    Write("T" &amp; i.Value.ToString())
                Case j As S?
                    Write("S" &amp; j.Value.ToString())
                Case Else
                    Write("?")
            End Select
        Next
    End Sub
End Module
    </file>
</compilation>)

            AssertTheseDiagnostics(compilation,
<expected>
BC37251: Type must be a reference type, type parameter type, or non-nullable value type.
                Case i As T?
                          ~~
BC37251: Type must be a reference type, type parameter type, or non-nullable value type.
                Case j As S?
                          ~~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ReferenceTypeToNonNullableValueType()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="Program.vb">
Imports System
Imports System.Console
Imports System.Collections
Imports System.Collections.Generic
Imports System.Linq

Module Program
    Sub Main()
        '?T1??S6
        M(Nothing, New String("*", 7), New ArrayList(), 2S, 3L, New List(Of String) From {"Hi"}, ({"Hello", "Goodbye"}), 1, DayOfWeek.Monday)
    End Sub

    Sub M(ParamArray args As Object())
        For Each obj In args
            Select Case obj
                Case i As Integer
                    Write("I" &amp; i)
                Case j As Short
                    Write("S" &amp; j)
                Case k As DayOfWeek
                    Write(k.ToString())
                Case Else
                    Write("?")
            End Select
        Next
    End Sub
End Module
    </file>
</compilation>,
expectedOutput:="???S2???I1Monday")

            AssertNoDiagnostics(CType(verifier.Compilation, VisualBasicCompilation))

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClauseInterfaceTypeToNonNullableValueType()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="Program.vb">
Option Strict On

Imports System

Module Module1
    Sub Main()
        Dim os1 As New S1
        Dim os2 As New S2
        Dim x As Object = os1
        Dim y As I = os2
        Select Case y
            Case s1 As S1
                Console.WriteLine("S1")
                Console.WriteLine(s1.x)
                s1.Mutate()
                Console.WriteLine(s1.x)
            Case s2 As S2
                Console.WriteLine("S2")
                Console.WriteLine(s2.x)
                s2.Mutate()
                Console.WriteLine(s2.x)
            Case Else
                Console.WriteLine("else")
        End Select
        Console.WriteLine("os1: {0}", os1.x)
        Console.WriteLine("os2: {0}", os2.x)
    End Sub
End Module

Interface I
    Sub Mutate()
End Interface

Structure S1 : Implements I
    Public x As Integer
    Sub Mutate() Implements I.Mutate
        x += 1
    End Sub
End Structure

Structure S2 : Implements I
    Public x As Integer
    Sub Mutate() Implements I.Mutate
        x += 1
    End Sub
End Structure
    </file>
</compilation>)

            AssertNoDiagnostics(CType(verifier.Compilation, VisualBasicCompilation))

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ReferenceTypeToNonNullableValueType_TypeParameter()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="Program.vb">
Imports System
Imports System.Console

Module Program
    Sub Main()
        '?T1??S6
        M(Of Integer, Short)(DayOfWeek.Monday, 1, 4L, Date.Today, 6S)
    End Sub

    Sub M(Of T As Structure, S As Structure)(ParamArray args As ValueType())
        For Each obj In args
            Select Case obj
                Case i As T
                    Write("T" &amp; i.ToString())
                Case j As S
                    Write("S" &amp; j.ToString())
                Case Else
                    Write("?")
            End Select
        Next
    End Sub
End Module
    </file>
</compilation>,
expectedOutput:="?T1??S6")

            AssertNoDiagnostics(CType(verifier.Compilation, VisualBasicCompilation))

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ValueTypeToReferenceType()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="Program.vb">
Imports System
Imports System.Console

Module Program
    Sub Main()
        ' John Doe, 1920-01-01 - 01/01/2010 00:00:00
        M("John Doe", #1920-01-01#, #2010-01-01#)
        ' Methuselah Doe, 1920-01-01 - 01/01/0001 00:00:00
        M("Methuselah Doe", #1920-01-01#, Nothing)
    End Sub

    Sub M(name As String, dateOfBirth As Date, dateOfDeath As Date)

        Write(name &amp; ", " &amp; dateOfBirth &amp; " - ")
        
        Select Case dateOfDeath
            Case some As ValueType
                Write(some)
            Case Else
                Return
        End Select
    End Sub
End Module
    </file>
</compilation>,
expectedOutput:="John Doe, 1920-01-01 - 01/01/2010 00:00:00Methuselah Doe, 1920-01-01 - 01/01/0001 00:00:00")

            AssertNoDiagnostics(CType(verifier.Compilation, VisualBasicCompilation))

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ValueTypeToValueType_NarrowingNullable()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="Program.vb">
Imports System.Console

Module Program
    Sub Main()
        ' John Doe, 1920-01-01 - 2010-01-01
        M("John Doe", #1920-01-01#, #2010-01-01#)
        ' Methuselah Doe, 1920-01-01 - 
        M("Methuselah Doe", #1920-01-01#, Nothing)
    End Sub

    Sub M(name As String, dateOfBirth As Date, dateOfDeath As Date?)

        Write(name &amp; ", " &amp; dateOfBirth &amp; " - ")
        
        Select Case dateOfDeath
            Case some As Date
                Write(some.ToString("yyyy-MM-dd"))
            Case Else
                Return
        End Select
    End Sub
End Module
    </file>
</compilation>,
expectedOutput:="John Doe, 1920-01-01 - 2010-01-01Methuselah Doe, 1920-01-01 - ")

            AssertNoDiagnostics(CType(verifier.Compilation, VisualBasicCompilation))

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ERR_TypeCaseUnnecessaryNullableType_ValueTypeToValueType_WideningNullable()

            Dim compilation = CreateCompilationWithMscorlibAndVBRuntime(
<compilation>
    <file name="Program.vb">
Imports System.Console

Module Program
    Sub Main()
        ' John Doe, 1920-01-01 - 2010-01-01
        M("John Doe", #1920-01-01#, #2010-01-01#)
        ' Methuselah Doe, 1920-01-01 - 0001-01-01
        M("Methuselah Doe", #1920-01-01#, Nothing)
    End Sub

    Sub M(name As String, dateOfBirth As Date, dateOfDeath As Date)

        Write(name &amp; ", " &amp; dateOfBirth &amp; " - ")
        
        Select Case dateOfDeath
            Case some As Date?
                Write(some.Value.ToString("yyyy-MM-dd"))
            Case Else
                Return
        End Select
    End Sub
End Module
    </file>
</compilation>)

            AssertTheseDiagnostics(compilation,
<expected>
BC37251: Type must be a reference type, type parameter type, or non-nullable value type.
            Case some As Date?
                         ~~~~~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_FaultTolerance_SelectCaseExpressionErrorType()

            Dim compilation = CreateCompilationWithMscorlibAndVBRuntime(
<compilation>
    <file name="a.vb"><![CDATA[
Module Program
    Sub Main()
        Select Case M()
            Case i As Integer
                Return
            Case s As String
                Return
            Case b As Boolean
                Return
            Case Else
                Return
        End Select
    End Sub
End Module
    ]]></file>
</compilation>)

            AssertTheseDiagnostics(compilation,
<expected>
BC30451: 'M' is not declared. It may be inaccessible due to its protection level.
        Select Case M()
                    ~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_FaultTolerance_MatchTypeIsErrorType()

            Dim compilation = CreateCompilationWithMscorlibAndVBRuntime(
<compilation>
    <file name="a.vb"><![CDATA[
Module Program
    Sub Main()
        Select Case New Object()
            Case i As T
                Return
            Case s As U
                Return
            Case b As V
                Return
            Case Else
                Return
        End Select
    End Sub
End Module
    ]]></file>
</compilation>)

            AssertTheseDiagnostics(compilation,
<expected>
BC30002: Type 'T' is not defined.
            Case i As T
                      ~
BC30002: Type 'U' is not defined.
            Case s As U
                      ~
BC30002: Type 'V' is not defined.
            Case b As V
                      ~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_FaultTolerance_SelectCaseExpressionNotClassifiedAsValue()

            Dim compilation = CreateCompilationWithMscorlibAndVBRuntime(
<compilation>
    <file name="Program.vb">
Imports System
Imports System.Console

Module Program
    Sub Main()

        Select Case Nothing
            Case s As String
                Write(s)
            Case Else
                Write("?")
        End Select

        Select Case AddressOf Main
            Case m As Action
                m()
                Write("Action")
            Case Else
                Write("?")
        End Select

        Select Case {"A"}
            Case arr As String()
                Write(arr.Length)
            Case Else
                Write("?")
        End Select

        Select Case Sub() Return
            Case a As Action
                a()
                Write("Action")
            Case Else
                Write("?")
        End Select

        Select Case Function() 1
            Case f As Func(Of Integer)
                Write(f())
            Case Else
                Write("?")
        End Select
    End Sub
End Module
    </file>
</compilation>)

            AssertTheseDiagnostics(compilation,
<expected>
BC36636: 'AddressOf' expressions are not valid in the first expression of a 'Select Case' statement.
        Select Case AddressOf Main
                    ~~~~~~~~~~~~~~
BC42104: Variable 'm' is used before it has been assigned a value. A null reference exception could result at runtime.
                m()
                ~
BC36635: Lambda expressions are not valid in the first expression of a 'Select Case' statement.
        Select Case Sub() Return
                    ~~~~~~~~~~~~
BC37249: Value of type 'Sub &lt;generated method&gt;()' can never be of type 'Action'.
            Case a As Action
                      ~~~~~~
BC42104: Variable 'a' is used before it has been assigned a value. A null reference exception could result at runtime.
                a()
                ~
BC36635: Lambda expressions are not valid in the first expression of a 'Select Case' statement.
        Select Case Function() 1
                    ~~~~~~~~~~~~
BC37249: Value of type 'Function &lt;generated method&gt;() As Integer' can never be of type 'Func(Of Integer)'.
            Case f As Func(Of Integer)
                      ~~~~~~~~~~~~~~~~
BC42104: Variable 'f' is used before it has been assigned a value. A null reference exception could result at runtime.
                Write(f())
                      ~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ERR_TypeCaseThereCanBeOnlyOne()

            Dim compilation = CreateCompilationWithMscorlibAndVBRuntime(
<compilation>
    <file name="Program.vb">
Module Program
    Dim x, y As Object

    Sub Main()
        Select Case New Object()
            Case x As Byte,

                Return
            Case x As Short, y
                Return
            Case x As Integer, y As
                Return
            Case x As Long, y As Single
                Return
            Case x, y As Double
                Return
            Case Else
                Return
        End Select
    End Sub
End Module
    </file>
</compilation>)

            AssertTheseDiagnostics(compilation,
<expected>
BC37250: Type case must be the only clause of 'Case' statement.
            Case x As Byte,
                 ~~~~~~~~~
BC30201: Expression expected.
            Case x As Byte,
                           ~
BC37250: Type case must be the only clause of 'Case' statement.
            Case x As Short, y
                 ~~~~~~~~~~
BC37250: Type case must be the only clause of 'Case' statement.
            Case x As Integer, y As
                 ~~~~~~~~~~~~
BC37250: Type case must be the only clause of 'Case' statement.
            Case x As Integer, y As
                               ~~~~
BC30182: Type expected.
            Case x As Integer, y As
                                   ~
BC37250: Type case must be the only clause of 'Case' statement.
            Case x As Long, y As Single
                 ~~~~~~~~~
BC37250: Type case must be the only clause of 'Case' statement.
            Case x As Long, y As Single
                            ~~~~~~~~~~~
BC37250: Type case must be the only clause of 'Case' statement.
            Case x, y As Double
                    ~~~~~~~~~~~
</expected>)
        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ERR_BlockLocalShadowing1()

            Dim compilation = CreateCompilationWithMscorlibAndVBRuntime(
<compilation>
    <file name="a.vb"><![CDATA[
Module Program
    Sub Main()
        Dim i As String = "",
            s As Integer = 0,
            b As Boolean = False

        Select Case New Object()
            Case i As Integer
                Return
            Case s As String
                Return
            Case b As Boolean
                Return
            Case Else
                Return
        End Select
    End Sub
End Module
    ]]></file>
</compilation>)

            AssertTheseDiagnostics(compilation,
<expected>
BC30616: Variable 'i' hides a variable in an enclosing block.
            Case i As Integer
                 ~~~~~~~~~~~~
BC30616: Variable 's' hides a variable in an enclosing block.
            Case s As String
                 ~~~~~~~~~~~
BC30616: Variable 'b' hides a variable in an enclosing block.
            Case b As Boolean
                 ~~~~~~~~~~~~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ERR_BlockLocalShadowing1_NotReportedForFields()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="Program.vb">
Module Program
    Dim i As String = "",
        s As Integer = 0,
        b As Boolean = False

    Sub Main()

        Select Case New Object()
            Case i As Integer
                Return
            Case s As String
                Return
            Case b As Boolean
                Return
            Case Else
                Return
        End Select
    End Sub
End Module
    </file>
</compilation>,
expectedOutput:="")

            AssertNoDiagnostics(CType(verifier.Compilation, VisualBasicCompilation))

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ERR_LocalNamedSameAsParam1()

            Dim compilation = CreateCompilationWithMscorlibAndVBRuntime(
<compilation>
    <file name="a.vb"><![CDATA[
Module Program
    Sub Main()
        M("", 0, False)
    End Sub

    Sub M(i As String, s As Integer, b As Boolean)
        Select Case New Object()
            Case i As Integer
                Return
            Case s As String
                Return
            Case b As Boolean
                Return
            Case Else
                Return
        End Select
    End Sub
End Module
    ]]></file>
</compilation>)

            AssertTheseDiagnostics(compilation,
<expected>
BC30734: 'i' is already declared as a parameter of this method.
            Case i As Integer
                 ~~~~~~~~~~~~
BC30734: 's' is already declared as a parameter of this method.
            Case s As String
                 ~~~~~~~~~~~
BC30734: 'b' is already declared as a parameter of this method.
            Case b As Boolean
                 ~~~~~~~~~~~~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ERR_TypeCaseNoConversion()

            Dim compilation = CreateCompilationWithMscorlibAndVBRuntime(
<compilation>
    <file name="a.vb"><![CDATA[
Imports System

Class C
    Shared Narrowing Operator CType(obj As String) As C
        Return New C
    End Operator
End Class

Module Program
    Sub Main()
        Select Case String.Empty
            Case a As EventArgs
                Return
            Case b As Integer
                Return
            Case c As C
                Return
            Case d As Date?
                Return
            Case e As Char
                Return
            Case Else
                Return
        End Select
    End Sub
End Module
    ]]></file>
</compilation>)

            AssertTheseDiagnostics(compilation,
<expected>
BC37249: Value of type 'String' can never be of type 'EventArgs'.
            Case a As EventArgs
                      ~~~~~~~~~
BC37249: Value of type 'String' can never be of type 'Integer'.
            Case b As Integer
                      ~~~~~~~
BC37249: Value of type 'String' can never be of type 'C'.
            Case c As C
                      ~
BC37251: Type must be a reference type, type parameter type, or non-nullable value type.
            Case d As Date?
                      ~~~~~
BC37249: Value of type 'String' can never be of type 'Char'.
            Case e As Char
                      ~~~~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_TypeCaseTypeParameterSourceType_Unconstrained()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="a.vb"><![CDATA[
Imports System

Module Program
    Sub Main()
        M(Of String, IDisposable)(1)
    End Sub

    Sub M(Of T, U As Class)(obj As T)

        Select Case obj
            Case m As String
                Console.WriteLine("String: " & m)
            Case n As U
                Return
            Case p As Integer
                Return
            Case Else
                Return
        End Select
    End Sub
End Module
    ]]></file>
</compilation>, expectedOutput:="String: 1")

            AssertNoDiagnostics(CType(verifier.Compilation, VisualBasicCompilation))

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_TypeCaseTypeParameterSourceType_HasClassConstraint()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="Program.vb">
Imports System
Imports System.IO
Imports System.Console

Module Program
    Sub Main()
        M(Of IDisposable, Stream)(Stream.Null)
        M(Of IDisposable, Stream)(Nothing)
        M(Of Object, String)("")
        M(Of Object, String)(0)
    End Sub

    Sub M(Of T As Class, U As {T, Class})(obj As T)

        Select Case obj
            Case n As U
                Write("U")
            Case Else
                Write("?")
        End Select
    End Sub

End Module
    </file>
</compilation>,
expectedOutput:="U?U?")

            AssertNoDiagnostics(CType(verifier.Compilation, VisualBasicCompilation))

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_TypeCaseTypeParameterMatchType_Unconstrained()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="a.vb"><![CDATA[
Imports System

Module Program
    Sub Main()
        M(Of String, Integer)(1)
    End Sub

    Sub M(Of T, U)(obj As Object)

        Select Case obj
            Case m As T
                Console.WriteLine("T: " & m.ToString())
            Case n As U
                Console.WriteLine("U: " & n.ToString())
            Case Else
                Return
        End Select
    End Sub
End Module
    ]]></file>
</compilation>, expectedOutput:="U: 1")

            AssertNoDiagnostics(CType(verifier.Compilation, VisualBasicCompilation))

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_TypeCaseTypeParameterMatch_HasClassOrStructureConstraint()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="Program.vb">
Imports System.Console

Module Program
    Sub Main()
        M(Of String, Integer)(1)
        M(Of String, Integer)(Nothing)
        M(Of String, Integer)(1S)
        M(Of String, Integer)("A")
    End Sub

    Sub M(Of T As Class, U As Structure)(obj As Object)

        Select Case obj
            Case m As T
                Write("T" &amp; m.ToString())
            Case n As U
                Write("U" &amp; n.ToString())
            Case Else
                Write("?")
        End Select
    End Sub
End Module
    </file>
</compilation>,
expectedOutput:="U1??TA")

            AssertNoDiagnostics(CType(verifier.Compilation, VisualBasicCompilation))

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_ERR_GotoIntoTypeCaseBlock()

            Dim compilation = CreateCompilationWithMscorlibAndVBRuntime(
<compilation>
    <file name="Program.vb">
Imports System

Module Program
    Sub Main(args As String())
        M(Nothing)
    End Sub

    Sub M(obj As Object)

        GoTo 2

        Select Case obj
            Case i As Integer
1:              Console.WriteLine(i)
            Case i As String
2:              Console.WriteLine(i)
            Case i As Double
3:              Console.WriteLine(i)

                GoTo 2
        End Select
    End Sub
End Module
    </file>
</compilation>)

            AssertTheseDiagnostics(compilation,
<expected>
BC37252: 'GoTo 2' is not valid because '2' is inside a 'Case' block with a type-case clause that does not contain this statement.
        GoTo 2
             ~
BC42104: Variable 'i' is used before it has been assigned a value. A null reference exception could result at runtime.
2:              Console.WriteLine(i)
                                  ~
BC37252: 'GoTo 2' is not valid because '2' is inside a 'Case' block with a type-case clause that does not contain this statement.
                GoTo 2
                     ~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_WRN_TypeCaseDuplicateCase()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="Program.vb">
Imports System.Console

Module Program
    Sub Main()
        Select Case CObj("A")
            Case i As Integer
                Write(i)
            Case s As String
                Write("!A")
            Case s2 As String
                Write(s2)
            Case Else
                Write("?")
        End Select
    End Sub
End Module
    </file>
</compilation>,
expectedOutput:="!A")

            AssertTheseDiagnostics(verifier.Compilation,
<expected>
BC42379: 'Case' block is unreachable; value of type 'String' matched above in the same 'Select Case' block.
            Case s2 As String
                       ~~~~~~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_WRN_TypeCaseOverlappingCase()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="Program.vb">
Imports System.Console
Imports System.Collections
Imports System.Collections.Generic
Imports System.Linq

Module Program
    Sub Main()
        Select Case CObj("ABCDEF")
            Case s As Short
                Write("#")
            Case chars As IEnumerable(Of Char)
                For Each c In chars.Reverse()
                    Write(c)
                Next
            Case s As String
                Write(s)
            Case e As IEnumerable
                Write("...")
            Case Else
                Write("?")
        End Select
    End Sub
End Module
    </file>
</compilation>,
expectedOutput:="FEDCBA")

            AssertTheseDiagnostics(verifier.Compilation,
<expected>
BC42380: 'Case' block is unreachable; value of type 'String' matched above by 'IEnumerable(Of Char)'.
            Case s As String
                      ~~~~~~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_WRN_TypeCaseOverlappingCase_ObjectToInterfaceType()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="Program.vb">
Imports System
Imports System.Console

Module Program
    Sub Main()
        Select Case CObj("ABCDEF")
            Case s As Short
                Return
            Case obj As Object
                Write("Object: " &amp; obj.ToString())
            Case i As IDisposable
                Return
            Case Else
                Write("?")
        End Select
    End Sub
End Module
    </file>
</compilation>,
expectedOutput:="Object: ABCDEF")

            AssertTheseDiagnostics(verifier.Compilation,
<expected>
BC42380: 'Case' block is unreachable; value of type 'IDisposable' matched above by 'Object'.
            Case i As IDisposable
                      ~~~~~~~~~~~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectCase_TypeCaseClause_WRN_TypeCaseOverlappingCase_NotReportedForIdentityAndWidening()

            Dim verifier = CompileAndVerify(
<compilation>
    <file name="Program.vb">
Imports System
Imports System.Console
Imports System.Collections.Generic
Imports System.Linq

Module Program
    Sub Main()
        M("Hello, World!")
        M2(Nothing)
    End Sub

    Sub M(obj As String)
        Select Case obj
            Case s As String
                Write(s.Length)
            Case Else
                Throw New ArgumentNullException("obj")
        End Select
    End Sub

    Sub M2(obj2 As String)
        Select Case obj2
            Case s As IEnumerable(Of Char)
                Write(s.Count())
            Case Else
                Write("obj2")
        End Select
    End Sub
End Module
    </file>
</compilation>,
expectedOutput:="13obj2")

            AssertNoDiagnostics(CType(verifier.Compilation, VisualBasicCompilation))

        End Sub

    End Class
End Namespace
