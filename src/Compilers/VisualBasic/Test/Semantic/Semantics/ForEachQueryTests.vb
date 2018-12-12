' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Microsoft.CodeAnalysis.VisualBasic.SyntaxFacts
Imports Roslyn.Test.Utilities

Namespace Microsoft.CodeAnalysis.VisualBasic.UnitTests.Semantics

    Public Class ForEachQueryTests
        Inherits BasicTestBase

        <Fact>
        Public Sub SimpleCaseWithWhereClause()
            Dim compilationDef =
<compilation name="QueryExpressions">
    <file name="a.vb">
Option Strict Off
Option Infer On

Imports System
Imports System.Collections
Imports System.Linq

Module Program
    Sub Main()
        Dim digits = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

        For Each i In digits Where i Mod 2 = 0
            Console.Write(i)
        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            Dim verifier = CompileAndVerify(compilation, expectedOutput:="02468")

        End Sub

        <Fact>
        Public Sub SimpleCaseWithSelectClause()
            Dim compilationDef =
<compilation name="QueryExpressions">
    <file name="a.vb">
Option Strict Off
Option Infer On

Imports System
Imports System.Collections
Imports System.Linq

Module Program
    Sub Main()
        Dim digits = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

        For Each i In digits Where i &lt; 4 Select square = i * i
            Console.Write(square)
        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            Dim verifier = CompileAndVerify(compilation, expectedOutput:="0149")

        End Sub

        <Fact>
        Public Sub AdditionalCollectionRangeVariables()
            Dim compilationDef =
<compilation name="QueryExpressions">
    <file name="a.vb">
Option Strict Off
Option Infer On

Imports System
Imports System.Collections
Imports System.Linq

Module Program
    Sub Main()
        Dim digits = {1, 2, 3}

        For Each x In digits, 
                 y In digits

            Console.Write(x * y)
        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            Dim verifier = CompileAndVerify(compilation, expectedOutput:="123246369")

        End Sub

        <Fact>
        Public Sub SelectClauseHidesRangeVariables()
            Dim compilationDef =
<compilation name="QueryExpressions">
    <file name="a.vb">
Option Strict Off
Option Infer On

Imports System
Imports System.Collections
Imports System.Linq

Module Program
    Sub Main()
        Dim digits = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

        For Each i In digits Where i &lt; 4 Select square = i * i
            Console.Write(i)
        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            AssertTheseCompileDiagnostics(compilation,
<expected>
BC30451: 'i' is not declared. It may be inaccessible due to its protection level.
            Console.Write(i)
                          ~
</expected>)

        End Sub

        <Fact>
        Public Sub MultiSelect()
            Dim compilationDef =
<compilation name="QueryExpressions">
    <file name="a.vb">
Option Strict Off
Option Infer On

Imports System
Imports System.Collections
Imports System.Linq

Module Program
    Sub Main()
        Dim digits = {1, 2, 3, 4, 5}

        For Each i In digits Select square = i * i, cube = i * i * i
            Console.Write(square &amp; cube)
        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            Dim verifier = CompileAndVerify(compilation, expectedOutput:="1148927166425125")

        End Sub

        <Fact>
        Public Sub EmptySelectClauseRecoversGracefully()
            Dim compilationDef =
<compilation name="QueryExpressions">
    <file name="a.vb">
Option Strict Off
Option Infer On

Imports System
Imports System.Collections
Imports System.Linq

Module Program
    Sub Main()
        Dim digits = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

        For Each i In digits Select

        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            AssertTheseCompileDiagnostics(compilation,
<expected>
BC30201: Expression expected.
        For Each i In digits Select
                                   ~
</expected>)

        End Sub

        <Fact>
        Public Sub SelectClauseWithoutInferableNameRecoversGracefully()
            Dim compilationDef =
<compilation name="QueryExpressions">
    <file name="a.vb">
Option Strict Off
Option Infer On

Imports System
Imports System.Collections
Imports System.Linq

Module Program
    Sub Main()
        Dim digits = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

        For Each i In digits Select
            Console.Write(i)
        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            AssertTheseCompileDiagnostics(compilation,
<expected>
BC30491: Expression does not produce a value.
            Console.Write(i)
            ~~~~~~~~~~~~~~~~
BC36599: Range variable name can be inferred only from a simple or qualified name with no arguments.
            Console.Write(i)
            ~~~~~~~~~~~~~~~~
</expected>)

        End Sub

        <Fact>
        Public Sub IL_CurrentIsNotCalledMoreThanOncePerIteration()
            Dim compilationDef =
<compilation name="QueryExpressions">
    <file name="a.vb">
Option Strict Off
Option Infer On

Imports System
Imports System.Collections
Imports System.Linq

Module Program
    Sub Main()
        Dim digits = {1, 2, 3}

        For Each x In digits, 
                 y In digits

            Console.Write(x * y)
        Next
    End Sub
End Module
    </file>
</compilation>

            Dim compilation = CompilationUtils.CreateCompilationWithMscorlib40AndVBRuntime(compilationDef,
                                                                                           additionalRefs:={SystemCoreRef},
                                                                                           TestOptions.ReleaseExe)

            Dim verifier = CompileAndVerify(compilation, expectedOutput:="123246369")

            ' TODO: Move this to Emit tests.
            verifier.VerifyIL("Program.Main", <![CDATA[
{
  // Code size      172 (0xac)
  .maxstack  4
  .locals init (Program._Closure$__0-0 V_0, //$VB$Closure_0
                System.Collections.Generic.IEnumerator(Of <anonymous type: Key x As Integer, Key y As Integer>) V_1,
                System.Func(Of Integer, System.Collections.Generic.IEnumerable(Of Integer)) V_2,
                Integer V_3, //x
                Integer V_4) //y
  IL_0000:  ldloc.0
  IL_0001:  newobj     "Sub Program._Closure$__0-0..ctor(Program._Closure$__0-0)"
  IL_0006:  stloc.0
  IL_0007:  ldloc.0
  IL_0008:  ldc.i4.3
  IL_0009:  newarr     "Integer"
  IL_000e:  dup
  IL_000f:  ldtoken    "<PrivateImplementationDetails>.__StaticArrayInitTypeSize=12 <PrivateImplementationDetails>.E429CCA3F703A39CC5954A6572FEC9086135B34E"
  IL_0014:  call       "Sub System.Runtime.CompilerServices.RuntimeHelpers.InitializeArray(System.Array, System.RuntimeFieldHandle)"
  IL_0019:  stfld      "Program._Closure$__0-0.$VB$Local_digits As Integer()"
  .try
  {
    IL_001e:  ldloc.0
    IL_001f:  ldfld      "Program._Closure$__0-0.$VB$Local_digits As Integer()"
    IL_0024:  ldloc.0
    IL_0025:  ldfld      "Program._Closure$__0-0.$I0 As System.Func(Of Integer, System.Collections.Generic.IEnumerable(Of Integer))"
    IL_002a:  brfalse.s  IL_0034
    IL_002c:  ldloc.0
    IL_002d:  ldfld      "Program._Closure$__0-0.$I0 As System.Func(Of Integer, System.Collections.Generic.IEnumerable(Of Integer))"
    IL_0032:  br.s       IL_0049
    IL_0034:  ldloc.0
    IL_0035:  ldloc.0
    IL_0036:  ldftn      "Function Program._Closure$__0-0._Lambda$__0(Integer) As System.Collections.Generic.IEnumerable(Of Integer)"
    IL_003c:  newobj     "Sub System.Func(Of Integer, System.Collections.Generic.IEnumerable(Of Integer))..ctor(Object, System.IntPtr)"
    IL_0041:  dup
    IL_0042:  stloc.2
    IL_0043:  stfld      "Program._Closure$__0-0.$I0 As System.Func(Of Integer, System.Collections.Generic.IEnumerable(Of Integer))"
    IL_0048:  ldloc.2
    IL_0049:  ldsfld     "Program._Closure$__.$I0-1 As System.Func(Of Integer, Integer, <anonymous type: Key x As Integer, Key y As Integer>)"
    IL_004e:  brfalse.s  IL_0057
    IL_0050:  ldsfld     "Program._Closure$__.$I0-1 As System.Func(Of Integer, Integer, <anonymous type: Key x As Integer, Key y As Integer>)"
    IL_0055:  br.s       IL_006d
    IL_0057:  ldsfld     "Program._Closure$__.$I As Program._Closure$__"
    IL_005c:  ldftn      "Function Program._Closure$__._Lambda$__0-1(Integer, Integer) As <anonymous type: Key x As Integer, Key y As Integer>"
    IL_0062:  newobj     "Sub System.Func(Of Integer, Integer, <anonymous type: Key x As Integer, Key y As Integer>)..ctor(Object, System.IntPtr)"
    IL_0067:  dup
    IL_0068:  stsfld     "Program._Closure$__.$I0-1 As System.Func(Of Integer, Integer, <anonymous type: Key x As Integer, Key y As Integer>)"
    IL_006d:  call       "Function System.Linq.Enumerable.SelectMany(Of Integer, Integer, <anonymous type: Key x As Integer, Key y As Integer>)(System.Collections.Generic.IEnumerable(Of Integer), System.Func(Of Integer, System.Collections.Generic.IEnumerable(Of Integer)), System.Func(Of Integer, Integer, <anonymous type: Key x As Integer, Key y As Integer>)) As System.Collections.Generic.IEnumerable(Of <anonymous type: Key x As Integer, Key y As Integer>)"
    IL_0072:  callvirt   "Function System.Collections.Generic.IEnumerable(Of <anonymous type: Key x As Integer, Key y As Integer>).GetEnumerator() As System.Collections.Generic.IEnumerator(Of <anonymous type: Key x As Integer, Key y As Integer>)"
    IL_0077:  stloc.1
    IL_0078:  br.s       IL_0097
    IL_007a:  ldloc.1
    IL_007b:  callvirt   "Function System.Collections.Generic.IEnumerator(Of <anonymous type: Key x As Integer, Key y As Integer>).get_Current() As <anonymous type: Key x As Integer, Key y As Integer>"
    IL_0080:  dup
    IL_0081:  callvirt   "Function VB$AnonymousType_0(Of Integer, Integer).get_x() As Integer"
    IL_0086:  stloc.3
    IL_0087:  callvirt   "Function VB$AnonymousType_0(Of Integer, Integer).get_y() As Integer"
    IL_008c:  stloc.s    V_4
    IL_008e:  ldloc.3
    IL_008f:  ldloc.s    V_4
    IL_0091:  mul.ovf
    IL_0092:  call       "Sub System.Console.Write(Integer)"
    IL_0097:  ldloc.1
    IL_0098:  callvirt   "Function System.Collections.IEnumerator.MoveNext() As Boolean"
    IL_009d:  brtrue.s   IL_007a
    IL_009f:  leave.s    IL_00ab
  }
  finally
  {
    IL_00a1:  ldloc.1
    IL_00a2:  brfalse.s  IL_00aa
    IL_00a4:  ldloc.1
    IL_00a5:  callvirt   "Sub System.IDisposable.Dispose()"
    IL_00aa:  endfinally
  }
  IL_00ab:  ret
}
]]>)
        End Sub

    End Class
End Namespace
