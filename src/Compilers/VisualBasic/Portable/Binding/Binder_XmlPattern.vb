' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports System.Collections.Generic
Imports System.Collections.Immutable
Imports System.Runtime.InteropServices
Imports Microsoft.CodeAnalysis.Collections
Imports Microsoft.CodeAnalysis.PooledObjects
Imports Microsoft.CodeAnalysis.VisualBasic.Symbols
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace Microsoft.CodeAnalysis.VisualBasic
    Partial Friend Class Binder
        Private Function BindXmlPatternElement(
                             syntax As XmlElementSyntax,
                             rootInfo As XmlElementRootInfo,
                             elementNamespaceValue As String,
                             diagnostics As DiagnosticBag
                         ) As BoundExpression

            Dim start = syntax.StartTag

            Dim type = XmlPatternHelpers.BindXmlNameAsType(start.Name, Me, diagnostics)

            ' Some of this should happen in lowering.
            Dim temp = New SynthesizedLocal(ContainingMember, type, SynthesizedLocalKind.LoweringTemp)
            Dim boundTemp = New BoundLocal(syntax, temp, type)

            Dim sideEffectsBuilder = ArrayBuilder(Of BoundExpression).GetInstance()

            sideEffectsBuilder.Add(BindAssignment(syntax,
                                                  boundTemp,
                                                  BindObjectCreationExpression(start.Name, type, ImmutableArray(Of BoundExpression).Empty, diagnostics),
                                                  diagnostics))

            For Each attribute In start.Attributes
                If attribute.Kind = SyntaxKind.XmlAttribute Then
                    sideEffectsBuilder.Add(XmlPatternHelpers.BindXmlAttribute(DirectCast(attribute, XmlAttributeSyntax), boundTemp, type, Me, diagnostics))
                Else
                    ' TODO: Report an error.
                End If
            Next

            ' TODO: Bind children.

            Return New BoundSequence(syntax, ImmutableArray.Create(Of LocalSymbol)(temp), sideEffectsBuilder.ToImmutableAndFree(), boundTemp.MakeRValue(), type)
        End Function

        Private Class XmlPatternHelpers
            Private Sub New()
                Return
            End Sub

            Public Shared Function BindXmlAttribute(attribute As XmlAttributeSyntax, subject As BoundExpression, subjectType As TypeSymbol, binder As Binder, diagnostics As DiagnosticBag) As BoundExpression

                Dim symbol = BindXmlNameAsMember(attribute.Name, subject, subjectType, binder, diagnostics)

                If symbol Is Nothing Then
                    ' Error has been reported by BindXmlNameAsMember already.
                    Return BadExpression(attribute, ErrorTypeSymbol.UnknownResultType)
                End If

                Dim targetType As TypeSymbol
                Dim assignmentTarget As BoundExpression = Nothing
                Dim accessorMethod As MethodSymbol = Nothing

                Select Case symbol.Kind
                    Case SymbolKind.Property
                        ' TODO: Should `SubjectType.P = V` be permitted if `p` is shared? No reason not to.
                        ' Becomes: `subject.P = V`
                        Dim p = DirectCast(symbol, PropertySymbol)
                        ' TODO: Test if `p` has protected `Set` and is called from within non-derived type.
                        Debug.Assert(p.HasSet)
                        targetType = p.Type
                        assignmentTarget = New BoundPropertyAccess(attribute,
                                                                   p,
                                                                   propertyGroupOpt:=Nothing,
                                                                   PropertyAccessKind.Set,
                                                                   isWriteable:=True,
                                                                   subject,
                                                                   ImmutableArray(Of BoundExpression).Empty)

                    Case SymbolKind.Event
                        ' Becomes: `subject.add_E(V)`
                        Dim e = DirectCast(symbol, EventSymbol)
                        targetType = e.Type
                        accessorMethod = e.AddMethod

                    Case SymbolKind.Field
                        ' Becomes: `subject.F = V`
                        Dim f = DirectCast(symbol, FieldSymbol)
                        ' There's no way to initialize *your* read-only field (or auto-prop) with an XML literal.
                        Debug.Assert(Not f.IsReadOnly)
                        targetType = f.Type
                        assignmentTarget = New BoundFieldAccess(attribute, subject, f, isLValue:=True, f.Type)

                    Case SymbolKind.Method
                        ' TODO: What happens is `T.P` names a `Shared` property on `T`?
                        ' Becomes: `T.M(subject, V)`

                        Dim m = DirectCast(symbol, MethodSymbol)
                        Debug.Assert(m.ParameterCount = 0)

                        targetType = m.Parameters(0).Type
                        accessorMethod = m

                    Case Else
                        Throw ExceptionUtilities.UnexpectedValue(symbol.Kind)
                End Select

                Dim value = BindXmlAttributeValue(attribute.Value, targetType, binder, diagnostics)

                If assignmentTarget IsNot Nothing Then
                    Return binder.BindAssignment(attribute, assignmentTarget, value, diagnostics)
                End If

                Debug.Assert(accessorMethod IsNot Nothing)

                If accessorMethod.IsShared Then
                    Return binder.BindInvocationExpression(attribute,
                                                           attribute.Name,
                                                           TypeCharacter.None,
                                                           New BoundMethodGroup(attribute.Name,
                                                                                typeArgumentsOpt:=Nothing,
                                                                                ImmutableArray.Create(accessorMethod),
                                                                                LookupResultKind.Good,
                                                                                receiverOpt:=Nothing,
                                                                                QualificationKind.QualifiedViaTypeName),
                                                           ImmutableArray.Create(subject, value),
                                                           ImmutableArray(Of String).Empty,
                                                           diagnostics,
                                                           callerInfoOpt:=Nothing)
                Else
                    Return binder.BindInvocationExpression(attribute,
                                                           attribute.Name,
                                                           TypeCharacter.None,
                                                           New BoundMethodGroup(attribute.Name,
                                                                                typeArgumentsOpt:=Nothing,
                                                                                ImmutableArray.Create(accessorMethod),
                                                                                LookupResultKind.Good,
                                                                                receiverOpt:=subject,
                                                                                QualificationKind.QualifiedViaValue),
                                                           ImmutableArray.Create(value),
                                                           ImmutableArray(Of String).Empty,
                                                           diagnostics,
                                                           callerInfoOpt:=Nothing)
                End If

            End Function

            Public Shared Function BindXmlAttributeValue(node As XmlNodeSyntax, targetType As TypeSymbol, binder As Binder, diagnostics As DiagnosticBag) As BoundExpression

                Dim value As BoundExpression

                Select Case node.Kind
                    Case SyntaxKind.XmlString
                        Dim xmlString = DirectCast(node, XmlStringSyntax)

                        Dim valueString = GetXmlString(xmlString.TextTokens)

                        Select Case targetType.TypeKind
                            Case TypeKind.Delegate

                                ' TODO: Refactor this to share code with binding of AddressOf.
                                If String.IsNullOrEmpty(valueString) Then
                                    ' Empty string must have been a syntax error. 
                                    ' Just produce a bad expression and get out without producing any new errors.
                                    Return BadExpression(node, ErrorTypeSymbol.UnknownResultType)
                                End If

                                Dim options As LookupOptions = LookupOptions.AllMethodsOfAnyArity ' overload resolution filters methods by arity.
                                options = options Or LookupOptions.MustNotBeReturnValueVariable

                                'If skipLocalsAndParameters Then
                                '    options = options Or LookupOptions.MustNotBeLocalOrParameter
                                'End If

                                Dim result As LookupResult = LookupResult.GetInstance()

                                Dim useSiteDiagnostics As HashSet(Of DiagnosticInfo) = Nothing
                                binder.Lookup(result, valueString, arity:=0, options, useSiteDiagnostics)
                                diagnostics.Add(node, useSiteDiagnostics)

                                If Not result.HasSymbol Then
                                    ' Did not find anything with that name.
                                    result.Free()

                                    Return ReportDiagnosticAndProduceBadExpression(diagnostics, node, ErrorFactory.ErrorInfo(ERRID.ERR_NameNotDeclared1, valueString))
                                End If

                                Dim boundOperand = binder.BindSimpleName(result, node, options, typeArguments:=Nothing, diagnostics)
                                result.Free()

                                ' only accept MethodGroups as operands. More detailed checks (e.g. for Constructors follow later)
                                If boundOperand.Kind <> BoundKind.MethodGroup Then
                                    If Not boundOperand.HasErrors Then
                                        ReportDiagnostic(diagnostics, node, ERRID.ERR_AddressOfOperandNotMethod)
                                    End If

                                    Return BadExpression(node, boundOperand, LookupResultKind.NotAValue, ErrorTypeSymbol.UnknownResultType)
                                End If

                                Dim hasErrors As Boolean = False
                                Dim group = DirectCast(boundOperand, BoundMethodGroup)

                                If IsGroupOfConstructors(group) Then
                                    ReportDiagnostic(diagnostics, node, ERRID.ERR_InvalidConstructorCall)
                                    hasErrors = True
                                End If

                                value = New BoundAddressOfOperator(node, binder, group, hasErrors)

                            Case TypeKind.Enum

                                ' TODO: Refactor this into some kind of BindMemberAccess overload.
                                Dim lookupResult As LookupResult = LookupResult.GetInstance()
                                Dim options As LookupOptions = LookupOptions.AllMethodsOfAnyArity
                                Dim useSiteDiagnostics As HashSet(Of DiagnosticInfo) = Nothing

                                binder.LookupMember(lookupResult, targetType, valueString, arity:=0, options, useSiteDiagnostics) ' overload resolution filters methods by arity.

                                If LookupResult.HasSymbol Then
                                    value = binder.BindSymbolAccess(node,
                                                                    lookupResult,
                                                                    options,
                                                                    New BoundTypeExpression(node, targetType),
                                                                    typeArgumentsOpt:=Nothing,
                                                                    QualificationKind.QualifiedViaTypeName,
                                                                    diagnostics)
                                Else
                                    Return ReportDiagnosticAndProduceBadExpression(diagnostics, node, ErrorFactory.ErrorInfo(ERRID.ERR_NameNotMember2, valueString, targetType))
                                End If

                            Case Else
                                value = binder.CreateStringLiteral(node, valueString, compilerGenerated:=False, diagnostics)

                        End Select

                    Case SyntaxKind.XmlEmbeddedExpression

                        Dim expressionBinder = New XmlEmbeddedExpressionBinder(binder)
                        ' Review: BindExpression or BindValue?
                        value = expressionBinder.BindValue(DirectCast(node, XmlEmbeddedExpressionSyntax).Expression, diagnostics)
                        Debug.Assert(value IsNot Nothing)

                    Case Else
                        Throw ExceptionUtilities.UnexpectedValue(node.Kind)
                End Select

                ' TODO: Classify conversion. If no conversion exists go find extension conversion helper.
                Return binder.ApplyConversion(node, targetType, value, isExplicit:=False, diagnostics)
            End Function

            Public Shared Function BindXmlContent(content As XmlNodeSyntax, subject As BoundExpression, subjectType As TypeSymbol, binder As Binder, diagnostics As DiagnosticBag) As BoundExpression
                Throw New NotImplementedException()
            End Function

            Public Shared Function BindXmlNameAsType(node As XmlNodeSyntax, binder As Binder, diagnostics As DiagnosticBag) As TypeSymbol

                If node.Kind <> SyntaxKind.XmlName Then
                    ' TODO: Report error.
                    Return Nothing
                End If

                Dim name = DirectCast(node, XmlNameSyntax)

                ' TODO: Report error if `name` specifies an XML namespace.

                Dim prefix As String = Nothing,
                    typeName As String = Nothing,
                    memberName As String = Nothing

                If Not TryGetXmlTypeOrMemberNameParts(name, diagnostics, prefix, typeName, memberName) Then

                    Return Nothing

                ElseIf memberName IsNot Nothing Then

                    ' TODO: Report error.
                    Return Nothing

                End If

                Dim xmlNamespaceRaw As String = Nothing,
                    isFromImports = False

                binder.LookupXmlNamespace(prefix, ignoreXmlNodes:=False, xmlNamespaceRaw, isFromImports)

                Dim elementClrNamespaces = xmlNamespaceRaw.Split(":"c)(1).Split(","c)

                If elementClrNamespaces.Length = 0 Then
                    ' TODO: Report error.
                    Return Nothing
                End If

                If Not SyntaxFacts.IsValidIdentifier(typeName) Then
                    ' TODO: Report error.
                    Return Nothing
                End If

                Dim lookupResult As LookupResult = LookupResult.GetInstance()
                binder.Lookup(lookupResult, typeName, 0, LookupOptions.NamespacesOrTypesOnly, Nothing)

                If Not (lookupResult.IsGood AndAlso lookupResult.HasSingleSymbol AndAlso lookupResult.SingleSymbol.Kind = SymbolKind.NamedType) Then
                    ' TODO: Report error.
                    lookupResult.Free()
                    Return Nothing
                End If

                Dim type = DirectCast(lookupResult.SingleSymbol, NamedTypeSymbol)
                lookupResult.Clear()

                Return type
            End Function

            Public Shared Function BindXmlNameAsMember(node As XmlNodeSyntax, subject As BoundExpression, subjectType As TypeSymbol, binder As Binder, diagnostics As DiagnosticBag) As Symbol

                ' TODO: Figure out how to unify with BindMemberAccess.

                If node.Kind <> SyntaxKind.XmlName Then
                    ' TODO: Report error.
                    Return Nothing
                End If

                Dim name = DirectCast(node, XmlNameSyntax)

                ' TODO: Report error if `name` specifies an XML namespace.

                Dim prefix As String = Nothing,
                    typeName As String = Nothing,
                    memberName As String = Nothing

                If Not TryGetXmlMemberNameParts(name, diagnostics, prefix, typeName, memberName) Then
                    Return Nothing
                End If

                Dim xmlNamespaceRaw As String = Nothing,
                    isFromImports = False

                binder.LookupXmlNamespace(prefix, ignoreXmlNodes:=False, xmlNamespaceRaw, isFromImports)

                Dim elementClrNamespaces = xmlNamespaceRaw.Split(":"c)(1).Split(","c)

                If elementClrNamespaces.Length = 0 Then
                    ' TODO: Report error.
                    Return Nothing
                End If

                If Not SyntaxFacts.IsValidIdentifier(memberName) OrElse (typeName IsNot Nothing AndAlso Not SyntaxFacts.IsValidIdentifier(typeName)) Then
                    ' TODO: Report error.
                    Return Nothing
                End If

                Dim lookupResult As LookupResult = LookupResult.GetInstance()
                Dim options As LookupOptions = LookupOptions.AllMethodsOfAnyArity

                ' If typename not null we should check against subject type.
                Dim left = binder.AdjustReceiverValue(subject, node, diagnostics)

                Dim type = left.Type

                If type Is Nothing OrElse type.IsErrorType() Then
                    Return Nothing
                End If

                If String.IsNullOrEmpty(memberName) Then
                    ' Must have been a syntax error.
                    Return Nothing
                End If

                Dim effectiveOptions = options

                Dim useSiteDiagnostics As HashSet(Of DiagnosticInfo) = Nothing

                binder.LookupMember(lookupResult, type, memberName, arity:=0, effectiveOptions, useSiteDiagnostics) ' overload resolution filters methods by arity.

                If lookupResult.HasSymbol Then

                    Select Case lookupResult.SingleSymbol.Kind
                        Case SymbolKind.Property,
                             SymbolKind.Event,
                             SymbolKind.Field

                            Return lookupResult.SingleSymbol

                        Case Else

                            ' TODO: Report error.
                            Return Nothing

                    End Select

                ElseIf left.HasErrors Then
                    Return Nothing

                Else
                    Return Nothing

                    ' TODO: Figure out if this makes sense.
                    If type.IsInterfaceType() Then
                        ' In case IsExtensibleInterfaceNoUseSiteDiagnostics above failed because there were bad inherited interfaces.
                        type.AllInterfacesWithDefinitionUseSiteDiagnostics(useSiteDiagnostics)
                    End If

                    ReportDiagnosticAndProduceBadExpression(diagnostics, node, ErrorFactory.ErrorInfo(ERRID.ERR_NameNotMember2, memberName, type), left)

                    Return Nothing
                End If

            End Function

            Public Shared Function BindXmlNameAsTypeOrMember(name As XmlNameSyntax, subjectType As TypeSymbol, binder As Binder, diagnostics As DiagnosticBag) As Symbol
                Throw New NotImplementedException()
            End Function

            Public Shared Function TryGetXmlTypeOrMemberNameParts(
                                       name As XmlNameSyntax,
                                       diagnostics As DiagnosticBag,
                                       ByRef prefix As String,
                                       ByRef typeName As String,
                                       ByRef memberName As String
                                   ) As Boolean

                prefix = If(name.Prefix?.Name.ValueText, "")

                Dim localNameText = name.LocalName.ValueText
                Dim localNameParts = localNameText.Split("."c)

                typeName = localNameParts(0)

                If localNameParts.Length > 1 Then
                    memberName = localNameParts(1)
                Else
                    memberName = Nothing
                End If

                If localNameParts.Length > 2 Then
                    ' TODO: Report an error here.
                    Return False
                End If

                Return True
            End Function

            ' This method may look identical to TryGetXmlTypeOrMemberNameParts it but it's not;
            ' TryGetXmlTypeOrMemberNameParts treats `Canvas` as a type name and this method
            ' treats it as a member name.
            Public Shared Function TryGetXmlMemberNameParts(
                                       name As XmlNameSyntax,
                                       diagnostics As DiagnosticBag,
                                       ByRef prefix As String,
                                       ByRef typeName As String,
                                       ByRef memberName As String
                                   ) As Boolean

                prefix = If(name.Prefix?.Name.ValueText, "")

                Dim localNameText = name.LocalName.ValueText
                Dim localNameParts = localNameText.Split("."c)

                typeName = localNameParts(0)

                If localNameParts.Length > 1 Then
                    typeName = localNameParts(0)
                    memberName = localNameParts(1)
                Else
                    typeName = Nothing
                    memberName = localNameParts(0)
                End If

                If localNameParts.Length > 2 Then
                    ' TODO: Report an error here.
                    Return False
                End If

                Return True
            End Function
        End Class
    End Class
End Namespace
