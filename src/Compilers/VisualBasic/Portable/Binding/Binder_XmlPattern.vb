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

        Private Function TryBindXmlPatternElement(
                             syntax As XmlNodeSyntax,
                             name As XmlNodeSyntax,
                             attributes As SyntaxList(Of XmlNodeSyntax),
                             content As SyntaxList(Of XmlNodeSyntax),
                             rootInfoOpt As XmlElementRootInfo,
                             diagnostics As DiagnosticBag
                         ) As BoundExpression

            If name.Kind <> SyntaxKind.XmlName Then Return Nothing

            Dim elementNamespaceRaw As String = Nothing,
                isFromImports = False

            ' TODO: Come up with an actual check to enable this feature.
            If LookupXmlNamespace(If(DirectCast(name, XmlNameSyntax).Prefix?.Name.ValueText, ""), ignoreXmlNodes:=True, elementNamespaceRaw, isFromImports) AndAlso
               elementNamespaceRaw.Trim().StartsWith("clr-namespace:") _
            Then
                Debug.Assert(isFromImports)

                Return BindXmlPatternElement(syntax, name, attributes, content, elementNamespaceRaw, rootInfoOpt, diagnostics)
            Else

                Return Nothing
            End If
        End Function

        Private Function BindXmlPatternElement(
                             syntax As XmlNodeSyntax,
                             name As XmlNodeSyntax,
                             attributes As SyntaxList(Of XmlNodeSyntax),
                             content As SyntaxList(Of XmlNodeSyntax),
                             elementNamespaceRaw As String,
                             rootInfoOpt As XmlElementRootInfo,
                             diagnostics As DiagnosticBag
                         ) As BoundExpression

            Dim type = XmlPatternHelpers.BindXmlNameAsType(name, Me, diagnostics)

            If type Is Nothing Then
                Return BadExpression(syntax, ErrorTypeSymbol.UnknownResultType)
            End If

            ' Some of this should happen in lowering.
            Dim temp = New SynthesizedLocal(ContainingMember, type, SynthesizedLocalKind.LoweringTemp)
            Dim boundTemp = New BoundLocal(syntax, temp, type)

            Dim sideEffectsBuilder = ArrayBuilder(Of BoundExpression).GetInstance()

            sideEffectsBuilder.Add(BindAssignment(syntax,
                                                  boundTemp,
                                                  BindObjectCreationExpression(name, type, ImmutableArray(Of BoundExpression).Empty, diagnostics),
                                                  diagnostics))

            XmlPatternHelpers.BindXmlAttributes(attributes, sideEffectsBuilder, boundTemp, type, Me, diagnostics)

            If content.Count > 0 Then
                Dim memberElements = ArrayBuilder(Of XmlNodeSyntax).GetInstance()
                Dim childElements = ArrayBuilder(Of XmlNodeSyntax).GetInstance()

                XmlPatternHelpers.SeparateMemberAndChildElementsAndReportErrorForInterleaving(content, memberElements, childElements, diagnostics)

                ' Bind member elements.
                For Each element In memberElements
                    XmlPatternHelpers.BindXmlMemberElement(element, sideEffectsBuilder, boundTemp, type, Me, diagnostics)
                Next
                memberElements.Free()

                XmlPatternHelpers.BindXmlChildElements(childElements, sideEffectsBuilder, boundTemp, syntax, Me, diagnostics)
                childElements.Free()
            End If

            Return New BoundSequence(syntax, ImmutableArray.Create(Of LocalSymbol)(temp), sideEffectsBuilder.ToImmutableAndFree(), boundTemp.MakeRValue(), type)
        End Function

        ' Bite the bullet and move this to a binder.
        Private Class XmlPatternHelpers
            Private Sub New()
                Return
            End Sub

            Public Shared Sub BindXmlAttributes(
                                  attributes As SyntaxList(Of XmlNodeSyntax),
                                  sideEffectsBuilder As ArrayBuilder(Of BoundExpression),
                                  subject As BoundExpression,
                                  subjectType As TypeSymbol,
                                  binder As Binder,
                                  diagnostics As DiagnosticBag
                              )

                For Each attribute In attributes
                    If attribute.Kind = SyntaxKind.XmlAttribute Then
                        Dim trueAttribute = DirectCast(attribute, XmlAttributeSyntax)

                        If trueAttribute.Name.Kind = SyntaxKind.XmlName AndAlso
                           binder.TryGetXmlnsPrefix(DirectCast(trueAttribute.Name, XmlNameSyntax), prefix:=Nothing, New DiagnosticBag()) _
                        Then
                            ' Don't do anything with these for now, but they should actually be filtered out by this point in binding as,
                            ' it's necessary to bind the namespaces before binding the element name itself.
                            Continue For
                        End If

                        sideEffectsBuilder.Add(BindXmlAttribute(trueAttribute, subject, subjectType, binder, diagnostics))
                    Else
                        ' TODO: Report an error.
                    End If
                Next
            End Sub

            Private Shared Function BindXmlAttribute(attribute As XmlAttributeSyntax, subject As BoundExpression, subjectType As TypeSymbol, binder As Binder, diagnostics As DiagnosticBag) As BoundExpression

                Dim name = DirectCast(attribute.Name, XmlNameSyntax)

                ' TODO: Report error if `name` specifies an XML namespace.

                Dim prefix As String = Nothing,
                    typeName As String = Nothing,
                    memberName As String = Nothing

                If Not TryGetXmlMemberNameParts(name, diagnostics, prefix, typeName, memberName) Then
                    Return Nothing
                End If

                If Not SyntaxFacts.IsValidIdentifier(memberName) OrElse (typeName IsNot Nothing AndAlso Not SyntaxFacts.IsValidIdentifier(typeName)) Then
                    ' TODO: Report error.
                    Return Nothing
                End If

                Dim methodGroup As BoundMethodGroup = Nothing
                Dim boundAccess As BoundExpression = Nothing
                Dim targetType As TypeSymbol = Nothing
                Dim targetIsLValue As Boolean

                If Not TryBindXmlMemberNameToPattern(name,
                                                     searchForChildContentMethods:=False,
                                                     subject,
                                                     binder,
                                                     diagnostics,
                                                     targetType,
                                                     targetIsLValue,
                                                     methodGroup,
                                                     boundAccess,
                                                     supportsMultiContent:=Nothing) _
                Then
                    ' TODO: Report correct error.
                    ReportDiagnostic(diagnostics, name, ERRID.ERR_NameNotMember2, memberName, subjectType)
                    Return BadExpression(attribute, ErrorTypeSymbol.UnknownResultType)
                End If

                Dim value As BoundExpression = Nothing,
                    valueString As String = Nothing,
                    otherValue As BoundExpression = Nothing

                BindXmlAttributeValue(attribute.Value, targetType, targetIsLValue, binder, diagnostics, value, valueString, otherValue)

                If boundAccess IsNot Nothing Then
                    If otherValue IsNot Nothing Then
                        ' Type converter support required; report an error.
                        value = binder.ApplyConversion(otherValue.Syntax, targetType, otherValue, isExplicit:=False, diagnostics)

                    ElseIf (targetType.IsStringType() OrElse targetType.IsObjectType()) AndAlso valueString?.StartsWith("{") Then
                        ' Type converter support suggested; report a warning?
                        ' e.g. <TextBox Text="{Binding Name}" />
                    End If

                    boundAccess = binder.BindAssignmentTarget(boundAccess.Syntax, boundAccess, diagnostics)
                    Return binder.BindAssignment(attribute, boundAccess, value, diagnostics)
                End If

                ' TODO: This is definitely wrong; method groups can mix shared/instance.
                Dim theMethod = methodGroup.Methods(0)
                Dim arguments = ArrayBuilder(Of BoundExpression).GetInstance()

                If theMethod.IsShared Then
                    arguments.Add(subject)
                End If

                arguments.Add(value)

                If theMethod.ParameterCount > arguments.Count Then
                    arguments.Add(binder.CreateStringLiteral(attribute.Value, valueString, compilerGenerated:=True, diagnostics))
                End If

                If otherValue Is Nothing Then
                    If arguments.Count < theMethod.ParameterCount Then
                        arguments.Add(New BoundLiteral(attribute, ConstantValue.Nothing, type:=Nothing))
                    End If
                Else
                    ' Type converter required.
                    If arguments.Count + 1 > theMethod.ParameterCount Then
                        ' TODO: Report an error.
                    End If

                    arguments.Add(otherValue)
                End If

                Return binder.BindInvocationExpression(attribute,
                                                       attribute.Name,
                                                       TypeCharacter.None,
                                                       methodGroup,
                                                       arguments.ToImmutableAndFree(),
                                                       argumentNames:=Nothing,
                                                       diagnostics,
                                                       callerInfoOpt:=attribute)

            End Function

            Public Shared Sub BindXmlAttributeValue(
                                  node As XmlNodeSyntax,
                                  targetType As TypeSymbol,
                                  targetIsLValue As Boolean,
                                  binder As Binder,
                                  diagnostics As DiagnosticBag,
                                  ByRef value As BoundExpression,
                                  ByRef valueString As String,
                                  ByRef otherValue As BoundExpression
                              )

                ' TODO: This is probably the wrong place for this.
                targetType = targetType.GetNullableUnderlyingTypeOrSelf()

                Dim valueSyntax As SyntaxNode = node

                Select Case node.Kind
                    Case SyntaxKind.XmlString
                        Dim xmlString = DirectCast(node, XmlStringSyntax)

                        ' Review: Is trimming OK or should text exactly as written be preserved for converters?
                        valueString = GetXmlString(xmlString.TextTokens).Trim()
                        otherValue = Nothing

                        Select Case targetType.TypeKind
                            Case TypeKind.Delegate

                                ' TODO: Refactor this to share code with binding of AddressOf.
                                If String.IsNullOrEmpty(valueString) Then
                                    ' Empty string must have been a syntax error. 
                                    ' Just produce a bad expression and get out without producing any new errors.
                                    value = BadExpression(node, ErrorTypeSymbol.UnknownResultType)
                                    Return
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

                                    value = ReportDiagnosticAndProduceBadExpression(diagnostics, node, ErrorFactory.ErrorInfo(ERRID.ERR_NameNotDeclared1, valueString))
                                    Return
                                End If

                                Dim boundOperand = binder.BindSimpleName(result, node, options, typeArguments:=Nothing, diagnostics)
                                result.Free()

                                ' only accept MethodGroups as operands. More detailed checks (e.g. for Constructors follow later)
                                If boundOperand.Kind <> BoundKind.MethodGroup Then
                                    If Not boundOperand.HasErrors Then
                                        ReportDiagnostic(diagnostics, node, ERRID.ERR_AddressOfOperandNotMethod)
                                    End If

                                    value = BadExpression(node, boundOperand, LookupResultKind.NotAValue, ErrorTypeSymbol.UnknownResultType)
                                    Return
                                End If

                                Dim hasErrors As Boolean = False
                                Dim group = DirectCast(boundOperand, BoundMethodGroup)

                                If IsGroupOfConstructors(group) Then
                                    ReportDiagnostic(diagnostics, node, ERRID.ERR_InvalidConstructorCall)
                                    hasErrors = True
                                End If

                                value = New BoundAddressOfOperator(node, binder, group, hasErrors)

                            Case TypeKind.Enum

                                If Not SyntaxFacts.IsValidIdentifier(valueString) Then
                                    ' This supports the case of value converters like AccessKeys `Alt+S`.
                                    value = New BoundLiteral(node, ConstantValue.Null, type:=Nothing)
                                    Return
                                End If

                                ' TODO: Refactor this into some kind of BindMemberAccess overload.
                                Dim lookupResult As LookupResult = LookupResult.GetInstance()
                                Dim options As LookupOptions = LookupOptions.AllMethodsOfAnyArity
                                Dim useSiteDiagnostics As HashSet(Of DiagnosticInfo) = Nothing

                                binder.LookupMember(lookupResult, targetType, valueString, arity:=0, options, useSiteDiagnostics) ' overload resolution filters methods by arity.

                                If lookupResult.HasSymbol Then
                                    value = binder.BindSymbolAccess(node,
                                                                    lookupResult,
                                                                    options,
                                                                    New BoundTypeExpression(node, targetType),
                                                                    typeArgumentsOpt:=Nothing,
                                                                    QualificationKind.QualifiedViaTypeName,
                                                                    diagnostics)
                                Else
                                    value = ReportDiagnosticAndProduceBadExpression(diagnostics, node, ErrorFactory.ErrorInfo(ERRID.ERR_NameNotMember2, valueString, targetType))
                                End If

                            Case Else

                                Dim cv As ConstantValue = Nothing

                                Select Case targetType.SpecialType
                                    Case SpecialType.System_SByte
                                        Dim result As SByte
                                        If SByte.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                    Case SpecialType.System_Int16
                                        Dim result As Short
                                        If Short.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                    Case SpecialType.System_Int32
                                        Dim result As Integer
                                        If Integer.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                    Case SpecialType.System_Int64
                                        Dim result As Long
                                        If Long.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                    Case SpecialType.System_Byte
                                        Dim result As Byte
                                        If Byte.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                    Case SpecialType.System_UInt16
                                        Dim result As UShort
                                        If UShort.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                    Case SpecialType.System_UInt32
                                        Dim result As UInteger
                                        If UInteger.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                    Case SpecialType.System_UInt64
                                        Dim result As ULong
                                        If ULong.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                    Case SpecialType.System_Single
                                        Dim result As Single
                                        If Single.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                    Case SpecialType.System_Double
                                        Dim result As Double
                                        If Double.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                    Case SpecialType.System_Decimal
                                        ' TODO: Review if Decimal is XAML special type.
                                        Dim result As Decimal
                                        If Decimal.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                    Case SpecialType.System_Char
                                        ' TODO: Review is Char is a XAML special type.
                                        Dim result As Char
                                        If Char.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                    Case SpecialType.System_Boolean
                                        Dim result As Boolean
                                        If Boolean.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                    Case SpecialType.System_String,
                                         SpecialType.System_Object

                                        cv = ConstantValue.Create(valueString)

                                    Case SpecialType.System_DateTime
                                        ' TODO: Review if Date is a XAML special type.
                                        Dim result As Date
                                        If Date.TryParse(valueString, result) Then
                                            cv = ConstantValue.Create(result)
                                        End If

                                End Select

                                If cv IsNot Nothing Then
                                    value = New BoundLiteral(node, cv, binder.GetSpecialType(targetType.SpecialType, node, diagnostics))
                                Else
                                    value = New BoundLiteral(node, ConstantValue.Nothing, type:=Nothing)

                                    If targetIsLValue Then

                                        Dim options As LookupOptions = LookupOptions.AllMethodsOfAnyArity ' overload resolution filters methods by arity.
                                        options = options Or LookupOptions.MustNotBeReturnValueVariable

                                        Dim result As LookupResult = LookupResult.GetInstance()

                                        Dim useSiteDiagnostics As HashSet(Of DiagnosticInfo) = Nothing
                                        binder.Lookup(result, valueString, arity:=0, options, useSiteDiagnostics)
                                        diagnostics.Add(node, useSiteDiagnostics)

                                        If result.HasSymbol Then
                                            Dim boundOperand = binder.MakeValue(binder.BindSimpleName(result, node, options, typeArguments:=Nothing, diagnostics), diagnostics)

                                            ' TODO: Likely not the right way to ask but IsLValue isn't correct either.
                                            If boundOperand.Kind = BoundKind.PropertyAccess OrElse boundOperand.Kind = BoundKind.FieldAccess Then
                                                value = boundOperand
                                            End If
                                        End If

                                        result.Free()
                                    End If
                                End If

                        End Select

                    Case SyntaxKind.XmlEmbeddedExpression

                        Dim expressionBinder = New XmlEmbeddedExpressionBinder(binder)
                        ' Review: BindExpression or BindValue?
                        value = expressionBinder.BindValue(DirectCast(node, XmlEmbeddedExpressionSyntax).Expression, diagnostics)
                        valueSyntax = value.Syntax
                        valueString = Nothing

                    Case Else
                        Throw ExceptionUtilities.UnexpectedValue(node.Kind)
                End Select

                ' TODO: VB supports conversions that might be inappropriate to apply here; the set must be defined.
                Dim conversionSiteDiagnostics As HashSet(Of DiagnosticInfo) = Nothing
                Dim conversionInfo = Conversions.ClassifyConversion(value, targetType, binder, conversionSiteDiagnostics)

                If Conversions.ConversionExists(conversionInfo.Key) Then
                    If Not targetIsLValue Then
                        value = binder.ApplyConversion(valueSyntax, targetType, value, isExplicit:=False, diagnostics)
                    End If

                    otherValue = Nothing
                Else
                    otherValue = value
                    value = New BoundLiteral(node, ConstantValue.Nothing, targetType)
                End If
            End Sub

            Public Shared Sub BindXmlMemberElement(
                                  element As XmlNodeSyntax,
                                  sideEffectsBuilder As ArrayBuilder(Of BoundExpression),
                                  subject As BoundExpression,
                                  subjectType As TypeSymbol,
                                  binder As Binder,
                                  diagnostics As DiagnosticBag
                              )

                Select Case element.Kind
                    Case SyntaxKind.XmlElement
                        BindXmlMemberElement(DirectCast(element, XmlElementSyntax), sideEffectsBuilder, subject, subjectType, binder, diagnostics)

                    Case SyntaxKind.XmlEmptyElement

                        ' For now these are being filtered out as child elements, rather than member elements.
                        ' This may change however if a scenario like `<Type.Property SubProperty="Value" />` becomes interesting.
                        Throw ExceptionUtilities.Unreachable

                    Case Else
                        ' Should have been filtered out already.
                        Throw ExceptionUtilities.Unreachable
                End Select
            End Sub

            Public Shared Sub BindXmlMemberElement(
                                  element As XmlElementSyntax,
                                  sideEffectsBuilder As ArrayBuilder(Of BoundExpression),
                                  subject As BoundExpression,
                                  subjectType As TypeSymbol,
                                  binder As Binder,
                                  diagnostics As DiagnosticBag
                              )

                Debug.Assert(element.StartTag.Name.Kind = SyntaxKind.XmlName)

                Dim name = DirectCast(element.StartTag.Name, XmlNameSyntax)

                ' TODO: Report error if `name` specifies an XML namespace.

                Dim prefix As String = Nothing,
                    typeName As String = Nothing,
                    memberName As String = Nothing

                If Not TryGetXmlMemberNameParts(name, diagnostics, prefix, typeName, memberName) Then
                    Return
                End If

                If Not SyntaxFacts.IsValidIdentifier(memberName) OrElse (typeName IsNot Nothing AndAlso Not SyntaxFacts.IsValidIdentifier(typeName)) Then
                    ' TODO: Report error.
                    Return
                End If

                Dim methodGroup As BoundMethodGroup = Nothing
                Dim boundAccess As BoundExpression = Nothing
                Dim supportsMultiContent As Boolean = False
                Dim targetType As TypeSymbol = Nothing

                If Not TryBindXmlMemberNameToPattern(name,
                                                     searchForChildContentMethods:=True,
                                                     subject,
                                                     binder,
                                                     diagnostics,
                                                     targetType,
                                                     targetIsLValue:=Nothing,
                                                     methodGroup,
                                                     boundAccess,
                                                     supportsMultiContent) _
                Then
                    ' TODO: Report correct error.
                    ReportDiagnostic(diagnostics, name, ERRID.ERR_NameNotMember2, memberName, subjectType)
                End If

                Dim memberElements = ArrayBuilder(Of XmlNodeSyntax).GetInstance()
                Dim childElements = ArrayBuilder(Of XmlNodeSyntax).GetInstance()

                SeparateMemberAndChildElementsAndReportErrorForInterleaving(element.Content, memberElements, childElements, diagnostics)

                If memberElements.Count > 0 Then
                    ' TODO: Report error.

                End If

                If Not supportsMultiContent Then
                    For i = 1 To childElements.Count - 1
                        ' TODO: Report error.
                    Next
                End If

                For Each child In childElements
                    Dim boundChild = BindXmlChildContent(child, targetType, subject, subject.Type, binder, diagnostics)

                    If methodGroup IsNot Nothing Then
                        sideEffectsBuilder.Add(binder.BindInvocationExpression(child,
                                                                               element,
                                                                               TypeCharacter.None,
                                                                               methodGroup,
                                                                               ImmutableArray.Create(boundChild),
                                                                               Nothing,
                                                                               diagnostics,
                                                                               callerInfoOpt:=child))

                    ElseIf boundAccess IsNot Nothing Then
                        Debug.Assert(boundAccess.Kind = BoundKind.PropertyAccess OrElse boundAccess.Kind = BoundKind.FieldAccess)

                        boundAccess = binder.BindAssignmentTarget(boundAccess.Syntax, boundAccess, diagnostics)
                        boundChild = binder.ApplyConversion(child, targetType, boundChild, isExplicit:=False, diagnostics)
                        sideEffectsBuilder.Add(binder.BindAssignment(child, boundAccess, boundChild, diagnostics))

                    Else

                        sideEffectsBuilder.Add(boundChild)
                    End If
                Next

            End Sub

            Private Shared Function TryBindXmlMemberNameToPattern(
                                        name As XmlNameSyntax,
                                        searchForChildContentMethods As Boolean,
                                        subject As BoundExpression,
                                        binder As Binder,
                                        diagnostics As DiagnosticBag,
                                        ByRef targetType As TypeSymbol,
                                        ByRef targetIsLValue As Boolean,
                                        ByRef methodGroup As BoundMethodGroup,
                                        ByRef boundAccess As BoundExpression,
                                        ByRef supportsMultiContent As Boolean
                                    ) As Boolean

                targetType = Nothing
                methodGroup = Nothing
                boundAccess = Nothing
                supportsMultiContent = False

                ' TODO: Report error if `name` specifies an XML namespace.

                Dim prefix As String = Nothing,
                    typeName As String = Nothing,
                    memberName As String = Nothing

                If Not TryGetXmlMemberNameParts(name, diagnostics, prefix, typeName, memberName) Then
                    Return False
                End If

                If Not SyntaxFacts.IsValidIdentifier(memberName) OrElse (typeName IsNot Nothing AndAlso Not SyntaxFacts.IsValidIdentifier(typeName)) Then
                    ' TODO: Report error.
                    Return False
                End If

                Dim eventSymbol As EventSymbol = Nothing

                ' Possible patterns:
                ' Dim g = New Grid
                ' g.AddPChildContent(v) OR g.AddTPChildContent(v)
                ' g.AddP(v) OR g.AddTP(v)

                ' g.P.AddChildContent(v)

                ' g.SetP(v) OR g.SetTP(v)

                ' g.add_P(v) OR T.AddP(g, v)

                ' g.P = v OR T.SetP(g, v)

                ' Only support converters in the simplest case (because author wouldn't have supported them already).
                ' Can you think of a property that would need a value converter but NOT data binding?
                ' g.P = g.ConvertValue(v) OR T.SetP(g, g.ConvertValue(v))

                ' Possible? g.AddP(v1, v2)
                ' Possible? g.SetP(v1, v2)

                If searchForChildContentMethods AndAlso
                   (TryGetMethodGroup("Add" & typeName & memberName & "ChildContent", subject, name, binder, diagnostics, methodGroup) OrElse
                    TryGetMethodGroup("Add" & memberName & "ChildContent", subject, name, binder, diagnostics, methodGroup)) _
                Then
                    supportsMultiContent = True

                ElseIf TryGetMethodGroup("Add" & typeName & memberName, subject, name, binder, diagnostics, methodGroup) OrElse
                       TryGetMethodGroup("Add" & memberName, subject, name, binder, diagnostics, methodGroup) _
                Then
                    ' It's an event so technically it's true.
                    supportsMultiContent = True

                ElseIf TryGetPropertyOrFieldOrEventAccess(memberName, subject, name, binder, diagnostics, boundAccess) Then

                    If boundAccess.Kind = BoundKind.EventAccess Then
                        supportsMultiContent = True
                        eventSymbol = DirectCast(boundAccess, BoundEventAccess).EventSymbol
                        boundAccess = Nothing

                    ElseIf searchForChildContentMethods AndAlso TryGetMethodGroup("AddChildContent", boundAccess, name, binder, diagnostics, methodGroup) Then
                        supportsMultiContent = True

                    Else
                        targetType = boundAccess.Type
                        supportsMultiContent = False
                    End If
                End If

                If methodGroup Is Nothing AndAlso
                   (TryGetMethodGroup("Set" & typeName & memberName, subject, name, binder, diagnostics, methodGroup) OrElse
                    TryGetMethodGroup("Set" & memberName, subject, name, binder, diagnostics, methodGroup)) _
                Then
                    boundAccess = Nothing
                    supportsMultiContent = False
                End If

                If methodGroup Is Nothing AndAlso eventSymbol IsNot Nothing Then

                    supportsMultiContent = True
                    methodGroup = New BoundMethodGroup(name,
                                                       typeArgumentsOpt:=Nothing,
                                                       ImmutableArray.Create(eventSymbol.AddMethod),
                                                       LookupResultKind.Good,
                                                       receiverOpt:=subject,
                                                       QualificationKind.QualifiedViaValue)
                    targetType = eventSymbol.Type
                    Return True
                End If

                If methodGroup Is Nothing AndAlso typeName IsNot Nothing Then

                    Dim type = TryGetType(typeName, name, binder, diagnostics)
                    Dim boundType = New BoundTypeExpression(name, type)

                    If type IsNot Nothing Then

                        If TryGetMethodGroup("Add" & memberName, boundType, name, binder, diagnostics, methodGroup) Then
                            supportsMultiContent = True

                        ElseIf TryGetMethodGroup("Set" & memberName, boundType, name, binder, diagnostics, methodGroup) Then
                            supportsMultiContent = False

                        Else
                            supportsMultiContent = False
                        End If
                    End If
                End If

                If methodGroup IsNot Nothing Then
                    ' TODO: Ensure all methods have same first parameter type?
                    targetType = methodGroup.Methods(0).Parameters(0).Type
                    targetIsLValue = methodGroup.Methods(0).Parameters(0).IsByRef
                    Return True

                ElseIf boundAccess IsNot Nothing Then
                    Debug.Assert(targetType = boundAccess.Type)
                    Return True

                Else
                    Return False
                End If

            End Function

            Public Shared Sub BindXmlChildElements(
                                  childElements As ArrayBuilder(Of XmlNodeSyntax),
                                  sideEffectsBuilder As ArrayBuilder(Of BoundExpression),
                                  subject As BoundExpression,
                                  syntax As SyntaxNode,
                                  binder As Binder,
                                  diagnostics As DiagnosticBag
                              )

                If childElements.Count = 0 Then Return

                ' TODO: Should search each level of type-hierachy in turn so that derived classes can "override" content method.
                Dim methodGroup As BoundMethodGroup = Nothing
                Dim supportsMultiContent As Boolean

                If TryGetMethodGroup("AddChildContent", subject, syntax, binder, diagnostics, methodGroup) Then
                    supportsMultiContent = True
                ElseIf TryGetMethodGroup("SetChildContent", subject, syntax, binder, diagnostics, methodGroup) Then
                    supportsMultiContent = False
                Else
                    Debug.Assert(methodGroup Is Nothing)
                    ' TODO: Report error.

                    ' This is already an error. We want to bind the content anyway to be resilient but not report superfluous "cascading" errors.
                    supportsMultiContent = True
                End If

                If Not supportsMultiContent Then
                    For i = 1 To childElements.Count - 1
                        ' TODO: Report error.
                    Next
                End If

                ' TODO: This is probably wrong. If there are multiple `Add` or `Set` methods they may each take different types to support
                ' different outcomes.
                Dim naturalType = If(methodGroup IsNot Nothing, methodGroup.Methods(0).Parameters(0).Type, ErrorTypeSymbol.UnknownResultType)

                For Each child In childElements
                    Dim boundChild = BindXmlChildContent(child, naturalType, subject, subject.Type, binder, diagnostics)

                    If methodGroup IsNot Nothing Then
                        sideEffectsBuilder.Add(binder.BindInvocationExpression(child,
                                                                               syntax,
                                                                               TypeCharacter.None,
                                                                               methodGroup,
                                                                               ImmutableArray.Create(boundChild),
                                                                               Nothing,
                                                                               diagnostics,
                                                                               callerInfoOpt:=child))
                    Else

                        sideEffectsBuilder.Add(boundChild)
                    End If
                Next
            End Sub

            Public Shared Function BindXmlChildContent(
                                       content As XmlNodeSyntax,
                                       targetType As TypeSymbol,
                                       subject As BoundExpression,
                                       subjectType As TypeSymbol,
                                       binder As Binder,
                                       diagnostics As DiagnosticBag
                                   ) As BoundExpression

                ' Fold string content, implement whitespace rules.
                ' TODO: Verify whether XmlText and embedded are the only cases which should get use of the natural type.

                Select Case content.Kind
                    Case SyntaxKind.XmlElement
                        Return BindXmlContentElement(DirectCast(content, XmlElementSyntax), binder, diagnostics)

                    Case SyntaxKind.XmlEmptyElement
                        Return BindXmlContentEmptyElement(DirectCast(content, XmlEmptyElementSyntax), binder, diagnostics)

                    Case SyntaxKind.XmlText
                        ' This is possibly a bug. WPF will combine adjacent text sections (including CDATA), ignoring comments and processing instructions.
                        ' If that doesn't happen here it'll change the results for that scenario (e.g. what should be 1 ListBox item becomes 3).
                        Return BindXmlContentText(DirectCast(content, XmlTextSyntax), targetType, subject, subjectType, binder, diagnostics)

                    Case SyntaxKind.XmlCDataSection
                        Return BindXmlContentCDataSection(DirectCast(content, XmlCDataSectionSyntax), subject, subjectType, binder, diagnostics)

                    Case SyntaxKind.XmlProcessingInstruction,
                         SyntaxKind.XmlComment
                        ' Should have been filtered out.
                        Throw ExceptionUtilities.Unreachable

                    Case SyntaxKind.XmlEmbeddedExpression
                        Return BindXmlContentEmbeddedExpression(DirectCast(content, XmlEmbeddedExpressionSyntax), targetType, subject, subjectType, binder, diagnostics)

                    Case Else
                        Throw ExceptionUtilities.UnexpectedValue(content.Kind)
                End Select
            End Function

            Private Shared Function TryGetMethodGroup(
                                       methodName As String,
                                       receiver As BoundExpression,
                                       syntax As SyntaxNode,
                                       binder As Binder,
                                       diagnostics As DiagnosticBag,
                                       ByRef methodGroup As BoundMethodGroup
                                   ) As Boolean

                Dim result = LookupResult.GetInstance()

                If binder.GetMemberIfMatchesRequirements(methodName,
                                                         receiver.Type,
                                                         Function(s) s.Kind = SymbolKind.Method,
                                                         result,
                                                         syntax,
                                                         diagnostics) _
                Then
                    Debug.Assert(result.IsGood)
                    Debug.Assert(Not result.Symbols(0).IsShared)

                    ' TODO: This method should handle setting shared members through declarative syntax.
                    methodGroup = binder.CreateBoundMethodGroup(syntax,
                                                                result,
                                                                LookupOptions.AllMethodsOfAnyArity,
                                                                receiver,
                                                                Nothing,
                                                                QualificationKind.QualifiedViaValue).MakeCompilerGenerated()
                    result.Free()
                    Return True
                Else
                    methodGroup = Nothing
                    result.Free()
                    Return False
                End If
            End Function

            Private Shared Function TryGetPropertyOrFieldOrEventAccess(
                                       name As String,
                                       receiver As BoundExpression,
                                       syntax As SyntaxNode,
                                       binder As Binder,
                                       diagnostics As DiagnosticBag,
                                       ByRef boundAccess As BoundExpression
                                   ) As Boolean

                Dim result = LookupResult.GetInstance()

                If binder.GetMemberIfMatchesRequirements(name,
                                                         receiver.Type,
                                                         Function(s) (s.Kind = SymbolKind.Property AndAlso DirectCast(s, PropertySymbol).ParameterCount = 0) OrElse
                                                                     s.Kind = SymbolKind.Event OrElse
                                                                     s.Kind = SymbolKind.Field,
                                                         result,
                                                         syntax,
                                                         diagnostics) _
                Then
                    Debug.Assert(result.IsGood)

                    Select Case result.SingleSymbol.Kind
                        Case SymbolKind.Property

                            Dim p = DirectCast(result.SingleSymbol, PropertySymbol)
                            boundAccess = New BoundPropertyAccess(syntax,
                                                                  p,
                                                                  propertyGroupOpt:=Nothing,
                                                                  PropertyAccessKind.Unknown,
                                                                  isWriteable:=True,
                                                                  receiver,
                                                                  ImmutableArray(Of BoundExpression).Empty)

                        Case SymbolKind.Field

                            Dim f = DirectCast(result.SingleSymbol, FieldSymbol)
                            boundAccess = New BoundFieldAccess(syntax, receiver, f, isLValue:=True, f.Type)

                        Case SymbolKind.Event

                            Dim e = DirectCast(result.SingleSymbol, EventSymbol)
                            boundAccess = New BoundEventAccess(syntax, receiver, e, receiver.Type)

                        Case Else
                            Throw ExceptionUtilities.Unreachable
                    End Select

                    result.Free()
                    Return True
                Else
                    boundAccess = Nothing
                    result.Free()
                    Return False
                End If
            End Function

            Public Shared Function BindXmlContentElement(
                                       content As XmlElementSyntax,
                                       binder As Binder,
                                       diagnostics As DiagnosticBag
                                   ) As BoundExpression

                Dim startTag = content.StartTag

                If startTag.Name.Kind = SyntaxKind.XmlName Then
                    ' This is likely wrong. Just binding this normally would likely be more correct because it's not a given that this
                    ' element matches the pattern. But verifying the pattern involves looking up namespaces and for the purposes of this
                    ' prototype, I'm just going to say that all XML literals inside the topmost literal must be bound using the pattern
                    ' rather than allowing interleaving pattern literals and traditional XElement literals. 
                    Return binder.BindXmlPatternElement(content, startTag.Name, startTag.Attributes, content.Content, elementNamespaceRaw:=Nothing, rootInfoOpt:=Nothing, diagnostics)

                Else
                    Return binder.BindXmlElement(content, rootInfoOpt:=Nothing, diagnostics)
                End If

            End Function

            Public Shared Function BindXmlContentEmptyElement(
                                       content As XmlEmptyElementSyntax,
                                       binder As Binder,
                                       diagnostics As DiagnosticBag
                                   ) As BoundExpression

                If content.Name.Kind = SyntaxKind.XmlName Then
                    ' This is likely wrong. Just binding this normally would likely be more correct because it's not a given that this
                    ' element matches the pattern. But verifying the pattern involves looking up namespaces and for the purposes of this
                    ' prototype, I'm just going to say that all XML literals inside the topmost literal must be bound using the pattern
                    ' rather than allowing interleaving pattern literals and traditional XElement literals. 
                    Return binder.BindXmlPatternElement(content, content.Name, content.Attributes, content:=Nothing, elementNamespaceRaw:=Nothing, rootInfoOpt:=Nothing, diagnostics)

                Else
                    Return binder.BindXmlEmptyElement(content, rootInfoOpt:=Nothing, diagnostics)
                End If

            End Function

            Public Shared Function BindXmlContentText(
                                       content As XmlTextSyntax,
                                       targetType As TypeSymbol,
                                       subject As BoundExpression,
                                       subjectType As TypeSymbol,
                                       binder As Binder,
                                       diagnostics As DiagnosticBag
                                   ) As BoundExpression

                Dim textValue = GetXmlString(content.TextTokens)

                ' TODO: Support conversion from string to numerics, boolean, enum, etc. (but not delegate?)
                Return binder.CreateStringLiteral(content, textValue, compilerGenerated:=False, diagnostics)
            End Function

            Public Shared Function BindXmlContentCDataSection(
                                       content As XmlCDataSectionSyntax,
                                       subject As BoundExpression,
                                       subjectType As TypeSymbol,
                                       binder As Binder,
                                       diagnostics As DiagnosticBag
                                   ) As BoundExpression

                Dim textValue = GetXmlString(content.TextTokens)

                Return binder.CreateStringLiteral(content, textValue, compilerGenerated:=False, diagnostics)
            End Function

            Public Shared Function BindXmlContentEmbeddedExpression(
                                       content As XmlEmbeddedExpressionSyntax,
                                       targetType As TypeSymbol,
                                       subject As BoundExpression,
                                       subjectType As TypeSymbol,
                                       binder As Binder,
                                       diagnostics As DiagnosticBag
                                   ) As BoundExpression

                Dim expressionBinder = New XmlEmbeddedExpressionBinder(binder)
                ' Review: BindExpression or BindValue?
                Return expressionBinder.BindValue(content.Expression, diagnostics)
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

            Private Shared Function TryGetType(typeName As String,
                                        syntax As SyntaxNode,
                                        binder As Binder,
                                        diagnostics As DiagnosticBag
                                    ) As NamedTypeSymbol

                Dim lookupResult As LookupResult = LookupResult.GetInstance()
                binder.Lookup(lookupResult, typeName, 0, LookupOptions.NamespacesOrTypesOnly, useSiteDiagnostics:=Nothing)

                If Not (lookupResult.IsGood AndAlso lookupResult.HasSingleSymbol AndAlso lookupResult.SingleSymbol.Kind = SymbolKind.NamedType) Then
                    lookupResult.Free()
                    Return Nothing
                End If

                Dim type = DirectCast(lookupResult.SingleSymbol, NamedTypeSymbol)
                lookupResult.Clear()

                Return type

            End Function

            Private Shared Function BindXmlNameAsMember(node As XmlNodeSyntax, subject As BoundExpression, subjectType As TypeSymbol, isInAttribute As Boolean, binder As Binder, diagnostics As DiagnosticBag) As Symbol

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

                If Not SyntaxFacts.IsValidIdentifier(memberName) OrElse (typeName IsNot Nothing AndAlso Not SyntaxFacts.IsValidIdentifier(typeName)) Then
                    ' TODO: Report error.
                    Return Nothing
                End If

                ' Possible patterns:
                ' Dim g = New Grid
                ' g.AddPChildContent(v) OR g.AddTPChildContent(v)
                ' g.P.AddChildContent(v)

                ' g.SetP(v) OR g.SetTP(v)
                ' g.P = v OR T.SetP(g, v)

                ' Only support converters in the simplest case (because author wouldn't have supported them already).
                ' Can you think of a property that would need a value converter but NOT data binding?
                ' g.P = g.ConvertValue(v) OR T.SetP(g, g.ConvertValue(v))

                ' Possible? g.AddP(v1, v2)
                ' Possible? g.SetP(v1, v2)



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

            Public Shared Sub SeparateMemberAndChildElementsAndReportErrorForInterleaving(
                                  mixedContent As SyntaxList(Of XmlNodeSyntax),
                                  memberElements As ArrayBuilder(Of XmlNodeSyntax),
                                  childElements As ArrayBuilder(Of XmlNodeSyntax),
                                  diagnostics As DiagnosticBag
                              )

                Dim contentEnumerator = mixedContent.GetEnumerator()

                If Not contentEnumerator.MoveNext() Then Return

                ' First look for member elements like `<Grid.RowDefinitions>` at the beginning.
                If GetMemberElements(contentEnumerator, memberElements, diagnostics) Then

                    ' Then look for children like `<Button />`
                ElseIf GetChildElements(contentEnumerator, childElements, diagnostics) Then

                    ' But member elements can also come at the end without error if they're all at the end.
                ElseIf memberElements.Count = 0 AndAlso GetMemberElements(contentEnumerator, memberElements, diagnostics) Then

                    ' We only get here if there's still content and we've already seen both members and children.
                Else
                    ' TODO: Report error on 'current'.

                    Do Until GetMemberElements(contentEnumerator, memberElements, diagnostics) OrElse
                             GetChildElements(contentEnumerator, childElements, diagnostics)
                    Loop
                End If
            End Sub

            ''' <summary>
            ''' Returns true if <paramref name="enumerator"/> was entirely consumed.
            ''' </summary>
            Private Shared Function GetMemberElements(
                                        ByRef enumerator As SyntaxList(Of XmlNodeSyntax).Enumerator,
                                        builder As ArrayBuilder(Of XmlNodeSyntax),
                                        diagnostics As DiagnosticBag
                                    ) As Boolean

                Do
                    Select Case enumerator.Current.Kind
                        Case SyntaxKind.XmlElement

                            Dim content = DirectCast(enumerator.Current, XmlElementSyntax)

                            ' Note: If name is malformed TryGetXmlTypeOrMemberNameParts will return false. 
                            ' This is fine as such errors should be bound as content, if at all.
                            Dim memberName As String = Nothing
                            If content.StartTag.Name.Kind = SyntaxKind.XmlName AndAlso
                               TryGetXmlTypeOrMemberNameParts(DirectCast(content.StartTag.Name, XmlNameSyntax), diagnostics, prefix:=Nothing, typeName:=Nothing, memberName) AndAlso
                               memberName IsNot Nothing _
                            Then
                                builder.Add(content)
                            Else
                                Return False
                            End If

                        Case SyntaxKind.XmlEmptyElement

                            ' Even if its using a name like `<Grid.RowDefinitions Capacity="4" />` process it as content.
                            Return False

                        Case SyntaxKind.XmlText

                            Dim content = DirectCast(enumerator.Current, XmlTextSyntax)

                            Dim text = GetXmlString(content.TextTokens)

                            If Not String.IsNullOrWhiteSpace(text) Then
                                Return False
                            End If

                        Case SyntaxKind.XmlComment,
                             SyntaxKind.XmlProcessingInstruction

                            ' Ignore.
                            Continue Do

                        Case Else
                            Return False
                    End Select

                Loop While enumerator.MoveNext()

                Return True
            End Function

            ''' <summary>
            ''' Returns true if <paramref name="enumerator"/> was entirely consumed.
            ''' </summary>
            Private Shared Function GetChildElements(
                                        ByRef enumerator As SyntaxList(Of XmlNodeSyntax).Enumerator,
                                        builder As ArrayBuilder(Of XmlNodeSyntax),
                                        diagnostics As DiagnosticBag
                                    ) As Boolean

                Do
                    Select Case enumerator.Current.Kind
                        Case SyntaxKind.XmlElement

                            Dim content = DirectCast(enumerator.Current, XmlElementSyntax)
                            Dim memberName As String = Nothing

                            If content.StartTag.Name.Kind <> SyntaxKind.XmlName Then
                                ' Any node with an embedded expression as its name are children.
                                builder.Add(content)

                            ElseIf Not TryGetXmlTypeOrMemberNameParts(DirectCast(content.StartTag.Name, XmlNameSyntax), diagnostics, prefix:=Nothing, typeName:=Nothing, memberName) Then
                                ' Malformed nodes such as `<System.Windows.Controls.Button></System.Windows.Controls.Button>` are children.
                                builder.Add(content)

                            ElseIf memberName Is Nothing Then
                                builder.Add(content)

                            Else
                                Return False
                            End If

                        Case SyntaxKind.XmlEmptyElement

                            ' Because binding the attributes might produce a better editing experience in the presence of errors,
                            ' include all empty elements as children even malformed ones like `<System.Windows.Controls.Button />`.
                            builder.Add(enumerator.Current)

                        Case SyntaxKind.XmlText

                            Dim content = DirectCast(enumerator.Current, XmlTextSyntax)

                            Dim text = GetXmlString(content.TextTokens)

                            If Not String.IsNullOrWhiteSpace(text) Then
                                builder.Add(enumerator.Current)
                            End If

                        Case SyntaxKind.XmlComment,
                             SyntaxKind.XmlProcessingInstruction

                            ' Ignore.
                            Continue Do

                        Case Else
                            Return False
                    End Select

                Loop While enumerator.MoveNext()

                Return True

            End Function

            ''' <summary>
            ''' Returns true if <paramref name="name"/> is well-formed (`Type` or `Type.Member` but not `Type.Member.More`). 
            ''' If <paramref name="name"/> contains no dots (e.g. `&lt;Button&gt;`), <paramref name="memberName"/> will be null.
            ''' </summary>
            Private Shared Function TryGetXmlTypeOrMemberNameParts(
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

            ''' <summary>
            ''' If <paramref name="name"/> contains no dots (e.g. `&lt;Button&gt;`), <paramref name="typeName"/> will be null.
            ''' </summary>
            Private Shared Function TryGetXmlMemberNameParts(
                                        name As XmlNameSyntax,
                                        diagnostics As DiagnosticBag,
                                        ByRef prefix As String,
                                        ByRef typeName As String,
                                        ByRef memberName As String
                                    ) As Boolean

                prefix = If(name.Prefix?.Name.ValueText, "")

                Dim localNameText = name.LocalName.ValueText
                Dim localNameParts = localNameText.Split("."c)

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
