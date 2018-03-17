﻿' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis.PooledObjects

Namespace Microsoft.CodeAnalysis.VisualBasic.Symbols

    Friend Module SynthesizedPropertyAccessorHelper

        Friend Function GetBoundMethodBody(accessor As MethodSymbol,
                                           backingField As FieldSymbol,
                                           ByRef methodBodyBinder As Binder,
                                           compilationState As TypeCompilationState,
                                           diagnostics As DiagnosticBag) As BoundBlock

            Dim result As BoundBlock = GetBoundMethodBodyWithPropertyHandlers(accessor, backingField, methodBodyBinder, compilationState, diagnostics)
            If result IsNot Nothing Then Return result

            methodBodyBinder = Nothing

            ' NOTE: Current implementation of this method does generate the code for both getter and setter,
            '       Ideally it could have been split into two different implementations, but the code gen is
            '       quite similar in these two cases and current types hierarchy makes this solution preferable

            Dim propertySymbol = DirectCast(accessor.AssociatedSymbol, PropertySymbol)

            Dim syntax = DirectCast(VisualBasic.VisualBasicSyntaxTree.Dummy.GetRoot(), VisualBasicSyntaxNode)
            Dim meSymbol As ParameterSymbol = Nothing
            Dim meReference As BoundExpression = Nothing

            If Not accessor.IsShared Then
                meSymbol = accessor.MeParameter
                meReference = New BoundMeReference(syntax, meSymbol.Type)
            End If

            Dim isOverride As Boolean = propertySymbol.IsWithEvents AndAlso propertySymbol.IsOverrides

            Dim field As FieldSymbol = Nothing
            Dim fieldAccess As BoundFieldAccess = Nothing

            Dim myBaseReference As BoundExpression = Nothing
            Dim baseGet As BoundExpression = Nothing

            If isOverride Then
                ' overriding property gets its value via a base call
                myBaseReference = New BoundMyBaseReference(syntax, meSymbol.Type)
                Dim baseGetSym = propertySymbol.GetMethod.OverriddenMethod

                baseGet = New BoundCall(
                    syntax,
                    baseGetSym,
                    Nothing,
                    myBaseReference,
                    ImmutableArray(Of BoundExpression).Empty,
                    Nothing,
                    type:=baseGetSym.ReturnType,
                    suppressObjectClone:=True)
            Else
                ' not overriding property operates with field
                field = backingField
                fieldAccess = New BoundFieldAccess(syntax, meReference, field, True, field.Type)
            End If

            Dim exitLabel = New GeneratedLabelSymbol("exit")

            Dim statements As ArrayBuilder(Of BoundStatement) = ArrayBuilder(Of BoundStatement).GetInstance
            Dim locals As ImmutableArray(Of LocalSymbol)
            Dim returnLocal As BoundLocal

            If accessor.MethodKind = MethodKind.PropertyGet Then
                ' Declare local variable for function return.
                Dim local = New SynthesizedLocal(accessor, accessor.ReturnType, SynthesizedLocalKind.LoweringTemp)

                Dim returnValue As BoundExpression
                If isOverride Then
                    returnValue = baseGet
                Else
                    returnValue = fieldAccess.MakeRValue()
                End If

                statements.Add(New BoundReturnStatement(syntax, returnValue, local, exitLabel).MakeCompilerGenerated())

                locals = ImmutableArray.Create(Of LocalSymbol)(local)
                returnLocal = New BoundLocal(syntax, local, isLValue:=False, type:=local.Type)
            Else
                Debug.Assert(accessor.MethodKind = MethodKind.PropertySet)

                ' NOTE: at this point number of parameters in a VALID property must be 1.
                '       In the case when an auto-property has some parameters we assume that 
                '       ERR_AutoPropertyCantHaveParams(36759) is already generated,
                '       in this case we just ignore all the parameters and assume that the 
                '       last parameter is what we need to use below
                Debug.Assert(accessor.ParameterCount >= 1)
                Dim parameter = accessor.Parameters(accessor.ParameterCount - 1)
                Dim parameterAccess = New BoundParameter(syntax, parameter, isLValue:=False, type:=parameter.Type)

                Dim eventsToHookup As ArrayBuilder(Of ValueTuple(Of EventSymbol, PropertySymbol)) = Nothing

                ' contains temps for handler delegates followed by other stuff that is needed.
                ' so it will have at least eventsToHookup.Count temps
                Dim temps As ArrayBuilder(Of LocalSymbol) = Nothing
                ' accesses to the handler delegates
                ' we use them once to unhook from old source and then again to hook to the new source
                Dim handlerlocalAccesses As ArrayBuilder(Of BoundLocal) = Nothing

                ' //process Handles that need to be hooked up in this method
                ' //if there are events to hook up, the body will look like this:
                '
                ' Dim tempHandlerLocal = AddressOf handlerMethod   ' addressOf is already bound and may contain conversion
                ' . . .
                ' Dim tempHandlerLocalN = AddressOf handlerMethodN   
                '
                ' Dim valueTemp = [ _backingField | BaseGet ] 
                ' If valueTemp isnot nothing
                '
                '       // unhook handlers from the old value. 
                '       // Note that we can use the handler temps we have just created. 
                '       // Delegate identity is {target, method} so that will work
                '
                '       valueTemp.E1.Remove(tempLocalHandler1)
                '       valueTemp.E2.Remove(tempLocalHandler2)
                '
                ' End If
                '
                ' //Now store the new value
                '
                ' [ _backingField = value | BaseSet(value) ]
                ' 
                ' // re-read the value (we use same assignment here as before)
                ' valueTemp = [ _backingField | BaseGet ] 
                '
                ' If valueTemp isnot nothing
                '
                '       // re-hook handlers to the new value. 
                '
                '       valueTemp.E1.Add(tempLocalHandler1)
                '       valueTemp.E2.Add(tempLocalHandler2)
                '
                ' End If
                '
                If propertySymbol.IsWithEvents Then
                    For Each member In accessor.ContainingType.GetMembers()
                        If member.Kind = SymbolKind.Method Then
                            Dim methodMember = DirectCast(member, MethodSymbol)

                            Dim handledEvents = methodMember.HandledEvents

                            ' if method has definition and implementation parts
                            ' their "Handles" should be merged.
                            If methodMember.IsPartial Then
                                Dim implementationPart = methodMember.PartialImplementationPart
                                If implementationPart IsNot Nothing Then
                                    handledEvents = handledEvents.Concat(implementationPart.HandledEvents)
                                Else
                                    ' partial methods with no implementation do not handle anything
                                    Continue For
                                End If
                            End If

                            If Not handledEvents.IsEmpty Then
                                For Each handledEvent In handledEvents
                                    If handledEvent.hookupMethod = accessor Then
                                        If eventsToHookup Is Nothing Then
                                            eventsToHookup = ArrayBuilder(Of ValueTuple(Of EventSymbol, PropertySymbol)).GetInstance
                                            temps = ArrayBuilder(Of LocalSymbol).GetInstance
                                            handlerlocalAccesses = ArrayBuilder(Of BoundLocal).GetInstance
                                        End If

                                        eventsToHookup.Add(New ValueTuple(Of EventSymbol, PropertySymbol)(
                                                           DirectCast(handledEvent.EventSymbol, EventSymbol),
                                                           DirectCast(handledEvent.WithEventsSourceProperty, PropertySymbol)))
                                        Dim handlerLocal = New SynthesizedLocal(accessor, handledEvent.delegateCreation.Type, SynthesizedLocalKind.LoweringTemp)
                                        temps.Add(handlerLocal)

                                        Dim localAccess = New BoundLocal(syntax, handlerLocal, handlerLocal.Type)
                                        handlerlocalAccesses.Add(localAccess.MakeRValue())

                                        Dim handlerLocalinit = New BoundExpressionStatement(
                                                               syntax,
                                                               New BoundAssignmentOperator(
                                                                   syntax,
                                                                   localAccess,
                                                                   handledEvent.delegateCreation,
                                                                   False,
                                                                   localAccess.Type))

                                        statements.Add(handlerLocalinit)

                                    End If
                                Next
                            End If
                        End If
                    Next
                End If

                Dim withEventsLocalAccess As BoundLocal = Nothing
                Dim withEventsLocalStore As BoundExpressionStatement = Nothing

                ' need to unhook old handlers before setting a new event source
                If eventsToHookup IsNot Nothing Then
                    Dim withEventsValue As BoundExpression
                    If isOverride Then
                        withEventsValue = baseGet
                    Else
                        withEventsValue = fieldAccess.MakeRValue()
                    End If

                    Dim withEventsLocal = New SynthesizedLocal(accessor, withEventsValue.Type, SynthesizedLocalKind.LoweringTemp)
                    temps.Add(withEventsLocal)

                    withEventsLocalAccess = New BoundLocal(syntax, withEventsLocal, withEventsLocal.Type)
                    withEventsLocalStore = New BoundExpressionStatement(
                        syntax,
                        New BoundAssignmentOperator(
                            syntax,
                            withEventsLocalAccess,
                            withEventsValue,
                            True,
                            withEventsLocal.Type))

                    statements.Add(withEventsLocalStore)

                    ' if witheventsLocalStore isnot nothing
                    '           ...
                    '           withEventsLocalAccess.eventN_remove(handlerLocalN)
                    '           ...
                    Dim eventRemovals = ArrayBuilder(Of BoundStatement).GetInstance
                    For i As Integer = 0 To eventsToHookup.Count - 1
                        Dim eventSymbol As EventSymbol = eventsToHookup(i).Item1
                        ' Normally, we would synthesize lowered bound nodes, but we know that these nodes will
                        ' be run through the LocalRewriter.  Let the LocalRewriter handle the special code for
                        ' WinRT events.
                        Dim withEventsProviderAccess As BoundExpression = withEventsLocalAccess

                        Dim providerProperty = eventsToHookup(i).Item2
                        If providerProperty IsNot Nothing Then
                            withEventsProviderAccess = New BoundPropertyAccess(syntax,
                                                                               providerProperty,
                                                                               Nothing,
                                                                               PropertyAccessKind.Get,
                                                                               False,
                                                                               If(providerProperty.IsShared, Nothing, withEventsLocalAccess),
                                                                               ImmutableArray(Of BoundExpression).Empty)
                        End If

                        eventRemovals.Add(
                            New BoundRemoveHandlerStatement(
                                syntax:=syntax,
                                eventAccess:=New BoundEventAccess(syntax, withEventsProviderAccess, eventSymbol, eventSymbol.Type),
                                handler:=handlerlocalAccesses(i)))
                    Next

                    Dim removalStatement = New BoundStatementList(syntax, eventRemovals.ToImmutableAndFree)

                    Dim conditionalRemoval = New BoundIfStatement(
                                             syntax,
                                             (New BoundBinaryOperator(
                                                 syntax,
                                                 BinaryOperatorKind.IsNot,
                                                 withEventsLocalAccess.MakeRValue(),
                                                 New BoundLiteral(syntax, ConstantValue.Nothing,
                                                                  accessor.ContainingAssembly.GetSpecialType(SpecialType.System_Object)),
                                                 False,
                                                 accessor.ContainingAssembly.GetSpecialType(SpecialType.System_Boolean))).MakeCompilerGenerated,
                                             removalStatement,
                                             Nothing)

                    statements.Add(conditionalRemoval.MakeCompilerGenerated)
                End If

                ' set the value of the property
                ' if it is overriding, call the base
                ' otherwise assign to associated field.
                Dim valueSettingExpression As BoundExpression
                If isOverride Then
                    Dim baseSet = accessor.OverriddenMethod
                    valueSettingExpression = New BoundCall(
                        syntax,
                        baseSet,
                        Nothing,
                        myBaseReference,
                        ImmutableArray.Create(Of BoundExpression)(parameterAccess),
                        Nothing,
                        suppressObjectClone:=True,
                        type:=baseSet.ReturnType)
                Else
                    valueSettingExpression = New BoundAssignmentOperator(
                        syntax,
                        fieldAccess,
                        parameterAccess,
                        suppressObjectClone:=False,
                        type:=propertySymbol.Type)
                End If

                statements.Add(
                    (New BoundExpressionStatement(
                        syntax,
                        valueSettingExpression).MakeCompilerGenerated()))


                ' after setting new event source, hookup handlers
                If eventsToHookup IsNot Nothing Then
                    statements.Add(withEventsLocalStore)

                    ' if witheventsLocalStore isnot nothing
                    '           ...
                    '           withEventsLocalAccess.eventN_add(handlerLocalN)
                    '           ...
                    Dim eventAdds = ArrayBuilder(Of BoundStatement).GetInstance
                    For i As Integer = 0 To eventsToHookup.Count - 1
                        Dim eventSymbol As EventSymbol = eventsToHookup(i).Item1
                        ' Normally, we would synthesize lowered bound nodes, but we know that these nodes will
                        ' be run through the LocalRewriter.  Let the LocalRewriter handle the special code for
                        ' WinRT events.
                        Dim withEventsProviderAccess As BoundExpression = withEventsLocalAccess
                        Dim providerProperty = eventsToHookup(i).Item2
                        If providerProperty IsNot Nothing Then
                            withEventsProviderAccess = New BoundPropertyAccess(syntax,
                                                                               providerProperty,
                                                                               Nothing,
                                                                               PropertyAccessKind.Get,
                                                                               False,
                                                                               If(providerProperty.IsShared, Nothing, withEventsLocalAccess),
                                                                               ImmutableArray(Of BoundExpression).Empty)
                        End If

                        eventAdds.Add(
                            New BoundAddHandlerStatement(
                                syntax:=syntax,
                                eventAccess:=New BoundEventAccess(syntax, withEventsProviderAccess, eventSymbol, eventSymbol.Type),
                                handler:=handlerlocalAccesses(i)))
                    Next

                    Dim addStatement = New BoundStatementList(syntax, eventAdds.ToImmutableAndFree())

                    Dim conditionalAdd = New BoundIfStatement(
                                             syntax,
                                             (New BoundBinaryOperator(
                                                 syntax,
                                                 BinaryOperatorKind.IsNot,
                                                 withEventsLocalAccess.MakeRValue(),
                                                 New BoundLiteral(syntax, ConstantValue.Nothing,
                                                                  accessor.ContainingAssembly.GetSpecialType(SpecialType.System_Object)),
                                                 False,
                                                 accessor.ContainingAssembly.GetSpecialType(SpecialType.System_Boolean))).MakeCompilerGenerated,
                                             addStatement,
                                             Nothing)

                    statements.Add(conditionalAdd.MakeCompilerGenerated)
                End If

                locals = If(temps Is Nothing, ImmutableArray(Of LocalSymbol).Empty, temps.ToImmutableAndFree)
                returnLocal = Nothing

                If eventsToHookup IsNot Nothing Then
                    eventsToHookup.Free()
                    handlerlocalAccesses.Free()
                End If
            End If

            statements.Add((New BoundLabelStatement(syntax, exitLabel)).MakeCompilerGenerated())
            statements.Add((New BoundReturnStatement(syntax, returnLocal, Nothing, Nothing)).MakeCompilerGenerated())

            Return (New BoundBlock(syntax, Nothing, locals, statements.ToImmutableAndFree())).MakeCompilerGenerated()
        End Function

        Private Function GetBoundMethodBodyWithPropertyHandlers(
                             accessor As MethodSymbol,
                             backingField As FieldSymbol,
                             binder As Binder,
                             compilationState As TypeCompilationState,
                             diagnostics As DiagnosticBag
                         ) As BoundBlock
            ' TODO: Handler Getters.
            If accessor.MethodKind <> MethodKind.PropertyGet AndAlso accessor.MethodKind <> MethodKind.PropertySet Then Return Nothing

            Dim isGet = (accessor.MethodKind = MethodKind.PropertyGet)

            Debug.Assert(binder IsNot Nothing)

            Dim propertySymbol = DirectCast(accessor.AssociatedSymbol, SourcePropertySymbol)

            Dim attributes = propertySymbol.GetAttributes()
            If attributes.Length = 0 Then Return Nothing

            Dim handlerAttributes = ArrayBuilder(Of VisualBasicAttributeData).GetInstance()
            For Each attribute In propertySymbol.GetAttributes()
                If attribute.AttributeClass.IsErrorType() Then Continue For

                ' TODO: This should be an assignability check with a well-known type.
                If Not CaseInsensitiveComparison.Equals(attribute.AttributeClass.BaseTypeNoUseSiteDiagnostics.Name, "PropertyHandlerAttribute") Then Continue For

                handlerAttributes.Add(attribute)
            Next
            If handlerAttributes.Count = 0 Then Return Nothing

            Dim syntax = VisualBasicSyntaxTree.Dummy.GetRoot()

            Dim factory = New SyntheticBoundNodeFactory(accessor, accessor, syntax, compilationState, diagnostics)

            ' TODO: Getters
            ' Given a property of the form:
            ' <Handler1(...), Handler2(...), ...> Property P As T
            ' Getter should look like this:
            ' Get
            '     Dim value = _P
            '     If Handler1Method(NameOf(P), _P, value) Then Return value
            '     If Handler2Method(NameOf(P), _P, value) Then Return value
            '     Return value
            ' End Get

            ' Setter should look like this:
            ' Set
            '     If Handler1Method(NameOf(P), _P, value) Then Return
            '     If Handler2Method(NameOf(P), _P, value) Then Return
            '     _P = value
            ' End Set

            Dim fieldAccessReceiver As BoundExpression
            If accessor.IsShared Then
                fieldAccessReceiver = factory.Type(accessor.ContainingType)
            Else
                fieldAccessReceiver = factory.Me()
            End If

            Dim propertyName As BoundLiteral = factory.StringLiteral(ConstantValue.Create(propertySymbol.Name))
            Dim fieldAccess As BoundFieldAccess = factory.Field(fieldAccessReceiver, backingField, isLValue:=True)
            Dim valueExpression As BoundExpression

            Dim statements = ArrayBuilder(Of BoundStatement).GetInstance(2 + handlerAttributes.Count)

            Dim handlerSuffix As String
            Dim returnLocal As LocalSymbol
            Dim valueLocal As LocalSymbol
            If isGet Then
                returnLocal = factory.SynthesizedLocal(accessor.ReturnType)
                valueLocal = factory.SynthesizedLocal(accessor.ReturnType)
                valueExpression = factory.Local(valueLocal, isLValue:=True)
                statements.Add(factory.Assignment(valueExpression, fieldAccess.MakeRValue()))
                handlerSuffix = "OnPropertyGet"
            Else
                returnLocal = Nothing
                valueLocal = Nothing
                valueExpression = factory.Parameter(accessor.Parameters.Last)
                handlerSuffix = "OnPropertySet"
            End If

            For Each attribute In handlerAttributes
                Dim handlerName = attribute.AttributeClass.Name
                If handlerName.EndsWith("Attribute", StringComparison.OrdinalIgnoreCase) Then
                    handlerName = handlerName.Substring(0, handlerName.Length - 9)
                End If

                Dim lookup = LookupResult.GetInstance()
                Dim useSiteDiagnostics As HashSet(Of DiagnosticInfo) = Nothing
                Dim options = LookupOptions.MethodsOnly Or
                              LookupOptions.AllMethodsOfAnyArity
                Dim qualificationKind As QualificationKind

                If accessor.IsShared Then
                    options = options Or LookupOptions.MustNotBeInstance
                    qualificationKind = QualificationKind.QualifiedViaTypeName
                Else
                    options = options Or LookupOptions.MustBeInstance Or LookupOptions.IgnoreExtensionMethods
                    qualificationKind = QualificationKind.QualifiedViaValue
                End If

                ' First look for a method in the containing class named "On{handlerName}PropertySet".
                binder.LookupMember(lookup, accessor.ContainingType, handlerName & handlerSuffix, 0, options, useSiteDiagnostics)
                diagnostics.Add(syntax, useSiteDiagnostics)

                Dim handlerIsInAttribute = False

                Dim methodGroup As BoundMethodGroup
                If lookup.HasSymbol Then
                    methodGroup = New BoundMethodGroup(syntax, Nothing, lookup.Symbols.ToDowncastedImmutable(Of MethodSymbol), lookup.Kind, fieldAccessReceiver, qualificationKind).MakeCompilerGenerated()
                Else
                    options = LookupOptions.MethodsOnly Or
                              LookupOptions.AllMethodsOfAnyArity Or
                              LookupOptions.MustNotBeInstance
                    binder.LookupMember(lookup, attribute.AttributeClass, handlerSuffix, 0, options, useSiteDiagnostics)
                    diagnostics.Add(syntax, useSiteDiagnostics)

                    If lookup.HasSymbol Then
                        methodGroup = New BoundMethodGroup(syntax, Nothing, lookup.Symbols.ToDowncastedImmutable(Of MethodSymbol), lookup.Kind, factory.Type(attribute.AttributeClass), QualificationKind.QualifiedViaTypeName).MakeCompilerGenerated()
                        handlerIsInAttribute = True
                    Else
                        ' TODO: Report diagnostic: No handler method with expected name found.
                        ' NOTE: A set handler OR a get handler is required but not both.
                        Continue For
                    End If
                End If

                Dim arguments = ArrayBuilder(Of BoundExpression).GetInstance(5)
                arguments.Add(propertyName)
                arguments.Add(fieldAccess)
                arguments.Add(valueExpression)

                If handlerIsInAttribute Then
                    Dim sender As BoundExpression
                    If accessor.IsShared Then
                        sender = factory.Null()
                    Else
                        sender = fieldAccessReceiver
                    End If
                    arguments.Insert(0, sender)
                End If

                For Each argument As TypedConstant In attribute.ConstructorArguments
                    If argument.Kind = TypedConstantKind.Primitive Then
                        If TypeOf argument.Value Is String Then
                            arguments.Add(factory.Literal(DirectCast(argument.Value, String)))

                        ElseIf TypeOf argument.Value Is Integer Then
                            arguments.Add(factory.Literal(DirectCast(argument.Value, Integer)))

                        ElseIf TypeOf argument.Value Is Boolean Then
                            arguments.Add(factory.Literal(DirectCast(argument.Value, Boolean)))

                        Else
                            ' TODO: Handle other types.
                            Continue For
                        End If
                    End If
                Next

                Dim handlerCall As BoundExpression =
                        binder.BindInvocationExpression(syntax,
                                                        syntax,
                                                        VisualBasic.Syntax.TypeCharacter.None,
                                                        methodGroup,
                                                        arguments.ToImmutableAndFree(),
                                                        argumentNames:=Nothing,
                                                        diagnostics:=diagnostics,
                                                        callerInfoOpt:=Nothing) _
                              .MakeRValue()

                If handlerCall.Type.IsVoidType Then
                    statements.Add(factory.ExpressionStatement(handlerCall))
                ElseIf isGet Then
                    statements.Add(factory.If(handlerCall, factory.Return(valueExpression.MakeRValue())))
                Else
                    statements.Add(factory.If(handlerCall, factory.Return()))
                End If

                ' Sub OnPropertyGet(propertyName, ByRef currentValue)
                ' Sub OnPropertySet(propertyName, ByRef currentValue, ByRef newValue)

                ' Sub OnEventAdd(eventName, ByRef handlerList, handler)
                ' Sub OnEventRemove(eventName, ByRef handlerList, handler)
                ' Sub OnEventRaise(eventName, ByRef handlerList, [arguments])
            Next

            Dim locals As ImmutableArray(Of LocalSymbol)
            If isGet Then
                locals = ImmutableArray.Create(returnLocal, valueLocal)
                Dim exitLabel As LabelSymbol = factory.GenerateLabel("exit")
                statements.Add(New BoundReturnStatement(syntax, valueExpression.MakeRValue(), returnLocal, exitLabel).MakeCompilerGenerated())
                statements.Add(factory.Label(exitLabel))
                statements.Add(factory.Return(factory.Local(returnLocal, isLValue:=False)))
            Else
                locals = ImmutableArray(Of LocalSymbol).Empty
                statements.Add(factory.Assignment(fieldAccess, valueExpression.MakeRValue()))
                statements.Add(factory.Return())
            End If


            Return factory.Block(locals, statements.ToImmutableAndFree())
        End Function
    End Module
End Namespace
