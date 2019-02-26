' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Option Strict On
Option Explicit On
Imports System.Runtime.InteropServices
Imports System.Runtime.CompilerServices

Namespace Microsoft.VisualStudio.Editors.XmlIntellisense

    '--------------------------------------------------------------------------
    ' IXmlIntellisenseService:
    '     Interface that defines the contract for the XmlIntellisense service.
    '     Must be kept in sync with its unmanaged version in vbidl.idl
    '--------------------------------------------------------------------------
    <Guid("94B71D3D-628F-4036-BF89-7FE1508E78AE")>
    <InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>
    <ComImport()>
    Friend Interface IXmlIntellisenseService

        <MethodImpl(MethodImplOptions.InternalCall)>
        Function CreateSchemas(
            <[In]()> ProjectGuid As Guid
            ) _
            As IXmlIntellisenseSchemas

    End Interface

    '--------------------------------------------------------------------------
    ' IXmlIntellisenseSchemas:
    '     Interface that defines the contract for the Xml intellisense schemas
    '     manager.
    '     Must be kept in sync with its unmanaged version in vbidl.idl
    '--------------------------------------------------------------------------
    <Guid("1E9E02D2-0532-4BD2-8114-A24262CC9770")>
    <InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>
    <ComImport()>
    Friend Interface IXmlIntellisenseSchemas

        <MethodImpl(MethodImplOptions.InternalCall)>
        Sub AsyncCompile()

        ReadOnly Property CompiledEvent() As IntPtr

        ReadOnly Property TargetNamespaces() As String()

        ReadOnly Property MemberList() As IXmlIntellisenseMemberList

        ReadOnly Property FirstErrorSource() As String

        ReadOnly Property IsEmpty() As <MarshalAs(UnmanagedType.Bool)> Boolean

        <MethodImpl(MethodImplOptions.InternalCall)>
        Sub ShowInXmlSchemaExplorer(
            <[In](), MarshalAs(UnmanagedType.BStr)> NamespaceName As String,
            <[In](), MarshalAs(UnmanagedType.BStr)> LocalName As String,
            <MarshalAs(UnmanagedType.Bool)> ByRef ElementFound As Boolean,
            <MarshalAs(UnmanagedType.Bool)> ByRef NamespaceFound As Boolean)

    End Interface

    '--------------------------------------------------------------------------
    ' IXmlIntellisenseMemberList:
    '     Interface that defines the contract for lists of element and attribute
    '     declarations used as the basis of intellisense member dropdowns.
    '     Must be kept in sync with its unmanaged version in vbidl.idl
    '--------------------------------------------------------------------------
    <Guid("E90363FC-4246-4df8-869E-7BA42D29F526")>
    <InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>
    <ComImport()>
    Friend Interface IXmlIntellisenseMemberList

        <MethodImpl(MethodImplOptions.InternalCall)>
        Function Document() As IXmlIntellisenseMemberList

        <MethodImpl(MethodImplOptions.InternalCall)>
        Function AllElements() As IXmlIntellisenseMemberList

        <MethodImpl(MethodImplOptions.InternalCall)>
        Function GlobalElements() As IXmlIntellisenseMemberList

        <MethodImpl(MethodImplOptions.InternalCall)>
        Function All() As IXmlIntellisenseMemberList

        <MethodImpl(MethodImplOptions.InternalCall)>
        Function ElementsByNamespace(
            <[In](), MarshalAs(UnmanagedType.BStr)> NamespaceName As String
            ) As IXmlIntellisenseMemberList

        <MethodImpl(MethodImplOptions.InternalCall)>
        Function ElementsByName(
            <[In](), MarshalAs(UnmanagedType.BStr)> NamespaceName As String,
            <[In](), MarshalAs(UnmanagedType.BStr)> LocalName As String
            ) As IXmlIntellisenseMemberList

        <MethodImpl(MethodImplOptions.InternalCall)>
        Function AttributesByNamespace(
            <[In](), MarshalAs(UnmanagedType.BStr)> NamespaceName As String
            ) As IXmlIntellisenseMemberList

        <MethodImpl(MethodImplOptions.InternalCall)>
        Function AttributesByName(
            <[In](), MarshalAs(UnmanagedType.BStr)> NamespaceName As String,
            <[In](), MarshalAs(UnmanagedType.BStr)> LocalName As String
            ) As IXmlIntellisenseMemberList

        <MethodImpl(MethodImplOptions.InternalCall)>
        Function DescendantsByNamespace(
            <[In](), MarshalAs(UnmanagedType.BStr)> NamespaceName As String
            ) As IXmlIntellisenseMemberList

        <MethodImpl(MethodImplOptions.InternalCall)>
        Function DescendantsByName(
            <[In](), MarshalAs(UnmanagedType.BStr)> NamespaceName As String,
            <[In](), MarshalAs(UnmanagedType.BStr)> LocalName As String
            ) As IXmlIntellisenseMemberList

        <MethodImpl(MethodImplOptions.InternalCall)>
        Function GetEnumerator() As IXmlIntellisenseMemberEnumerator

        ReadOnly Property MatchesNamedType() As Boolean

    End Interface

    '--------------------------------------------------------------------------
    ' IXmlIntellisenseMemberEnumerator:
    '     This is a list enumerator interface that is very to use from native
    '     code.  The enumerator begins just before the first item in the list.
    '--------------------------------------------------------------------------
    <Guid("C5E8D87C-674B-4966-9245-AA32914B05F7")>
    <InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>
    <ComImport()>
    Friend Interface IXmlIntellisenseMemberEnumerator

        <MethodImpl(MethodImplOptions.InternalCall)>
        Function GetNext() As IXmlIntellisenseMember

    End Interface

    '--------------------------------------------------------------------------
    ' IXmlIntellisenseMember:
    '     Represents an Xml schema element or attribute declaration, which
    '     is what is displayed in intellisense dropdowns.
    '--------------------------------------------------------------------------
    <Guid("AB892676-9227-4c8e-AD84-DE887646D416")>
    <InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>
    <ComImport()>
    Friend Interface IXmlIntellisenseMember

        ReadOnly Property IsElement() As Boolean

        ReadOnly Property NamespaceName() As String

        ReadOnly Property LocalName() As String

    End Interface

End Namespace
