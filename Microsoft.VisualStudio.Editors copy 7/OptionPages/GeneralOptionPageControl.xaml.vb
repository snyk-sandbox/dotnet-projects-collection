﻿' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports System.Windows.Controls.Primitives
Imports System.Windows.Data

Namespace Microsoft.VisualStudio.Editors.OptionPages
    Partial Friend NotInheritable Class GeneralOptionPageControl
        Inherits OptionPageControl

        Private ReadOnly _generalOptions As GeneralOptions

        Public Shared ReadOnly FastUpToDateLogLevelItemSource As String() = {
            My.Resources.GeneralOptionPageResources.General_FastUpToDateCheck_LogLevel_None,
            My.Resources.GeneralOptionPageResources.General_FastUpToDateCheck_LogLevel_Info,
            My.Resources.GeneralOptionPageResources.General_FastUpToDateCheck_LogLevel_Verbose
        }

        Public Sub New(serviceProvider As IServiceProvider)
            MyBase.New()

            _generalOptions = New GeneralOptions(serviceProvider)

            InitializeComponent()

            Dim binding = New Binding() With {
                .Source = _generalOptions,
                .Path = New Windows.PropertyPath(NameOf(GeneralOptions.FastUpToDateCheckEnabled)),
                .UpdateSourceTrigger = UpdateSourceTrigger.Explicit
            }

            Dim bindingExpression = FastUpToDateCheck.SetBinding(ToggleButton.IsCheckedProperty, binding)
            AddBinding(bindingExpression)

            binding = New Binding() With {
                    .Source = _generalOptions,
                    .Path = New Windows.PropertyPath(NameOf(GeneralOptions.FastUpToDateLogLevel)),
                    .UpdateSourceTrigger = UpdateSourceTrigger.Explicit,
                    .Converter = LoggingLevelToInt32Converter.Instance
                    }

            bindingExpression = FastUpToDateLogLevel.SetBinding(Selector.SelectedIndexProperty, binding)
            AddBinding(bindingExpression)

            binding = New Binding() With {
                .Source = _generalOptions,
                .Path = New Windows.PropertyPath(NameOf(GeneralOptions.UsePreviewSdk)),
                .UpdateSourceTrigger = UpdateSourceTrigger.Explicit
            }

            bindingExpression = UsePreviewSdk.SetBinding(ToggleButton.IsCheckedProperty, binding)
            AddBinding(bindingExpression)

            binding = New Binding() With {
                .Source = _generalOptions,
                .Path = New Windows.PropertyPath(NameOf(GeneralOptions.CanChangeUsePreviewSdk)),
                .UpdateSourceTrigger = UpdateSourceTrigger.Explicit
            }

            bindingExpression = UsePreviewSdk.SetBinding(IsEnabledProperty, binding)
            AddBinding(bindingExpression)

        End Sub
    End Class
End Namespace
