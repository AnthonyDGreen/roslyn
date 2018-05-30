﻿// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.ComponentModel.Composition;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.Options;
using Microsoft.VisualStudio.CodingConventions;

namespace Microsoft.CodeAnalysis.Editor.Options
{
    [Export(typeof(IDocumentOptionsProviderFactory))]
    class EditorConfigDocumentOptionsProviderFactory : IDocumentOptionsProviderFactory
    {
        private readonly ICodingConventionsManager _codingConventionsManager;

        [ImportingConstructor]
        [Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
        public EditorConfigDocumentOptionsProviderFactory(ICodingConventionsManager codingConventionsManager)
        {
            _codingConventionsManager = codingConventionsManager;
        }

        public IDocumentOptionsProvider Create(Workspace workspace)
        {
            return new EditorConfigDocumentOptionsProvider(workspace, _codingConventionsManager);
        }
    }
}
