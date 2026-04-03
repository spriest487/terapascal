using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Text;
using Avalonia.Controls;
using AvaloniaEdit.Document;
using CommunityToolkit.Mvvm.ComponentModel;
using Terapascal.LibViewer.Models;

namespace Terapascal.LibViewer.ViewModels;

public partial class LibraryTreeNode : ObservableObject {
    [ObservableProperty]
    private bool isExpanded = true;

    [ObservableProperty]
    private ObservableCollection<LibraryTreeNode>? visibleChildren;

    public ulong? ID { get; init; }

    public bool HasID => this.ID != null;
    public string IDSeparator => this.HasID ? ": " : "";

    public required string Title { get; init; }

    public LibraryContentItem? MainContentItem { get; init; }

    public bool HasMainContentItem => this.MainContentItem != null;
    public bool HasCode => (this.MainContentItem?.CodeItems.Length ?? 0) > 0;

    public Classes? StyleClasses { get; init; }

    public IReadOnlyList<LibraryTreeNode>? Children { get; set; }

    public static LibraryTreeNode FromFunction(IR.FunctionID id, IR.IFunction func, IR.Metadata metadata) {
        string? funcTitle = null;

        var details = new List<LibraryContentDetailRow>();
        var codeItems = new List<CodeItem>(2);
        
        if (metadata.Functions.TryGetValue(id, out var funcInfo) && funcInfo.GlobalName != null) {
            funcTitle = funcInfo.GlobalName.ToPrettyString(metadata);
        } else {
            funcInfo = null;
        }

        switch (func) {
            case IR.LocalFunction(var def): {
                funcTitle ??= def.DebugName;
                
                details.Add(new LibraryContentDetailRow("Debug Name", def.DebugName ?? "-"));
                
                var code = IR.InstructionList.FormatInstructions(def.Body.Instructions, metadata);
                codeItems.Add(new CodeItem {
                    Title = "Body",
                    CodeDocument = new TextDocument(code),
                    Scope = CodeViewerRegistryOptions.CodeScope,
                });
                break;
            }

            case IR.ExternalFunction(var externRef): {
                funcTitle = $"{externRef.Source}!{externRef.Symbol}";

                details.Add(new LibraryContentDetailRow("Source", externRef.Source));
                details.Add(new LibraryContentDetailRow("Symbol", externRef.Symbol));

                break;
            } 
        }
        
        funcTitle ??= id.ToString();

        if (funcInfo is { Tags.Count: > 0 }) {
            var tagsCode = new StringBuilder();
            foreach (var tag in funcInfo.Tags) {
                tag.ToPrettyString(metadata, tagsCode);
                tagsCode.AppendLine();
            }
            
            codeItems.Add(new CodeItem {
                Title = "Tags",
                CodeDocument = new TextDocument(tagsCode.ToString()),
            });
        }

        var sig = func.Signature();
        CreateFunctionSigDetails(sig, details, metadata);

        return new LibraryTreeNode {
            ID = id.ID,
            Title = funcTitle,
            StyleClasses = ["Function", "LibraryItem"],
            MainContentItem = new LibraryContentItem {
                Title = funcTitle,
                ContentType = LibraryContentType.Function,
                ID = id.ID,
                Details = details.ToArray(),
                CodeItems = codeItems.ToArray(),
            },
        };
    }

    public static LibraryTreeNode FromStringLit(IR.StringID id, string text) {
        const int maxLength = 32;
        const string ellipsis = "...";

        var title = text;
        if (title.Length > maxLength) {
            title = title[..(maxLength - ellipsis.Length)] + ellipsis;
        }

        title = $"\"{title}\"";
        
        return new LibraryTreeNode {
            ID = id.ID,
            Title = title,
            StyleClasses = ["String", "LibraryItem"],
            MainContentItem = new LibraryContentItem {
                Title = $"string {id.ID}",
                ContentType = LibraryContentType.StringLiteral,
                ID = id.ID,
                CodeItems = [
                    new CodeItem {
                        Title = "Contents",
                        CodeDocument = new TextDocument(text), 
                    },
                ],
                Details = [
                    new LibraryContentDetailRow("Length", text.Length.ToString())
                ],
            },
        };
    }

    public static LibraryTreeNode FromInterfaceDecl(IR.InterfaceID id, IR.IInterfaceDecl decl, IR.Metadata metadata) {
        var title = decl.GetGlobalName().ToPrettyString(metadata);

        return new LibraryTreeNode {
            ID = id.ID,
            Title = title,
            StyleClasses = ["Interface", "LibraryItem"],
            MainContentItem = new LibraryContentItem {
                Title = title,
                ID = id.ID,
                ContentType = LibraryContentType.Interface,
                Details = [
                ],
            },
        };
    }

    public static LibraryTreeNode FromTypeDecl(IR.TypeDefID id, IR.ITypeDecl decl, IR.Metadata metadata) {
        var details = new List<LibraryContentDetailRow>(8);
        
        string? title;

        var codeItems = new List<CodeItem>(2);

        IReadOnlyCollection<IR.TagInfo>? tags = null;
        
        switch (decl) {
            case IR.DefTypeDecl(IR.StructTypeDef(var structDef)): {
                title = structDef.Identity.ToPrettyString(metadata);

                tags = structDef.Tags;

                details.Add(new LibraryContentDetailRow("Kind", "Struct"));
                
                details.Add(new LibraryContentDetailRow("Identity Type", structDef.Identity switch {
                    IR.ArrayStructIdentity => "array",
                    IR.ClassStructIdentity => "class",
                    IR.ClosureStructIdentity => "closure",
                    IR.DynArrayStructIdentity => "dynamic array",
                    IR.RecordStructIdentity => "record",
                    IR.SetFlagsStructIdentity => "flags",
                    _ => structDef.Identity.GetType().Name,
                }));

                codeItems.Add(new CodeItem {
                    Title = "Definition",
                    CodeDocument = new TextDocument(structDef.ToPrettyString(metadata)), 
                });

                break;
            }

            case IR.DefTypeDecl(IR.VariantTypeDef(var variantDef)): {
                title = variantDef.Name.ToPrettyString(metadata);

                tags = variantDef.Tags;

                details.Add(new LibraryContentDetailRow("Kind", "Variant"));
                
                details.Add(new LibraryContentDetailRow("Tag Type", variantDef.TagType.ToPrettyString(metadata)) {
                    Link = LibraryLink.FromType(variantDef.TagType),
                });

                codeItems.Add(new CodeItem {
                    Title = "Definition",
                    CodeDocument = new TextDocument(variantDef.ToPrettyString(metadata)), 
                });
                break;
            }

            case IR.DefTypeDecl(IR.FunctionTypeDef(var sig)): {
                details.Add(new LibraryContentDetailRow("Kind", "Function"));
                CreateFunctionSigDetails(sig, details, metadata);

                title = sig.ToPrettyString(metadata); 
                break;
            }
            
            case IR.ForwardTypeDecl(var name): {
                title = name.ToPrettyString(metadata);
                
                details.Add(new LibraryContentDetailRow("Kind", "Forward"));
                break;
            }

            default: {
                title = $"type {id.ID}";
                break;
            }
        }

        if (tags != null) {
            var tagsCode = new StringBuilder();
            foreach (var tag in tags) {
                tag.ToPrettyString(metadata, tagsCode);
                tagsCode.AppendLine();
            }
            
            codeItems.Add(new CodeItem {
                Title = "Tags",
                CodeDocument = new TextDocument(tagsCode.ToString()),
            });
        }

        return new LibraryTreeNode {
            ID = id.ID,
            Title = title,
            StyleClasses = ["TypeDecl", "LibraryItem"],
            MainContentItem = new LibraryContentItem {
                Title = title,
                ContentType = LibraryContentType.Type,
                ID = id.ID,
                Details = details.ToArray(),
                CodeItems = codeItems.ToArray(),
            },
        };
    }
    
    public static LibraryTreeNode FromVariable(IR.VariableID id, IR.VariableInfo decl, IR.Metadata metadata) {
        var title = decl.Name.ToPrettyString(metadata);

        return new LibraryTreeNode {
            ID = id.ID,
            Title = title,
            StyleClasses = ["Variable", "LibraryItem"],
            MainContentItem = new LibraryContentItem {
                Title = title,
                ContentType = LibraryContentType.Variable,
                ID = id.ID,
                Details = [
                    new LibraryContentDetailRow("Type", decl.Type.ToPrettyString(metadata)) {
                        Link = decl.Type.GetTypeDefID() is { } typeId 
                            ? new LibraryLink(LibraryContentType.Type, typeId.ID) 
                            : null,
                    },
                ],
            },
        };
    }
    
    public static LibraryTreeNode FromStaticClosure(IR.StaticClosure staticClosure, IR.Metadata metadata) {
        string funcTypeName;
        if (metadata.TypeDecls.TryGetValue(staticClosure.FunctionTypeID, out var funcTypeDecl)
            && funcTypeDecl is IR.DefTypeDecl(IR.FunctionTypeDef def)
        ) {
            funcTypeName = def.Sig.ToPrettyString(metadata);
        } else {
            funcTypeName = $"type {staticClosure.FunctionTypeID.ID}";
        }

        var initFunc = staticClosure.InitFunction.ToPrettyString(metadata);

        var title = $"static closure {staticClosure.ID}";
        return new LibraryTreeNode {
            ID = staticClosure.ID.ID,
            Title = title,
            StyleClasses = ["StaticClosure", "LibraryItem"],
            MainContentItem = new LibraryContentItem {
                ID = staticClosure.ID.ID,
                Title = title,
                ContentType = LibraryContentType.StaticClosure,
                Details = [
                    new LibraryContentDetailRow("Function Type", funcTypeName) {
                        Link = new LibraryLink(LibraryContentType.Type, staticClosure.FunctionTypeID.ID),
                    },
                    new LibraryContentDetailRow("Initializer", initFunc) {
                        Link = new LibraryLink(LibraryContentType.Function, staticClosure.InitFunction.ID),
                    },
                ],
            },
        };
    }

    public bool ApplySearch(string? term) {
        var visibleChildren = new ObservableCollection<LibraryTreeNode>();

        if (this.Children != null) {
            foreach (var child in this.Children) {
                if (child.ApplySearch(term)) {
                    visibleChildren.Add(child);
                }
            }
        }

        this.VisibleChildren = visibleChildren;

        if (this.VisibleChildren.Count > 0) {
            return true;
        }
        
        if (term == null) {
            return true;
        }
        
        var visibleSelf = this.Title.Contains(term, StringComparison.InvariantCultureIgnoreCase);
        if (this.ID?.ToString() is { } itemID) {
            visibleSelf |= itemID.Contains(term, StringComparison.InvariantCultureIgnoreCase);
        }

        return visibleSelf;
    }

    private static void CreateFunctionSigDetails(IR.FunctionSig sig, List<LibraryContentDetailRow> details, IR.Metadata metadata) {
        details.Add(new LibraryContentDetailRow("Result Type", sig.ReturnType.ToPrettyString(metadata)));

        for (var param = 0; param < sig.ParameterTypes.Count; param += 1) {
            var paramType = sig.ParameterTypes[param];
            var paramTypeName = paramType.ToPrettyString(metadata);
            
            details.Add(new LibraryContentDetailRow($"Parameter Type {param}", paramTypeName) {
                Link = LibraryLink.FromType(paramType),
            });
        }
    }
}
