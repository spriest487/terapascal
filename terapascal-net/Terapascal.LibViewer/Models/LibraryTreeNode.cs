using System.Collections.Generic;
using System.Collections.ObjectModel;
using Avalonia.Controls;
using CommunityToolkit.Mvvm.ComponentModel;
using Terapascal.IR;

namespace Terapascal.LibViewer.Models;

public partial class LibraryTreeNode : ObservableObject {
    [ObservableProperty]
    private bool isExpanded = true;
    
    public ulong? ID { get; init; }

    public bool HasID => this.ID != null;
    public string IDSeparator => this.HasID ? ": " : "";

    public required string Title { get; init; }

    public LibraryContentItem? MainContentItem { get; init; }

    public bool HasMainContentItem => this.MainContentItem != null;
    public bool HasFormattedCode => this.MainContentItem?.FormattedCode != null;

    public Classes? StyleClasses { get; init; }

    public ObservableCollection<LibraryTreeNode>? Children { get; init; }

    public static LibraryTreeNode FromFunction(FunctionID id, IFunction func, Metadata metadata) {
        string? funcTitle = null;

        var details = new List<LibraryContentDetailRow>();
        
        if (metadata.Functions.TryGetValue(id, out var funcInfo) && funcInfo.GlobalName != null) {
            funcTitle = funcInfo.GlobalName.ToPrettyString(metadata);
        } else {
            switch (func) {
                case LocalFunction(var def): {
                    funcTitle = def.DebugName;
                    break;
                }

                case ExternalFunction(var externRef): {
                    funcTitle = $"{externRef.Source}!{externRef.Symbol}";

                    details.Add(new LibraryContentDetailRow("Source", externRef.Source));
                    details.Add(new LibraryContentDetailRow("Symbol", externRef.Symbol));

                    break;
                } 
            }
        }

        var sig = func.Signature();
        details.Add(new LibraryContentDetailRow("Result Type", sig.ReturnType.ToPrettyString(metadata)));

        for (var param = 0; param < sig.ParameterTypes.Count; param += 1) {
            var paramType = sig.ParameterTypes[param];
            var paramTypeName = paramType.ToPrettyString(metadata);
            
            details.Add(new LibraryContentDetailRow($"Parameter Type {param}", paramTypeName) {
                Link = LibraryLink.FromType(paramType),
            });
        }

        funcTitle ??= id.ToString();

        string? formattedCode;
        switch (func) {
            case LocalFunction(var def):
                formattedCode = InstructionList.FormatInstructions(def.Body.Instructions, metadata);
                break;
            default:
                formattedCode = null;
                break;
        }   

        return new LibraryTreeNode {
            ID = id.ID,
            Title = funcTitle,
            StyleClasses = ["Function", "LibraryItem"],
            MainContentItem = new LibraryContentItem {
                Title = funcTitle,
                ContentType = LibraryContentType.Function,
                ID = id.ID,
                Details = details.ToArray(),
                FormattedCode = formattedCode,
            },
        };
    }

    public static LibraryTreeNode FromStringLit(StringID id, string text) {
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
                FormattedCode = text,
                Details = [
                    new LibraryContentDetailRow("Length", text.Length.ToString())
                ],
            },
        };
    }

    public static LibraryTreeNode FromInterfaceDecl(InterfaceID id, IInterfaceDecl decl, Metadata metadata) {
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

    public static LibraryTreeNode FromTypeDecl(TypeDefID id, ITypeDecl decl, Metadata metadata) {
        var title = decl switch {
            DefTypeDecl(StructTypeDef(var structDef)) => structDef.Identity.ToPrettyString(metadata),
            DefTypeDecl(VariantTypeDef(var variantDef)) => variantDef.Name.ToPrettyString(metadata),
            DefTypeDecl(FunctionTypeDef(var sig)) => sig.ToPrettyString(metadata),
            _ => $"type {id.ID}",
        };

        return new LibraryTreeNode {
            ID = id.ID,
            Title = title,
            StyleClasses = ["TypeDecl", "LibraryItem"],
            MainContentItem = new LibraryContentItem {
                Title = title,
                ContentType = LibraryContentType.Type,
                ID = id.ID,
            },
        };
    }
    
    public static LibraryTreeNode FromVariable(VariableID id, VariableInfo decl, Metadata metadata) {
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
    
    public static LibraryTreeNode FromStaticClosure(StaticClosure staticClosure, Metadata metadata) {
        string funcTypeName;
        if (metadata.TypeDecls.TryGetValue(staticClosure.FunctionTypeID, out var funcTypeDecl)
            && funcTypeDecl is DefTypeDecl(FunctionTypeDef def)
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
}
