using System.Diagnostics.CodeAnalysis;
using AvaloniaEdit.Document;
using AvaloniaEdit.Highlighting;
using JetBrains.Annotations;

namespace Terapascal.LibViewer.Models;

public class LibraryContentItem {
    public required string Title { get; init; }
    public required LibraryContentType ContentType { get; init; }
    
    public ulong? ID { get; init; }
    
    [MemberNotNullWhen(true, nameof(ID))]
    [UsedImplicitly]
    public bool HasID => this.ID.HasValue;

    public LibraryContentDetailRow[]? Details { get; init; }
    
    // public string? Code { get; init; }
    public TextDocument? Code { get; init; }
    public IHighlightingDefinition? CodeHighlighting { get; init; }

    public LibraryLink? ToLink() {
        if (!this.HasID) {
            return null;
        }

        return new LibraryLink(this.ContentType, this.ID.Value);
    }
}

public enum LibraryContentType {
    Library,
    Type,
    Interface,
    Function,
    Variable,
    StringLiteral,
    StaticClosure,
}

public readonly record struct LibraryLink(LibraryContentType ContentType, ulong ID) {
    public static LibraryLink? FromType(IR.IType type) {
        var id = type.GetTypeDefID();
        if (id == null) {
            return null;
        }

        return new LibraryLink(LibraryContentType.Type, id.Value.ID);
    }
}

public record LibraryContentDetailRow(string Header, string Content) {
    public LibraryLink? Link { get; init; }
    
    public bool HasLink => this.Link != null;
}
