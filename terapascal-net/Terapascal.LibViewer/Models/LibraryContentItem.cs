namespace Terapascal.LibViewer.Models;

public class LibraryContentItem {
    public required string Title { get; init; }
    public required LibraryContentType ContentType { get; init; }
    
    public ulong? ID { get; init; }
    public bool HasID => this.ID.HasValue;

    public LibraryContentDetailRow[]? Details { get; init; }
    
    public string? FormattedCode { get; init; }
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

public record LibraryLink(LibraryContentType ContentType, ulong ID) {
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
}
