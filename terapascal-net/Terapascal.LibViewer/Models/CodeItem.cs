using AvaloniaEdit.Document;

namespace Terapascal.LibViewer.Models;

public record CodeItem {
    public required string Title { get; init; }
    public required TextDocument CodeDocument { get; init; }

    public string? Scope { get; init; }
}
