using Terapascal.LibViewer.Models;

namespace Terapascal.LibViewer.ViewModels;

public static class Designer {
    public static LibraryViewModel LibraryViewModel {
        get {
            var viewModel = new LibraryViewModel {
                VisibleItems = [
                    new LibraryTreeNode {
                        Title = "Strings",
                        Children = [
                            new LibraryTreeNode { ID = 0, Title = "\"Hello, \"" },
                            new LibraryTreeNode { ID = 1, Title = "\"world!\"" },
                        ],
                    },
                    new LibraryTreeNode {
                        Title = "Functions",
                        Children = [
                            new LibraryTreeNode { ID = 0, Title = "Main" },
                            new LibraryTreeNode { ID = 6, Title = "<generated1>" },
                        ],
                    },
                    new LibraryTreeNode {
                        Title = "Types",
                        Children = [
                            new LibraryTreeNode { ID = 1, Title = "String" },
                            new LibraryTreeNode { ID = 2, Title = "Result" },
                            new LibraryTreeNode { ID = 3, Title = "Option" },
                        ],
                    },
                    new LibraryTreeNode {
                        Title = "Variables",
                        Children = [
                            new LibraryTreeNode { ID = 123, Title = "Var1" },
                            new LibraryTreeNode { ID = 523, Title = "Global2" },
                        ],
                    },
                ],
            };

            return viewModel;
        }
    }
}
