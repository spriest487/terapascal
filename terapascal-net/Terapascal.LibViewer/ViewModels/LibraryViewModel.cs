using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using CommunityToolkit.Mvvm.ComponentModel;
using Terapascal.LibViewer.Models;

namespace Terapascal.LibViewer.ViewModels;

// ReSharper disable once PartialTypeWithSinglePart
public partial class LibraryViewModel : ViewModelBase {
    [ObservableProperty]
    private ObservableCollection<LibraryTreeNode>? libraryItems;

    [ObservableProperty]
    private LibraryTreeNode? selectedItem;

    public bool HasSelectedItem => this.SelectedItem != null;

    public LibraryViewModel() {
        this.libraryItems = [];
    }

    public LibraryViewModel(IR.Library lib) {
        var funcItems = lib.Functions
            .OrderBy(entry => entry.Key)
            .Select(entry => LibraryTreeNode.FromFunction(entry.Key, entry.Value, lib.Metadata))
            .ToList();
        
        var stringItems = lib.Metadata.StringLiterals
            .OrderBy(entry => entry.Key)
            .Select(entry => LibraryTreeNode.FromStringLit(entry.Key, entry.Value))
            .ToList();
        
        var interfaceItems = lib.Metadata.Interfaces
            .OrderBy(entry => entry.Key)
            .Select(entry => LibraryTreeNode.FromInterfaceDecl(entry.Key, entry.Value, lib.Metadata))
            .ToList();

        var typeItems = lib.Metadata.TypeDecls
            .OrderBy(entry => entry.Key)
            .Select(entry => LibraryTreeNode.FromTypeDecl(entry.Key, entry.Value, lib.Metadata))
            .ToList();

        var varItems = lib.Metadata.Variables
            .OrderBy(entry => entry.Key)
            .Select(entry => LibraryTreeNode.FromVariable(entry.Key, entry.Value, lib.Metadata))
            .ToList();

        var staticClosureItems = lib.StaticClosures
            .OrderBy(entry => entry.ID)
            .Select(entry => LibraryTreeNode.FromStaticClosure(entry, lib.Metadata))
            .ToList();

        this.selectedItem = new LibraryTreeNode {
            Title = "Library",
            MainContentItem = new LibraryContentItem {
                Title = lib.Name,
                ContentType = LibraryContentType.Library,
                Details = [
                    new LibraryContentDetailRow("Version", lib.Version.ToString()),
                ],
                FormattedCode = IR.InstructionList.FormatInstructions(lib.Initialization.Instructions, lib.Metadata),
            },
        };

        this.libraryItems = [
            this.selectedItem,
            
            new LibraryTreeNode {
                Title = $"Types ({typeItems.Count})",
                StyleClasses = ["Category", "Types"],
                Children = new ObservableCollection<LibraryTreeNode>(typeItems),
                IsExpanded = false,
            },
            
            new LibraryTreeNode {
                Title = $"Interfaces ({interfaceItems.Count})",
                StyleClasses = ["Category", "Interfaces"],
                Children = new ObservableCollection<LibraryTreeNode>(interfaceItems),
                IsExpanded = false,
            },

            new LibraryTreeNode {
                Title = $"Functions ({funcItems.Count})",
                StyleClasses = ["Category", "Functions"],
                Children = new ObservableCollection<LibraryTreeNode>(funcItems),
                IsExpanded = false,
            },
            
            new LibraryTreeNode {
                Title = $"Variables ({varItems.Count})",
                StyleClasses = ["Category", "Variables"],
                Children = new ObservableCollection<LibraryTreeNode>(varItems),
                IsExpanded = false,
            },

            new LibraryTreeNode {
                Title = $"Strings ({stringItems.Count})",
                StyleClasses = ["Category", "Strings"],
                Children = new ObservableCollection<LibraryTreeNode>(stringItems),
                IsExpanded = false,
            },

            new LibraryTreeNode {
                Title = $"Static Closures ({staticClosureItems.Count})",
                StyleClasses = ["Category", "StaticClosures"],
                Children = new ObservableCollection<LibraryTreeNode>(stringItems),
                IsExpanded = false,
            },
        ];
    }

    public IEnumerable<LibraryTreeNode> GetAllNodes() {
        return GetAllNodes(this.LibraryItems);
    }
    
    private static IEnumerable<LibraryTreeNode> GetAllNodes(IEnumerable<LibraryTreeNode>? nodes) {
        if (nodes == null) {
            yield break;
        }

        foreach (var node in nodes) {
            yield return node;
            
            foreach (var child in GetAllNodes(node.Children)) {
                yield return child;
            }
        }
    }
}
