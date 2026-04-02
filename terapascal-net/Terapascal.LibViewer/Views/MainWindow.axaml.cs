using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Avalonia.Controls;
using Avalonia.Controls.Primitives;
using Avalonia.Interactivity;
using Avalonia.Logging;
using Avalonia.Platform.Storage;
using Avalonia.VisualTree;
using Terapascal.LibViewer.Models;
using Terapascal.LibViewer.Services;
using Terapascal.LibViewer.ViewModels;

namespace Terapascal.LibViewer.Views;

public partial class MainWindow : Window {
    private CancellationTokenSource? cancellation;
    
    private readonly TreeView itemsView;

    public required LibraryLoader LibraryLoader { get; init; }

    private readonly List<LibraryLink> historyItems;

    private bool pauseHistory;
    private int historyPosition;
    
    public MainWindow() {
        this.InitializeComponent();

        this.cancellation = new CancellationTokenSource();

        this.Closed += this.OnClose;

        this.FixScrollBars();
        
        this.itemsView = this.Find<TreeView>("LibraryItemsView")
            ?? throw new NullReferenceException("missing reference: LibraryItemsView");

        this.historyItems = new List<LibraryLink>();
    }

    private void FixScrollBars() {
        var scrollView = this.itemsView.FindDescendantOfType<ScrollViewer>();
        if (scrollView == null) {
            return;
        }

        scrollView.AllowAutoHide = false;
        scrollView.HorizontalScrollBarVisibility = ScrollBarVisibility.Hidden;
        
        scrollView.ScrollChanged -= LibraryItemsView_OnScrollChanged;
        scrollView.ScrollChanged += LibraryItemsView_OnScrollChanged;
        LibraryItemsView_OnScrollChanged(scrollView, null);
    }

    private void OnClose(object? sender, EventArgs args) {
        if (this.cancellation == null) {
            return;
        }

        this.cancellation.Cancel();
        this.cancellation.Dispose();
        this.cancellation = null;
    }

    public async void LoadLibrary(string path) {
        try {
            var lib = await this.LibraryLoader.LoadLibrary(path, this.cancellation?.Token ?? CancellationToken.None);

            this.Log(LogEventLevel.Information, $"loaded library: {path} ({lib.Name}-{(Version)lib.Version})");

            this.UpdateLibrary();
        } catch (Exception e) {
            this.Log(LogEventLevel.Error, $"error loading library: {e}");
        }
    }

    private void UpdateLibrary() {
        var lib = this.LibraryLoader.CurrentLibrary;
        if (lib == null) {
            this.Title = "LibViewer";
            return;
        }

        this.Title = $"LibViewer ({lib.Name}-{(Version)lib.Version})";

        var viewModel = new LibraryViewModel(lib);
        foreach (var item in viewModel.GetAllNodes()) {
            item.PropertyChanged += (_, args) => {
                if (args.PropertyName == nameof(LibraryTreeNode.IsExpanded)) {
                    this.FixScrollBars();
                }
            };
        }
        
        this.DataContext = viewModel;
    }

    private void AddHistoryItem(LibraryLink item) {
        while (this.historyPosition < this.historyItems.Count - 1) {
            this.historyItems.RemoveAt(this.historyItems.Count - 1);
        }

        this.historyItems.Add(item);

        this.SelectNextHistoryItem();
    }

    private void SelectLastHistoryItem() {
        this.historyPosition = Math.Max(0, this.historyPosition - 1);
        
        if (this.DataContext is LibraryViewModel viewModel) {
            viewModel.SelectLink(this.historyItems[this.historyPosition]);
        }

        this.UpdateHistoryButtons();
    }
    
    private void SelectNextHistoryItem() {
        this.historyPosition = Math.Min(this.historyItems.Count - 1, this.historyPosition + 1);
        
        if (this.DataContext is LibraryViewModel viewModel) {
            viewModel.SelectLink(this.historyItems[this.historyPosition]);
        }
        
        this.UpdateHistoryButtons();
    }

    private void UpdateHistoryButtons() {
        if (this.DataContext is LibraryViewModel viewModel) {
            viewModel.HasBackItem = this.historyPosition > 0 && this.historyItems.Count > 0;
            viewModel.HasForwardItem = this.historyPosition < this.historyItems.Count - 1;
        }
    }

    private void Log(LogEventLevel level, string messageFormat, params object[] args) {
        if (Logger.TryGet(level, nameof(MainWindow), out var logger)) {
            logger.Log(this, messageFormat, args);
        }
    }

    private void Quit_OnClick(object? sender, RoutedEventArgs args) {
        this.Close();
    }

    private async void Open_OnClick(object? sender, RoutedEventArgs args) {
        try {
            var result = await this.StorageProvider.OpenFilePickerAsync(new() {
                AllowMultiple = false,
                FileTypeFilter = [
                    new FilePickerFileType("lib"),
                ],
                Title = "Open",
            });

            if (result.Count == 0) {
                return;
            }

            var path = result[0].Path.AbsolutePath;
            this.LoadLibrary(path);
        } catch (Exception e) {
            this.Log(LogEventLevel.Error, $"error opening file: {e}");
        }
    }

    private void LibraryItemsView_OnSelectionChanged(object? sender, SelectionChangedEventArgs e) {
        this.FixScrollBars();

        if (!this.pauseHistory) {
            foreach (var selected in e.AddedItems.OfType<LibraryTreeNode>()) {
                if (selected.MainContentItem?.ToLink() is { } link) {
                    this.AddHistoryItem(link);
                }
            }
        }
    }

    private static void LibraryItemsView_OnScrollChanged(object? sender, ScrollChangedEventArgs? e) {
        if (sender is ScrollViewer scrollView) {
            scrollView.Offset = scrollView.Offset.WithX(0);
        }
    }

    private void ItemSearchBox_OnTextChanged(object? sender, TextChangedEventArgs e) {
        if (this.DataContext is LibraryViewModel viewModel) {
            viewModel.ApplyItemSearch();
        }
    }

    private void DetailLinkButton_OnClick(object? sender, RoutedEventArgs e) {
        if (sender is Button { DataContext: LibraryContentDetailRow { Link: {} link } }
            && this.DataContext is LibraryViewModel viewModel
        ) {
            viewModel.SelectLink(link);
        }
    }

    private void BackButton_OnClick(object? sender, RoutedEventArgs e) {
        this.pauseHistory = true;
        this.SelectLastHistoryItem();
        this.pauseHistory = false;
    }

    private void ForwardButton_OnClick(object? sender, RoutedEventArgs e) {
        this.pauseHistory = true;
        this.SelectNextHistoryItem();
        this.pauseHistory = false;
    }
}
