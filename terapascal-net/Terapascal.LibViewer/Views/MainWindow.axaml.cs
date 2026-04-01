using System;
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
    
    public MainWindow() {
        this.InitializeComponent();

        this.cancellation = new CancellationTokenSource();

        this.Closed += this.OnClose;

        this.FixScrollBars();
        
        this.itemsView = this.Find<TreeView>("LibraryItemsView")
            ?? throw new NullReferenceException("missing reference: LibraryItemsView");
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
    }

    private static void LibraryItemsView_OnScrollChanged(object? sender, ScrollChangedEventArgs? e) {
        if (sender is ScrollViewer scrollView) {
            scrollView.Offset = scrollView.Offset.WithX(0);
        }
    }
}
