using System;
using System.Linq;
using System.Threading.Tasks;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.ApplicationLifetimes;
using Avalonia.Data.Core.Plugins;
using Avalonia.Markup.Xaml;
using Microsoft.Extensions.DependencyInjection;
using Terapascal.LibViewer.Services;
using Terapascal.LibViewer.ViewModels;
using Terapascal.LibViewer.Views;

namespace Terapascal.LibViewer;

// ReSharper disable once PartialTypeWithSinglePart
public partial class App : Application, IAsyncDisposable {
    private ServiceProvider? serviceProvider;

    public async ValueTask DisposeAsync() {
        if (this.serviceProvider != null) {
            await this.serviceProvider.DisposeAsync();
            this.serviceProvider = null;
        }
        
        GC.SuppressFinalize(this);
    }

    public override void Initialize() {
        AvaloniaXamlLoader.Load(this);
    }

    public override void OnFrameworkInitializationCompleted() {
        var container = new ServiceCollection();
        container.AddSingleton<LibraryLoader>();

        this.serviceProvider = container.BuildServiceProvider();
        var libraryLoader = this.serviceProvider.GetService<LibraryLoader>() ??
            throw new InvalidOperationException("missing service: MainWindow");
        
        if (this.ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop) {
            this.DisableAvaloniaDataAnnotationValidation();

            var mainWindow = new MainWindow {
                LibraryLoader = libraryLoader,
                DataContext = new LibraryViewModel(),
            };
            
            var launchArg = desktop.Args?.FirstOrDefault();
            if (!string.IsNullOrWhiteSpace(launchArg)) {
                mainWindow.LoadLibrary(launchArg);
            }

            mainWindow.Closed += (e, b) => {
                desktop.TryShutdown();
            };

            desktop.MainWindow = mainWindow;
        }

        base.OnFrameworkInitializationCompleted();
    }

    private void DisableAvaloniaDataAnnotationValidation() {
        // Get an array of plugins to remove
        var dataValidationPluginsToRemove =
            BindingPlugins.DataValidators.OfType<DataAnnotationsValidationPlugin>().ToArray();

        // remove each entry found
        foreach (var plugin in dataValidationPluginsToRemove) {
            BindingPlugins.DataValidators.Remove(plugin);
        }
    }
}
