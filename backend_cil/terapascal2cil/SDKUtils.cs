using System.Diagnostics;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;

namespace Terapascal.CIL;

public static class SDKUtils {
    public static async Task<string> FindReferenceLibPath(string? requestedVersion, bool verbose) {
        var dotnetCommand = Process.Start(new ProcessStartInfo {
            FileName = "dotnet",
            Arguments = "--list-sdks",
            RedirectStandardOutput = true,
        });

        if (dotnetCommand == null) {
            throw new IOException("failed to list SDKs");
        }

        await dotnetCommand.WaitForExitAsync().ConfigureAwait(false);

        string? libPath = null;

        var sdkItemRegex = new Regex(@"^(?<Version>.+) \[(?<Path>.+)\]$");
        while (await dotnetCommand.StandardOutput.ReadLineAsync().ConfigureAwait(false) is { } nextLine) {
            if (verbose) {
                Console.WriteLine(nextLine);
            }

            var match = sdkItemRegex.Match(nextLine);
            if (!match.Success) {
                continue;
            }

            var version = match.Groups["Version"].Value;
            var path = match.Groups["Path"].Value;

            libPath = Path.Join(path, version, "ref");

            if (requestedVersion != null && version == requestedVersion) {
                if (verbose) {
                    Console.WriteLine($"Found dotnet SDK {requestedVersion} found at {path}");
                }

                break;
            }
        }

        if (libPath == null) {
            throw new IOException("no SDK path found");
        }

        if (requestedVersion == null && verbose) {
            Console.WriteLine($"Using latest dotnet SDK path: {libPath}");
        }

        return libPath;
    }

    public static async Task<string> FindTargetRuntimeVersion(bool verbose) {
        var dotnetCommand = Process.Start(new ProcessStartInfo {
            FileName = "dotnet",
            Arguments = "--list-runtimes",
            RedirectStandardOutput = true,
        });

        if (dotnetCommand == null) {
            throw new IOException("failed to list runtimes");
        }

        await dotnetCommand.WaitForExitAsync().ConfigureAwait(false);

        string? version = null;

        // look for NETCore App runtimes
        var appRuntimeRegex = new Regex(@"^Microsoft\.NETCore\.App (?<Version>.+) \[.+\]$");

        while (await dotnetCommand.StandardOutput.ReadLineAsync().ConfigureAwait(false) is { } nextLine) {
            if (verbose) {
                Console.WriteLine(nextLine);
            }

            var match = appRuntimeRegex.Match(nextLine);
            if (!match.Success) {
                continue;
            }

            version = match.Groups["Version"].Value;
        }

        if (version == null) {
            throw new IOException("no runtime version found");
        }

        Console.WriteLine($"Using latest dotnet app runtime version: {version}");
        return version;
    }

    public static async Task CreateTemplatedFile(
        string filename,
        string outputPath,
        params IEnumerable<KeyValuePair<string, string>> replacements
    ) {
        var template = new StringBuilder();

        await using (var templateResource = Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream(typeof(SDKUtils), filename)
            ?? throw new FileNotFoundException($"missing template resource: {filename}")
        ) {
            using var reader = new StreamReader(templateResource);
            template.Append(await reader.ReadToEndAsync());
        }

        foreach (var (name, value) in replacements) {
            template.Replace(name, value);
        }

        await using var outFile = File.CreateText(outputPath);
        await outFile.WriteAsync(template);
    }
}
