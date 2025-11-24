using System.Diagnostics;
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
        while (await dotnetCommand.StandardOutput.ReadLineAsync().ConfigureAwait(false) is {} nextLine) {
            if (verbose) {
                Console.WriteLine(nextLine);
            }
            
            var match = sdkItemRegex.Match(nextLine);
            if (match.Success) {
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
        }
        
        if (libPath == null) {
            throw new IOException("no SDK path found");
        }

        if (requestedVersion == null && verbose) {
            Console.WriteLine($"Using latest dotnet SDK path: {libPath}");
        }

        return libPath;
    }
}
