using System.Diagnostics;
using System.Text.RegularExpressions;
using Mono.Cecil;

namespace Terapascal.CIL;

public static class SDKUtils {
    public static async Task<string> FindLibPath() {
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
            var match = sdkItemRegex.Match(nextLine);
            if (match.Success) {
                var version = match.Groups["Version"].Value;
                var path = match.Groups["Path"].Value;

                libPath = Path.Join(path, version, "ref");
            }
        }
        
        if (libPath == null) {
            throw new IOException("no SDK path found");
        }

        return libPath;
    }
}
