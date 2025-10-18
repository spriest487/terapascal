namespace Terapascal.CIL;

public static class SDKUtils {
    public static string FindReferenceLibPath(string? dotnetPath) {
        if (dotnetPath == null) {
            dotnetPath = Environment.OSVersion.Platform switch {
                PlatformID.Win32S or PlatformID.Win32Windows or PlatformID.Win32NT or PlatformID.WinCE => 
                    @"C:\Program Files\dotnet",
                _ => 
                    Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".dotnet"),
            };
        }

        return Path.Join(dotnetPath, "packs", "NETStandard.Library.Ref", "2.1.0", "ref", "netstandard2.1");
    }
}
