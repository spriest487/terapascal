using System.Diagnostics.CodeAnalysis;
using CommandLine;

namespace Terapascal.CIL;

public record Args {
    [Option("assembly-name", Required = true)]
    public required string AssemblyName { get; init; }

    [Option("assembly-version", Required = false)]
    public Version? Version { get; init; }
    
    [Option("module-kind", Required = false)]
    public string? ModuleKind { get; init; }
        
    [Option("output-path", Default = false, Required = true)]
    public required string OutputPath { get; init; }
    
    
    [Value(0, Required = false)]
    public string? LibPath { get; init; }

    public static bool Parse(IEnumerable<string> args, [NotNullWhen(true)] out Args? result) {
        using var parser = new Parser(parserSettings => {
            parserSettings.CaseInsensitiveEnumValues = true;
            parserSettings.AllowMultiInstance = false;
            parserSettings.AutoHelp = true;
            parserSettings.AutoVersion = true;
            parserSettings.EnableDashDash = true;
            parserSettings.HelpWriter = Console.Out;
        });

        var resultArgs = default(Args?);

        parser.ParseArguments<Args>(args)
            .WithParsed(parsedArgs => { resultArgs = parsedArgs; });

        if (resultArgs == null) {
            result = null;
            return false;
        }

        result = resultArgs;
        return true;
    }
}
