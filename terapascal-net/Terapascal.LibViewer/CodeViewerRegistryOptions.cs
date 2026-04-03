using System.Collections.Generic;
using System.IO;
using TextMateSharp.Grammars;
using TextMateSharp.Internal.Grammars.Reader;
using TextMateSharp.Internal.Types;
using TextMateSharp.Registry;
using TextMateSharp.Themes;

namespace Terapascal.LibViewer;

public class CodeViewerRegistryOptions : IRegistryOptions {
    public const string CodeScope = "source.terapascal.ir";
    
    private readonly RegistryOptions defaultOptions;

    private readonly IRawGrammar? codeGrammar;

    public CodeViewerRegistryOptions(RegistryOptions defaultOptions) {
        this.defaultOptions = defaultOptions;

        var assembly = this.GetType().Assembly;
        var grammarName = $"{assembly.GetName().Name}.Grammars.ircode.tmLanguage.json";

        using var grammarStream = this.GetType().Assembly.GetManifestResourceStream(grammarName);
        if (grammarStream != null) {
            using var reader = new StreamReader(grammarStream);
            this.codeGrammar = GrammarReader.ReadGrammarSync(reader);
        }
    }

    public IRawTheme GetTheme(string scopeName) {
        return this.defaultOptions.GetTheme(scopeName);
    }

    public IRawGrammar GetGrammar(string scopeName) {
        if (scopeName == CodeScope && this.codeGrammar != null) {
            return this.codeGrammar;
        }
        
        return this.defaultOptions.GetGrammar(scopeName);
    }

    public ICollection<string> GetInjections(string scopeName) {
        return [];
    }

    public IRawTheme GetDefaultTheme() {
        return this.defaultOptions.GetDefaultTheme();
    }
}
