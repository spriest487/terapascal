using System.Diagnostics.CodeAnalysis;
using System.Text;
using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public class DeclPath {
    [Key("path")]
    public required StringPath Path { get; init; }

    [Key("type_params")]
    public IReadOnlyList<TypeParam>? TypeParams { get; init; }

    [IgnoreMember]
    [MemberNotNullWhen(true, nameof(TypeParams))]
    public bool HasTypeParams => this.TypeParams is { Count: > 0 };

    public string ToString(IMetadataSource? metadata) {
        var path = new StringBuilder(this.Path.ToString());

        FormatTypeParamsList(this.TypeParams, metadata, path);
        
        return path.ToString();
    }

    public override string ToString() {
        return this.ToString(null);
    }

    public StringPath? GetParent() {
        return this.Path.GetParent();
    }

    public IType[] GetGenericArgs() {
        return this.TypeParams?
            .Select(p => new GenericType(p.Name))
            .Cast<IType>()
            .ToArray()
            ?? [];
    }

    public NamePath ToGenericName() {
        return new NamePath {
            Path = this.Path,
            TypeArgs = this.GetGenericArgs(),
        };
    }

    public string ToGlobalName(out string ns) {
        if (this.Path.Count > 1) {
            ns = string.Join('.', this.Path.Parts.Take(this.Path.Count - 1));
        } else {
            ns = "";
        }

        return this.Path.Last;
    }
    
    public NamePath ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return this.ToGenericName().ResolveGeneric(typeMap);
    }

    public static void FormatTypeParamsList(
        IReadOnlyList<TypeParam>? paramsList,
        IMetadataSource? metadata,
        StringBuilder result
    ) {
        if (paramsList is not { Count: > 0 }) {
            return;
        }

        result.Append('[');

        for (var i = 0; i < paramsList.Count; i += 1) {
            if (i > 0) {
                result.Append(", ");
            }

            var param = paramsList[i];
            result.Append(param.Name);

            if (param.Constraint != null) {
                result.Append($" is {param.Constraint.ToString(metadata)}");
            }
        }

        result.Append(']');
    }
}
