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

    public string ToPrettyString(Metadata metadata) {
        var path = new StringBuilder(this.Path.ToString());
        
        if (this.TypeParams is { Count: > 0 }) {
            path.Append('[');

            for (var i = 0; i < this.TypeParams.Count; i += 1) {
                if (i > 0) {
                    path.Append(", ");
                }

                var param = this.TypeParams[i];
                path.Append(param.Name);

                if (param.Constraint != null) {
                    path.Append($" is {param.Constraint.ToPrettyString(metadata)}");
                }
            }
            
            path.Append(']');
        }
        
        return path.ToString();
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
            ns = string.Join('.', this.Path.Take(this.Path.Count - 1));
        } else {
            ns = "";
        }

        return this.Path.Last;
    }
    
    public NamePath ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return this.ToGenericName().ResolveGeneric(typeMap);
    }
}
