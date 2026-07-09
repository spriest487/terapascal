using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public class MethodInfo {
    [Key("name")]
    public required StringID Name { get; init; }
    
    [Key("index")]
    public required ulong Index { get; init; }

    [Key("instance_ty")]
    public required IType InstanceType {
        get;
        set => field = value ?? IType.Nothing;
    }

    [Key("function")]
    public FunctionID? Function { get; init; }

    [Key("result_ty")]
    public required IType ResultType {
        get;
        set => field = value ?? IType.Nothing;
    }

    [Key("params")]
    public required IReadOnlyList<IType> ParameterTypes {
        get;
        init => field = value.ToArrayNonNull();
    }

    [Key("tags")]
    public required IReadOnlyList<TagInfo> Tags {
        get;
        init => field = value.ToArrayNonNull();
    }
}