using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public class StaticClosure {
    [Key("id")]
    public required StaticClosureID ID { get; init; }

    [Key("init_func")]
    public required FunctionID InitFunction { get; init; }

    [Key("closure_id")]
    public required TypeDefID ClosureID { get; init; }
    
    [Key("func_ty_id")]
    public required TypeDefID FunctionTypeID { get; init; }
}

[MessagePackObject]
public class Library {
    [Key("metadata")]
    public required Metadata Metadata { get; set; }

    [Key("variables")]
    public required IReadOnlyDictionary<VariableID, IType> Variables {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    [Key("functions")]
    public required IReadOnlyDictionary<FunctionID, IFunction> Functions {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    [Key("static_closures")]
    public required IReadOnlyList<StaticClosure> StaticClosures {
        get;
        init => field = value!.ToArrayNonNull();
    }

    [Key("init")]
    public required IReadOnlyList<IInstruction> Initialization {
        get;
        init => field = value!.ToArrayNonNull();
    }
}
