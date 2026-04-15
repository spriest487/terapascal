using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public class StaticClosure {
    [Key("id")]
    public required VariableID ID { get; init; }

    [Key("init_func")]
    public required FunctionID InitFunction { get; init; }

    [Key("closure_id")]
    public required TypeDefID ClosureID { get; init; }
    
    [Key("func_ty_id")]
    public required TypeDefID FunctionTypeID { get; init; }
}
