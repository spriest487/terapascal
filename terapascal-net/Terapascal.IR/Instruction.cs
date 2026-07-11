using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public interface IInstruction {
    IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap);
}

public record CommentInstruction(string Text) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return this;
    }
}

public record LocalAllocInstruction(LocalID At, IType Type) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return this with { Type = this.Type.ResolveGeneric(typeMap) };
    }
}

[MessagePackObject]
public record MoveInstruction : IInstruction {
    [Key("out")]
    public required IRef Out {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("new_val")]
    public required IValue NewValue {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new MoveInstruction {
            Out = this.Out.ResolveGeneric(typeMap),
            NewValue = this.NewValue.ResolveGeneric(typeMap),
        };
    }
}

public record AddInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new AddInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record SubInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new SubInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record MulInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new MulInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record IntDivInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new IntDivInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record FloatDivInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new FloatDivInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record ModInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new ModInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record ShlInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new ShlInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record ShrInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new ShrInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record BitAndInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new BitAndInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record BitOrInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new BitOrInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record BitXorInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new BitXorInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record EqInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new EqInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record GtInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new GtInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record LtInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new LtInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record LteInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new LteInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record GteInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new GteInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record AndInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new AndInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record OrInstruction(BinOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new OrInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

[MessagePackObject]
public record BinOpInstructionData {
    [Key("out")]
    public required IRef Out {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("a")]
    public required IValue ArgA {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("b")]
    public required IValue ArgB {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    public BinOpInstructionData ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new BinOpInstructionData {
            Out = this.Out.ResolveGeneric(typeMap),
            ArgA = this.ArgA.ResolveGeneric(typeMap),
            ArgB = this.ArgB.ResolveGeneric(typeMap),
        };
    }
}

public record NotInstruction(UnaryOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new NotInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

public record BitNotInstruction(UnaryOpInstructionData Op) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new BitNotInstruction(this.Op.ResolveGeneric(typeMap));
    }
}

[MessagePackObject]
public sealed record UnaryOpInstructionData {
    [Key("out")]
    public required IRef Out {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("a")]
    public required IValue Arg {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    public UnaryOpInstructionData ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new UnaryOpInstructionData {
            Out = this.Out.ResolveGeneric(typeMap),
            Arg = this.Arg.ResolveGeneric(typeMap),
        };
    }
}

[MessagePackObject]
public record AddrOfInstruction : IInstruction {
    [Key("out")]
    public required IRef Out {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("a")]
    public required IRef Arg {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new AddrOfInstruction {
            Out = this.Out.ResolveGeneric(typeMap),
            Arg = this.Arg.ResolveGeneric(typeMap),
        };
    }
}

[MessagePackObject]
public record MakeRefInstruction : IInstruction {
    [Key("out")]
    public required IRef Out {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("a")]
    public required IRef Arg {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new MakeRefInstruction {
            Out = this.Out.ResolveGeneric(typeMap),
            Arg = this.Arg.ResolveGeneric(typeMap),
        };
    }
}

[MessagePackObject]
public record LengthInstruction : IInstruction {
    [Key("out")]
    public required IRef Out {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("a")]
    public required IRef Arg {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("of_type")]
    public required IType ArrayType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new LengthInstruction {
            Out = this.Out.ResolveGeneric(typeMap),
            Arg = this.Arg.ResolveGeneric(typeMap),
            ArrayType = this.ArrayType.ResolveGeneric(typeMap),
        };
    }
}

[MessagePackObject]
public record CallInstruction : IInstruction {
    [Key("out")]
    [MessagePackFormatter(typeof(NullableRefFormatter))]
    public required IRef? Out { get; init; }

    [Key("function")]
    public required IValue Function {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("args")]
    public required IReadOnlyList<IValue> Args {
        get;
        init => field = value.ToArrayNonNull();
    }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new CallInstruction {
            Out = this.Out?.ResolveGeneric(typeMap),
            Args = this.Args.Select(val => val.ResolveGeneric(typeMap)).ToArray(),
            Function = this.Function.ResolveGeneric(typeMap),
        };
    }
}

[MessagePackObject]
public record VirtualCallInstruction : IInstruction {
    [Key("out")]
    [MessagePackFormatter(typeof(NullableRefFormatter))]
    public required IRef? Out { get; init; }

    [Key("iface_ref")]
    public required InterfaceRef InterfaceRef { get; init; }

    [Key("method")]
    public required MethodID MethodID { get; init; }

    [Key("self_arg")]
    public required IRef SelfArg {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("rest_args")]
    public required IReadOnlyList<IValue>? RestArgs {
        get;
        init => field = value.ToArrayNonNull();
    }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new VirtualCallInstruction {
            Out = this.Out?.ResolveGeneric(typeMap),
            InterfaceRef = this.InterfaceRef.ResolveGeneric(typeMap),
            MethodID = this.MethodID,
            SelfArg = this.SelfArg,
            RestArgs = this.RestArgs?.Select(val => val.ResolveGeneric(typeMap)).ToArray(),
        };
    }
}

[MessagePackObject]
public record IsTypeInstruction : IInstruction {
    [Key("out")]
    public required IRef Out {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("a")]
    public required IValue Arg {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("value_type")]
    public required IType ValueType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("is_type")]
    public required IType IsType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new IsTypeInstruction {
            Out = this.Out.ResolveGeneric(typeMap),
            Arg = this.Arg.ResolveGeneric(typeMap),
            ValueType = this.ValueType.ResolveGeneric(typeMap),
            IsType = this.IsType.ResolveGeneric(typeMap),
        };
    }
}

public record LabelInstruction(Label Label) : IInstruction {
    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return this;
    }
}

public readonly record struct Label(ulong ID) {
    public override string ToString() {
        return $"{this.ID}:";
    }
}

public class LabelFormatter : IMessagePackFormatter<Label> {
    public void Serialize(ref MessagePackWriter writer, Label value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public Label Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var id = reader.ReadUInt64();
        return new Label(id);
    }
}

[MessagePackObject]
public record JumpInstruction : IInstruction {
    [Key("dest")]
    public required Label Destination { get; init; }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return this;
    }
}

[MessagePackObject]
public record JumpIfInstruction : IInstruction {
    [Key("dest")]
    public required Label Destination { get; init; }

    [Key("test")]
    public required IValue Condition {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new JumpIfInstruction {
            Destination = this.Destination,
            Condition = this.Condition.ResolveGeneric(typeMap),
        };
    }
}

[MessagePackObject]
public record NewObjectInstruction : IInstruction {
    [Key("out")]
    public required IRef Out {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("type_id")]
    public required TypeDefID TypeID { get; init; }

    [Key("type_args")]
    public IReadOnlyList<IType>? TypeArgs { get; init; }

    [Key("immortal")]
    public required bool Immortal { get; init; }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new NewObjectInstruction {
            Out = this.Out.ResolveGeneric(typeMap),
            Immortal = this.Immortal,
            TypeID = this.TypeID,
            TypeArgs = this.TypeArgs?.Select(t => t.ResolveGeneric(typeMap)).ToArray(),
        };
    }
}

[MessagePackObject]
public record NewArrayInstruction : IInstruction {
    [Key("out")]
    public required IRef Out {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("element_type")]
    public required IType ElementType { get; init; }

    [Key("count")]
    public required IValue Count { get; init; }

    [Key("immortal")]
    public required bool Immortal { get; init; }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new NewArrayInstruction {
            Out = this.Out.ResolveGeneric(typeMap),
            ElementType = this.ElementType.ResolveGeneric(typeMap),
            Count = this.Count.ResolveGeneric(typeMap),
            Immortal = this.Immortal,
        };
    }
}

[MessagePackObject]
public record NewBoxInstruction : IInstruction {
    [Key("out")]
    public required IRef Out {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("value_type")]
    public required IType ElementType { get; init; }

    [Key("immortal")]
    public required bool Immortal { get; init; }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new NewBoxInstruction {
            Out = this.Out.ResolveGeneric(typeMap),
            ElementType = this.ElementType.ResolveGeneric(typeMap),
            Immortal = this.Immortal,
        };
    }
}

[MessagePackObject]
public record ReleaseInstruction : IInstruction {
    [Key("at")]
    public required IRef At {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("value_type")]
    public required IType ValueType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("released_out")]
    [MessagePackFormatter(typeof(NullableRefFormatter))]
    public required IRef? ReleasedOut {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new ReleaseInstruction {
            At = this.At.ResolveGeneric(typeMap),
            ReleasedOut = this.ReleasedOut?.ResolveGeneric(typeMap),
            ValueType = this.ValueType.ResolveGeneric(typeMap),
        };
    }
}

[MessagePackObject]
public record RetainInstruction : IInstruction {
    [Key("at")]
    public required IRef At {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("value_type")]
    public required IType ValueType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new RetainInstruction {
            At = this.At.ResolveGeneric(typeMap),
            ValueType = this.ValueType.ResolveGeneric(typeMap),
        };
    }
}

[MessagePackObject]
public record RaiseInstruction : IInstruction {
    [Key("val")]
    public required IRef Value {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new RaiseInstruction {
            Value = this.Value.ResolveGeneric(typeMap),
        };
    }
}

[MessagePackObject]
public record CastInstruction : IInstruction {
    [Key("out")]
    public required IRef Out {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("ty")]
    public required IType Type {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("a")]
    public required IValue Value {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    public IInstruction ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new CastInstruction {
            Out = this.Out.ResolveGeneric(typeMap),
            Type = this.Type.ResolveGeneric(typeMap),
            Value = this.Value.ResolveGeneric(typeMap),
        };
    }
}

public class InstructionFormatter : IMessagePackFormatter<IInstruction> {
    public void Serialize(ref MessagePackWriter writer, IInstruction? value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public IInstruction Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        if (reader.NextMessagePackType != MessagePackType.String) {
            var count = reader.ReadMapHeader();
            if (count != 1) {
                throw new MessagePackSerializationException($"unexpected instruction value count: {count}");
            }
        }

        var key = reader.ReadString()!;

        switch (key) {
            case "Comment": {
                return new CommentInstruction(reader.ReadString()!);
            }

            case "LocalAlloc": {
                var (at, type) = reader.ReadPair<LocalID, IType>(options);
                return new LocalAllocInstruction(at, type);
            }

            case "Move": {
                return MessagePackSerializer.Deserialize<MoveInstruction>(ref reader, options);
            }

            case "Add": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new AddInstruction(op);
            }
            case "Sub": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new SubInstruction(op);
            }
            case "Mul": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new MulInstruction(op);
            }
            case "IDiv": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new IntDivInstruction(op);
            }
            case "FDiv": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new FloatDivInstruction(op);
            }
            case "Mod": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new ModInstruction(op);
            }
            case "Shl": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new ShlInstruction(op);
            }
            case "Shr": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new ShrInstruction(op);
            }
            case "BitAnd": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new BitAndInstruction(op);
            }
            case "BitOr": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new BitOrInstruction(op);
            }
            case "BitXor": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new BitXorInstruction(op);
            }
            case "Eq": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new EqInstruction(op);
            }
            case "Gt": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new GtInstruction(op);
            }
            case "Lt": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new LtInstruction(op);
            }
            case "Lte": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new LteInstruction(op);
            }
            case "Gte": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new GteInstruction(op);
            }
            case "And": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new AndInstruction(op);
            }
            case "Or": {
                var op = MessagePackSerializer.Deserialize<BinOpInstructionData>(ref reader, options);
                return new OrInstruction(op);
            }
            case "Not": {
                var op = MessagePackSerializer.Deserialize<UnaryOpInstructionData>(ref reader, options);
                return new NotInstruction(op);
            }
            case "BitNot": {
                var op = MessagePackSerializer.Deserialize<UnaryOpInstructionData>(ref reader, options);
                return new BitNotInstruction(op);
            }

            case "AddrOf": {
                return MessagePackSerializer.Deserialize<AddrOfInstruction>(ref reader, options);
            }
            
            case "MakeRef": {
                return MessagePackSerializer.Deserialize<MakeRefInstruction>(ref reader, options);
            }

            case "Length": {
                return MessagePackSerializer.Deserialize<LengthInstruction>(ref reader, options);
            }

            case "Call": {
                return MessagePackSerializer.Deserialize<CallInstruction>(ref reader, options);
            }

            case "VirtualCall": {
                return MessagePackSerializer.Deserialize<VirtualCallInstruction>(ref reader, options);
            }

            case "IsType": {
                return MessagePackSerializer.Deserialize<IsTypeInstruction>(ref reader, options);
            }

            case "Label": {
                var label = MessagePackSerializer.Deserialize<Label>(ref reader, options);
                return new LabelInstruction(label);
            }

            case "Jump": {
                return MessagePackSerializer.Deserialize<JumpInstruction>(ref reader, options);
            }

            case "JumpIf": {
                return MessagePackSerializer.Deserialize<JumpIfInstruction>(ref reader, options);
            }

            case "NewObject": {
                return MessagePackSerializer.Deserialize<NewObjectInstruction>(ref reader, options);
            }

            case "NewArray": {
                return MessagePackSerializer.Deserialize<NewArrayInstruction>(ref reader, options);
            }
            
            case "NewBox": {
                return MessagePackSerializer.Deserialize<NewBoxInstruction>(ref reader, options);
            }

            case "Release": {
                return MessagePackSerializer.Deserialize<ReleaseInstruction>(ref reader, options);
            }

            case "Retain": {
                return MessagePackSerializer.Deserialize<RetainInstruction>(ref reader, options);
            }

            case "Raise": {
                return MessagePackSerializer.Deserialize<RaiseInstruction>(ref reader, options);
            }

            case "Cast": {
                return MessagePackSerializer.Deserialize<CastInstruction>(ref reader, options);
            }

            default: {
                throw new MessagePackSerializationException($"illegal instruction discriminator: {key}");
            }
        }
    }
}
