using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public interface IInstruction;

public record CommentInstruction(string Text) : IInstruction;

public record DebugPushInstruction(Span Context) : IInstruction;

public record DebugPopInstruction : IInstruction;

public record LocalAllocInstruction(LocalID At, IType Type) : IInstruction;

public record LocalBeginInstruction : IInstruction;

public record LocalEndInstruction : IInstruction;

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
}

public record AddInstruction(BinOpInstruction Op) : IInstruction;

public record SubInstruction(BinOpInstruction Op) : IInstruction;

public record MulInstruction(BinOpInstruction Op) : IInstruction;

public record IDivInstruction(BinOpInstruction Op) : IInstruction;

public record FDivInstruction(BinOpInstruction Op) : IInstruction;

public record ModInstruction(BinOpInstruction Op) : IInstruction;

public record ShlInstruction(BinOpInstruction Op) : IInstruction;

public record ShrInstruction(BinOpInstruction Op) : IInstruction;

public record BitAndInstruction(BinOpInstruction Op) : IInstruction;

public record BitOrInstruction(BinOpInstruction Op) : IInstruction;

public record BitXorInstruction(BinOpInstruction Op) : IInstruction;

public record EqInstruction(BinOpInstruction Op) : IInstruction;

public record GtInstruction(BinOpInstruction Op) : IInstruction;

public record LtInstruction(BinOpInstruction Op) : IInstruction;

public record LteInstruction(BinOpInstruction Op) : IInstruction;

public record GteInstruction(BinOpInstruction Op) : IInstruction;

public record AndInstruction(BinOpInstruction Op) : IInstruction;

public record OrInstruction(BinOpInstruction Op) : IInstruction;

[MessagePackObject]
public record BinOpInstruction : IInstruction {
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
}

public record NotInstruction(UnaryOpInstruction Op) : IInstruction;

public record BitNotInstruction(UnaryOpInstruction Op) : IInstruction;

[MessagePackObject]
public sealed record UnaryOpInstruction : IInstruction {
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
}

[MessagePackObject]
public record ElementInstruction : IInstruction {
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

    [Key("index")]
    public required IValue Index {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("of_type")]
    public required IType ArrayType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
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
}

[MessagePackObject]
public record VariantTagInstruction : IInstruction {
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

    [Key("of_ty")]
    public required IType VariantType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }
}

[MessagePackObject]
public record VariantDataInstruction : IInstruction {
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

    [Key("of_ty")]
    public required IType VariantType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("tag")]
    public required ulong Tag { get; init; }
}

[MessagePackObject]
public record FieldInstruction : IInstruction {
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

    [Key("of_ty")]
    public required IType BaseType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("field")]
    public required FieldID Field { get; init; }
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
}

[MessagePackObject]
public record VirtualCallInstruction : IInstruction {
    [Key("out")]
    [MessagePackFormatter(typeof(NullableRefFormatter))]
    public required IRef? Out { get; init; }

    [Key("iface_id")]
    public required InterfaceID InterfaceID { get; init; }

    [Key("method")]
    public required MethodID MethodID { get; init; }

    [Key("self_arg")]
    public required IValue SelfArg {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("rest_args")]
    public required IReadOnlyList<IValue>? RestArgs {
        get;
        init => field = value.ToArrayNonNull();
    }
}

[MessagePackObject]
public record ClassIsInstruction : IInstruction {
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

    [Key("class_id")]
    public required IObjectID ClassID {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }
}

public record LabelInstruction(Label Label) : IInstruction;

public readonly record struct Label(ulong ID);

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
}

[MessagePackObject]
public record NewInstruction : IInstruction {
    [Key("out")]
    public required IRef Out {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("type_id")]
    public required TypeDefID TypeID { get; init; }

    [Key("immortal")]
    public required bool Immortal { get; init; }
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
}

[MessagePackObject]
public record NewBoxInstruction : IInstruction {
    [Key("out")]
    public required IRef Out {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("element_type")]
    public required IType ElementType { get; init; }

    [Key("immortal")]
    public required bool Immortal { get; init; }
}

[MessagePackObject]
public record ReleaseInstruction : IInstruction {
    [Key("at")]
    public required IRef At {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("weak")]
    public required bool Weak { get; init; }

    [Key("released_out")]
    [MessagePackFormatter(typeof(NullableRefFormatter))]
    public required IRef? ReleasedOut {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }
}

[MessagePackObject]
public record RetainInstruction : IInstruction {
    [Key("at")]
    public required IRef At {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("weak")]
    public required bool Weak { get; init; }
}

[MessagePackObject]
public record RaiseInstruction : IInstruction {
    [Key("val")]
    public required IRef Value {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
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
            case "DebugPush": {
                var span = MessagePackSerializer.Deserialize<Span>(ref reader, options);
                return new DebugPushInstruction(span);
            }

            case "DebugPop": {
                return new DebugPopInstruction();
            }

            case "LocalBegin": {
                return new LocalBeginInstruction();
            }
            case "LocalEnd": {
                return new LocalEndInstruction();
            }
            case "LocalAlloc": {
                var (at, type) = reader.ReadPair<LocalID, IType>(options);
                return new LocalAllocInstruction(at, type);
            }

            case "Move": {
                return MessagePackSerializer.Deserialize<MoveInstruction>(ref reader, options);
            }

            case "Add": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new AddInstruction(op);
            }
            case "Sub": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new SubInstruction(op);
            }
            case "Mul": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new MulInstruction(op);
            }
            case "IDiv": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new IDivInstruction(op);
            }
            case "FDiv": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new FDivInstruction(op);
            }
            case "Mod": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new ModInstruction(op);
            }
            case "Shl": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new ShlInstruction(op);
            }
            case "Shr": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new ShrInstruction(op);
            }
            case "BitAnd": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new BitAndInstruction(op);
            }
            case "BitOr": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new BitOrInstruction(op);
            }
            case "BitXor": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new BitXorInstruction(op);
            }
            case "Eq": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new EqInstruction(op);
            }
            case "Gt": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new GtInstruction(op);
            }
            case "Lt": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new LtInstruction(op);
            }
            case "Lte": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new LteInstruction(op);
            }
            case "Gte": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new GteInstruction(op);
            }
            case "And": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new AndInstruction(op);
            }
            case "Or": {
                var op = MessagePackSerializer.Deserialize<BinOpInstruction>(ref reader, options);
                return new OrInstruction(op);
            }
            case "Not": {
                var op = MessagePackSerializer.Deserialize<UnaryOpInstruction>(ref reader, options);
                return new NotInstruction(op);
            }
            case "BitNot": {
                var op = MessagePackSerializer.Deserialize<UnaryOpInstruction>(ref reader, options);
                return new BitNotInstruction(op);
            }

            case "AddrOf": {
                return MessagePackSerializer.Deserialize<AddrOfInstruction>(ref reader, options);
            }
            
            case "MakeRef": {
                return MessagePackSerializer.Deserialize<MakeRefInstruction>(ref reader, options);
            }

            case "Element": {
                return MessagePackSerializer.Deserialize<ElementInstruction>(ref reader, options);
            }

            case "Length": {
                return MessagePackSerializer.Deserialize<LengthInstruction>(ref reader, options);
            }

            case "VariantTag": {
                return MessagePackSerializer.Deserialize<VariantTagInstruction>(ref reader, options);
            }

            case "VariantData": {
                return MessagePackSerializer.Deserialize<VariantDataInstruction>(ref reader, options);
            }

            case "Field": {
                return MessagePackSerializer.Deserialize<FieldInstruction>(ref reader, options);
            }

            case "Call": {
                return MessagePackSerializer.Deserialize<CallInstruction>(ref reader, options);
            }

            case "VirtualCall": {
                return MessagePackSerializer.Deserialize<VirtualCallInstruction>(ref reader, options);
            }

            case "ClassIs": {
                return MessagePackSerializer.Deserialize<ClassIsInstruction>(ref reader, options);
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
                return MessagePackSerializer.Deserialize<NewInstruction>(ref reader, options);
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
