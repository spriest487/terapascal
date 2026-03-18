using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public readonly record struct Version {
    [Key("major")]
    public uint Major { get; init; }
    
    [Key("minor")]
    public uint Minor { get; init; }
    
    [Key("patch")]
    public uint Patch { get; init; }

    public static implicit operator System.Version(Version version) {
        return new((int)version.Major, (int)version.Minor, (int)version.Patch);
    }

    public override string ToString() {
        return $"{this.Major}.{this.Minor}.{this.Patch}";
    }
};
