// ReSharper disable InconsistentNaming

#nullable disable
namespace Terapascal.Runtime;

public sealed unsafe class String : Object, IEquatable<String> {
    // TODO
    // move some of the direct string field accesses in the system unit into
    // runtime functions so we can hide these fields in this implementation
    public byte* chars;
    public int len;

    public override string ToString() {
        return SystemFunctions.ReadString(this);
    }

    protected internal override void Destroy() {
        if (this.chars != null) {
            SystemFunctions.FreeMem(this.chars);
            this.chars = null;
        }

        this.len = 0;

        base.Destroy();
    }

    public override int GetHashCode() {
        if (this.chars == null || this.len == 0) {
            return 0;
        }

        var hash = 5381;
        unchecked {
            for (var i = 0; i < this.len; i += 1) {
                hash = ((hash << 5) + hash) + this.chars[i];
            }
        }

        return hash;
    }

    public bool Equals(String other) {
        if (other == null || other.len != this.len) {
            return false;
        }

        for (var i = 0; i < this.len; i += 1) {
            if (this.chars[i] != other.chars[i]) {
                return false;
            }
        }

        return true;
    }

    public override bool Equals(object obj) {
        return obj is String other && this.Equals(other);
    }
}
