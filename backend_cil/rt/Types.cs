namespace Terapascal.Runtime;

public class Object {
    internal int strongCount;
    internal int weakCount;

    protected internal virtual void Destroy() {
    }
}

public static class ObjectUtil {
    public static T Create<T>(bool immortal) where T : Object, new() {
        return new T {
            strongCount = immortal ? -1 : 1,
            weakCount = 0,
        };
    }

    public static bool Is<T>(object? any) {
        if (any is not T instance) {
            return false;
        }

        if (instance is Object obj) {
            return obj.strongCount != 0;
        }

        return true;
    }
}

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

    public bool Equals(String? other) {
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

    public override bool Equals(object? obj) {
        return obj is String other && this.Equals(other);
    }
}

public sealed class TypeInfo : Object {
    public String name;
    public MethodInfo[] methods;

    public object[] tags;

    public Type impl;
}

public sealed class MethodInfo : Object {
    public String name;
    public TypeInfo owner;
    
    public System.Reflection.MethodInfo impl;

    public object[] tags;
}

public sealed class FunctionInfo : Object {
    public String name;

    public System.Reflection.MethodInfo impl;

    public object[] tags;
}

public class ClosureBase : Object { 
    public IntPtr functionPointer;
}
