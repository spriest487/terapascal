#nullable disable

namespace Terapascal.Runtime;

public delegate void RcMethod(TypedReference obj, bool weak);
public delegate void RcArrayMethod(Array array, bool weak);

internal struct RcMethodTable {
    internal RcMethod Retain;
    internal RcMethod Release;

    internal RcArrayMethod ArrayRelease;
}
