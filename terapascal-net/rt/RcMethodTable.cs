#nullable disable

namespace Terapascal.Runtime;

public delegate void RcMethod(TypedReference obj);
public delegate void RcArrayMethod(Array array);

internal struct RcMethodTable {
    internal RcMethod Retain;
    internal RcMethod Release;

    internal RcArrayMethod ArrayRelease;
}
