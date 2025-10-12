using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public static class Util {
    public static T[] ToArrayNonNull<T>(this IReadOnlyList<T?>? src) {
        if (src == null) {
            return [];
        }

        var result = new T[src.Count];
        for (var i = 0; i < src.Count; i += 1) {
            var item = src[i];
            if (item == null) {
                throw new ArgumentException("source list contains null elements");
            }

            result[i] = item;
        }

        return result;
    }

    public static Dictionary<TKey, TVal> ToDictionaryNonNull<TKey, TVal>(this IReadOnlyDictionary<TKey, TVal?>? src)
        where TKey : notnull 
    {
        if (src == null) {
            return new Dictionary<TKey, TVal>();
        }

        var result = new Dictionary<TKey, TVal>(src.Count);
        foreach (var (k, v) in src) {
            if (v == null) {
                throw new ArgumentException("source dictionary contains null values");
            }

            result.Add(k, v);
        }

        return result;
    }
    
    public static SortedDictionary<TKey, TVal> ToDictionaryNonNull<TKey, TVal>(this SortedDictionary<TKey, TVal?>? src)
        where TKey : notnull 
    {
        if (src == null) {
            return new SortedDictionary<TKey, TVal>();
        }

        var result = new SortedDictionary<TKey, TVal>();
        foreach (var (k, v) in src) {
            if (v == null) {
                throw new ArgumentException("source dictionary contains null values");
            }

            result.Add(k, v);
        }

        return result;
    }

    public static (TFirst, TSecond) ReadPair<TFirst, TSecond>(
        this ref MessagePackReader reader,
        MessagePackSerializerOptions options
    ) {
        var count = reader.ReadArrayHeader();
        if (count != 2) {
            throw new MessagePackSerializationException("expected pair value as array with 2 elements");
        }

        var first = MessagePackSerializer.Deserialize<TFirst>(ref reader, options);
        var second = MessagePackSerializer.Deserialize<TSecond>(ref reader, options);

        return (first, second);
    }
}
