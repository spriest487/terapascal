using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.CIL;

internal class CustomCollectionsResolver : IFormatterResolver {
    public static CustomCollectionsResolver Instance { get; } = new CustomCollectionsResolver();

    private readonly Dictionary<(Type, Type), IMessagePackFormatter> cache = new Dictionary<(Type, Type), IMessagePackFormatter>();
    
    internal class OrderedDictionaryFormatter<TKey, TVal> : IMessagePackFormatter<OrderedDictionary<TKey, TVal>>
        where TKey : notnull
    {
        public void Serialize(ref MessagePackWriter writer, OrderedDictionary<TKey, TVal> value, MessagePackSerializerOptions options) {
            throw new NotImplementedException();
        }

        public OrderedDictionary<TKey, TVal> Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
            var count = reader.ReadMapHeader();

            var dict = new OrderedDictionary<TKey, TVal>(count);
            for (var i = 0; i < count; i += 1) {
                var key = MessagePackSerializer.Deserialize<TKey>(ref reader, options);
                var val = MessagePackSerializer.Deserialize<TVal>(ref reader, options);
            
                dict.Add(key, val);
            }

            return dict;
        }
    }

    public IMessagePackFormatter<T>? GetFormatter<T>() {
        var type = typeof(T);
        var orderedDictType = typeof(OrderedDictionary<,>);

        if (!type.IsConstructedGenericType || type.GetGenericTypeDefinition() != orderedDictType) {
            return null;
        }

        var typeArgs = typeof(T).GetGenericArguments();
        
        var cacheKey = (typeArgs[0], typeArgs[1]);
        if (this.cache.TryGetValue(cacheKey, out var instance)) {
            return (IMessagePackFormatter<T>)instance;
        }
        
        var formatterType = typeof(OrderedDictionaryFormatter<,>).MakeGenericType(typeArgs);

        var newInstance = (IMessagePackFormatter<T>?)Activator.CreateInstance(formatterType);
        if (newInstance != null) {
            this.cache.Add(cacheKey, newInstance);
        }

        return newInstance;
    }
}
