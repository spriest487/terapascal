unit System.SortedDict;
interface

type
    SortedDictEntry[TKey, TVal] = record of IComparable 
        where TKey is IComparable
 
        key: TKey;
        val: TVal;
        
        function Compare(other: SortedDictEntry[TKey, TVal]): Integer;
    end;

    SortedDict[TKey, TVal] = class 
        where TKey is IComparable

        entries: array of SortedDictEntry[TKey, TVal];
    public
        constructor Create;
    end;

implementation

function 

function SortedDictEntry[TKey, TVal].Compare(other: SortedDictEntry[TKey, TVal]): Integer;
begin
    self.key.Compare(other.key)
end;

end.
