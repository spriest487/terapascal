unit System.SortedDict;
interface

type
    SortedDictEntry[TKey, TVal] = record of IComparable 
    where 
        TKey is IComparable
        
        key: TKey;
        val: TVal;
        
        function Compare(other: SortedDictEntry[TKey, TVal]): Integer;
    end;
    
    SortedDict[TKey, TVal] = class where TKey is IComparable;

    SortedDictSequence[TKey, TVal] = record
    where
        TKey is IComparable
    private
        dict: SortedDict[TKey, TVal];
        next: Integer;
    public
        function Next: Option[SortedDictEntry[TKey, TVal]];
    end;

    SortedDict[TKey, TVal] = class 
    where 
        TKey is IComparable

        entries: array of SortedDictEntry[TKey, TVal];
    public
        constructor Create;
        
        function Insert(k: TKey; v: TVal);
        function Find(k: TKey): Option[TVal];
        function Remove(k: TKey): Option[TVal];
        
        function Count: Integer;
        
        function Sequence: SortedDictSequence[TKey, TVal];
    end;

implementation

function SortedDictEntry[TKey, TVal].Compare(other: SortedDictEntry[TKey, TVal]): Integer;
begin
    self.key.Compare(other.key)
end;

constructor SortedDict[TKey, TVal].Create;
begin
    (entries: [])
end; 

function SortedDict[TKey, TVal].Insert(k: TKey; v: TVal);
begin
    var entry := SortedDictEntry[TKey, TVal]
    (
        key: k;
        val: v;
    );

    var index := BinarySearch(self.entries, entry); 
    if index >= 0 then 
    begin
        self.entries[index] := entry;
    end
    else unsafe begin
        var insertIndex := ~index;

        self.entries.SetLength(self.entries.Length + 1, default);

        for var i := insertIndex to self.entries.Length - 2 do
        begin
            self.entries[i + 1] := self.entries[i];
        end;
        
        self.entries[insertIndex] := entry;
    end;
end;

function SortedDict[TKey, TVal].Find(k: TKey): Option[TVal];
unsafe begin
    var entry := SortedDictEntry with [TKey, TVal]
    (
        key: k; 
        val: default(TVal);
    );

    var index := BinarySearch(self.entries, entry);
    if index >= 0 then 
        Option.Some(self.entries[index].val)
    else 
        Option.None
end;

function SortedDict[TKey, TVal].Remove(k: TKey): Option[TVal];
unsafe begin
    var entry := SortedDictEntry with [TKey, TVal]
    (
        key: k; 
        val: default(TVal);
    );

    var index := BinarySearch(self.entries, entry); 
    if index >= 0 then 
    begin
        var removed := self.entries[index].val;
        var currenLen := self.entries.Length;

        for var i := index to currenLen - 2 do
        begin
            self.entries[i] := self.entries[i + 1];
        end;
        
        self.entries.SetLength(currenLen - 1, default);
        
        Option.Some(removed)
    end
    else begin
        Option.None
    end
end;

function SortedDict[TKey, TVal].Sequence: SortedDictSequence[TKey, TVal];
begin
    (
        dict: self; 
        next: -1
    );
end;

function SortedDict[TKey, TVal].Count: Integer;
begin
    self.entries.Length
end;

function SortedDictSequence[TKey, TVal].Next: Option[SortedDictEntry[TKey, TVal]];
begin
    if self.next + 1 >= self.dict.Count then
    begin
        exit Option.None;
    end;
    
    self.next += 1;
    Option.Some(self.dict.entries[self.next]);
end;

end.
