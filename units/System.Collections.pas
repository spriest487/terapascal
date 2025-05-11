unit System.Collections;
interface

uses System;

type
    LinkedListNode[T] = class
    private
        next: Option[LinkedListNode[T]];
        val: T;
    public
        function Next: Option[LinkedListNode[T]];
        function Value: T;
    end;
    
    LinkedListSequence[T] = record
    private
        next: Option[LinkedListNode[T]];
    public
        function Next: Option[T];
    end;

    LinkedList[T] = class
    private
        head: Option[LinkedListNode[T]];
    
    public
        function Length: Integer;
        
        function Get(n: Integer): T;
        function TryGet(n: Integer): Option[T];
        
        function Set(n: Integer; value: T);
        
        function Head: Option[LinkedListNode[T]];
        
        function Add(item: T);
        function Remove(n: Integer);
    
        function Clear;
        
        function Sequence: LinkedListSequence[T];

        constructor Create;
    end;
    
    ArrayList[T] = class;
    
    ArrayListSequence[T] = record
    private
        list: ArrayList[T];
        pos: Integer;
    
    public
        function Next: Option[T];
    end;

    ArrayList[T] = class
    private
        items: array of T;
        len: Integer;
        
    public
        function Length: Integer;
        function Capacity: Integer;
        
        function Get(n: Integer): T;
        function TryGet(n: Integer): Option[T];
        
        function Set(n: Integer; value: T);
        
        function Add(item: T);
        function Remove(n: Integer);
        
        function Clear;
        
        function Sequence: ArrayListSequence[T];
        
        constructor Create;
    end;

implementation

const 
    ARRAYLIST_INITIAL_CAPACITY = 4;
    ARRAYLIST_GROWTH = 2;

function GetNodeAt[T](list: LinkedList[T]; n: Integer): Option[LinkedListNode[T]];
begin
    if list.head is Option.Some head then begin
        if n = 0 then
            Option.Some(head)
        else begin
            var current := head;
            var tooShort := false;

            for var i := 0 to n - 1 do begin
                if current.next is Option.Some node then begin
                    current := node;
                end
                else begin
                    tooShort := true;
                    break;
                end;
            end;

            if tooShort then
                Option.None
            else
                Option.Some(current)
        end
    end
    else begin
        Option.None
    end
end;

constructor LinkedList[T].Create;
begin
    LinkedList(
        head: Option.None;
    );
end;

function LinkedListNode[T].Next: Option[LinkedListNode[T]];
begin
    self.next;
end;

function LinkedListNode[T].Value: T;
begin
    self.val;
end;

function LinkedList[T].Head: Option[LinkedListNode[T]];
begin
    self.head;
end;

function LinkedList[T].Length: Integer;
begin
    if self.head is Option.Some head then 
    begin
        var current := head.next;
        var count := 0;
    
        while true do begin
            count := count + 1;
    
            if current is Option.Some node then 
            begin
                current := node.next;
            end
            else begin
                break;
            end;
        end;
    
        count;
    end
    else 0;
end;

function LinkedList[T].Get(n: Integer): T;
begin
    match self.TryGet(n) of
        Option.Some item: item;
        Option.None: raise 'index out of range: ' + n;
    end;
end;

function LinkedList[T].TryGet(n: Integer): Option[T];
begin
    var nth := self.GetNodeAt(n);

    var result: Option[T] := if nth is Option.Some node then
        Option.Some(node.val)
    else
        Option.None;

    result
end;

function LinkedList[T].Set(n: Integer; value: T);
begin
    var nth := self.GetNodeAt(n);

    if nth is Option.Some node then
        node.val := value
    else
        raise 'index out of range: ' + n;
end;

function LinkedList[T].Remove(n: Integer);
begin
    if n = 0 then
    begin
        match self.head of
            Option.Some head:
                self.head := head.next;
                
            Option.None:
                raise 'index out of range: 0'; 
        end; 
    end
    else
        match self.GetNodeAt(n - 1) of
            Option.Some parent: 
                match parent.next of
                    Option.Some removed:
                        parent.next := removed.next;
                    
                    Option.None:
                        raise 'index out of range: ' + n;    
                
            Option.None:
                raise 'index out of range: ' + n;
        end;
    end;
end;

function LinkedList[T].Clear;
begin
    self.head := Option.None;
end;

function LinkedList[T].Add(item: T);
begin
    if self.head is Option.Some head then begin
        var current := head;

        while true do begin
            if current.next is Option.Some next then
                current := next
            else begin
                current.next := Option.Some(LinkedListNode(
                    next: Option.None;
                    val: item;
                ));
                break;
            end
        end
    end
    else begin
        self.head := Option.Some(LinkedListNode(
            next: Option.None;
            val: item;
        ));
    end;
end;

function LinkedList[T].Sequence: LinkedListSequence[T];
begin
    LinkedListSequence(next: self.head);
end;

function LinkedListSequence[T].Next: Option[T];
begin
    match self.next of
        Option.Some nextNode:
        begin
            var nextVal := nextNode.val;
            self.next := nextNode.next;
            
            Option.Some(nextVal);
        end;
        
        else
            Option.None;
    end; 
end;

constructor ArrayList[T].Create;
begin
    ArrayList(
        items: [];
        len: 0;
    )
end;

function ArrayList[T].Length: Integer;
begin
    self.len;
end;

function ArrayList[T].Capacity: Integer;
begin
    self.items.Length
end;
    
function ArrayList[T].Get(n: Integer): T;
begin
    match self.TryGet(n) of
        Option.Some item: item;
        Option.None: raise 'index out of range: ' + n;
    end;
end;

function ArrayList[T].TryGet(n: Integer): Option[T];
begin
    if n >= 0 and n < self.len then
    begin
        var item := self.items[n];
        Option.Some(item);
    end
    else 
        Option.None;
end;

function ArrayList[T].Set(n: Integer; value: T);
begin
    if n < 0 or n >= self.len then
    begin
        raise 'index out of range: ' + n;
    end
    else
        self.items[n] := value;
end;

function ReallocItems[T](list: ArrayList[T]; size: Integer);
unsafe begin
    // unsafe: element type might be non-nullable reference
    list.items.SetLength(size, default(T)); 
end;

function ArrayList[T].Add(item: T);
begin
    var capacity := self.items.Length;
    if self.len + 1 > capacity then
    begin
        capacity := if capacity = 0 then 
            ARRAYLIST_INITIAL_CAPACITY
        else
            capacity * ARRAYLIST_GROWTH;

        self.ReallocItems(capacity);
    end;
        
    self.items[self.len] := item;
    self.len += 1;
end;

function ArrayList[T].Remove(n: Integer);
unsafe begin
    if n < 0 or n >= self.len then 
        exit;

    var capacity := self.items.Length;
    for var i := n to capacity - 2 do
    begin
        self.items[i] := self.items[i + 1];
    end;
    
    self.len -= 1;

    // unsafe: element type might be non-nullable reference
    self.items[self.len] := default;
end;

function ArrayList[T].Clear;
unsafe begin
    for var i := 0 to self.len - 1 do
    begin
        // unsafe: element type might be non-nullable reference 
        self.items[i] := default;
    end;

    self.len := 0;
end;

function ArrayList[T].Sequence: ArrayListSequence[T];
begin
    ArrayListSequence(list: self; pos: 0)
end;

function ArrayListSequence[T].Next: Option[T];
begin
    if self.pos >= self.list.Length then
    begin
        exit Option.None;
    end;

    var next := self.list.Get(self.pos);
    self.pos += 1;
    
    Option.Some(next)
end;

end
