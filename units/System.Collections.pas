unit System.Collections;
interface

uses System;

type LinkedListNode[T] = class
    private
        next: Option[LinkedListNode[T]];
        val: T;
    public
        function Next: Option[LinkedListNode[T]];
        function Value: T;
end;

type LinkedList[T] = class
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
end;

type ArrayList[T] = class
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
end;

function NewLinkedList[T](): LinkedList[T];
function NewArrayList[T](): ArrayList[T];

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

            for var i := 0 to (n - 1) do begin
                if current.next is Option.Some node then begin
                    current := node;
                end
                else begin
                    tooShort := true;
                    break;
                end;
            end;

            if tooShort then
                Option.None()
            else
                Option.Some(current)
        end
    end
    else begin
        Option.None()
    end
end;

function NewLinkedList[T](): LinkedList[T];
begin
    LinkedList(
        head: Option.None();
    )
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
        Option.None: raise 'index out of range: ' + n.ToString();
    end;
end;

function LinkedList[T].TryGet(n: Integer): Option[T];
begin
    var nth := GetNodeAt[T](self, n);

    var result: Option[T] := if nth is Option.Some node then
        Option.Some(node.val)
    else
        Option.None();

    result
end;

function LinkedList[T].Set(n: Integer; value: T);
begin
    var nth := GetNodeAt[T](self, n);

    if nth is Option.Some node then
        node.val := value
    else
        raise 'index out of range: ' + n.ToString();
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
        match GetNodeAt[T](self, n - 1) of
            Option.Some parent: 
                match parent.next of
                    Option.Some removed:
                        parent.next := removed.next;
                    
                    Option.None:
                        raise 'index out of range: ' + n.ToString();    
                
            Option.None:
                raise 'index out of range: ' + n.ToString();
        end;
    end;
end;

function LinkedList[T].Clear;
begin
    self.head := Option.None();
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
                    next: Option.None();
                    val: item;
                ));
                break;
            end
        end
    end
    else begin
        self.head := Option.Some(LinkedListNode(
            next: Option.None();
            val: item;
        ));
    end;
end;

function NewArrayList[T]: ArrayList[T];
begin
    ArrayList(
        items: [];
        len: 0;
    );
end;

function ArrayList[T].Length: Integer;
begin
    self.len;
end;

function ArrayList[T].Capacity: Integer;
begin
    Length(self.items);
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
    else Option.None();
end;

function ArrayList[T].Set(n: Integer; value: T);
begin
    if n < 0 or n >= self.len then
    begin
        raise 'index out of range: ' + n.ToString();
    end
    else
        self.items[n] := value;
end;

function ReallocItems[T](list: ArrayList[T]; size: Integer);
unsafe begin
    // unsafe: element type might be non-nullable reference
    SetLength(list.items, size, default(T)); 
end;

function ArrayList[T].Add(item: T);
begin
    var capacity := Length(self.items);
    if self.len + 1 > capacity then
    begin
        capacity := if capacity = 0 then 
            ARRAYLIST_INITIAL_CAPACITY
        else
            capacity * ARRAYLIST_GROWTH;

        ReallocItems(self, capacity);
    end;
        
    self.items[self.len] := item;
    self.len += 1;
end;

function ArrayList[T].Remove(n: Integer);
unsafe begin
    if n < 0 or n >= self.len then 
        exit;

    var capacity := Length(self.items);
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

end
