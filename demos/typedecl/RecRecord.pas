uses System;

type Node = record
    next: ^Node;
end;

function ListLen(head: ^Node): Integer
begin
    var next := head;
    var count := 0;

    while next <> nil do begin
        count := count + 1;
        next := next^.next;
    end;

    count
end;

var last := Node(next: nil);
var mid := Node(next: @last);
var head := Node(next: @mid);

let len := ListLen(@head);

WriteLn(IntToStr(len));