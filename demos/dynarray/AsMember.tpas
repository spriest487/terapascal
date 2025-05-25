implementation
uses System;

type B = class
    item: Integer;
end;

type A = record
    Items: array of B;
end;

initialization
    var a := A(
        Items: [
            B(item: 123)
        ]
    );
    
    var b2 := B(item: 456);
    
    var newIndex := a.Items.Length;    
    SetLength(a.Items, newIndex + 1, b2);
    
    a.Items[0] := B(item: 123);

    WriteLn('a.Items[0].item = ' + a.Items[0].item.ToString());
    WriteLn('a.Items[1].item = ' + a.Items[1].item.ToString());
end
