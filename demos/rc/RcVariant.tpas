implementation
uses System;

type Things = variant
    Box: Box[Integer];
    Text: String;
    None;
end;

initialization
    var boxVal := NewBox[Integer](123);
    var v := Things.Box(boxVal);
    v := Things.Text('hello world');
    v := Things.None();
end
