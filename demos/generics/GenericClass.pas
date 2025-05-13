implementation
uses System;

type GenericBox[Val] = class
    val: Val;
end;

initialization
    var x := GenericBox(val: 100);
    var x2: GenericBox[String] := (val: 'hello');

    var y: GenericBox[Integer] := GenericBox(val: 123);
    WriteLn('y contains ' + IntToStr(y.val));
    
    var z: GenericBox[String] := GenericBox(val: 'hello world');
    WriteLn('z contains ' + z.val);
end
