implementation
uses System;

function One: Int32;
begin
    1;
end;

function Two(x: Int32): Int32;
begin
    x + 1
end;

function WriteA;
begin
    WriteLn('A');
end;

type C = interface
    function M(): Int32;
end;

type A = record of C
    value: Int32;

    function M(): Int32;
end;

function A.M(): Int32;
begin
    self.value + 1
end;

initialization
    // direct call to function
    var a := One;
    WriteLn('a: ' + a);

    // ufcs call
    var b := (1 as Int32).Two;
    WriteLn('b: ' + b);

    // method call
    var cObj := A(value: 2);
    var c := cObj.M;
    WriteLn('c: ' + c);
end.
