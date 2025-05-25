implementation
uses System;

function Array3Identity(a: array[3] of Integer): array[3] of Integer;
begin
    a
end;

function DynArrayIdentity(a: array of Integer): array of Integer;
begin
    a
end;

initialization
    var a1 := Array3Identity([1, 2, 3]);
    var a2 := [1, 2, 3].Array3Identity();
    
    var a3 := DynArrayIdentity([1, 2, 3]);
    var a4: array of Integer := [1, 2, 3];
    var a5 := a4.DynArrayIdentity();
end
