unit Enum;

interface

implementation
uses System;

type
    OneValue = (One);
    SomeValues = (ValOne, ValTwo = 4, ValThree, ValFour);
    SomeMoreValues = (MoreValOne = -100, MoreValTwo = 1, MoreValThree = 30);

initialization
    var x := ValThree;
    var y := ValFour;
    var z := MoreValThree;
    
    WriteLn('x: ' + x as NativeInt);
    WriteLn('y: ' + y as NativeInt);
    WriteLn('z: ' + z as NativeInt);
end.
