implementation
uses System;

const ExternFuncsLib: String = 'ExternFuncs.dll';

function X(); external ExternFuncsLib;
function Y(val: Integer); external ExternFuncsLib;
function Z(): Integer; external ExternFuncsLib;
function ReturnsIntPtr(): ^Integer; external ExternFuncsLib;
function PrintIntPtr(ptr: ^Integer); external ExternFuncsLib;

initialization
    X();
    Y(1);
    var z := Z();
    
    WriteLn(z.ToString());

    begin
        var intPtr := ReturnsIntPtr();
        PrintIntPtr(intPtr);
    
        WriteLn('printing value of intPtr^: ' + intPtr^.ToString());
    end;
    
    // pass pointer to stack value
    begin
        var one := 1;
        PrintIntPtr(@one);
    end;

    // pass pointer to heap value
    unsafe begin
        var intBytes: Pointer := GetMem(sizeof(Integer));
        var intPtr: ^Integer := intBytes;
        intPtr^ := 999888777;
        PrintIntPtr(intPtr);
    end;
end
