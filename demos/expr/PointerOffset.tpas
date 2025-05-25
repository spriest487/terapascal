implementation
uses System;

initialization
    var mem := GetMem(3);
    //var mem: array[3] of Byte := [123, 131, 213];
    var x := mem[0];
    var pmem := @mem[0];
    
    pmem[0] := $1;
    pmem[1] := $2;
    pmem[2] := $3;
    
    WriteLn(mem[0].ToString());
    WriteLn(mem[1].ToString());
    WriteLn(mem[2].ToString());
end
