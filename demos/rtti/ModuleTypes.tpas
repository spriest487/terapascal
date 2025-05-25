program ModuleTypes;

uses
    System.Collections;

type
    A = class
    end;
    
    B = record
        val: Integer;
    end;

begin
    var xs: array of Integer := [2];
    xs[0] := 1;

    for var ty in TypeInfo.LoadedTypes do 
    begin
        WriteLn('loaded type: ' + ty.Name);
    end;

    var aType := TypeInfo.Find('ModuleTypes.A');
    WriteLn('found A? ' + aType.IsSome);

    var bType := TypeInfo.Find('ModuleTypes.B');
    WriteLn('found B? ' + bType.IsSome);

    var cType := TypeInfo.Find('ModuleTypes.C');
    WriteLn('found C? ' + cType.IsSome);
    
    var dType := TypeInfo.Find('array of System.Int32');
    WriteLn('found array of Integer? ' + dType.IsSome);
end.
