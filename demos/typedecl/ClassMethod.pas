implementation
uses
    System;

type
    MyClass = class
        val: Integer;
    public
        class function DefaultValue: Integer;
        class function Double(val: Integer): Integer;
        constructor Create;
    end;
    
class function MyClass.DefaultValue: Integer;
begin
    123
end;

class function MyClass.Double(val: Integer): Integer;
begin
    val * 2
end;

constructor MyClass.Create;
begin
    MyClass(val: MyClass.DefaultValue);
end;

initialization
    var instance := MyClass.Create();    
    WriteLn('default val: ' + instance.val);
    
    var doubled := MyClass.Double(100);    
    WriteLn('doubled: ' + doubled);
end.
