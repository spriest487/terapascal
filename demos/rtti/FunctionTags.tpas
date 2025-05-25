program FunctionTags;

type
    MyTag1 = class
        name: String;
    end;
    
    MyTag2 = class
    end;

[MyTag1(name: 'World'); MyTag2]
[MyTag2]
function A;
begin
end;

begin
    WriteLn('all named functions:');

    var funcInfos := FunctionInfo.LoadedFunctions;
    for var funcInfo in funcInfos do
    begin
        if funcInfo.Name.Length > 0 then
        begin
            WriteLn(' * ' + funcInfo.Name);
        end;
    end;

    A();

    var funcInfo := FunctionInfo.Find('FunctionTags.A').Get;
    var myTag1 := funcInfo.FindTag[MyTag1]().Get;
    
    WriteLn('Hello, ' + myTag1.name);
    
    for var myTag2 in funcInfo.FindTags(typeinfo(MyTag2)) do
    begin
        WriteLn('found a MyTag2');
    end;
end.
