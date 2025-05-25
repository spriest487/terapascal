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
    var funcInfo := FunctionInfo.Find('FunctionTags.A').Get;
end.
