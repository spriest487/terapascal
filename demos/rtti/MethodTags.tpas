implementation

type 
    MyTag = class
        message: String;
    end;

    ClassWithMethods = class
        [MyTag(message: 'Class method A')]
        function A;
    end;
    
    InterfaceWithMethods = interface
        [MyTag(message: 'Interface method B')]
        function B;
    end;
    
function ClassWithMethods.A;
begin
end;

initialization
    var methodA := typeinfo(ClassWithMethods).FindMethod('A').Get;
    var aTag := methodA.FindTag[MyTag]().Get;
    WriteLn(aTag.message);

    var methodB := typeinfo(InterfaceWithMethods).FindMethod('B').Get;
    var bTag := methodB.FindTag[MyTag]().Get;
    WriteLn(bTag.message);
end
