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
    var a := typeinfo(ClassWithMethods).FindMethod('A').Get.FindTag[MyTag]().Get;
    var b := typeinfo(InterfaceWithMethods).FindMethod('A').Get.FindTag[MyTag]().Get;
    
    WriteLn(a.message);
    WriteLn(b.message);
end
