implementation

type
    MyClass = class
    published
        function MyMethod;
        function MyMethod1: String;
        function MyMethod2(i: Integer);
    end;

function MyClass.MyMethod;
begin
    WriteLn('Hello world!');
end;

function MyClass.MyMethod1: String;
begin
    var name := 'world';
    'Hello again, ' + name;
end;

function MyClass.MyMethod2(i: Integer);
begin
    WriteLn('Hello again! i is ' + i);
end;

initialization
    var myClassInfo := typeinfo(MyClass);
    var methods := myClassInfo.Methods;

    for var i := 0 to methods.Length - 1 do begin
        WriteLn(myClassInfo.Name + ' has method ' + methods[i].Name);
    end;
    
    var instance := MyClass();

    unsafe begin
        methods[0].Invoke(@instance as Pointer, [], nil);

        var message := default(String);
        methods[1].Invoke(@instance as Pointer, [], @message);
        WriteLn(message); 

        var i := 456;
        methods[2].Invoke(@instance, [@i as Pointer], nil);
    end
end
