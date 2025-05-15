unit Tags;
implementation

type
    MyCustomTag = class
    public
        Greeting: String;
    end;

    [MyCustomTag(Greeting: 'Hello')]
    MyClass = class
    end;
    
initialization
    var myClass := typeinfo(MyClass);
    
    var tagInstance := if myClass.FindTag(typeinfo(MyCustomTag)) is Option.Some t then t else
        raise 'Tag is missing!';
    
    var myTag := tagInstance.Downcast[MyCustomTag]().Get;

    WriteLn(myTag.Greeting + ', world!');
end.
