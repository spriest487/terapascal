unit Tags;
implementation

type
    MyCustomTag = class
    public
        Greeting: String;
    end;
    
    MySecondTag = class
    public
        Value: Integer;
    end;

    [MyCustomTag(Greeting: 'Hello')]
    [MySecondTag(Value: 123); MySecondTag(Value: 456)]
    MyClass = class
    end;
    
initialization
    var myClass := typeinfo(MyClass);
    var tagInstance := if myClass.FindTag(typeinfo(MyCustomTag)) is Option.Some t 
        then t 
        else raise 'Tag is missing!';
    
    var myTag := tagInstance.Downcast[MyCustomTag]().Get;

    WriteLn(myTag.Greeting + ', world!');

    var secondTags := myClass.FindTags[MySecondTag](); // TODO: generic error
    
    for var secondTag in secondTags do 
    begin
        WriteLn('second tag with value: ' + secondTag.Value);
    end;
end.
