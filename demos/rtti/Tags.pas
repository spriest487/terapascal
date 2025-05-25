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
    
    [MyCustomTag(Greeting: 'Hi')]
    MyRecord = record
    end;

    [MyCustomTag(Greeting: 'Greetings')]
    MyVariant = variant
        One;
        Two;
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
    
    var myRecordTag := typeinfo(MyRecord).FindTag[MyCustomTag]().Get;
    WriteLn(myRecordTag.Greeting +', world!');
    
    var myVariantTag := typeinfo(MyVariant).FindTag[MyCustomTag]().Get;
    WriteLn(myVariantTag.Greeting +', world!');
end.
