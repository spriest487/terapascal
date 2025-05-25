implementation
uses System;

type Greeting = record
    msg: String;
    num: Integer;
end;

initialization
    var greeting := Greeting(
        msg: 'Hi';
        num: 123;
    );

    greeting.msg := 'Hello';
    greeting.num := 456;
    
    WriteLn(greeting.msg + ', World! ' + IntToStr(greeting.num));
end
