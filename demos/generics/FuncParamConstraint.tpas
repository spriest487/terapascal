implementation
uses System;

type 
    Greetable = interface
        function Name: String;
    end;

    Person = record of Greetable
        name: String;
        function Name: String;
    end;

    Cat = class of Greetable
        function Name: String;
    end;

function Person.Name: String;
begin
    self.name
end;

function Cat.Name: String;
begin
    'kitty'
end;

function Greet[T](t: T) 
where 
    T is Greetable;
begin
    WriteLn('hello, ' + t.Name);
    WriteLn('hello again, ' + Greetable.Name(t));
end;

initialization
    var alice := Person(name: 'Alice');
    Greet(alice);
    
    var kitty := Cat();
    Greet(kitty);

    //Greet('nobody');
end
