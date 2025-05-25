implementation
uses System;

type Animal = interface
    function Say();
end;

type Cat = class of Animal
    name: String;
    
    function Say();
end;

function Cat.Say();
begin
    WriteLn(self.name + ' says meow!');
end;

type Dog = class of Animal
    name: String;
    
    function Say();
end;

function Dog.Say();
begin
    WriteLn(self.name + ' says woof!');
end;

initialization
    var animal: Animal;
    
    animal := Cat(name: 'Jesse');
    animal.Say();
    
    animal := Dog(name: 'James');
    animal.Say();
end
