implementation
uses System;

type Animal = interface
    function Speak();
end;

type Cat = class of Animal
    function Speak();
end;

function Cat.Speak();
begin
    WriteLn('nya');
end;

type Dog = class of Animal
    function Speak();
    function Fetch();
end;

function Dog.Speak();
begin
    WriteLn('wan');
end;

function Dog.Fetch();
begin
    WriteLn('fetching a ball');
end;

initialization
    var cat: Animal := Cat();
    if cat is Cat then cat.Speak();
    
    var dog: Animal := Dog();
    if dog is not Cat then dog.Speak();
    
    if dog is Dog d then d.Fetch();
    
    if dog is Animal then 
        WriteLn('yes, a dog is an animal');
    
    if dog is not Animal then 
        raise 'a dog is an animal!';
end
