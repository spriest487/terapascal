implementation

type
    IAnimal = interface
    end;

    Cat = class of IAnimal
    end;
    
    Dog = class of IAnimal
    public
        function Bark;
    end;
    
function Dog.Bark;
begin
    WriteLn('woof!');
end;

initialization
    var animals: array of IAnimal := [
        Cat() as IAnimal,
        Dog() as IAnimal,
        Cat() as IAnimal,
    ];
    
    for var animal in animals do
    begin
        var animalType := TypeInfo.Get(animal);
        WriteLn('this animal is a ' + animalType.Name);
        
        if animalType.FindMethod('Bark') is Option.Some method then
        unsafe begin
            var animalPtr := animal;
            method.Invoke(@animalPtr as Pointer, [], nil);
        end;
    end;
end.
