unit ObjectCtor;

implementation

uses
    System;

type
    SomeObject = record
        field1: Integer;
        field2: Boolean;
    end;

function DisplaySomeObject(name: String; obj: SomeObject); inline;
begin
    WriteLn(name + ': field1 = ' + obj.field1 +', field2 = ' + obj.field2);
end;

initialization
    // named
    var someA := SomeObject(field1: 123; field2: true);
    
    // inferred
    var someB: SomeObject := (field1: 456; field2: false);
    
    // empty
    var someC := SomeObject();
    
    // empty, inferred
    var someD: SomeObject := ();
    
    // partially inferred
    var someE := SomeObject(field2: true);
    
    DisplaySomeObject('someA', someA);
    DisplaySomeObject('someB', someB);
    DisplaySomeObject('someC', someC);
    DisplaySomeObject('someD', someD);
    DisplaySomeObject('someE', someE);
end.
