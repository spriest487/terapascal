program Unit1;

uses 
    System,
    Unit2;

begin
    // error: class constructors are private to the declaring module
    { var c := Unit2.C(value: 123); }
    
    // ok: record constructors are public
    var r := Unit2.R(value: 123);
    
    // ok: variant constructors are public
    var vL := Unit2.V.Left();
    var vR := Unit2.V.Right();
    
    // error: record is not exported
    //var r2 := Unit2.R2(value: 456);
end
