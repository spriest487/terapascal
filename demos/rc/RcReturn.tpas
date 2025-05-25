implementation
uses System;

type IntBox = class
    val: Integer;
end;

function Id(box: IntBox): IntBox;
begin
    box
end;

function New(): IntBox;
begin
    IntBox(val:456)
end;

initialization
    New();
    
    var box := IntBox(val: 1);
    Id(box);
    
    var box2 := IntBox(val: 1);
    box2 := New();
end
