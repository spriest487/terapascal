unit Lambda;

initialization

// explicit arg type
var f1 := lambda(x: Integer): x + 1;
var y1 := f1(1);

// inferred arg type
var f2: function(Integer; Integer): Integer := lambda(x; y): x + y;
var y2 := f2(9, 1);

WriteLn('Result 1 = ' + y1);
WriteLn('Result 2 = ' + y2);

end
