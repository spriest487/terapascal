implementation
uses System;

function Identity[T](id: T): T;
begin
    id
end;

function Identity2[T](id: T): T;
begin
    id.Identity
end;

function Void[T];
begin
    WriteLn('void! ' + typeinfo(T).Name);
end;

initialization
    var two := Identity(2);
    
    var one := Identity2[Integer](1);
    WriteLn('one is: ' + IntToStr(one));
    
    var t := Identity2[Boolean](true);
    WriteLn('t is: ' + t);
    
    Void[array of Integer]();
    
    // the syntax of explicit type args could be ambiguous when calling a value returned
    // from an indexable object, e.g. an array of functions. in this case, we should always
    // interpret the pattern `X[Y](Z)` as a call to X with a type argument list and parameter list,
    // not a call to `X[Y]` with parameter Z.
    var funcs: array of function := [
        function; begin WriteLn('anon func 1'); end, 
        function; begin WriteLn('anon func 2'); end
    ];
    
    // this is OK
    var f1 := funcs[0];
    f1();
    
    // this is not
    // funcs[1]();
end
