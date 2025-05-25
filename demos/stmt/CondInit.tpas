implementation
uses System;

initialization
    var a: String;
    var b: String;
    if true then begin
        a := 'true';
    
        // error: not initialized
        // WriteLn(b);
    end
    else begin
        b := 'false';
    
        // error: not initialized
        // WriteLn(a);
    end;
    
    // error: not initialized in both branches
    // WriteLn(a);
    // WriteLn(b);
    
    var c: String;
    var d: String;
    if true then begin
        c := 'first';
        d := 'second';
    end;
    
    // ok: initialized in single branch
    WriteLn(c);
    WriteLn(d);
    
    var e: String;
    var f: String;
    if true then begin
        e := 'first';
        f := 'second';
    end
    else begin
        e := 'third';
        f := 'fourth';
    end;
    
    // ok: initialized in both branches
    WriteLn(c);
    WriteLn(d);
end
