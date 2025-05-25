implementation
uses System;

initialization
    var nothingPtr: ^Integer := nil;
    
    //var x := 1;
    
    //x := 2;
    //x := x + 1;
    //WriteLn(IntToStr(x));
    
    var y: Integer := 123321;
    var yPtr := @y;
    var z := yPtr^;
    
    WriteLn(IntToStr(z));
    
    // illegal: @y returns a temporary ^Integer
    // var yPtrPtr: ^^Integer := @@y;
    // WriteLn(IntToStr(yPtrPtr^^));
    
    var yPtrPtr := @yPtr;
    WriteLn(IntToStr(yPtrPtr^^));
    
    y := 9876;
    WriteLn(IntToStr(yPtr^));
    
    yPtrPtr^^ := 56654;
    WriteLn(IntToStr(y));
    
    var bothTrue := true and true;
    WriteLn(if bothTrue then 'Both true' else 'Not both true');
end
