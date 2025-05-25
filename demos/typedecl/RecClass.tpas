implementation
uses System;

{ can't construct without an instance of itself }
type ValidButImpossible = class
    self: ValidButImpossible;
end;

type Rec = class
    self: ^Rec;
end;

initialization
    var uninit: ValidButImpossible;
    
    var rec: Rec := Rec(self: nil);
end
