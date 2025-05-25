implementation

type
    MyList = class;
    
    MyItem = class
        val: Integer;
    end;

    MySeq = class
        list: MyList;
        count: Integer;
        function Next: Option[MyItem];
    end;

    MyList = class
    public
        function Sequence: MySeq;
    end;

function MySeq.Next: Option[MyItem];
begin
    var next: Option[MyItem] := if self.count > 0 then 
    begin
        Option.Some((val: self.count))
    end
    else begin
        Option.None;
    end;

    self.count -= 1;
    
    next;
end;

function MyList.Sequence: MySeq;
begin
    MySeq(list: self; count: 2);
end;

initialization
    var list := MyList();
    
    for var i in list do 
    begin
        WriteLn('next item: ' + i.val);
    end;
end.
