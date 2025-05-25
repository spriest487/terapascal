implementation

type     
    MyClass = class
    private
        name: String;
        destructor Destroy;
    end;

destructor MyClass.Destroy;
begin
    WriteLn('destroying MyClass ' + self.name);
end;

initialization    
    var class1 := MyClass(name: 'Bob');
end.
