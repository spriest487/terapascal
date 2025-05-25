program FuncInvoke;

function Greet(name: String): String;
begin
    'Hello, ' + name + '!';
end;

begin
    // TODO: usage required to avoid unused function being discarded
    WriteLn(Greet('Nobody'));
    
    var greet := FunctionInfo.Find('FuncInvoke.Greet').Get;
    
    unsafe begin
        var name := 'World';
        var msg := '';
        greet.Invoke([@name as Pointer], @msg);
        
        WriteLn(msg); 
    end;
end.
