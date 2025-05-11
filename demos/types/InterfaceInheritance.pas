unit InterfaceInheritance;

implementation

type
    IBase = interface
        function BaseMethod;
    end;
    
    IChild = interface of IBase
        function ChildMethod;
    end;
    
    IGrandchild = interface of IChild
        function GrandchildMethod;
    end;
    
    Impl = class of IGrandchild
        function BaseMethod;
        function ChildMethod;
        function GrandchildMethod;
    end;
    
function Impl.BaseMethod;
begin
    WriteLn('Impl.BaseMethod');
end;

function Impl.ChildMethod;
begin
    WriteLn('Impl.ChildMethod');
end;

function Impl.GrandchildMethod;
begin
    WriteLn('Impl.GrandchildMethod');
end;

initialization
    var instance := Impl();
    instance.BaseMethod;
    instance.ChildMethod;
    instance.GrandchildMethod;

    WriteLn('instance is IBase? ' + (if instance is IBase then true else false));
    WriteLn('instance is IChild? ' + (if instance is IChild then true else false));
    WriteLn('instance is IGrandchild? ' + (if instance is IGrandchild then true else false));

    var asBase: IBase := instance; 
    var asChild: IChild := instance; 
    var asGrandchild: IGrandchild := instance;
    
    asBase.BaseMethod;
    asChild.ChildMethod;
    asGrandchild.GrandchildMethod;
    
    asBase := asChild;
    asBase := asGrandchild;
end.
