unit Packing;

interface

implementation

uses
    System;

type
    A = class
        member1, member2: UInt8;
        member3: UInt32;
        member4: UInt8;
    end;

    B = record
        member1, member2: UInt8;
        member3: UInt32;
        member4: UInt8;
    end;

    C = packed record
        member1, member2: UInt8;
        member3: UInt32;
        member4: UInt8;
    end;

procedure WriteOffset(name: String; base, member: Pointer);
begin
    var baseBytes := base as ^Byte;
    var memberBytes := member as ^Byte;
    var diff: NativeInt := memberBytes - baseBytes;

    WriteLn('offset of ' + name + ': ' + diff);
end;

initialization

unsafe begin
    var a := A(member1: 1; member2: 2; member3: 3; member4: 4);
    
    { the offsets for A's members should one class pointer and two i32s for reference counting }
    WriteOffset('a.member1', a as Pointer, @a.member1);
    WriteOffset('a.member2', a as Pointer, @a.member2);
    WriteOffset('a.member3', a as Pointer, @a.member3);
    WriteOffset('a.member4', a as Pointer, @a.member4);
    WriteLn('');

    var b := B(member1: 1; member2: 2; member3: 3; member4: 4);
    WriteOffset('b.member1', @b, @b.member1);
    WriteOffset('b.member2', @b, @b.member2);
    WriteOffset('b.member3', @b, @b.member3);
    WriteOffset('b.member4', @b, @b.member4);
    WriteLn('size of B: ' + sizeof(B));
    WriteLn('');

    var c := C(member1: 1; member2: 2; member3: 3; member4: 4);
    WriteOffset('c.member1', @c, @c.member1);
    WriteOffset('c.member2', @c, @c.member2);
    WriteOffset('c.member3', @c, @c.member3);
    WriteOffset('c.member4', @c, @c.member4);
    WriteLn('size of C: ' + sizeof(C));
    WriteLn('');
end;

end.
