interface
uses System;

function QuickSort[T](arr: array of T)
    where T is IComparable;

implementation

function QuickSortPartition[T](arr: array of T; left, right: Integer): Integer
where 
    T is IComparable;
begin
    var l := left;
    var r := right;

    var pivot := arr[l];
    var result := r;

    while true do begin
        while arr[l].Compare(pivot) < 0 do begin
            l += 1;
        end;
        while arr[r].Compare(pivot) > 0 do begin
            r -= 1;
        end;

        if l < r then begin
            if arr[l].Compare(arr[r]) = 0 then begin
                exit r;
            end;

            var temp := arr[l];
            arr[l] := arr[r];
            arr[r] := temp;
        end
        else begin
            exit r;
        end;
    end;

    result
end;

function QuickSortRange[T](arr: array of T; left, right: Integer)
where 
    T is IComparable;
begin
    if left < right then begin
        var pivot := arr.QuickSortPartition(left, right);

        if pivot > 1 then begin
            arr.QuickSortRange(left, pivot - 1);
        end;
        if pivot + 1 < right then begin
            arr.QuickSortRange(pivot + 1, right);
        end;
    end;
end;

function QuickSort[T](arr: array of T)
where 
    T is IComparable;
begin
    arr.QuickSortRange(0, arr.Length - 1);
end;

end
