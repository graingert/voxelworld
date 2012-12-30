{$apptype console}
type arr=array[0..maxlongint div sizeof(longint) -1] of longint;
parr=^arr;
var p:parr;
bigarr:packed array[1..90000000]of byte;
begin
bigarr[1]:=1;
getmem(p,2000);
p^[1]:=0;
readln;
freemem(p);
end.