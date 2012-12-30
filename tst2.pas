(*  Voxel World - looks like some test thing for testing cache-efficient map packing.   (C) 2004-2008 Dmytry Lavrov  *)
(*  http://dmytry.pandromeda.com                                                                                     *)

uses graphobj,objscr;
// index=
var myimgIndexToXY_LUT:packed array[0..255] of byte;
{y are storen in high four bits}
procedure InitLUTs;
var i,x,y:longint;
begin
for i:=0 to 255 do begin
{ yxyxyxyx }
{ yyyyxxxx }
myimgIndexToXY_LUT[i]:=
{x}
(i and (1      )) +
(i and (1 shl 2)) shr 1 +
(i and (1 shl 4)) shr 2 +
(i and (1 shl 6)) shr 3
{y}+
(i and (1 shl 1)) shl 3 +
(i and (1 shl 3)) shl 2 +
(i and (1 shl 5)) shl 1 +
(i and (1 shl 7)) shl 0
;
end;
end;
procedure myimgIndexToXY(i:longint;var x,y:longint);
var tmp1,tmp2,tmp3,tmp4:byte;
begin
tmp1:=myimgIndexToXY_LUT[i and 255];
tmp2:=myimgIndexToXY_LUT[(i shr 8) and 255];
tmp3:=myimgIndexToXY_LUT[(i shr 16) and 255];
tmp4:=myimgIndexToXY_LUT[(i shr 24) and 255];
x:=(tmp1 and 15)+(tmp2 and 15) shl 4+(tmp3 and 15) shl 8+(tmp4 and 15) shl 12;
y:=(tmp1 and $F0)shr 4+(tmp2 and $F0)+(tmp3 and $F0) shl 4+(tmp4 and $F0) shl 8;
end;

var x,y,i,j:longint;
begin
InitLUTs;
initscreen(1280,1024,8);
screen.curcolor:=colors[15];
for i:=0 to 1024*1024-1 do begin
myimgIndexToXY(i,x,y);
if (x>=1024)or(y>=1024) then begin writeln(i);readln;end;
screen.putpixel(x,y);
{readkey;}
end;
readln;

end.