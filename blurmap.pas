(* A tool for bluring raw terrain map *)
(* (C) 2004-2008 Dmytry Lavrov. *)

var f:file;
const size2=10;size=1 shl size2;
var
i,o:packed array[0..size*size-1] of smallint;

function mi(x,y:longint):longint;
begin
result:=x and (size-1)+(y and(size-1))shl size2;
end;
function kernel(x,y:longint):longint;
begin
result:=(i[mi(x-1,y)]+i[mi(x+1,y)]+i[mi(x,y-1)]+i[mi(x,y+1)]) div 4;
end;

var x,y:longint;
begin
if (paramstr(1)='')or(paramstr(2)='') then exit;
assign(f,paramstr(1));
reset(f,1);
blockread(f,i,sizeof(i));
close(f);
 for y:=0 to size-1 do begin
  for x:=0 to size-1 do begin
   o[mi(x,y)]:=kernel(x,y);
  end;
 end;
assign(f,paramstr(2));
rewrite(f,1);
blockwrite(f,o,sizeof(o));
close(f);
end.