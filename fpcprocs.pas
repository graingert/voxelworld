(*  Voxel World Renderer, some utils unit       (C) 2004-2008 Dmytry Lavrov     *)
(*  http://dmytry.pandromeda.com                                                *)
{$ifndef fpc}
unit fpcprocs;
interface
procedure FillDword(var dest;count,value:longint);
procedure FillByte(var dest;count:longint;value:byte);
implementation
procedure FillDword(var dest;count,value:longint);
var i,e:longint;type pv=^longint;
begin
i:=longint(@dest);
e:=i+count*4;
while i<e do begin
pv(i)^:=value;
inc(i,4);
end;
end;
procedure FillByte(var dest;count:longint;value:byte);
begin
fillchar(dest,count,value);
end;


end.
{$endif}