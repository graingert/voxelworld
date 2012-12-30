(* Error reporting unit for voxelworld *)
(* (C) 2002-2008 Dmytry Lavrov *)
unit myerrors;
interface
var errorcount,warningcount:longint;
procedure error(const s:string);
procedure warning(const s:string);
procedure showerrorcount;
procedure BeginFatals;
procedure EndFatals;
implementation
var fatal:longint;
procedure error(const s:string);
begin
inc(errorcount);
writeln('Error [No. ',errorcount,']  :',s);
if fatal>0 then begin
  Writeln('It was fatal error,exiting.');
  showerrorcount;
  halt(1);
 end;
end;
procedure warning(const s:string);
begin
inc(warningcount);
writeln('Warning [No. ',warningcount,']  :',s);
end;

procedure showerrorcount;
begin
writeln('Statistics:');
if   errorcount=0 then
write('no errors') else
write(errorcount,' errors');

if warningcount=0 then
write(',no warnings') else begin
write(',',warningcount,' warnings');
end;
if (errorcount>0)or(warningcount>0)then writeln(',it probably was incorrectly executed.')
else writeln;
end;
procedure BeginFatals;
begin
inc(fatal);
end;
procedure EndFatals;
begin
dec(fatal);
end;
begin
fatal:=0;
errorcount:=0;
warningcount:=0;
end.