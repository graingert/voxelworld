(*  Voxel World tool - image post-processing.       (C) 2004-2008 Dmytry Lavrov     *)
(*  http://dmytry.pandromeda.com                                                    *)

uses myerrors,sysutils;
type real=single;
procedure myfree(var a:pointer);
begin
if a<>nil then begin freemem(a);a:=nil; end;
end;
type array2Ddesc=record
  mx,my,xcount,ycount:longint;
  base:pointer;
end;
type tRGB24=packed record
r,g,b:byte;
end;
pRGB24=^tRGB24;

procedure getpixel(const image:array2Ddesc;x,y:longint;var color:tRGB24);
begin
{if image.base=nil then exit(0);}
if x>=image.xcount then x:=image.xcount-1 else
if x<0 then x:=0;
if y>=image.ycount then y:=image.ycount-1 else
if y<0 then y:=0;
color:=pRGB24(pointer(x*image.mx+y*image.my+longint(image.base)))^;
end;
const zeroRGB24:tRGB24=(r:0;g:0;b:0);

procedure getpixel0(const image:array2Ddesc;x,y:longint;var color:tRGB24);
begin
{if image.base=nil then exit(0);}
if (dword(x)>=image.xcount)or(dword(y)>=image.ycount)or(image.base=nil) then begin
color:=zerorgb24;
if image.base=nil then writeln('nil base!');
writeln('bug xcount=',image.xcount,' ycount=',image.ycount,' x=',x,' y=',y);
exit;
end;
color:=pRGB24(pointer(x*image.mx+y*image.my+longint(image.base)))^;
end;

procedure setpixel(const image:array2Ddesc;x,y:longint;color:tRGB24);
begin
{if image.base=nil then exit(0);}
if (x>=image.xcount)or(x<0)or(y>=image.ycount)or(y<0)or(image.base=nil) then exit;
pRGB24(pointer(x*image.mx+y*image.my+longint(image.base)))^:=color;
end;
procedure allocate(var image:array2Ddesc);
begin
myfree(image.base);
image.mx:=3;
image.my:=image.mx*image.xcount;
getmem(image.base, image.xcount*image.ycount*3 );
end;

{type tRGB=packed record
r,g,b:byte;
end;
pRGB24=^tRGB24;}

var inimage,outimage:array2Ddesc;
inf,outf:file;
var inwidth,inheight:longint;
function getPBMhead(var f:file;var width,height:longint):boolean;
var c:char;P6:packed array[1..2] of char;head:array[1..3] of string;
n,i:longint;
label skip;
begin
  blockread(f,P6,sizeof(P6));
  if (P6[1]<>'P') or (P6[2]<>'6') then begin
   error('BAD FILE FORMAT. Only P6 PBM images without comments in the header are supported.');
   exit(false);
  end;
  for i:=1 to 3 do head[i]:='';
  i:=1;
  c:=' ';
  repeat
     n:=0;
     if eof(f) then exit(false);
     skip:
     blockread(f,c,1);
     while c in [#10,#13,' ',';',','] do begin
      if eof(f) then exit(false);
      blockread(f,c,1);
      inc(n);
      if n>255 then exit(false);
     end;
     if c='#' then begin
       repeat
        if eof(f) then exit(false);
        blockread(f,c,1);
       until c in[#10,#13];
       if eof(f) then exit(false);
       goto skip;
     end;
     repeat
      head[i]:=head[i]+c;
      if eof(f) then break;
      blockread(f,c,1);
     until c in [#10,#13,' ',';',','];
    inc(i);
   until i>3;
   width:=strToInt(head[1]);
   height:=strToInt(head[2]);
   {third parameter are ignored.}
end;


var imgErr:longint;const imgErr_badhead=1;

procedure Load_P6_PBM(const name:string;var image:array2Ddesc);
var f:file;
begin
if imgerr<>0 then exit;
assign(f,name);
reset(f,1);
if not getPBMhead(f,image.xcount,image.ycount) then begin
imgErr:=imgErr_badhead;exit;
end;
writeln('Loading image: ',name,'  ',image.xcount,'x',image.ycount);
write('allocating memory');
allocate(image);
if image.base=nil then writeln('!!!!!!!??????????!!!!!!!!!!!!???????????');
writeln('.');
write('loading');
blockread(f,image.base^,image.xcount*image.ycount*3);
writeln('.');
close(f);
end;
procedure Save_P6_PBM(const name:string;image:array2Ddesc);
var head:string;f:file;
begin
write('Saving image: ',name,'  ',image.xcount,'x',image.ycount,' ');
head:='P6 '+IntToStr(image.xcount)+' '+IntToStr(image.ycount)+' 255'#10;
assign(f,name);
rewrite(f,1);
blockwrite(f,head[1],length(head));
blockwrite(f,image.base^,image.xcount*image.ycount*3);
close(f);
writeln('.');
end;
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
type tmyimghead=record
sign:array[0..3] of char;
xres,yres:word;
end;
procedure Save_myimg(const name:string;image:array2Ddesc);
const buffersize=1024;
var f:file;
//temp:pointer;
head:tmyimghead;
i,x,y:longint;
color:tRGB24;
begin
head.sign:='T001';
assign(f,name);
rewrite(f,1);
head.xres:=image.xcount;
head.yres:=image.ycount;
blockwrite(f,head,sizeof(head));
//getmem(temp,buffersize);
for i:=0 to image.xcount*image.xcount-1 do begin
myimgIndexToXY(i,x,y);
getpixel0(image,x,y,color);
blockwrite(f,color,sizeof(color));
end;
//freemem(temp);
close(f);
end;
procedure Load_myimg(const name:string;image:array2Ddesc);
var f:file;
begin
assign(f,name);

close(f);
end;

const convr=2;
var conv:array[-convR..convR,-convR..convR] of real;(*=(
{-2}(),
{-1}(),
 {0}(),
{+1}(),
{+2}()
)
*)
function rclamp(a:real):longint;
begin
result:=round(a);
if result<0 then result:=0;
if result>255 then result:=255;
end;
procedure process(src:array2Ddesc;var dest:array2Ddesc);
var x,y,xc,yc:longint;incolor,outcolor:tRGB24;r,g,b:real;
begin
if src.base=nil then exit;
myfree(dest.base);
dest.xcount:=src.xcount div 2;
dest.ycount:=src.ycount div 2;
allocate(dest);
 for y:=0 to dest.ycount-1 do begin
  for x:=0 to dest.xcount-1 do begin
   r:=0;
   g:=0;
   b:=0;
   for yc:=-convr to convr do begin
    for xc:=-convr to convr do begin
     getpixel(src,x*2+xc,y*2+yc,incolor);
     r:=r+conv[xc,yc]*incolor.r;
     g:=g+conv[xc,yc]*incolor.g;
     b:=b+conv[xc,yc]*incolor.b;
    end;
   end;
   outcolor.r:=rclamp(r);
   outcolor.g:=rclamp(g);
   outcolor.b:=rclamp(b);
   setpixel(dest,x,y,outcolor);
  end;
 end;
end;

var x,y,i:longint;
var sharpen:real;var offset,scale:real;
var f:file;
begin
 decimalseparator:='.';
 InitLUTs;
 {repeat
 readln(i);
 myimgIndexToXY(i,x,y);
 writeln('x=',x,' y=',y);
 until eof;}
 sharpen:=0.70;
 scale:=0;
 for y:=-convr to convr do begin
  for x:=-convr to convr do begin
  conv[x,y]:=1/(1+x*x+y*y);
  scale:=scale+conv[x,y];
  end;
 end;
 if sharpen>1 then sharpen:=1;
 offset:=scale*sharpen/sqr(convr*2+1);
 scale:=scale-offset*sqr(convr*2+1);
 scale:=1/scale;
 for y:=-convr to convr do begin
  for x:=-convr to convr do begin
  conv[x,y]:=(conv[x,y]-offset)*scale;
  write(format('%6.3f ',[conv[x,y]]) );
  end;
  writeln;
 end;
 {}

 writeln('Scaling color by ',scale:3:3);
 Load_P6_PBM(paramstr(1),inimage);
 if inimage.base=nil then begin writeln('no image loaded!');readln;exit; end;
 save_myimg(paramstr(2),inimage);
 {
 write('Processing');
 process(inimage,outimage);
 writeln('.');
 myfree(inimage.base);
 assign(f,'test.raw');
 rewrite(f,1);
 {write('Saving');
 blockwrite(f,outimage.base^,outimage.xcount*outimage.ycount*3);
 close(f);}
 Save_P6_PBM(paramstr(2),outimage);
 writeln('.');}
end.