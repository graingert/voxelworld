(*Some test*)
uses vw;
var x,y:longint;p:pint32;t:int32;z,dzdx,dzdy:real;
const
{width=2000;
height=2000;}
width  =2000;
height =2000;
_pbmhead ='P6 2000 2000 255'+#10;
pbmhead:string=_pbmhead;
var buff:packed array[0..width-1] of packed record r,g,b:byte; end;
f:file;
function geth(x,y:longint):longint;
begin
if (dword(x)< dword(landscapeelevation.xcount))and(dword(y)< dword(landscapeelevation.ycount)) then
result:=pint16(longint(landscapeelevation.base)+x*landscapeelevation.mx+y*landscapeelevation.my)^
else result:=0;
end;
const filesize=width*height*3+length(_pbmhead);
begin
 vw.init;
 writeln('setting render port');
 renderport(n_stdperspective,width,height);
 SetCameraPosition4(512,512,0, 0 );
 setFarDist(1000);
 writeln('rendering...');
 render;
 lock_rendered;
 writeln('storing image to disc,',filesize,' bytes total.(',filesize div 1024,'K)');
 if rendered.datatype<> n_uint32 then begin writeln('Internal bug!');exit; end;
 assign(f,'img1.pbm');
 rewrite(f,1);
 blockwrite(f,pbmhead[1],length(pbmhead));
 for y:=height-1 downto 0 do begin
   p:=pointer(longint(rendered.base)+y*rendered.my) ;
  for x:=0 to width-1 do begin
   t:=p^;
   {getLandscapePoint(x,y,z,dzdx,dzdy);}
   buff[x].r:=byte(t shr 16);
   buff[x].g:=byte(t shr 8);
   buff[x].b:=byte(t);
   inc(longint(p),rendered.mx);
  end;
  blockwrite(f,buff,sizeof(buff));
 end;
 close(f);
 unlock_rendered;
 vw.uninit;
 writeln('ok,exiting.');
end.
