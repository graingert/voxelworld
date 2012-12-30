(*  Voxel World Renderer. Some bugged version that looked cool.      (C) 2004 Dmytry Lavrov     *)
(*  http://dmytry.pandromeda.com                                                                     *)

{Use Pascal Pretty Printer, (C)2001 by Lavrov Dmyry}
{debug of application that uses this unit: levels}
{ debug parameters does not affect speed of rendering,only time of calling
various functions,except "render"}
{errors that are surely application bugs will cause run-time error 255}
{ for that kind of errors,exceptions is just only crap.Instead of writing error
recovery routines,focuss on debugging}
{$define debug3}{maximal debug}
{$define debug2}{moderate time-consuming debug}
{$define debug1}{basic not time-consuming debug}

(*
Commands required:
* setSeed;
* Init;
* uninit;
* setRenderPort;
* SetLandscapeHeighxels;
/ GenerateLandscape;
* Sun;
? UpdateShadowmaps;
? SetTexture
? UpdateTexture

/ Atmosphere;
/ LoadAtmopsphere;
/ SetClouds;

* setDiagonalFOV(a:real);
* setHorisontalFOV(a:real);
* setVerticalFOV(a:real);
* setCameraPosition4
* getLandscapePoint
* Render

Sample program:
setSeed(0);init;RenderPort(n_perspective,1024,768);SetLandscapeHeighxels(destination:SubArray2Ddesc;source:array2Ddesc);
All angles are in radians,not degrees,measured counterclockwise from y axis.
Coordinate system like on graph on the table: x to the right,z to the up,y to the front
where it's reasonable to have getXXX command,setXXX/getXXX syntax are used.
*)
(*search keys: dirt
unsafe_pointers
*)


unit vw;
{define texturesbilinear}
{$define useRDTSC}
{define trep}
{define useglobals}
{$IFDEF FPC}
{define useinline}
{$ENDIF}
{define preclean}
{define nofill}
{define landgrid1}
{define skygrids}
{$define inertialcam}
{$define advmovement}
{define scaner}
{define testtexture}
{(c) 2003,2004 by Dmytry Lavrov mail m31415@freemail.lt}
{}
{homepage http://dmytrylavrov.narod.ru}
{Voxel Demo}
{voxel texturing, 3dfog&3dlight demo}
(*
Hi all,it's my voxel-fog  3D light demo( it's demo of my realtime
raytracing algorithms( technologies sounds better or not?)).
I'm working on some optimisations  assembler version.
( on some de-optimisations for java version ;-)
if you like this algorithm,or if you want to use it ,ask me for lastest optimisa

*)
{$ifdef FPC}
{$asmmode intel}
{$undef DELPHI}
{$else}
{$define DELPHI}
{$endif}
interface

(*basic data types*)
Type
{$IFNDEF FPC}
{$I fpctypes.inc}
{$ENDIF}
real = double;
int8 = shortint;
uint8 = byte;
int16 = smallint;
uint16 = word;
int32=longint;
uint32=dword;
(*redefine pointer types*)
preal = ^real;
pint8 = ^int8;
puint8 = ^uint8;
pint16 = ^int16;
puint16 = ^uint16;
pint32 = ^int32;
puint32 = ^uint32;

landint = int16;{elevation data are stored in 16 bits}
plandint = ^landint;

type n_elemtypes=(n_nothing,n_uint8,n_int8,n_uint16,n_int16,n_uint32,n_int32,n_single,n_real);
{god idea about marking dependent text.}
{on change of n_elemtypes,search for search key: "change n_elemtypes"}

const n_landint=n_int16;
type array1Ddesc=record
  mx,xcount:longint;
  base:pointer;
  datatype:n_elemtypes;
end;
type array2Ddesc=record
  mx,my,xcount,ycount:longint;
  base:pointer;
  datatype:n_elemtypes;
end;
type SubArray2Ddesc=record
  x1,y1,mx,my,xcount,ycount:longint;
end;
function SubArrayToArray2D(const a:array2Ddesc;const s:SubArray2Ddesc):array2Ddesc;

type Tdwordarray=array[0..maxint div 4 -1] of dword;pdwordarray=^tdwordarray;

{const numfoglayers = 2;}
var LandscapeElevation:array2Ddesc;(* ! description is read only,but you could write to array itself *)
{var FogLayers:array[0..numfoglayers*2-1] of array2Ddesc;}(* ! same ! *)
var rendered:array2Ddesc;(* ! same ! *)
(**Note: if mx<>ycount*4 do search for key "change rendered" **)

type n_projections=(n_stdperspective,n_biconepanoram);
(*****************v 0.5 Procedures********************)
procedure setSeed(s:longint);
procedure init;
procedure uninit;
procedure Renderport(projection:n_projections;w,h:longint);
procedure SetLandscapeHeighxels(const destination:SubArray2Ddesc;const source:array2Ddesc;heightoffset,heightunit:real);
procedure getLandscapePoint(x,y:real;var z,dzdx,dzdy:real);
function landH(x,y:real):real;

{works ONLY for n_stdperspective,outherwise,ignored}
{may cause overflow;}
procedure setFOVDiagonal(a:real);
procedure setFOVHorisontal(a:real);
procedure setFOVVertical(a:real);

procedure setCameraPosition4(x,y,z,_a:real);
procedure setFarDist(fd:longint);
procedure Sun(d,h:real;r,g,b:real);

procedure SetFogColor(r,g,b:real);{0..1}
procedure SetFogTime(t:real);{in seconds}
procedure SetFogdtime(dt:real);
procedure SetInterlacing(n:longint);

procedure render;(* may work inproperly if not everything was configured *)
procedure lock_rendered;(*should be called before using "rendered"array *)
procedure unlock_rendered;(*should be called before using "render" function *)

(**
Misc
**)
procedure setTextureSizeParam(n:longint);

{procedure setBackground(b:array2Ddesc);}(*after calling setBackground,don't dispose array.*)
(**For on-the-fly visualisation**)
type column_update_proc=procedure(x1,x2:longint;userdata:longint);
(** Note:should draw columns x1<=x<x2  (!!!)**)
type rendered_update_proc=procedure(userdata:longint);
(** It's recommended to install both procedures,and rendered_update_proc
should do nothing if column_update_proc have been called between calls to
rendered_update_proc
**)

procedure SetColumnUpdateProc(callback:column_update_proc;userdata:longint);
procedure CallColumnUpdate(x1,x2:longint);{$ifdef useinline}{inline;}{$endif}
procedure resetColumnUpdateProc;

procedure SetRenderedUpdateProc(callback:rendered_update_proc;userdata:longint);
procedure CallRenderedUpdate;{$ifdef useinline}{inline;}{$endif}
procedure resetRenderedUpdateProc;
(*
Commands that are kept only for tests
*)
procedure writeTerragenRAW(xofs,yofs:longint);

(******************************************************)
{var dfLandscape,dfFogLayersData,dfSun,dfCamera,dfFOV,dfTexture,dfProjection:boolean;}
var initialized_settings:record
  Landscape,LandscapeShadows,BumpAmpitudeMap,FogLayersData,Sun,SunTables,Camera,FOV,Texture,TextureShadows,Projection:boolean;
end;

const eps=1E-6;
{$ifndef openi}implementation{$endif}
(**************************************************************)
(****************************General helpers*********************************)
function tan(a:real):real;assembler;
asm
  fld a
  fptan
  fstp st(0)
  //fwait
end{['st0','st1','st2','st3','st4','st5','st6','st7']};
function round64(a:extended):int64;assembler;{$ifdef useinline}inline;{$endif}
asm
        {PUSH    EAX}
        fld a
        lea esp,[esp-8]
        FISTP   qword ptr [ESP]
        FWAIT
        POP     EAX
        POP     EDX
end['eax','edx'];
function my_round(d:extended):longint;assembler;{$ifdef useinline}inline;{$endif}
asm
 sub esp,8
 fnstcw word ptr [ebp-4]
 fwait
 mov word ptr [ebp-8],$1373
 fldcw word ptr [ebp-8]
 fwait
 fld d
 fistp dword ptr [ebp-8]
 fclex
 mov eax,[ebp-8]
 fldcw [ebp-4]
end['EAX','ECX'];

function SubArrayToArray2D(const a:array2Ddesc;const s:SubArray2Ddesc):array2Ddesc;
begin
  if (s.x1+(s.xcount-1)*s.mx > a.xcount-1)or
  (s.y1+(s.ycount-1)*s.my > a.ycount-1)then begin runerror(255);end;
  result.xcount:=s.xcount;
  result.ycount:=s.ycount;
  result.base:=pointer(longint(a.base)+s.x1*a.mx+s.y1*a.my);
  result.mx:=a.mx*s.mx;
  result.my:=a.my*s.my;
  result.datatype:=a.datatype;
end;
(***************************** Specific stuff *******************************)
(** landscape,sky,textures **)
var rseed:longint;
const landres2=10;landres=1 shl landres2;
const textureres2=landres2;textureres=1 shl textureres2;
const numfoglayers=2;var land:array[0..landres*landres-1] of packed record h,lh:landint;fog:packed array[0..numfoglayers*2-1] of landint; c:byte;tba{texture bump amplitude}:byte;end;
var FTable{for assembler optimisation}: packed array[0..$FFFF] of byte;{ RGB }
texture: packed array[0..textureres*textureres-1] of byte;
texturebump: packed array[0..textureres*textureres-1] of int16;
texturederivative: packed array[0..textureres*textureres-1] of uint16;
const derivative_unit2 = 5;
derivative_unit = 1 shl derivative_unit2;{32}
var dotprod: packed array[uint16] of byte;
var FogTime,Fogdtime:real;
var FogColor:record r,g,b:longint;{shifted by 8} end;var Fog100color:uint32;


var snowlevel: longint;
snownoise,texturesize: byte;
angle,m: real;

const shift = 12{};
mult = 1 shl shift;
hmult = 1 shl (shift div 2);
js = mult div 4;
vjs = mult;

const v_unit2 = 6;
{to how many horisontal cells one vertical are equal.Should be less than mult}
v_unit = 1 shl v_unit2;


var width,height,Cscreendist:longint;{}
const
camDDZ = - mult div 32 ;

{/}

{transponed form,pt:=x*height+y}
{y grows from bottom}

const
mulZ = 1;
var lightX,lightZ,lightNorm: longint ;
var camH{=100},maxdist{=255},foge: longint;
var CamX,CamY,CamZ,camdx,camdy,camdz,fogX,fogY,dfx,dfy: longint{multiplied!!!!!!!!};

type tliarray=packed array[0..maxlongint div 4 -1] of longint;
pliarray=^tliarray;
var light:pliarray{array of longint} ;{packed array[0..height] of longint;}{also y from bottom!}
lightk: longint;
_lightk: byte;{foger}


Function max(a,b:longint): longint;{$ifdef useinline}inline;{$endif}
begin
  if a>b then result := a
  else result := b
end;

Function min(a,b:longint): longint;{$ifdef useinline}inline;{$endif}
begin
  if a>b then result := b
  else result := a;
end;
{function min(a,b:longint):longint;assembler;{$ifdef useinline}inline;{$endif}
asm
end;}

Function mi(x,y:longint): longint;{$ifdef useinline}inline;{$endif}
begin
  mi := (x and (landres-1))+(y and (landres-1))*landres;
end;
{$ifdef useRDTSC}
Function GetClocks: qword;{$ifdef useinline}inline;{$endif}
assembler;
asm
  rdtsc
end
{$IFDEF FPC}['EAX','EDX']{$ENDIF};
{$endif}
var startclock,endclock: qword;
{function fixed2int(x:longint):longint;assembler;
asm
mov eax,x
sar eax,shift
end;}
{function fixed2map(x:integer):byte;assembler;
asm
mov eax,x
shr eax,shift
end;}

Function point2map(x,y:longint): longint;{$ifdef useinline}inline;{$endif}
assembler;
asm
  mov eax,y
  shr eax,shift
  and eax,landres-1
  shl eax,landres2
  mov ebx,x
  shr ebx,shift
  and ebx,landres-1
  or eax,ebx
end
{$ifdef FPC}['ebx']{$endif};

{const sunlight=$FFE080;
const skylight=$202030;}

{var sunR:longint:=400;sunG:longint:=250;sunB:longint:=100;skyR:longint:=25;skyG}

var sunR,sunG,sunB,skyR,skyG,skyB: longint;

procedure PutPixel(x,y:longint;c:uint32);
begin

  if (uint32(x)<uint32(rendered.xcount))and(uint32(y)<uint32(rendered.ycount)) then
    puint32(longint(rendered.base)+x*rendered.mx+y*rendered.my)^:=c;

end;

const MaxShade=20*v_unit;
var ShadeTable:array[0..MaxShade] of longint;

Procedure InitTables;

var x,y,tmp,n: longint;{y=shader}
{var r,g,b:longint;}

begin
  for y:=0 to 255 do
  begin
    for x:=0 to 255 do
    begin
      tmp := 255-((255-x)*(255-y)) div 256;
      Ftable[x+y*256] := byte(tmp);
      {R:=x*(y*sunR+256*skyR) div (256*256);
      G:=x*(y*sunG+256*skyG) div (256*256);
      B:=x*(y*sunB+256*skyB) div (256*256);
      if r>255 then r:=255;
      if g>255 then g:=255;
      if b>255 then b:=255;
      LtableR[mi(x,y)]:=R;
      LtableG[mi(x,y)]:=G;
      LtableB[mi(x,y)]:=B;}

      {Ltable[mi(x,y)]:=(x*y) div 256 ;}
      {texture[mi(x,y)]:=;}
    end;
  end;
  for n:=0 to MaxShade do begin
    tmp:=round(65535*( exp(-n/(1*v_unit)) +0.2) );
    if tmp>$FFFF then tmp:=$FFFF;
    ShadeTable[n]:=tmp;

  end;
end;
{pixelcolor:=(texelcolor*(shadowk*suncolor+skycolor) )*(1-fogk)+fogk
pixelcolor:=texelcolor.x*k+c;
where k=(shadowk*suncolor+skycolor)*fogk;
c=1-fogk
}


{textured span drawing routine}
{for y1>= n <y2 }

Type rgb = record
  r,g,b: longint;
end;



Procedure GradVLine(x,y1,y2{coords  ,draw to y2-1}{},s1,s2{texture selector},tx1,ty1,tx2,
ty2{texel coords},dhdx1,dhdy1,dhdx2,dhdy2:longint;k1,k2:rgb;tba1,tba2:longint; lh1,lh2:longint);

var tmpv,{delta,}sel,deltasel,tx,deltatx,ty,deltaty,dhdx,dhdy,deltadhdx,deltadhdy,
tba,deltaTBA,y,limit,pt, lh,dlh:
longint;
var destp,destlim,lightp,

ti
{$ifdef texturesbilinear}
,ti1,v1,ti2,v2,ti3,v3,ti4,v4
{$endif}
:longint;
tn,tnx,tny:longint;tc:dword;
resultcolor:longint;
{bx=tx dx=ty cx=lightval si=temp pointer}

var kr,kg,kb,dkr,dkg,dkb,c: longint;
{color:=(c*(kr>>8))>>8}

begin
  //search key:unsafe_pointers
  {writeln('hi,y=',y1/256:5:5,' y2=',y2/256:5:5,' x=',x);
  if (width<>rendered.xcount)or(height<>rendered.ycount)then  writeln('wrong dimesnions');}
  if (x<rendered.xcount)and(x>=0) then
  begin
    if y1<0 then y1 := 0
    else if y1>(height-1)*256 then y1 := (height-1)*256;

    if y2<>y1 then
    begin
      {deltasel:=(s2-s1) div(y2-y1);}
      deltatx := ((tx2-tx1)*256)div(y2-y1);
      deltaty := ((ty2-ty1)*256)div(y2-y1);

      {deltatx:=1;
      deltaty:=1;}

      dkr := ((k2.r-k1.r)*256)div(y2-y1);
      dkg := ((k2.g-k1.g)*256)div(y2-y1);
      dkb := ((k2.b-k1.b)*256)div(y2-y1);
      deltadhdx := ((dhdx2-dhdx1)*256)div(y2-y1);
      deltadhdy := ((dhdy2-dhdy1)*256)div(y2-y1);

      dlh := ((lh2-lh1)*256)div(y2-y1);

      deltaTBA := ((TBA2-TBA1)*256)div(y2-y1);
    end
    else
    begin
      deltasel := 0;
      {delta:=0;}
      deltatx := 0;
      deltaty := 0;

    end;
    y1 := y1 div 256;
    y2 := y2 div 256;

    if y1>=height then writeln('y1>=height');
    //if y2>=height then writeln('y2>=height');
    if y1<0 then writeln('y1<0');
    if y2<0 then writeln('y2<0');

    y := y1;
    sel := s1;
    kr := k1.r;
    kg := k1.g;
    kb := k1.b;
    dhdx := dhdx1;
    dhdy := dhdy1;
    {c := (_lightk)*($010101);}

    if y2<0 then y2 := 0
    else if y2>=height then y2 := height-1;

    destp:=x*rendered.mx+longint(rendered.base);
    destlim:=(destp+y2*4);

    destp:=(destp+y1*4);


    (*
    tc:=$FFFFFFFF;
    puint32(destp)^:=tc;
    puint32(destlim)^:=tc;
    puint32((rendered.xcount-1)*rendered.mx+(rendered.ycount-1)*rendered.my+
    longint(rendered.base))^:=tc; *)

    lightp:=longint(light)+y1*4;
    tx:=tx1;
    ty:=ty1;
    tba:=tba1;
    lh:=lh1;
    repeat
     //Interesting,how damn bad it will look in assembber listings??
     //hmm.not THAT bad.
     lightk:=lightk+pint32(lightp)^;
     tc:=Fog100color;
     if lightk<=255 then
     begin
      _lightk:=lightk;
{$ifndef texturesbilinear}
      ti:=(tx {shl (textureres2-8)}shr (16-textureres2) )and((textureres-1)shl textureres2) +
      (ty shr {8}16)and (textureres-1);

      if lh<0 then tc:=texture[ti] else begin
       if lh<MaxShade*mult then tc:=(texture[ti]*shadetable[lh div mult])div $10000 else
        tc:=(texture[ti]*shadetable[maxShade])div $10000;
        {tc:=30;}
      end;
      {if lh < 100 then tc:=texture[ti] else tc:=40;}
      tn:=texturederivative[ti];
{$else}
      Texture is incomplete!

      ti1:=(tx {shl (textureres2-8)}shr (16-textureres2) )and((textureres-1)shl textureres2) +
      (ty shr {8}16)and (textureres-1);
      ti2:=(ti1+1)and (textureres*textureres-1);
      ti3:=(ti1+textureres)and (textureres*textureres-1);
      ti3:=(ti1+textureres+1)and (textureres*textureres-1);
      v1:=texture[ti1];
      v2:=texture[ti2];
      v3:=texture[ti3];
      v4:=texture[ti4];
      tc:=(v1 shl 8)+(v3-v1)*(tx shr 8)and($FFFF) +)
{$endif}

      {tnx:=((tn and 255)*tba)div 256;
      tny:= ( ( (tn shr 8) and 255) *(tba div mult)) and (255 shl 8);}

      tnx:=(int8(tn)*tba)div(mult*256)         + (dhdx) div ((mult*v_unit{*256})div derivative_unit){} ;
      tny:=(int8(tn shr 8)*tba)div(mult*256)   + (dhdy) div ((mult*v_unit{*256})div derivative_unit){} ;
      {tnx:=0;tny:=0;}
      tc:=(tc*dotprod[uint32(byte(tnx)+byte(tny)*256)]*(255-_lightk))shr 16;
      tc:=((tc*kr))and $FF0000 +
     ((tc*kg)shr 8)and $00FF00 +
     ((tc*kb)shr 16)
     +((lightk*FogColor.b)shr 16)and $FF +((lightk*FogColor.g)shr 8)and $FF00 +(lightk*FogColor.r)and $FF0000;
     end;
     puint32(pointer(destp) )^:=uint32(tc);
     inc(destp,4);
    if destp>=destlim then break;
     inc(lightp,4);
     inc(tx,deltatx);
     inc(ty,deltaty);
     inc(dhdx,deltadhdx);
     inc(dhdy,deltadhdy);
     inc(tba,deltatba);

     inc(lh,dlh);

     inc(kr,dkr);
     inc(kg,dkg);
     inc(kb,dkb);
     {resultcolor:=texture[];}
    until false;
  end{if x} else writeln('chezachert:X outside of range.');

end;

var phase,stopphase: longint;

var phases: longint{=3};

Function signum(a:longint): longint;
begin
  if a>0 then result := 1
  else if a<0 then result := -1
  else result := 0;
end;

{$ifndef useglobals}Procedure _Render;{$endif}
var sX,sY,{non-normalised}dx,dy,dz: longint{landscape};
{sy=screen Y coordinate of LAST drawed pixel - 1 (!)}
ix,iy: record
  x,y,d,dx,dy,dd: longint;
end;

span0,span1: record{span0 near to camera, span1 far}
  lx,ly,d, h,  lh  ,fh,c,s, sy,dhdx,dhdy  ,TBA: longint;
  hcalc_a: real;
  hcalc_b: longint;{screen_y= surface_y_multiplied*a+b}
  {interpolation:}
  n1,n2,k: longint;
  {rgbs}
  crgb: rgb;
  need_to_update: boolean;
end;

const spandwords = sizeof(span0)div 4;

var nearx: boolean;{true:x is nearest.False:y}

fogcalcdist: longint;{while d>fogcalcdist,increase fogcalcdist  fog density}
{used for accurate density eval }
{layer 1}
fog1lowH,fog1lowY{lower altitude},fog1highH,fog1highY{highter altitude},
{layer 2}fog2lowH,fog2lowY,fog2highH,fog2highY,
lighth,lighty,n,i,j: longint;
fogh: array[0..numfoglayers*2-1] of longint;
fogsy,lastfogh: longint;
{x_i_x =>=  x integer value; x_i_y =>= mean y value for integer x  }
ll,l1,l2,backgrX: longint;{for interpolation}
rc: real;

tx0,ty0,tx1,ty1: longint;{interpolated texture coordinates}

var screendist: longint;

Label loopexit1;
{function h2s(h,d:longint):longint;
begin
result:=height div 2 + trunc(((h-camZ)*m*screendist)/(d+1));
end; }

Procedure addlight(y,v:longint);
begin
  y := y div 256;
  if y>height then exit;
  if y<(sy div 256) then inc(lightK,v)
  else inc(light^[y],v);
  {$ifdef skygrids}
  screen.putpixel(sx,height-y,255);
  {if (y>=0)(y<height) then rendered[sx*width+y]:=255;}{$endif}
end;



const gridtol = 800;
(*procedure RenderSkyLine;{$ifdef useinline}inline;{$endif}
begin

end;*)
var rslDest:puint32;rslFP,rslLimit:pint32;


{$ifdef useglobals}Procedure _Render;{$endif}
Label lightdone;
Label rslLoop1;
begin
  // search key:dirt
  //phases:=1;phase:=0;
  phase := (phase+1)mod phases;

  if rendered.base=nil then begin
    writeln('no port set');
    exit;
  end;

  angle := frac(angle/pi/2)*pi*2;
  if angle<0 then angle := angle+2*pi;
  {screen.clear(0);}
  {if camz<land[point2map(camX,camY)].h*mult then camz:=land[point2map(camX,camY)]}
  {$ifdef useRDTSC}
  startclock := getclocks;
  {$endif}

  sx := phase;
  repeat{plane-tracing loop}
    dx := (mult*(sx-width div 2))div Cscreendist;
    screendist := my_round(sqrt(sqr(real(Cscreendist))+sqr(real(sx-width div 2)) ));
    dy := mult;
    n := trunc( dx*cos(angle)-dy*sin(angle) );
    dy :=  trunc( dx*sin(angle)+dy*cos(angle) );
    dx := n;
    with span1 do
    begin
      lx := camX;
      ly := camY;
      {preparing steps}
      {$Q-}
      if dx>gridtol then
      begin
        ix.dx := mult;
        ix.dy := (dy*mult) div dx;
        ix.dd := my_round(sqrt(sqr(mult)+sqr(real(ix.dy)) ))  ;
        ix.x := (lx+mult)and{fixed floor} Not(mult-1);
        ix.y := ly+(ix.dy*(ix.x-lx))div mult;
        ix.d := (ix.dd*(ix.x-lx))div mult;
      end
      else if dx< -gridtol then
      begin
        ix.dx := -mult;
        ix.dy := -(dy*mult) div dx;
        {why division by zero is possible here? FIXED.}
        ix.dd := my_round(sqrt(sqr(mult)+sqr(real(ix.dy)) ) ) ;
        ix.x := lx and Not(mult-1);
        ix.y := ly+(ix.dy*(lx-ix.x))div mult;
        ix.d := (ix.dd*(lx-ix.x))div mult;
      end
      else{dx=0}
      begin
        ix.d := mult*mult;
        ix.dd := mult*mult;
      end;{Newer use ix ,d="infinity"}


      if dy>gridtol then
      begin
        iy.dy := mult;
        iy.dx := (dx*mult) div dy;
        iy.dd := my_round(sqrt(sqr(mult)+sqr(real(iy.dx)) ))  ;
        iy.y := (ly+mult)and{fiyed trunc} Not(mult-1);
        iy.x := lx+(iy.dx*(iy.y-ly))div mult;
        iy.d := (iy.dd*(iy.y-ly))div mult;
      end
      else if dy< -gridtol then
      begin
        iy.dy := -mult;
        iy.dx := -(dx*mult) div dy;
        iy.dd := my_round(sqrt(sqr(mult)+sqr(real(iy.dx)) ) ) ;
        iy.y := ly and Not(mult-1);
        iy.x := lx+(iy.dx*(ly-iy.y))div mult;{WHY overflow here? FIXED.}
        iy.d := (iy.dd*(ly-iy.y))div mult;
      end
      else{dy=0}
      begin
        iy.d := mult*mult;
        iy.dd := mult*mult;
      end;{Newer use iy ,d="infinity"}
    end{with span1};
    {ix.d:=my_round(ix.d/(dist*m));}
    {ix.dd:=my_round(ix.d/(dist*m));}
    {iy.d:=my_round(iy.d/(dist*m));}
    {iy.dd:=my_round(iy.d/(dist*m));}
    {--}
    sY := 0;
    span1.sy := sy;
    span1.d := 0;
    span1.n1 := point2map(span1.lx,span1.ly);
    span1.n2 := span1.n1;
    span1.hcalc_a := (screendist*m*256 );
    span1.hcalc_b := my_round((height*256 div 2)/span1.hcalc_a  -camZ/m);


    span1.need_to_update := true;
    filldword(light^,height,0);
    lightk := 0;
    fogcalcdist := 0;
    repeat{Vertical tracing loop}

      {select nearest}
      {curspan:=not(curspan);}
      {move(span1,span0,sizeof(span0));}
      span0 := span1;


      with span1 do
      begin
        {selecting nearest,preparing interpolation,etc}
        if ix.d<iy.d then
        begin
          nearX := true;
          lx := ix.x;
          ly := ix.y;
          span1.d := ix.d;
          n1 := point2map(lx,ly);
          n2 := point2map(lx,ly+mult);
          k := ly and (mult-1);
        end
        else
        begin
          nearX := false;
          span1.lx := iy.x;
          span1.ly := iy.y;
          span1.d := iy.d;
          n1 := point2map(lx,ly);
          n2 := point2map(lx+mult,ly);
          k := lx and (mult-1);
        end{else if ix.d<iy.d};
        {$ifdef scaner}
        screen.curcolor := 255+256*(abs(land[span1.n1].h-camZ div mult)+256*(camz div
        mult-land[span1.n1].h));
        screen.putpixel((lx-camx) div (mult div 2)+width div 2,height div 2-(ly-camy)
        div (mult div 2));
        {$endif}
        {readkey;}
        span1.hcalc_a := (screendist*m*256)/((span1.d+100));
        span1.hcalc_b := my_round((height*256 div 2)/(span1.hcalc_a) -camZ/m);
        {later,sy:=my_round((h+hcalc_b)*hcalc_a); }
      end{with};
      {check distance}
      if span1.d and Not(mult-1) >= mult*80 then
      begin{lowering quality/2}
        if ix.dd>iy.dd then ix.d := mult*mult
        else iy.d := mult*mult;
      end;
      if span1.d>maxdist*mult then
      begin
        {vertical fog span}
        begin

          sy := sy div 256;{}
          if sy>=height then break;


          {backgrX := (sx-width div 2 - trunc(angle/(2*pi)*background.max.x));
          if backgrx<0 then backgrx := background.max.x-((-backgrx)mod background.max.x)
          else backgrx := backgrx mod background.max.x;}
          (*Render Vertical skyline,temporary variables rslXxxx  *)

          //search key:unsafe_pointers
          {$ifndef nofill}
          rslFP:=@(light^[sy]);
          rslDest:=puint32(longint(rendered.base)+sx*rendered.mx+sy*rendered.my);
          rslLimit:=@(light^[height-1]);
          repeat
            inc(lightK,(rslFP^));
            if lightK<255 then rslDest^:=((lightk*FogColor.b)shr 16)and $FF + ((lightk*FogColor.g)shr 8)and $FF00 + (lightk*FogColor.r)and $FF0000
            else rslDest^:=fog100color;
            inc(longint(rslFP),4);
            inc(longint(rslDest),rendered.my);
          until longint(rslFP)>longint(rslLimit);
          {$endif}
        end;
        break;
      end;

      (*interpolation,fog,etc*)
      with span1 do
      begin
        l1:=land[n1].h;
        l2:=land[n2].h;
        h:=(l2-l1)*k+l1*mult;
        sy:=my_round((h+hcalc_b)*hcalc_a);
      end;
      with span1 do{note for myself:added when i optimised my h2s}
      if span1.d>fogcalcdist then
      begin
        lighth:=land[n1].lh;
        (*for n:=0 to numfoglayers*2-1 do
        begin
        fogh[n]:=land[{n1}point2map(lx+fogx*3,ly+fogy*(n+3)) ].fog[n]{+(n)*v_unit*20};
        if fogh[n]<lighth then fogh[n]:=lighth;
        end;*)

        {fogh[0]:=}
        {fog1lowh}fogh[0]:={20*v_unit}land[{n1}point2map(lx+fogx,ly+fogy) ].fog[0];
        {fog1highh}fogh[1]:={10*v_unit}fogh[0]+land[{n1}point2map(lx-fogy,ly+fogx) ].fog[1];
        {fog2lowh}fogh[2]:={5*v_unit}fogh[1]+land[{n1}point2map(lx+fogy,ly-fogx) ].fog[2];
        {fog2highh}fogh[3]:={40*v_unit}fogh[0]+land[{n1}point2map(lx-fogy,ly-fogx) ].fog[3];
        fogh[0]:=fogh[0]+fogh[3];
        for n:=0 to numfoglayers*2-1 do if fogh[n]<lighth then fogh[n]:=lighth;




        n := 1+(span1.d-fogcalcdist)div mult;
        if fogh[1]>fogH[0] then begin
        fogsy := my_round((fogh[0]*mult+hcalc_b)*hcalc_a);
        addlight(fogsy,n);
        fogsy := my_round((fogh[1]*mult+hcalc_b)*hcalc_a);
        addlight(fogsy,-n);
        end;
        if fogh[3]>fogH[2] then begin
        fogsy := my_round((fogh[2]*mult+hcalc_b)*hcalc_a);
        addlight(fogsy,n);
        fogsy := my_round((fogh[3]*mult+hcalc_b)*hcalc_a);
        addlight(fogsy,-n);
        end;
        {j:=0;


        while j<numfoglayers*2-1 do
        begin
          if fogh[j]<fogh[j+1] then
          begin
            fogsy := my_round((fogh[j]*mult+hcalc_b)*hcalc_a);
            addlight(fogsy,n);
            fogsy := my_round((fogh[j+1]*mult+hcalc_b)*hcalc_a);
            addlight(fogsy,-n);
          end;
          inc(j,2);
        end;
        }

        lightdone:
        fogcalcdist := (span1.d+mult)and Not(mult-1);
      end{with span1 do if span1.d>fogcalcdist};

      {if intersection!}
      if span1.sy>=sy+256{ } then
      begin

        with span1 do
        begin
          l1 := (land[(n1+1)and(landres*landres-1)].h-land[(n1-1)and(landres*landres-1
          )].h)div 2;
          l2 := (land[(n2+1)and(landres*landres-1)].h-land[(n2-1)and(landres*landres-1
          )].h)div 2;
          dhdx := (l2-l1)*k+l1*mult;
          l1 := (land[(n1+landres)and(landres*landres-1)].h-land[(n1-landres)and(
          landres*landres-1)].h)div 2;
          l2 := (land[(n2+landres)and(landres*landres-1)].h-land[(n2-landres)and(
          landres*landres-1)].h)div 2;
          dhdy := (l2-l1)*k+l1*mult;

          tx1 := lx shl texturesize {shl(texturesize +8 - shift)};
          ty1 := ly shl texturesize {shl(texturesize +8 - shift)};
          l1 := land[n1].c;
          l2 := land[n2].c;
          c := (l2-l1)*k+l1*mult;

          l1:=land[n1].tba;
          l2:=land[n2].tba;
          tba := (l2-l1)*k+l1*mult;

          l1:=land[n1].lh;
          l2:=land[n2].lh;
          lh:=(l2-l1)*k+l1*mult-span1.h;

          {l1:=selector[n1];l2:=selector[n2];
          s:=(l2-l1)*k+l1*mult;}
          span1.need_to_update := false;
        end;
        with span0 do
        begin
          if need_to_update then
          begin
            l1 := (land[(n1+1)and(landres*landres-1)].h-land[(n1-1)and(landres*
            landres-1)].h)div 2;
            l2 := (land[(n2+1)and(landres*landres-1)].h-land[(n2-1)and(landres*
            landres-1)].h)div 2;
            dhdx := ((l2-l1)*k+l1*mult);
            l1 := (land[(n1+landres)and(landres*landres-1)].h-land[(n1-landres)and(
            landres*landres-1)].h)div 2;
            l2 := (land[(n2+landres)and(landres*landres-1)].h-land[(n2-landres)and(
            landres*landres-1)].h)div 2;
            dhdy := (l2-l1)*k+l1*mult;

            l1 := land[n1].h;
            l2 := land[n2].h;
            h := (l2-l1)*k+l1*mult;

            span0.sy := {h2s(h,span0.d)}my_round((h+hcalc_b)*hcalc_a);
            l1 := land[n1].c;
            l2 := land[n2].c;
            c := (l2-l1)*k+l1*mult;
            l1:=land[n1].tba;
            l2:=land[n2].tba;
            tba:=(l2-l1)*k+l1*mult;

            l1:=land[n1].lh;
            l2:=land[n2].lh;
            lh:=(l2-l1)*k+l1*mult-span0.h;

            {l1:=selector[n1];l2:=selector[n2];
            s:=(l2-l1)*k+l1*mult;}
          end;
          tx0 := lx shl texturesize ;
          ty0 := ly shl texturesize  ;
        end{with span0};
        {for n:=sy  div 256{max(span1.sy,0)} to min(span1.sy div 256 -1,height-1) do
        begin
          lightK := lightK+light[n];
        end;}
        if span0.sy<sy then
        begin{span to be drawn is overclosed}
          rc := (span0.sy-sy)/(span0.sy-span1.sy{if alg. correct,zero inpossible});
          span0.c := span0.c+my_round((span1.c-span0.c)*rc);
          span0.s := span0.s+my_round((span1.s-span0.s)*rc);
          tx0 := tx0+my_round((tx1-tx0)*rc);
          ty0 := ty0+my_round((ty1-ty0)*rc);
          span0.lh := span0.lh+my_round((span1.lh-span0.lh)*rc);
          span0.sy := sy;
        end;
        if lightK>255 then _lightk := 255
        else _lightk := lightk;

        with span0.crgb do
        begin
          {k=(shadowk*suncolor+skycolor)*(1-fogk)}
          n := span0.c div mult;
          R := (min(n*sunR+skyR*256,65535));
          G := (min(n*sunG+skyG*256,65535));
          B := (min(n*sunB+skyB*256,65535));
        end;
        with span1.crgb do
        begin
          n := span1.c div mult;
          R := (min(n*sunR+skyR*256,65535));
          G := (min(n*sunG+skyG*256,65535));
          B := (min(n*sunB+skyB*256,65535));
        end;
        {$ifndef nofill}
        gradvline(sx,sy,span1.sy,span0.s,span1.s,tx0,ty0,tx1,ty1,span0.dhdx,span0.dhdy,
        span1.dhdx,span1.dhdy,span0.crgb,span1.crgb,span0.tba,span1.tba,
        span0.lh,span1.lh);
        {$endif}
        {$ifdef landgrid1}
        rendered[sx*height+min(sy div 256 ,height-1)] := 255-sy  255;
        rendered[sx*height+min(sy div 256 +1,height-1)] := sy  255;{$endif}

        sy := span1.sy;
        if sy>=height*256 then break;
      end
      else span1.need_to_update := true; {end if};

      {step nearest}
      if nearX then
      begin
        with ix do
        begin
          x := x+dx;
          y := y+dy;
          d := d+dd;
        end;
      end
      else
      begin
        with iy do
        begin
          x := x+dx;
          y := y+dy;
          d := d+dd;
        end;
      end;

    until sy>=height*256;
    loopexit1:
    CallColumnUpdate(sx,sx+1);
    inc(sx,phases);
  until sx>=width{for sx:=...};
  {$ifdef useRDTSC}
  endclock := getclocks;
  writeln('Clocks per frame:',endclock-startclock,' per pixel:',(endclock-startclock) div(width*height div phases));
  {$endif}


end;

var sizecoeff: real{=0.5};

{var rseed,landscaperandseed: dword;}
{function random(n:longint):longint;
begin
if rseed=0 then rseed:=1;
rseed:=(rseed*16807)mod dword(1 shl 31 -1);
random:=(rseed-100)mod n;
end;}


var amplitudes: array[0..31] of real;

var tmp: packed array[0..textureres*textureres-1] of byte;

Procedure PrepareLand;

var
{destination array paramethers}towhat: pointer;
sizeb,mpxb,mpyb: longint;

var size: longint;

Function arrptr(x,y:longint): plandint;
begin
  arrptr := plandint(longint(towhat)+(x and (size-1))*mpxb+ (y and (size-1))*mpyb);
end;

Procedure DoMyNoise(size2:longint);

{$ifopt r+}
{$define r_on}
{$r-}
{$endif}

var _1pts,_1pts2,x,y,rz,rz2,n: longint;
begin
  size := 1 shl size2;
  if sizeB<mpxb*(size-1)+mpyb*(size-1)+1 then
  begin
    runerror(255);
    exit;
  end;{while i don't like C pointer operations,i will protect my code against crashes}

  begin{copied code from my noise example}
    _1pts := size;
    n := size2;
    while _1pts >= 2 do
    begin
      dec(n);
      _1pts2 := _1pts div 2;
      rz2 := my_round(amplitudes[n]*v_unit*_1pts);
      rz := rz2*2+1;
      {��������}
      {do squares}
      y := _1pts2;
      repeat
        x := _1pts2;
        repeat
          arrptr(x,y)^ :=
          int16(
          (arrptr(x-_1pts2,y-_1pts2)^+arrptr(x+_1pts2,y-
          _1pts2)^+arrptr(x-_1pts2,y+_1pts2)^+ arrptr(x+
          _1pts2,y+_1pts2)^)div 4 +random(rz)-rz2
          );
          x := x+_1pts;
        until x>=size;
        y := y+_1pts;
      until y>=size;
      rz2 := my_round(amplitudes[n]*v_unit*_1pts*(1/sqrt(2)));
      rz := rz2*2+1;
      y := 0;
      repeat
        x := 0;
        repeat
          arrptr(x,y)^ :=
          int16(
          (arrptr(x-_1pts2,y-_1pts2)^+arrptr(x+_1pts2,y-
          _1pts2)^+arrptr(x-_1pts2,y+_1pts2)^+ arrptr(x+
          _1pts2,y+_1pts2)^)div 4 +random(rz)-rz2
          )
          ;
          x := x+_1pts;
        until x>=size;
        y := y+_1pts;
      until y>=size;
      {���������� ��������}
      {� 3� ��� �������� , ��ଠ ��-��࠭������ ������...��⮬� � diamonds}
      {do "diamonds"(it's really of non-processed diamond shape(octahedr) when in 3d)}

      y := 0;
      repeat
        x := ((y+_1pts2)and  _1pts2);{(((y/_1pts2)+1) 1)*_1pts2}
        repeat
          arrptr(x,y)^ :=
          int16(
          (arrptr(x,y-_1pts2)^+arrptr(x-_1pts2,y)^+arrptr(x
          +_1pts2,y)^+ arrptr(x,y+_1pts2)^)div 4 +my_round((random(rz)-rz2) )
          );
          x := x+_1pts;

        until x>=size;
        y := y+_1pts2;
      until y>=size;


      y := 0;
      repeat
        x:=((y)and  _1pts2);
        repeat
          arrptr(x,y)^:=
          int16(
          (arrptr(x,y-_1pts2)^+arrptr(x-_1pts2,y)^+arrptr(x
          +_1pts2,y)^+ arrptr(x,y+_1pts2)^)div 4
          +my_round((random(rz)-rz2) )
          );
          x := x+_1pts;
        until x>=size;
        y:=y+_1pts2;
      until y>=size;
      {pts:=pts*2;}
      _1pts := _1pts2;
    end;

  end;
end;
{$ifdef r_on}{$r+}{$endif}

Procedure noiselandscape;
begin
  towhat := @land[0].h;
  sizeb := sizeof(land);
  mpxb := sizeof(land[0]);
  mpyb := mpxb*landres;
  DoMyNoise(landres2);
end;

Procedure noisetexturemap;

var n: longint;
begin
  towhat := @texturebump;
  sizeb := sizeof(texturebump);
  mpxb := sizeof(texturebump[0]);
  mpyb := mpxb*textureres;
  DoMyNoise(textureres2);
  for n:=0 to textureres*textureres-1 do
  begin
    texture[n] := byte({127+}255-abs( texturebump[n] div v_unit));
  end;
end;

Procedure noisetexturebump;
begin
  towhat := @texturebump;
  sizeb := sizeof(texturebump);
  mpxb := sizeof(texturebump[0]);
  mpyb := mpxb*textureres;
  DoMyNoise(textureres2);
end;

Procedure noisefog(n:longint);
begin
  towhat := @land[0].fog[n];
  sizeb := sizeof(land);
  mpxb := sizeof(land[0]);
  mpyb := mpxb*landres;
  DoMyNoise(landres2);
end;

Procedure shadelandscape;

var x,y,z,tz,c,nx,ny,dp: longint;
begin
  for y:=0 to landres-1 do
  begin
    z := -10000;
    for x:=0 to landres*2-1 do
    begin
      tz := (land[mi(x,y)].h*lightx) div v_unit;

      {����᫨�� ᪠��୮� �ந�������� �����஢(�ᢥ饭�����)}
      {calculate dot-product of normalised vectors. Equation was simplified}
      nx:=(land[mi(x-1,y)].h-land[mi(x+1,y)].h) div 2;
      ny:=(land[mi(x,y-1)].h-land[mi(x,y+1)].h) div 2;
      dp:=-my_round(((nx*lightX+v_unit{z normal}*lightz)*255)/(lightnorm*sqrt(nx*nx+
      ny*ny+v_unit*v_unit )));
      if dp<0 then dp := -dp div 2;
      if tz>z then
      begin{� ⥭� ? Shadow test}
        land[mi(x,y)].c := 255;
        z := tz;
        if x>landres+1 then break;
        {���,��� ��ન ��⥭�,����᪮� �த���� All hills done in this span}
      end
      else land[mi(x,y)].c := {70}255{40};{� ⥭� ���� ������� ᢥ��}
       land[mi(x,y)].lh := (z*v_unit) div lightx;
       z := z+lightZ;
    end;
  end;
end;

Procedure shadetexture;

var x,y,z,tz,c,nx,ny,dp: longint;
begin
  for y:=0 to textureres-1 do
  begin
    z := -10000;
    for x:=0 to textureres*2-1 do
    begin
      tz := texturebump[mi(x,y)] div 2+{1/2}
      (texturebump[mi(x,y-1)]+texturebump[mi(x,y+1)]+texturebump[mi(x-1,y)]+
      texturebump[mi(x+1,y)]) div 12{1/3} +
      (texturebump[mi(x-1,y-1)]+texturebump[mi(x-1,y+1)]+texturebump[mi(x+1,y-1)
      ]+texturebump[mi(x+1,y-1)]) div 24;;

      tz := tz*(lightX div v_unit);

      {����᫨�� ᪠��୮� �ந�������� �����஢(�ᢥ饭�����)}
      {calculate dot-product of normalised vectors. Equation was simplified}
      {texturebump[mi(x,y)].lh:=z;}
      nx := (texturebump[mi(x-1,y)]-texturebump[mi(x+1,y)]) div 2;
      ny := (texturebump[mi(x,y-1)]-texturebump[mi(x,y+1)]) div 2;
      dp := -((nx*lightX+v_unit{z normal}*lightz)*255)div my_round(lightnorm*sqrt(nx*nx+
      ny*ny+v_unit*v_unit ));
      if dp<50 then dp := 50-dp;

      if tz>z then
      begin{� ⥭� ? Shadow test}

        z := tz;
        if x>textureres then break;
        {���,��� ��ન ��⥭�,����᪮� �த���� All hills done in this span}
      end
      else dp := dp div 3+20;{� ⥭� ���� ������� ᢥ��}
      if dp<0 then dp := 0
      else
      if dp>255 then dp := 255;
      tmp[mi(x,y)] := (dp*texture[mi(x,y)])div 256;
      z := z+lightZ;
    end;
  end;
  for y:=0 to textureres-1 do
  begin
    for x:=0 to textureres-1 do
    begin
      texture[mi(x,y)] := tmp[mi(x,y)] div 2+{1/2}
      (tmp[mi(x,y-1)]+tmp[mi(x,y+1)]+tmp[mi(x-1,y)]+tmp[mi(x+1,y)]
      ) div 12{1/3} +
      (tmp[mi(x-1,y-1)]+tmp[mi(x-1,y+1)]+tmp[mi(x+1,y-1)]+tmp[mi(x
      +1,y-1)]) div 24;

    end;
  end;
end;
procedure makeTBA;
var x,y:longint;v:real;
function h(x,y:longint):longint;{$ifdef useinline}inline;{$endif}
begin
 result:=land[mi(x,y)].h;
end;
begin
 for y:=0 to textureres-1 do begin
  for x:=0 to textureres-1 do begin
    v:=
    (1/v_unit)*
    sqrt(
    sqr( real( h(x,y)-0.25*( h(x-1,y-1)+h(x+1,y-1)+h(x-1,y+1)+h(x+1,y+1) ) ) )+
    sqr( real( h(x,y)-0.25*( h(x-1,y-1)+h(x+1,y-1)+h(x-1,y+1)+h(x+1,y+1) ) ) )
    );
    v:=v*{1000}{2000}1000;
    if v>255 then v:=255;
    {land[mi(x,y)].tba}tmp[mi(x,y)]:=my_round(v){255};
  end;
 end;
 for y:=0 to textureres-1 do begin
  for x:=0 to textureres-1 do begin
    land[mi(x,y)].tba:=tmp[mi(x,y)] div 2+{1/2}
      (tmp[mi(x,y-1)]+tmp[mi(x,y+1)]+tmp[mi(x-1,y)]+tmp[mi(x+1,y)]
      ) div 12{1/3} +
      (tmp[mi(x-1,y-1)]+tmp[mi(x-1,y+1)]+tmp[mi(x+1,y-1)]+tmp[mi(x
      +1,y-1)]) div 24;
  end;
 end;
end;


var n,x,y,z,dx,dy: longint;
begin
  RandSeed:=rseed;
  if not initialized_settings.sun then begin
   initialized_settings.sun:=true;
   initialized_settings.SunTables:=false;
   initialized_settings.LandscapeShadows:=false;
   initialized_settings.TextureShadows:=false;
  end;
  lightnorm := my_round(sqrt(sqr(lightX)+sqr(lightZ)));
  {landelevation:=1000;}
  (***fillchar(land,sizeof(land),0);
  fillchar(texture,sizeof(texture),0);
  fillchar(texturebump,sizeof(texturebump),0);***)
  for n:=0 to 31 do
  amplitudes[n] := 0;
  {for n:=0 to 6 do amplitudes[n]:=1;}
  if not initialized_settings.landscape then begin
    amplitudes[0] := 0.15;
    amplitudes[1] := 0.5;
    amplitudes[2] := 0.5;
    amplitudes[3] := 0.5;
    amplitudes[4] := 0.5;
    amplitudes[5] := 0.5;
    amplitudes[6] := 0.5;
    {amplitudes[5]:=1;}
    writeln('generating landscape');
    noiselandscape;
    {
    for y:=0 to landres-1 do begin
     for x:=0 to landres-1 do begin
      // canyons
      // land[mi(x,y)].h:=my_round(land[mi(x,y)].h/sqrt(1+(1/(v_unit*v_unit*1))*sqr(land[mi(x,y)].h))){max(land[mi(x,y)].h,0)};
      //       x*(1+x/sqrt(x*x+1))
       z:=land[mi(x,y)].h;
       land[mi(x,y)].h:=my_round({z}10*v_unit*(1+z/sqrt(z*z+100*v_unit*v_unit)));
     end;
    end;}
    initialized_settings.landscape:=true;
    initialized_settings.landscapeShadows:=false;
    initialized_settings.BumpAmpitudeMap:=false;
  end;



  //readln;
  if not initialized_settings.LandscapeShadows then begin
    writeln('shading landscape');
    shadelandscape;
    initialized_settings.LandscapeShadows:=true;
  end;


  if not initialized_settings.BumpAmpitudeMap then begin

   writeln('Generating surface bumpness map');
   makeTBA;

    for y:=0 to landres-1 do begin
     for x:=0 to landres-1 do begin
      putpixel(x,y,land[mi(x,y)].tba*$010101);
     end;
    end;

    for x:=0 to rendered.xcount-1 do CallColumnUpdate(x,x+1);

    {readln;}

   initialized_settings.BumpAmpitudeMap:=true;
  end;

  //readln;
  (***)
  if not initialized_settings.FogLayersData then begin
    write('preparing clouds simulator...');
    amplitudes[0] := 0.5;
    amplitudes[1] := 0.5;
    amplitudes[2] := 0.75;
    amplitudes[3] := 0.5;
    amplitudes[4] := 0.5;
    amplitudes[5] := 0.7;
    amplitudes[6] := 0.7;
    amplitudes[7] := 0;
    amplitudes[8] := 0;
    noisefog(0);
    write('3');
    amplitudes[0] := 0.4;
    amplitudes[1] := 0.5;
    amplitudes[2] := 0.6;
    amplitudes[3] := 0.5;
    amplitudes[4] := 0.1;
    amplitudes[5] := 0.1;
    amplitudes[6] := 0.1;
    amplitudes[7] := 0;
    amplitudes[8] := 0;
    noisefog(1);
    write('2');
    amplitudes[0] := 0.7;
    amplitudes[1] := 0.7;
    amplitudes[2] := 0.7;
    amplitudes[3] := 0.7;
    amplitudes[4] := 0.6;
    amplitudes[5] := 0.5;
    amplitudes[6] := 0.4;
    amplitudes[7] := 0.3;
    amplitudes[8] := 0.2;
    noisefog(2);
    write('1');
    amplitudes[0] := 0.7;
    amplitudes[1] := 0.7;
    amplitudes[2] := 0.7;
    amplitudes[3] := 0.5;
    amplitudes[4] := 0.4;
    amplitudes[5] := 0.1;
    amplitudes[6] := 0.2;
    amplitudes[7] := 0.1;
    amplitudes[8] := 0.1;
    noisefog(3);
    write('0');  (***)
    initialized_settings.FogLayersData:=true;
  end;


  {for n:=0 to numfoglayers*2-1 do
  begin
  noisefog(n);
  write('.');
  end;}


  writeln;
  if not initialized_settings.SunTables then begin
    initialized_settings.TextureShadows:=false;

    for y:=-128 to 127 do
    begin
      for x:=-128 to 127 do
      begin
        dotprod[x and 255+(y and 255)*256] := min(255,max({4}30,-((-x*lightX+
        derivative_unit*lightZ)*255)div my_round(
        lightnorm*sqrt(x*x+y*y+derivative_unit*
        derivative_unit))));
      end;
    end;
    initialized_settings.SunTables:=true;
  end;

  if not initialized_settings.Texture then
  begin
    initialized_settings.TextureShadows:=false;
    writeln('generating texture');
    amplitudes[0] := 4;
    amplitudes[1] := 4;
    amplitudes[2] := 4;
    amplitudes[3] := 3;
    amplitudes[4] := 2;
    amplitudes[5] := 1;
    amplitudes[6] := 0;
    amplitudes[7] := 0;
    amplitudes[8] := 0;
    amplitudes[9] := 0;
    noisetexturemap;
    //should be called before noisetexturebump because uses texturebump as temp
    amplitudes[0] := 0.4;
    amplitudes[1] := 0.5;
    amplitudes[2] := 0.5;
    amplitudes[3] := 0.5;
    amplitudes[4] := 0.5;
    amplitudes[5] := 0.5;
    amplitudes[6] := 0.5;
    noisetexturebump;
    {   for y:=0 to textureres-1 do begin
    for x:=0 to textureres-1 do begin
    texture[mi(x,y)]:=my_round(texture[mi(x,y)]*(  sqr(texturebump[mi(x+1,y)]-texturebu
    end;
    end;
    }
    {$ifdef testtexture}
    {x arrow}
    for y:=10 to 16 do
    for x:=10 to textureres-10-abs(y-13) do
    begin
      texture[mi(x,y)] := 128;
      inc(texturebump[mi(x,y)],v_unit div 2);
    end;
    {x mark}
    for x:=0 to 3 do
    for y:=0 to 15 do
    begin
      texture[mi(x+textureres-55+y,y+20)] := 128;
      inc(texturebump[mi(x+textureres-55+y,y+20)],v_unit div 2);
      texture[mi(x+textureres-40-y,y+20)] := 128;
      inc(texturebump[mi(x+textureres-40-y,y+20)],v_unit div 2);
    end;

    {y arrow}
    for x:=10 to 16 do
    for y:=10 to textureres-10-abs(x-13) do
    begin
      texture[mi(x,y)] := 128;
      inc(texturebump[mi(x,y)],v_unit div 2);
    end;
    {y mark}
    for x:=0 to 3 do
    for y:=0 to 7 do
    begin
      texture[mi(x+28,y+textureres-55)] := 128;
      inc(texturebump[mi(x+28,y+textureres-55)],v_unit div 2);
      texture[mi(x+28+y,y+textureres-48)] := 128;
      inc(texturebump[mi(x+28+y,y+textureres-48)],v_unit div 2);
      texture[mi(x+28-y,y+textureres-48)] := 128;
      inc(texturebump[mi(x+28-y,y+textureres-48)],v_unit div 2);
    end;
    {$endif}
    //readln;
    for y:=0 to textureres-1 do
    begin
      for x:=0 to textureres-1 do
      begin
        {screen.putpixel(x,y,texture[mi(x,y)]*$010101);}
        dx := (texturebump[mi(x+1,y)]-texturebump[mi(x-1,y)])*derivative_unit div (2*
        v_unit);
        dy := (texturebump[mi(x,y+1)]-texturebump[mi(x,y-1)])*derivative_unit div (2*
        v_unit);
        texturederivative[x+y*textureres] := dx and 255 +(dy and 255)*256;
        texture[x+y*textureres] :={255-} my_round(texture[x+y*textureres]*  (derivative_unit/sqrt
        (dx*dx+dy*dy+derivative_unit*derivative_unit) )   );
      end;
    end;
    Initialized_settings.Texture:=true;
  end{};
end;



Label norender;

var s_phases: longint;

Procedure StoreParams(const fn:String);

var f: text;
begin
  (*
  assign(f,fn);
  rewrite(f);
  Writeln(f,' { Include file generated by voxel world }');
  Writeln(f,' { Pre-draw world generator params} ');
  writeln(f,'snowlevel:=',snowlevel,';');
  writeln(f,'randseed:=',landscaperandseed,';');
  writeln(f,'sizecoeff:=',sizecoeff,';');
  Writeln(f,' {camera params} ');
  writeln(f,'phases:=',phases,';');
  writeln(f,'camX:=',camX,';');
  writeln(f,'camY:=',camY,';');
  writeln(f,'camZ:=',camZ,';');
  writeln(f,'camH:=',camH,';');
  writeln(f,'a:=',angle,';');
  writeln(f,'m:=',m,';');
  writeln(f,'camDZ:=',camDZ,';');
  writeln(f,'camgravity:=',camgravity,';');
  writeln(f,'caminertia:=',caminertia,';');
  writeln(f,'maxdist:=',maxdist,';');
  Writeln(f,' {draw-time generator params} ');
  writeln(f,'fogX:=',fogX,';');
  writeln(f,'dfX:=',dfX,';');
  writeln(f,'fogY:=',fogY,';');
  writeln(f,'dfY:=',dfY,';');
  writeln(f,'foge:=',foge,';');
  writeln(f,'snownoise:=',snownoise,';');
  writeln(f,'texturesize:=',texturesize,';');
  writeln(f,'sunR:=',sunR,';');
  writeln(f,'sunG:=',sunG,';');
  writeln(f,'sunB:=',sunB,';');
  writeln(f,'skyR:=',skyR,';');
  writeln(f,'skyG:=',skyG,';');
  writeln(f,'skyB:=',skyB,';');
  close(f);
  *)
end;
procedure writeTerragenRAW(xofs,yofs:longint);
var x,y,t:longint;
f:file;
b:packed array[0..256] of byte;
begin
  assign(f,'terra.raw');
  rewrite(f,1);
  for y:=0 to 256 do begin
    for x:=0 to 256 do begin
      t:=127+(land[mi(  ((x*4+xofs)),((y*4+yofs))    )].h) div v_unit ;
      if t>255 then t:=255 else if t<0 then t:=0;
      b[x]:=t;
    end;
    blockwrite(f,b,257);
  end;
  close(f);
end;
Procedure InitParams;
begin
  { Include file generated by voxel world }
  { Pre-draw world generator params}
  snowlevel := 25600;
  randseed := 0;
  sizecoeff := 5.000000000000000E-001;
  {camera params}
  phases := 1;
  camX := {-3357287}512*mult;
  camY := {1781669}512*mult;
  camZ := -0;
  camH := 6*mult;
  angle := {9.102266384984939E-002}-45*pi/180;
  m := 1/v_unit;
  maxdist := 255;
  {draw-time generator params}
  fogX := -24282951;
  dfX := -341;
  fogY := 72920064;
  dfY := 1024;
  foge := 208;
  snownoise := 4;
  texturesize := 10;
  sunR := 250;
  sunG := 150;
  sunB := 50;
  skyR := 10;
  skyG := 20;
  skyB := 40;

   fogcolor.r:=255*256;
   fogcolor.g:=200*256;
   fogcolor.b:=150*256;

  {end from include}
  lightX := 4*v_unit;
  lightZ := -1*v_unit;
  lightnorm := my_round(sqrt(sqr(lightX)+sqr(lightZ)));
end;
(******************************)
(*****************v 0.5 Procedures********************)
procedure setSeed(s:longint);
begin
  writeln('vw.setseed ',s,' called');
  rseed:=s;
end;
procedure init;
begin
  writeln('vw.init called');
  if Rendered.base<>nil then begin
    freemem(rendered.base);fillchar(rendered,sizeof(rendered),0);
    freemem(light);light:=nil;
  end;
  rendered.base:=nil;
  InitParams;
  InitTables;
  fillbyte(rendered,sizeof(rendered),0);
  fillbyte(initialized_settings,sizeof(initialized_settings),0);
  LandscapeElevation.base:=@(land[0].h);
  LandscapeElevation.mx:=sizeof(land[0]);
  LandscapeElevation.my:=sizeof(land[0])*landres;
  LandscapeElevation.xcount:=landres;
  LandscapeElevation.ycount:=landres;

end;
procedure uninit;
begin
  writeln('vw.uninit called');
  if Rendered.base<>nil then begin
    freemem(rendered.base);fillchar(rendered,sizeof(rendered),0);
    freemem(light);
  end;
end;

procedure renderport(projection:n_projections;w,h:longint);
begin
  writeln('vw.renderport w=',w,' h=',h,' called');
  if (w<1)or(h<1)then exit;
  case projection of
    n_biconepanoram:runerror(255);(*unsupported*)
    n_stdperspective:begin
      if rendered.base<>nil then freemem(rendered.base);
      rendered.base:=nil;
      if light<>nil then freemem(light);
      light:=nil;
      getmem(light,h*4);(****)

      GetMem(rendered.base,w*h*4);
      filldword(rendered.base^,w*h,0);
      rendered.datatype:=n_uint32;
      rendered.xcount:=w;
      rendered.ycount:=h;
      rendered.my:=4;
      rendered.mx:=rendered.ycount*rendered.my;

      width:=w;
      height:=h;
    end;
  end;
end;
var sourcepointer,destinationpointer:longint;
var _heightoffset,_heightunit:real;

procedure do_uint8;begin pint16(destinationpointer)^:=my_round(( puint8(sourcepointer)^ +_heightoffset)*_heightunit);end;
procedure do_int8;begin pint16(destinationpointer)^:=my_round(( pint8(sourcepointer)^ +  _heightoffset)*_heightunit);end;
procedure do_uint16;begin pint16(destinationpointer)^:=my_round((puint16(sourcepointer)^+_heightoffset)*_heightunit);end;
procedure do_int16;begin pint16(destinationpointer)^:=my_round((pint16(sourcepointer)^+  _heightoffset)*_heightunit);end;
procedure do_uint32;begin pint16(destinationpointer)^:=my_round((puint32(sourcepointer)^+_heightoffset)*_heightunit);end;
procedure do_int32;begin pint16(destinationpointer)^:=my_round((pint32(sourcepointer)^+  _heightoffset)*_heightunit);end;
procedure do_single;begin pint16(destinationpointer)^:=my_round((psingle(sourcepointer)^+_heightoffset)*_heightunit);end;
procedure do_real;begin pint16(destinationpointer)^:=my_round((preal(sourcepointer)^+    _heightoffset)*_heightunit);end;
type proc=procedure;
var dopoint:proc;

procedure setLandscapeHeighxels(const destination:SubArray2Ddesc;const source:array2Ddesc;heightoffset,heightunit:real);
(** Danger,unsafe pointer operations inside **)
var dest:array2Ddesc;x,y:longint;
{had to unroll macro for compatibility}

(** Search key: "change n_elemtypes" **)

var i_limit:longint;
begin
  {$ifdef trep}
  writeln('vw.setLandscapeHeighxels  called');
  {$endif}
  initialized_settings.landscape:=true;
  initialized_settings.landscapeshadows:=false;
  initialized_settings.BumpAmpitudeMap:=false;
  {first,check(even if nothing to do)}
  {$ifdef debug1}
  if (destination.xcount<>source.xcount)or(destination.xcount<>source.xcount) then runerror(255);
  {$endif}
  heightunit:=heightunit*v_unit;
  (** Search key: "change n_elemtypes" **)
  case source.datatype of
    n_nothing:exit;n_uint8:dopoint:=proc(@do_uint8);n_int8:dopoint:=proc(@do_int8);
    n_uint16:dopoint:=proc(@do_uint16);n_int16:dopoint:=proc(@do_int16);n_uint32:dopoint:=proc(@do_uint32);
    n_int32:dopoint:=proc(@do_int32);n_single:dopoint:=proc(@do_single);n_real:dopoint:=proc(@do_real);
    else runerror(255);
  end;
  _heightoffset:=heightoffset;
  _heightunit:=heightunit;
  dest:=SubArrayToArray2D(LandscapeElevation,destination);

  for y:=0 to dest.ycount-1 do begin
    sourcepointer:=longint(source.base)+y*source.my;
    i_limit:=sourcepointer+(source.xcount)*source.mx;
    destinationpointer:=longint(dest.base)+y*dest.my;
    while sourcepointer<i_limit do begin
      dopoint;
      sourcepointer:=sourcepointer+source.mx;
      destinationpointer:=destinationpointer+dest.mx;
    end;
  end;

end;
procedure getLandscapePoint(x,y:real;var z,dzdx,dzdy:real);
var a,b,c,d,e,f: real;_x,_y:longint;
begin
  _x:=my_round(x*mult);
  _y:=my_round(y*mult);
  a := land[point2map(_X,_Y)].h ;
  b := land[point2map(_X,_Y+mult)].h ;
  c := land[point2map(_X+mult,_Y)].h ;
  d := land[point2map(_X+mult,_Y+mult)].h ;

  e:=(1/(mult*v_unit))* ( a*mult+ (b-a)*(_Y and(mult-1)) );
  f:=(1/(mult*v_unit))* ( c*mult+ (d-c)*(_Y and(mult-1)) );

  z:=e+  (f-e)*(_X and(mult-1))*(1/mult);

  dzdx:=(1/(mult*v_unit))*  (  c-a+(d-b-c+a)*(_Y and(mult-1))  );
  dzdy:=(1/(mult*v_unit))*  (  b-a+(d-c-b+a)*(_X and(mult-1))  );


end;
function landH(x,y:real):real;
var a,b,c,d,e,f:real;_x,_y:longint;
begin
  _x:=my_round(x*mult);
  _y:=my_round(y*mult);
  a := land[point2map(_X,_Y)].h ;
  b := land[point2map(_X,_Y+mult)].h ;
  c := land[point2map(_X+mult,_Y)].h ;
  d := land[point2map(_X+mult,_Y+mult)].h ;

  e:=(1/(mult*v_unit))* ( a*mult+ (b-a)*(_Y and(mult-1)) );
  f:=(1/(mult*v_unit))* ( c*mult+ (d-c)*(_Y and(mult-1)) );

  result:=e+  (f-e)*(_X and(mult-1))*(1/mult);
end;
var zoom:real;zoommode:(ZoomVertical,ZoomHorisontal,ZoomDiagonal);
procedure setFOVDiagonal(a:real);
begin
  initialized_settings.fov:=true;
  zoom:=1/(2*tan(a*0.5));
  ZoomMode:=ZoomDiagonal;
end;
procedure setFOVHorisontal(a:real);
begin
  initialized_settings.fov:=true;
  zoom:=1/(2*tan(a*0.5));
  ZoomMode:=ZoomHorisontal;
end;
procedure setFOVVertical(a:real);
begin
  initialized_settings.fov:=true;
  zoom:=1/(2*tan(a*0.5));
  ZoomMode:=ZoomVertical;
end;
procedure setCameraPosition4(x,y,z,_a:real);
begin
  {$ifdef trep}
  writeln('vw.setcameraposition x=',x:2:2,' y=',y:2:2,' z=',z:2:2,' a=',_a:2:2,' called');
  {$endif}
  camX:=my_round(x*mult);
  camY:=my_round(y*mult);
  camZ:=my_round(z*mult);
  angle:=_a;
end;
procedure SetFogColor(r,g,b:real);{0..1}
begin
if r>1 then r:=1 else if r<0 then r:=0;
if g>1 then g:=1 else if g<0 then g:=0;
if b>1 then r:=1 else if b<0 then b:=0;
FogColor.r:=round(r*255*256);
FogColor.g:=round(g*255*256);
FogColor.b:=round(b*255*256);
if FogColor.r>$FF00 then FogColor.r:=$FF00;
if FogColor.g>$FF00 then FogColor.g:=$FF00;
if FogColor.b>$FF00 then FogColor.b:=$FF00;
end;
procedure SetFogTime(t:real);
begin
FogTime:=t;
end;
procedure SetFogdtime(dt:real);
begin
Fogdtime:=Dt;
end;

procedure Sun(d,h:real;r,g,b:real);
begin
end;
procedure initdefaults;
var x,y,z,dx,dy:real;
begin
  {$ifdef trep}writeln('vw.initdefaults called');{$endif}
  if not initialized_settings.FOV then begin
   setFOVhorisontal(pi*0.5);
   initialized_settings.FOV:=true;
  end;

  {$ifdef trep}writeln('cscreendist=',cscreendist);{$endif}
  {if not initialized_settings.Landscape then}
  PrepareLand;
  z:=0;
  getLandscapePoint(camx/mult,camy/mult,z,dx,dy);
  {$ifdef trep}writeln('z=',z:5:5);{$endif}
  z:=(z+3)*mult;
  if CamZ<z then CamZ:=my_round(z);
end;
procedure render;
begin
  Fog100color:=((fogcolor.b shr 8) and $FF)+(fogcolor.g and $FF00) + ((fogcolor.r shl 8)and $FF0000);
  if rendered.base=nil then exit;
  {$ifdef trep}writeln('vw.render called');{$endif}
  initdefaults;
  {$ifdef trep}writeln('camZ=',camZ div mult);{$endif}
  case zoommode of
   ZoomDiagonal:cscreendist:=my_round(zoom*sqrt(width*width+height*height));
   ZoomHorisontal:cscreendist:=my_round(zoom*width);
   ZoomVertical:cscreendist:=my_round(zoom*height);
  end;
  FogX:=my_round(dfX*FogTime);
  FogY:=my_round(dfY*FogTime);
  _render;

  CallRenderedUpdate;

  FogTime:=FogTime+Fogdtime;

end;
procedure lock_rendered;
begin
{$ifdef trep}
writeln('vw.lock_rendered called');
{$endif}
end;
procedure unlock_rendered;
begin
{$ifdef trep}
writeln('vw.unlock_rendered called');
{$endif}
end;
procedure setFarDist(fd:longint);
begin
  {$ifdef trep}
  writeln('vw.setfardist ',fd,' called');
  {$endif}
  maxdist:=fd;
end;
procedure setTextureSizeParam(n:longint);
begin
if (n<0)then texturesize := 0 else
if (n>14) then texturesize := 14 else texturesize := n;
end;

(**For on-the-fly visualisation**)
var
columnCallback:column_update_proc;
columnCallbackU:longint;
renderedCallback:rendered_update_proc;
renderedCallbackU:longint;
procedure SetColumnUpdateProc(callback:column_update_proc;userdata:longint);
begin
columnCallback:=callback;
columnCallbackU:=userdata;
end;
procedure CallColumnUpdate(x1,x2:longint);{$ifdef useinline}{inline;}{$endif}
begin
if assigned(columnCallback) then columnCallback(x1,x2,columnCallbackU);
end;
procedure resetColumnUpdateProc;
begin
columncallback:=nil;
end;
procedure SetRenderedUpdateProc(callback:rendered_update_proc;userdata:longint);
begin
renderedCallback:=callback;
renderedCallbackU:=userdata;
end;
procedure CallRenderedUpdate;{$ifdef useinline}{inline;}{$endif}
begin
if assigned(renderedCallback) then renderedCallback(renderedCallbackU);
end;
procedure resetRenderedUpdateProc;
begin
renderedCallback:=nil;
end;
procedure SetInterlacing(n:longint);
begin
phases:=n;
end;
(******************************************************)

begin
  SetFogTime(0);
  SetFogdtime(1);
  rseed:=0;
  init;

end.
