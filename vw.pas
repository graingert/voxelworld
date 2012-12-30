(*  Voxel World Renderer.       (C) 2004-2008 Dmytry Lavrov     *)
(*  http://dmytry.pandromeda.com                                *)
{debug of application that uses this unit: levels}
{ debug parameters does not affect speed of rendering,only time of calling
various functions,except "render"}
{errors that are surely application bugs will cause run-time error 255}
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
(*search keys:
dirt
unsafe_pointers
*)


unit vw;
{$ifdef FPC}
{$define fastrandom}
{$endif}
{$define noclip}
{define texturesbilinear}
{$define useRDTSC}
{define trep}
{define useglobals}
{$IFDEF FPC}
{define useinline}
{$define p3asm}
{$ENDIF}
{define preclean}
{define nofill}
{define landgrid1}
{define skygrids}
{$define inertialcam}
{$define advmovement}
{define scaner}
{define testtexture}
{define openi}
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
{$ifndef FPC}
uses fpcprocs;
{$endif}
(*basic data types*)
{$IFNDEF FPC}
{$I fpctypes.inc}
{$ENDIF}
{$X+}
Type
real = single;
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
type vector3r=record
  case integer of
  1:(a:array[0..2] of real);
  2:(x,y,z:real);
end;
colorRGB=record
  r,g,b:single;
end;

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
function summ(a:array2Ddesc):extended;
function average(const a:array2Ddesc):real;

type Tdwordarray=packed array[0..maxint div 4 -1] of dword;pdwordarray=^tdwordarray;
const ColorTableHeight=1024;
type Tcolortable=packed array[{uint16}0..256*ColorTableHeight - 1] of uint32;

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
procedure ReinitLand;
procedure Renderport(projection:n_projections;w,h:longint);
{const autoheightoffset=$FFFFFFFF;}
procedure SetLandscapeHeighxels(const destination:SubArray2Ddesc;const source:array2Ddesc;heightoffset,heightunit:real);
procedure getLandscapePoint(x,y:real;var z,dzdx,dzdy:real);
function landH(x,y:real):real;

{works ONLY for n_stdperspective,outherwise,ignored}
{may cause overflow;}
procedure setFOVDiagonal(a:real);
procedure setFOVHorisontal(a:real);
procedure setFOVVertical(a:real);

procedure setCameraPosition4(x,y,z,_a:real);
procedure SetSunPosition(Heading,Altitude:real);
procedure setFarDist(fd:longint);
procedure Sun(d,h:real;r,g,b:real);


procedure SetSunLightColor(r,g,b:real);{0..1}
procedure SetAboveLightColor(r,g,b:real);{0..1}
procedure SetReverseLightColor(r,g,b:real);{0..1}
procedure SetTextureColor(const a:Tcolortable);

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
Function GetClocks: qword;

(******************************************************)
{var dfLandscape,dfFogLayersData,dfSun,dfCamera,dfFOV,dfTexture,dfProjection:boolean;}
var initialized_settings:record
  Landscape,LandscapeShadows,BumpAmpitudeMap,BumpSelectorSubmap,FogLayersData,
  Sun,SunTables,Camera,FOV,Texture,TextureShadows,Projection,TextureColor,TextureColorScale:boolean;
end;
type tExpParams=record Density,HalfH:colorRGB; end;
type tWorldParams=
record
  cam:record x,y,z,f,a:real;projection:n_projections;{and camera quaternion in near future}end;
  fogt,fogdt:real;
  fogcolor,sunlightcolor,abovelightcolor,reverselightcolor:colorRGB;
  sky:record
    lightness,opacity:tExpParams;
  end;
  sun:record
    heading,altitude:real;//heading,"altitude" . Heading are currently ignored.
  end;
  ColorScaleA,ColorScaleB:real;
  generator:record
    seed:longint;
  end;
end;
var worldparams:tWorldParams;
const eps=1E-6;
{$ifndef openi}implementation{$endif}
(**************************************************************)
(****************************General helpers*********************************)
(**** function summ(a:array2Ddesc):extended; ****)
var __summ:extended;var summ__sourcepointer:longint;
type summ_proc=procedure;
(*do not edit*)
procedure summ_uint8;begin __summ:=__summ+puint8(summ__sourcepointer)^ ;end;
procedure summ_int8;begin __summ:=__summ+pint8(summ__sourcepointer)^ ;end;
procedure summ_uint16;begin __summ:=__summ+puint16(summ__sourcepointer)^ ;end;
procedure summ_int16;begin __summ:=__summ+pint16(summ__sourcepointer)^ ;end;
procedure summ_uint32;begin __summ:=__summ+puint32(summ__sourcepointer)^ ;end;
procedure summ_int32;begin __summ:=__summ+pint32(summ__sourcepointer)^ ;end;
procedure summ_single;begin __summ:=__summ+psingle(summ__sourcepointer)^ ;end;
procedure summ_real;begin __summ:=__summ+preal(summ__sourcepointer)^ ;end;
function summ(a:array2Ddesc):extended;
var i_limit,y:longint;
var dopoint:summ_proc;
begin
  __summ:=0;dopoint:=nil;result:=0;
  case a.datatype of
    n_nothing:exit;n_uint8:dopoint:=(@summ_uint8);n_int8:dopoint:=(@summ_int8);
    n_uint16:dopoint:=(@summ_uint16);n_int16:dopoint:=(@summ_int16);n_uint32:dopoint:=(@summ_uint32);
    n_int32:dopoint:=(@summ_int32);n_single:dopoint:=(@summ_single);n_real:dopoint:=(@summ_real);
    else runerror(255);
  end;
  for y:=0 to a.ycount-1 do begin
    summ__sourcepointer:=longint(a.base)+y*a.my;
    i_limit:=summ__sourcepointer+(a.xcount)*a.mx;
    while summ__sourcepointer<i_limit do begin
      dopoint;
      inc(summ__sourcepointer,a.mx);
    end;
  end;
  result:=__summ;
end(** function summ(a:array2Ddesc):extended; /**);
function average(const a:array2Ddesc):real;
begin
  result:=summ(a)/(a.xcount*a.ycount);
end;

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
end{$ifdef fpc}['eax','edx']{$endif};
const round_cw:word=$1073;{ $1373}
function my_round(d:real):longint;
{$ifdef fpc}
assembler;{$ifdef useinline}inline;{$endif}
asm
  mov edx,esp
  sub esp,8
  fnstcw word ptr [ebp-4]
  fwait
  fldcw round_cw
  fwait
  fld d
  fistp dword ptr [ebp-8]
  fclex
  mov eax,[ebp-8]
  fldcw [ebp-4]
  mov esp,edx
end{$ifdef fpc}['EAX','EDX']{$endif};
{$else}
begin
  result:=round(d);
end;
{$endif}
var myround_tmp:longint;
procedure setroundmode;assembler;
asm
  fldcw round_cw
end;
function my_round2(d:single):longint;
assembler;{$ifdef useinline}inline;{$endif}
asm
  fld d
  fistp myround_tmp
  fclex
  mov eax,myround_tmp
end{$ifdef fpc}['EAX']{$endif};

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

const landres2=10;landres=1 shl landres2;
const textureres2=landres2;textureres=1 shl textureres2;
const numfoglayers=2;

var land:array[0..landres*landres-1] of packed record h,lh,th:landint;fog:packed array[0..numfoglayers*2-1] of landint; c:byte;tba{texture bump amplitude}:uint8;end;

const bumpscount=2;
//type bumpsrec=packed record a:packed array[0..bumpscount-1] of int16;end;

var
bumps: packed array[0..textureres*textureres-1] of packed record a:packed array[0..bumpscount-1] of int16;end;
derivatives: packed array[0..textureres*textureres-1] of packed record a:packed array[0..bumpscount-1] of int16;end;
{var bumpselector: packed array[0..textureres*textureres-1] of int8;}
type rgbi=record r,g,b:longint;end;
{var bumpcolors:array[0..bumpscount-1] of rgbi;}
const derivative_unit2 = 5;
derivative_unit = 1 shl derivative_unit2;{32}

var sunlight,shadowlight: packed array[uint16] of packed record r,g,b:single;end;
Var texturecolor:Tcolortable;
{textureselectorSub: packed array[0..textureres*textureres-1] of uint8;}
(**use "texturecolor[(abs(slopex)+abs(slopey)) + $FF00 and (altitude shl 8)]" **)

//var FogTime,Fogdtime:real;

var FogColor:record r,g,b:longint;{shifted by 8} end;var Fog100color:uint32;


var snowlevel: longint;
snownoise,texturesize: byte;
angle,m: real;

const shift = 12{};
mult = 1 shl shift;
hmult = 1 shl (shift div 2);
js = mult div 4;
vjs = mult;


const v_unit2 = 6;v_unit = 1 shl v_unit2;{to how many horisontal cells one vertical are equal.Should be less than mult}
{const bumpselectorweight2 =(shift+v_unit2-6);}


var width,height,Cscreendist:longint;{}
const
camDDZ = - mult div 32 ;

{/}

{transponed form,pt:=x*height+y}
{y grows from bottom}

const
mulZ = 1;

var camH{=100}{=255},foge: longint;
var CamX,CamY,CamZ,camdx,camdy,camdz,fogX,fogY,dfx,dfy: longint{multiplied!!!!!!!!};

type tliarray=packed array[0..maxlongint div 4 -1] of longint;
pliarray=^tliarray;
type tskylight=packed array[0..maxlongint div sizeof(colorRGB) -1] of colorRGB;
pskylight=^tskylight;
var light:pliarray;skylight:pskylight;{y from bottom to top}
lightk,lightk_shifted: longint;
_lightk: byte;{foger}
const lightk_factor=2;


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
(*function min(a,b:longint):longint;assembler;{$ifdef useinline}inline;{$endif}
asm
end;*)

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
begin
  result:=(x shr shift)and(landres-1)+((y shr shift)and(landres-1))shl landres2;
end;
(*assembler;
asm
mov eax,y
shr eax,shift
and eax,landres-1
shl eax,landres2
mov ecx,x
shr ecx,shift
and ecx,landres-1
or eax,ecx
end
{$ifdef FPC}['eax','ecx']{$endif};
*)
{const sunlight=$FFE080;
const skylight=$202030;}

{var sunR:longint:=400;sunG:longint:=250;sunB:longint:=100;skyR:longint:=25;skyG}

var sunR,sunG,sunB,skyR,skyG,skyB: longint;
function clamp01(a:real):real;
begin
  if a<0 then result:=0 else if a>1 then result:=1 else result:=a;
end;
const _255:longint=255;
const _0:longint=0;
function clamp0_255(a:longint):longint;
{$ifdef p3asm}
assembler;
asm
  mov eax,a
  or eax,eax
  cmovb eax,_0
  cmp eax,255
  cmovg eax,_255
end;
{$else}
begin
  if a<0 then result:=0 else if a>255 then result:=255 else result:=a;
end;
{$endif}

procedure PutPixel(x,y:longint;c:uint32);
begin
  if (uint32(x)<uint32(rendered.xcount))and(uint32(y)<uint32(rendered.ycount)) then
  puint32(longint(rendered.base)+x*rendered.mx+y*rendered.my)^:=c;
end;

const MaxShade=20*v_unit;
var ShadeTable:packed array[0..MaxShade] of longint;

Procedure InitTables;

var x,y,tmp,n: longint;{y=shader}
{var r,g,b:longint;}

begin
  for y:=0 to 255 do
  begin
    for x:=0 to 255 do
    begin
      {tmp := 255-((255-x)*(255-y)) div 256;
      Ftable[x+y*256] := byte(tmp);}
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

{Type rgb = record
r,g,b: longint;
end;}


//var minh:longint;


//Procedure GradVLine(x,y1,y2{coords  ,draw to y2-1}{},s1,s2{texture selector},tx1,ty1,tx2,
//ty2{texel coords},dhdx1,dhdy1,dhdx2,dhdy2:longint;tba1,tba2:longint; lh1,lh2:longint; h1,h2:longint);

// draw portion of textured line. Pixels from y1 to y2-1 are drawn.
//Interpolation coefficient are computed as t=(a+b*y)/(c+d*y)
// Things is interpolated as x1+t*(x2-x1)

procedure TexturedStrip(x,y1,y2:longint;
a,b,c,d:single;
tx1,ty1,tx2,ty2,//tecture coordinates
dhdx1,dhdy1,dhdx2,dhdy2,// slope
tba1,tba2,// texture bumpness
lh1,lh2, // light height
h1,h2  // height
:longint);

var tmpv,{delta,}sel,deltasel,tx,deltatx,ty,deltaty,dhdx,dhdy,deltadhdx,deltadhdy,
tba,deltaTBA,y,limit,pt, lh,dlh ,h,dh:longint;invl:single;
var t:single;
//t_fixedpoint:integer;
var tmp_single:single;
var destp,destlim,lightp,

ti
{$ifdef texturesbilinear}
,ti1,v1,ti2,v2,ti3,v3,ti4,v4
{$endif}
:longint;
tn1,tn0,tnx,tny,th:longint;tc:dword;
resultcolor:longint;
{bx=tx dx=ty cx=lightval si=temp pointer}
var textureC,textureR,textureG,textureB:longint;lightk1:real;
lightR,lightG,lightB,pixelR,pixelG,pixelB:real;

{color:=(c*(kr>>8))>>8}

begin
  //search key:unsafe_pointers
  {writeln('hi,y=',y1/256:5:5,' y2=',y2/256:5:5,' x=',x);
  if (width<>rendered.xcount)or(height<>rendered.ycount)then  writeln('wrong dimesnions');}
  //if y1>=y2 then writeln('Bug1');
  if (x<rendered.xcount)and(x>=0) then
  begin
    if y1<0 then y1 := 0
    else if y1>(height-1)*256 then y1 := (height-1)*256;
    if h1<0 then begin
      writeln('Wooops[h1<0] !');
      h1:=0;
    end else
    if h1>(255)*mult*v_unit then begin
      writeln('Wooops[h1>max] !');
      h1:=(255)*mult*v_unit;
    end;
    if h2<0 then begin
      writeln('Wooops[h2<0] !');
      h2:=0;
    end
    else
    if h2>(255)*mult*v_unit then
    begin
      writeln('Wooops[h2>max] !');
      h2:=(255)*mult*v_unit;
    end;
    if y2<=y1 then writeln('y2<=y1');
    begin
      setroundmode;
      //      invl:=256/(y2-y1);
      //      deltatx :=my_round2((tx2-tx1)*invl);
      //      deltaty :=my_round2((ty2-ty1)*invl);
      //      deltadhdx :=my_round2((dhdx2-dhdx1)*invl);
      //      deltadhdy :=my_round2((dhdy2-dhdy1)*invl);
      //      dlh :=my_round2((lh2-lh1)*invl);
      //      dh :=my_round2((h2-h1)*invl);
      //      deltaTBA :=my_round2((TBA2-TBA1)*invl);
      deltatx:=tx2-tx1;
      deltaty:=ty2-ty1;
      deltadhdx:=dhdx2-dhdx1;
      deltadhdy:=dhdy2-dhdy1;
      dlh:=lh2-lh1;
      dh:=h2-h1;
      deltaTBA:=TBA2-TBA1;
      {deltasel :=(s2-s1) div(y2-y1);}
      {
      deltatx :=((tx2-tx1)*256)div(y2-y1);
      deltaty := ((ty2-ty1)*256)div(y2-y1);
      deltadhdx := ((dhdx2-dhdx1)*256)div(y2-y1);
      deltadhdy := ((dhdy2-dhdy1)*256)div(y2-y1);
      dlh := ((lh2-lh1)*256)div(y2-y1);
      dh := ((h2-h1)*256)div(y2-y1);
      deltaTBA := ((TBA2-TBA1)*256)div(y2-y1);
      }
    end
    {
    else
    begin
    deltasel := 0;
    //delta:=0;
    deltatx := 0;
    deltaty := 0;

    end};
    y1 := y1 div 256;
    y2 := y2 div 256;

    if y1>=height then writeln('y1>=height');
    //if y2>=height then writeln('y2>=height');
    //if y1<0 then writeln('y1<0');
    //if y2<0 then writeln('y2<0');

    {kr := k1.r;
    kg := k1.g;
    kb := k1.b;}
    {c := (_lightk)*($010101);}
    if y2<0 then begin
      y2 := 0;
      writeln('y2<0');
    end
    else if y2>=height then y2 := height-1;

    destp:=x*rendered.mx+longint(rendered.base);
    destlim:=(destp+y2*4 );

    destp:=(destp+y1*4);


    (*
    tc:=$FFFFFFFF;
    puint32(destp)^:=tc;
    puint32(destlim)^:=tc;
    puint32((rendered.xcount-1)*rendered.mx+(rendered.ycount-1)*rendered.my+
    longint(rendered.base))^:=tc; *)
    y := y1;

    lightp:=longint(light)+y1*4;
    //tx:=tx1;
    //ty:=ty1;
    //tba:=tba1;
    //lh:=lh1;
    //h:=h1;
    repeat
      tmp_single:=(c+d*y);
      if(tmp_single<>0)then t:=(a+b*y)/tmp_single else t:=0;
      //t:=(y-y1)/(y2-y1);
      //t_fixedpoint:=my_round(mult*t);
      //dhdx:=dhdx1+(deltadhdx*t_fixedpoint)div mult;
      //dhdy:=dhdy1+(deltadhdy*t_fixedpoint)div mult;
      //tba:=tba1+(deltaTBA*t_fixedpoint)div mult;
      //lh:=lh1+(dlh*t_fixedpoint)div mult;
      //h:=h1+(dh*t_fixedpoint)div mult;
      dhdx:=dhdx1+my_round(deltadhdx*t);
      dhdy:=dhdy1+my_round(deltadhdy*t);
      tba:=tba1+my_round(deltaTBA*t);
      lh:=lh1+my_round(dlh*t);
      h:=h1+my_round(dh*t);
      tx:=tx1+my_round(deltatx*t);
      ty:=ty1+my_round(deltaty*t);
      //Interesting,how damn bad it will look in assembber listings??
      //hmm.not THAT bad.
      lightk:=lightk+pint32(lightp)^;
      lightk_shifted:=lightk div lightk_factor;
      tc:=Fog100color;
      if lightk_shifted < 255 then
      begin
        {$ifndef texturesbilinear}

        ti:=(ty {shl (textureres2-8)}shr (16-textureres2) )and((textureres-1)shl textureres2) +
        (tx shr {8}16)and (textureres-1);

        tn1:=derivatives[ti].a[1];
        tn0:=derivatives[ti].a[0];
        th:=bumps[ti].a[0]+(bumps[ti].a[1]*tba)div (256*mult);

        {tc:=255;}
        (*
        if lh>0 then begin
        if lh<MaxShade*mult then tc:=(255*shadetable[lh div mult])div $10000 else
        tc:=(255*shadetable[maxShade])div $10000;
        {tc:=30;}
        end;*)
        {if lh < 100 then tc:=texture[ti] else tc:=40;}

        {$else}
        Texture code is incomplete!

        {$endif}

        tnx:=int8(tn0)*mult+ (int8(tn1)*tba)div(256)         + (dhdx) div ((v_unit)div derivative_unit){} ;
        tny:=int8(tn0 shr 8)*mult+ (int8(tn1 shr 8)*tba)div(256)   + (dhdy) div ((v_unit)div derivative_unit){} ;
        tnx:=tnx div mult;
        tny:=tny div mult;
        ti:=uint32(byte(tnx)+byte(tny)*256);


        if lh>1*v_unit*mult then begin
          //writeln('hi');
          lightR:=shadowlight[ti].r;
          lightG:=shadowlight[ti].g;
          lightB:=shadowlight[ti].b;
          end else begin
          lightR:=sunlight[ti].r+shadowlight[ti].r;
          lightG:=sunlight[ti].g+shadowlight[ti].g;
          lightB:=sunlight[ti].b+shadowlight[ti].b;
        end;
ti:=abs(tnx)+abs(tny)+ ($100*(ColorTableHeight-1)) and ($100*((abs(th)*(mult div 64) +h) div ((mult*v_unit*256) div ColorTableHeight{dirt!}) ));// 16-->64 changed at 4-01-2005 23:18:22
        textureC:=texturecolor[ti];
        textureR:=(textureC shr 16)and 255;
        textureG:=(textureC shr 8)and 255;
        textureB:=(textureC)and 255;
        lightk1:=(255-lightk_shifted)/255;
        pixelR:=textureR*lightk1*lightR;
        pixelG:=textureG*lightk1*lightG;
        pixelB:=textureB*lightk1*lightB;
        tc:=
        (clamp0_255(my_round2(pixelR)+(lightk_shifted*FogColor.r)shr 16)shl 16)and $FF0000 +
        (clamp0_255(my_round2(pixelG)+(lightk_shifted*FogColor.g)shr 16)shl 8)and $FF00 +
        (clamp0_255(my_round2(pixelB)+(lightk_shifted*FogColor.b)shr 16))and $FF;

      end;
      puint32(pointer(destp) )^:=uint32(tc);
      inc(destp,4);
      if destp>=destlim then break;
      inc(lightp,4);
      inc(y);
    until false;
    if(y<>y2-1)then writeln('chezachert1 :y[',y,']<>y2[',y2-1,']');
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
ix,iy,ixy: record
  x,y,d,dx,dy,dd: longint;
end;

span0,span1: record{span0 near to camera, span1 far}
  lx,ly,d,h,lh,th,fh,c,s,sy,dhdx,dhdy  ,TBA: longint;
  hcalc_a: real;
  hcalc_b: longint;{screen_y= surface_y_multiplied*a+b}
  {interpolation:}
  n1,n2,k: longint;
  {rgbs}
  crgb: rgbi;
  need_to_update: boolean;
end;

const spandwords = sizeof(span0)div 4;

//var nearx: boolean;{true:x is nearest.False:y}
var nearline:integer;{0 : x is nearest, 1:y is nearest, 2 : xy is nearest}

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
var PS_Params:record
  a,b,c,d:double;
end;

Label loopexit1;
{function h2s(h,d:longint):longint;
begin
result:=height div 2 + trunc(((h-camZ)*m*screendist)/(d+1));
end; }

Procedure addlight(y,v:longint);
begin
  y := y div 256;
  if y>height then exit;
  if y<(sy div 256) then begin
    inc(lightK,v);
    lightk_shifted:=lightk div lightk_factor;
  end
  else inc(light^[y],v);
  {$ifdef skygrids}
  screen.putpixel(sx,height-y,255);
  {if (y>=0)(y<height) then rendered[sx*width+y]:=255;}{$endif}
end;



const gridtol = 50;
(*procedure RenderSkyLine;{$ifdef useinline}inline;{$endif}
begin

end;*)
var rslDest:puint32;rslFP,rslLimit:pint32;
tmpc:colorRGB;

{$ifdef useglobals}Procedure _Render;{$endif}
Label lightdone;
Label rslLoop1;
var skylineprepare:record
  a:real;
end;
var maxdist:longint;
label l_step_x,l_step_y,l_step_xy,l_step_end;
label init_step_x,init_step_y,init_step_xy,init_step_end;
begin
  maxdist:=round(worldparams.cam.f);
  {$ifdef preclean}
  FillDword(rendered.base^,rendered.xcount*rendered.ycount,0);
  {$endif preclean}
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


  with skylineprepare do begin
    for sy:=0 to height-1 do begin
      a:=(height div 2)/(sy-(height div 2)-0.5);
      a:=0.2*sqrt(sqr(a)+1);
      a:=a/sqrt(sqr(a)+1);
      skylight^[sy].r:=a*0.6;
      skylight^[sy].g:=a*0.8;
      skylight^[sy].b:=a;
    end;
  end;

  sx := phase;
  repeat{plane-tracing loop}
    dx := (mult*(sx-width div 2))div Cscreendist;
    screendist := my_round(sqrt(sqr({real}1.0*(Cscreendist))+sqr({real}1.0*(sx-width div 2)) ));
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
        ix.dd := my_round(sqrt(sqr(mult)+sqr({real}1.0*(ix.dy)) ))  ;
        ix.x := (lx+mult)and{fixed floor} Not(mult-1);
        ix.y := ly+(ix.dy*(ix.x-lx))div mult;
        ix.d := (ix.dd*(ix.x-lx))div mult;
      end
      else if dx< -gridtol then
      begin
        ix.dx := -mult;
        ix.dy := -(dy*mult) div dx;
        ix.dd := my_round(sqrt(sqr(mult)+sqr({real}1.0*(ix.dy)) ) ) ;
        ix.x := lx and Not(mult-1);
        ix.y := ly+(ix.dy*(lx-ix.x))div mult;
        ix.d := (ix.dd*(lx-ix.x))div mult;
      end
      else{dx=0}
      begin
        ix.d := mult*mult;
        ix.dd := 2*mult*mult;
      end;{Newer use ix ,d="infinity"}


      if dy>gridtol then
      begin
        iy.dy := mult;
        iy.dx := (dx*mult) div dy;
        iy.dd := my_round(sqrt(sqr(mult)+sqr({real}1.0*(iy.dx)) ))  ;
        iy.y := (ly+mult)and{fiyed trunc} Not(mult-1);
        iy.x := lx+(iy.dx*(iy.y-ly))div mult;
        iy.d := (iy.dd*(iy.y-ly))div mult;
      end
      else if dy< -gridtol then
      begin
        iy.dy := -mult;
        iy.dx := -(dx*mult) div dy;
        iy.dd := my_round(sqrt(sqr(mult)+sqr({real}1.0*(iy.dx)) ) ) ;
        iy.y := ly and Not(mult-1);
        iy.x := lx+(iy.dx*(ly-iy.y))div mult;{WHY overflow here? FIXED.}
        iy.d := (iy.dd*(ly-iy.y))div mult;
      end
      else{dy=0}
      begin
        iy.d := mult*mult;
        iy.dd := 2*mult*mult;
      end;{Newer use iy ,d="infinity"}
      if dx+dy>gridtol then begin
        ixy.dx:=(dx*mult)div(dx+dy);
        ixy.dy:=(dy*mult)div(dx+dy);
        //correction
        ixy.dx:=ixy.dx-(ixy.dx+ixy.dy-mult);
        ixy.dd := my_round(sqrt(sqr(1.0*(ixy.dx))+sqr({real}1.0*(ixy.dy)) ) ) ;
        ixy.d:=mult-((lx+ly)-((lx+ly) and not(mult-1)));
        ixy.x:=lx+(ixy.dx*ixy.d)div mult;
        ixy.y:=ly+(ixy.dy*ixy.d)div mult;
        ixy.d:=(ixy.dd*ixy.d)div mult;
      end
      else if dx+dy<-gridtol then begin
        ixy.dx:=-(dx*mult)div(dx+dy);
        ixy.dy:=-(dy*mult)div(dx+dy);
        //correction
        ixy.dx:=ixy.dx-(ixy.dx+ixy.dy+mult);
        ixy.dd := my_round(sqrt(sqr(1.0*(ixy.dx))+sqr({real}1.0*(ixy.dy)) ) ) ;
        ixy.d:=((lx+ly)-((lx+ly) and not(mult-1)));
        ixy.x:=lx+(ixy.dx*ixy.d)div mult;
        ixy.y:=ly+(ixy.dy*ixy.d)div mult;
        ixy.d:=(ixy.dd*ixy.d)div mult;
      end
      else
      begin
        ixy.d := mult*mult;
        ixy.dd := 2*mult*mult;
      end;
      //ixy.d := mult*mult;
      //ixy.dd := mult*mult;
      //ix.d := mult*mult;
      //ix.dd := mult*mult;
      //iy.d := mult*mult;
      //iy.dd := mult*mult;
    end{with span1};
    {ix.d:=my_round(ix.d/(dist*m));}
    {ix.dd:=my_round(ix.d/(dist*m));}
    {iy.d:=my_round(iy.d/(dist*m));}
    {iy.dd:=my_round(iy.d/(dist*m));}
    {--}
    {$ifdef FPC}
    filldword(light^,height,0);
    {$else}
    for sy:=0 to height-1 do light^[sy]:=0;
    {$endif}
    sy := 0;
    span1.sy := sy;
	  //span1.d := 0;
    //span1.n1 := point2map(span1.lx,span1.ly);
    //span1.n2 := span1.n1;
    //span1.hcalc_a := (screendist*m*256 );
    //span1.hcalc_b := my_round((height*256 div 2)/span1.hcalc_a  -camZ/m);
		
		span1.hcalc_a := 0;
    span1.hcalc_b := 0;
    span1.need_to_update := true;
		
		with span1 do// initialize span1
		begin
        {selecting nearest,preparing interpolation,etc}
        if(ix.d-ix.dd>ixy.d-ixy.dd) then begin
          if(iy.d-iy.dd>ix.d-ix.dd) then goto init_step_y else goto init_step_x;
          end else begin
          if(iy.d-iy.dd>ixy.d-ixy.dd) then goto init_step_y;
        end;
        init_step_xy:
        k:=(ixy.y-ixy.dy) and (mult-1);
        if((k<5)or(k>mult-6)) then begin
          if(iy.d-iy.dd>ix.d-ix.dd) then goto init_step_y else goto init_step_x;
        end;
        lx:=ixy.x-ixy.dx;
        ly:=ixy.y-ixy.dy;
        span1.d:=ixy.d-ixy.dd;
        n1:=point2map(lx+mult,ly);
        n2:=point2map(lx,ly+mult);
        goto init_step_end;
        init_step_x:
        lx := ix.x-ix.dx;
        ly := ix.y-ix.dy;
        span1.d := ix.d-ix.dd;
        n1 := point2map(lx,ly);
        n2 := point2map(lx,ly+mult);
        k := ly and (mult-1);
        goto init_step_end;
        init_step_y:
        span1.lx := iy.x-iy.dx;
        span1.ly := iy.y-iy.dy;
        span1.d := iy.d-iy.dd;
        n1 := point2map(lx,ly);
        n2 := point2map(lx+mult,ly);
        k := lx and (mult-1);
        init_step_end://///////////////////////////////////
	  end;	
		

    lightk := 0;
    lightk_shifted:=0;
    fogcalcdist := 0;
    repeat{Vertical tracing loop}

      {select nearest}
      {curspan:=not(curspan);}
      {move(span1,span0,sizeof(span0));}
      span0 := span1;

      with span1 do
      begin
        {selecting nearest,preparing interpolation,etc}
        if(ix.d<ixy.d)then begin
          if(iy.d<ix.d) then goto l_step_y else goto l_step_x;
          end else begin
          if(iy.d<ixy.d)then goto l_step_y;
        end;
        l_step_xy:
        k:=ixy.y and (mult-1);
        if((k<5)or(k>mult-6)) then begin
          ixy.x := ixy.x+ixy.dx;
          ixy.y := ixy.y+ixy.dy;
          ixy.d := ixy.d+ixy.dd;
          if(iy.d<ix.d) then goto l_step_y else goto l_step_x;
        end;
        lx:=ixy.x;
        ly:=ixy.y;
        span1.d:=ixy.d;
        n1:=point2map(lx+mult,ly);
        n2:=point2map(lx,ly+mult);

        ixy.x := ixy.x+ixy.dx;
        ixy.y := ixy.y+ixy.dy;
        ixy.d := ixy.d+ixy.dd;

        goto l_step_end;
        l_step_x:
        lx := ix.x;
        ly := ix.y;
        span1.d := ix.d;
        n1 := point2map(lx,ly);
        n2 := point2map(lx,ly+mult);
        k := ly and (mult-1);
        ix.x := ix.x+ix.dx;
        ix.y := ix.y+ix.dy;
        ix.d := ix.d+ix.dd;
        goto l_step_end;
        l_step_y:
        span1.lx := iy.x;
        span1.ly := iy.y;
        span1.d := iy.d;
        n1 := point2map(lx,ly);
        n2 := point2map(lx+mult,ly);
        k := lx and (mult-1);
        iy.x := iy.x+iy.dx;
        iy.y := iy.y+iy.dy;
        iy.d := iy.d+iy.dd;
        l_step_end://///////////////////////////////////


        {$ifdef scaner}
        screen.curcolor := 255+256*(abs(land[span1.n1].h-camZ div mult)+256*(camz div
        mult-land[span1.n1].h));
        screen.putpixel((lx-camx) div (mult div 2)+width div 2,height div 2-(ly-camy)
        div (mult div 2));
        {$endif}
        {readkey;}
        span1.hcalc_a := (screendist*m*256)/((span1.d+1));
        span1.hcalc_b := my_round((height*256 div 2)/(span1.hcalc_a) -camZ/m);
        {later,sy:=my_round((h+hcalc_b)*hcalc_a); }
      end{with};
      {check distance}
      if span1.d and Not(mult-1) >= mult*80 then
      begin{lowering quality/2}
        if ix.dd>iy.dd then ix.d := mult*mult
        else iy.d := mult*mult;
        ixy.dd:=mult*mult;
      end;
      if (span1.d>maxdist*mult)or
      (span1.lx<0)or(span1.lx>landres*mult)or
      (span1.ly<0)or(span1.ly>landres*mult)
      then
      begin
        {vertical fog span}
        begin
          sy := sy div 256;{}
          if sy>=height then break;

          (*Render Vertical skyline,temporary variables rslXxxx  *)
          //search key:unsafe_pointers

          {$ifndef nofill}

          rslFP:=@(light^[sy]);
          rslDest:=puint32(longint(rendered.base)+sx*rendered.mx+sy*rendered.my);
          rslLimit:=@(light^[height-1]);
          repeat
            inc(lightK,(rslFP^));
            lightk_shifted:=lightk div lightk_factor;
            if lightK_shifted<255 then begin
              rslDest^:=
              clamp0_255(  (lightk_shifted*FogColor.b)shr 16 + my_round2(skylight^[sy].b*(255-lightk_shifted)) ) +
              clamp0_255(  (lightk_shifted*FogColor.g)shr 16 + my_round2(skylight^[sy].g*(255-lightk_shifted)) )shl 8 +
              clamp0_255(  (lightk_shifted*FogColor.r)shr 16 + my_round2(skylight^[sy].r*(255-lightk_shifted)) )shl 16;
            end
            else rslDest^:=fog100color;
            inc(longint(rslFP),4);
            inc(longint(rslDest),rendered.my);
            inc(sy);
          until longint(rslFP)>longint(rslLimit);

          {$endif}
        end;
        break;
      end;

      (*interpolation,fog,etc*)
      setroundmode();
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
        l1:=land[n1].lh;
        l2:=land[n2].lh;
        lighth := ((l2-l1)*k+l1*mult)div mult;
        //lighth:=land[n1].lh;
        (*for n:=0 to numfoglayers*2-1 do
        begin
        fogh[n]:=land[{n1}point2map(lx+fogx*3,ly+fogy*(n+3)) ].fog[n]{+(n)*v_unit*20};
        if fogh[n]<lighth then fogh[n]:=lighth;
        end;*)

        {fogh[0]:=}
        {fog1lowh}fogh[0]:={20*v_unit}land[{n1}point2map(lx+fogx,ly+fogy) ].fog[0];
        {fog1highh}fogh[1]:={10*v_unit}land[{n1}point2map(lx-fogy,ly+fogx) ].fog[1];
        {fog2lowh}fogh[2]:={5*v_unit}land[{n1}point2map(lx+fogy,ly-fogx) ].fog[2];
        {fog2highh}fogh[3]:={40*v_unit}land[{n1}point2map(lx-fogy,ly-fogx) ].fog[3];
        inc(fogh[0],-50*v_unit);
        inc(fogh[1],20*v_unit);
        inc(fogh[2],10*v_unit);
        inc(fogh[3],40*v_unit);

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

          tx1 := span1.lx shl texturesize {shl(texturesize +8 - shift)};
          ty1 := span1.ly shl texturesize {shl(texturesize +8 - shift)};
          l1 := land[n1].c;
          l2 := land[n2].c;
          c := (l2-l1)*k+l1*mult;

          l1:=land[n1].tba;
          l2:=land[n2].tba;
          tba := (l2-l1)*k+l1*mult;

          l1:=land[n1].th;
          l2:=land[n2].th;
          th := (l2-l1)*k+l1*mult;

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

            l1:=land[n1].th;
            l2:=land[n2].th;
            th := (l2-l1)*k+l1*mult;

            l1:=land[n1].lh;
            l2:=land[n2].lh;
            lh:=(l2-l1)*k+l1*mult-span0.h;

            {l1:=selector[n1];l2:=selector[n2];
            s:=(l2-l1)*k+l1*mult;}
          end;
          tx0 := span0.lx shl texturesize ;
          ty0 := span0.ly shl texturesize  ;
        end{with span0};
        //
        //span1.hcalc_a := (screendist*m*256)/((span1.d+100));
        //span1.hcalc_b := my_round((height*256 div 2)/(span1.hcalc_a) -camZ/m);
        //my_round((h+hcalc_b)*hcalc_a);
        //s = (height*256/2 + (screendist*m*256)*(h0-camZ/m)+t*( (screendist*m*256)*(h1-h0) ) )/(span0.d+t*(span1.d-span0.d))
        //writeln(span1.sy-span1.hcalc_a*(span1.h+span1.hcalc_b));
        //writeln(span1.sy-(height*256 div 2 + (span1.h-camZ/m)*(screendist*m*256)/(span1.d+100)   ) );
        //writeln((span0.sy/256.0)-( ( (height*0.5)*(span0.d+100) + ((span0.h-camZ/m)*(screendist*m*1.0)) )/(span0.d+100)   ) );
        {
        writeln((span0.sy/256.0)-(
( (height*0.5)*(span0.d+100)  + t*( (height*0.5)*(span1.d-span0.d) + (screendist*m*1.0)*(span1.h-span0.h) )  + (screendist*m*1.0)*(span0.h-camZ/m)  )/
        (span0.d+100+t*(span1.d-span0.d))
        )
        );
        }
        {
        writeln((span0.sy/256.0)-(
( (height*0.5)*(span0.d+100)  + (screendist*m*1.0)*(span0.h-camZ/m)  + 0*( (height*0.5)*(span1.d-span0.d) + (screendist*m*1.0)*(span1.h-span0.h) ) )/
        (span0.d+100+0*(span1.d-span0.d))
        )
        );

        writeln((span1.sy/256.0)-(
( (height*0.5)*(span0.d+100)  + (screendist*m*1.0)*(span0.h-camZ/m)  + 1*( (height*0.5)*(span1.d-span0.d) + (screendist*m*1.0)*(span1.h-span0.h) ) )/
        (span0.d+100+1*(span1.d-span0.d))
        )
        );
        }


        //writeln(span0.sy-(screendist*m*256)/((span1.d+100))*(span0.h+((height*256 div 2)/(span1.hcalc_a) -camZ/m)));
//writeln( span0.sy/256-(height*0.5 + ((screendist*m*1.0)*(span0.h-camZ/m)+0*( (screendist*m*1.0)*(span1.h-span0.h) ) )/(100+span0.d+0*(span1.d-span0.d)) )) ;
        // calculate parameters for prespective-corrected strip
        //s=(a+b*t)/(c+d*t)
        //s*(c+d*t)=(a+b*t)
        //s*c+s*d*t=a+b*t
        //s*d*t-b*t=a-s*c
        //t*(s*d-b)=a-s*c
        //t=(a+s*(-c))/(-b+s*d)
        //
        //
        PS_Params.a:=(height*0.5)*(span0.d+1)  + (screendist*m*1.0)*(span0.h-camZ/m) ;
        PS_Params.b:=-(span0.d+1);
        PS_Params.c:=-( (height*0.5)*(span1.d-span0.d) + (screendist*m*1.0)*(span1.h-span0.h) );
        PS_Params.d:=(span1.d-span0.d);
        //writeln( (PS_Params.a+PS_Params.b*(span0.sy/256))/(PS_Params.c+PS_Params.d*(span0.sy/256)));
        //writeln((span1.sy/256)-(ps_params.a-ps_params.c)/(-ps_params.b+ps_params.d));
        // remove?
        if span0.sy<sy then
        begin{span to be drawn is overclosed}
          {
          rc := (span0.sy-sy)/(span0.sy-span1.sy);//if alg. correct,zero inpossible
          span0.c := span0.c+my_round((span1.c-span0.c)*rc);
          span0.s := span0.s+my_round((span1.s-span0.s)*rc);
          tx0 := tx0+my_round((tx1-tx0)*rc);
          ty0 := ty0+my_round((ty1-ty0)*rc);
          span0.lh := span0.lh+my_round((span1.lh-span0.lh)*rc);
          span0.h := span0.h+my_round((span1.h-span0.h)*rc);
          span0.th := span0.th+my_round((span1.th-span0.th)*rc);
          span0.tba := span0.tba+my_round((span1.tba-span0.tba)*rc);}
          span0.sy := sy;
        end;
        if lightK_shifted>255 then _lightk := 255
        else _lightk := lightk_shifted;

        {$ifndef nofill}
        // draw portion of textured line. Pixels from y1 to y2-1 are drawn.
        //Interpolation coefficient are computed as t=(a+b*y)/(c+d*y)
        // Things is interpolated as x1+t*(x2-x1)
        //procedure TexturedStrip(x,y1,y2,
        //a,b,c,d,
        //tx1,ty1,tx2,ty2,//tecture coordinates
        //dhdx1,dhdy1,dhdx2,dhdy2,// slope
        //tba1,tba2,// texture bumpness
        //lh1,lh2, // light height
        //h1,h2  // height
        //:longint);
        TexturedStrip(sx,sy,span1.sy,
        PS_Params.a,PS_Params.b,PS_Params.c,PS_Params.d,
        tx0,ty0,tx1,ty1,
        span0.dhdx,span0.dhdy, span1.dhdx,span1.dhdy,
        span0.tba,span1.tba,
        span0.lh,span1.lh,
        span0.th, span1.th
        );
        {
        gradvline(sx,sy,span1.sy,  span0.s,span1.s,tx0,ty0,tx1,ty1,
        span0.dhdx,span0.dhdy, span1.dhdx,span1.dhdy,
        span0.tba,span1.tba,
        span0.lh-1000,span1.lh,
        span0.th, span1.th
        //my_round(span0.h*worldparams.ColorScaleA +worldparams.ColorScaleB*mult),
        //my_round(span1.h*worldparams.ColorScaleA +worldparams.ColorScaleB*mult)
        );
        }
        {if my_round(span1.h*worldparams.ColorScaleA +worldparams.ColorScaleB)>(256*mult*v_unit) then begin
        writeln('?, >max');
        end;
        if my_round(span1.h*worldparams.ColorScaleA +worldparams.ColorScaleB)<0 then begin
        writeln('?, <0');
        end;}
        {$endif}
        {$ifdef landgrid1}

        putpixel(sx,sy div 256 -1,255);
        //rendered[sx*height+min(sy div 256 ,height-1)] :={ 255-sy}  255;
        //rendered[sx*height+min(sy div 256 +1,height-1)] := {sy}  255;

        {$endif}
        sy := span1.sy;
        if sy>=height*256 then break;
      end
      else span1.need_to_update := true; {end if};

      {step nearest}
      {
      if 0=nearline then
      begin
      with ix do
      begin
      x := x+dx;
      y := y+dy;
      d := d+dd;
      end;
      end
      else
      if 1=nearline then
      begin
      with iy do
      begin
      x := x+dx;
      y := y+dy;
      d := d+dd;
      end;
      end else
      //if 2=nearline then
      begin
      with ixy do
      begin
      x := x+dx;
      y := y+dy;
      d := d+dd;
      end;
      end;
      }
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
{$ifdef fastrandom}
var RandSeed:longint;
function random(x:longint):longint;assembler;{$ifdef useinline}inline;{$endif}
asm
  mov eax,RandSeed;
  imul eax,RandSeed,$08088405
  inc eax
  mov RandSeed,eax
  mul x
  mov eax,edx
end{$ifdef FPC}['eax','edx']{$endif};
{$endif}

var amplitudes: array[0..31] of real;

var tmp: packed array[0..textureres*textureres-1] of byte;
var tmpi: packed array[0..textureres*textureres-1] of int16;

procedure updateColorScale;
var lowh,highh,i:longint;
begin
  for i:=0 to landres*landres-1 do land[i].th:=land[i].h-land[i].th;

  lowh:=land[0].th;highh:=lowh;
  for i:=1 to landres*landres-1 do begin
    if land[i].th>highh then highh:=land[i].th else
    if land[i].th<lowh then lowh:=land[i].th;
  end;
  worldparams.ColorScaleA:=((255)*v_unit)/(highH-lowH);
  worldparams.ColorScaleB:=-worldparams.ColorScaleA*lowH;

  for i:=0 to landres*landres-1 do land[i].th:=round(land[i].th*worldparams.colorscaleA+worldparams.colorscaleB);

  {writeln('a=',worldparams.ColorScaleA);
  writeln('b=',worldparams.ColorScaleB);}
end;

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
  arrptr(0,0)^:=0;
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
procedure noiseTH;
begin
  towhat := @land[0].th;
  sizeb := sizeof(land);
  mpxb := sizeof(land[0]);
  mpyb := mpxb*landres;
  DoMyNoise(landres2);
end;

(*
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
*)
Procedure noisebump(n:longint);
begin
  towhat := @bumps[0].a[n];
  sizeb := sizeof(bumps);
  mpxb := sizeof(bumps[0]);
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
var x,y,z,tz:longint;dz:longint;
{nx:=(land[mi(x+1,y)].h-land[mi(x-1,y)].h);
ny:=(land[mi(x,y+1)].h-land[mi(x,y-1)].h);
dp:=(nx*A+B)/sqrt(nx*nx+ny*ny+4*v_unit*v_unit);
if dp<0 then dp := -dp div 2;}
begin
  dz:=-round(mult*v_unit*tan(worldparams.sun.altitude));
  for y:=0 to landres-1 do
  begin
    z := -10000*mult;
    for x:=0 to landres*2-1 do
    begin
      tz := mult*land[mi(x,y)].h;
      if tz>z then
      begin{� ⥭� ? Shadow test}
        z := tz;
        if x>landres+1 then break;{���,��� ��ન ��⥭�,����᪮� �த���� All hills done in this span}
      end;
      land[mi(x,y)].lh := z div mult;
      z := z+dz;
      land[mi(x,y)].c :=255;
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
      sqr( {real}( h(x,y)-0.25*( h(x-1,y-1)+h(x+1,y-1)+h(x-1,y+1)+h(x+1,y+1) ) ) )+
      sqr( {real}( h(x,y)-0.25*( h(x-1,y-1)+h(x+1,y-1)+h(x-1,y+1)+h(x+1,y+1) ) ) )
      );{(1/v_unit)*sqrt(sqr(h(x,y+1)-h(x,y-1))+sqr(h(x+1,y)-h(x-1,y)));}
      v:=v*600;
      {if v>255 then v:=255;}
      {land[mi(x,y)].tba}
      tmpi[mi(x,y)]:=my_round(v){255};
    end;
  end;
  for y:=0 to textureres-1 do begin
    for x:=0 to textureres-1 do begin
      land[mi(x,y)].tba:=
      min(255,
      (tmpi[mi(x,y)]+{1/2}
      (tmpi[mi(x,y-1)]+tmpi[mi(x,y+1)]+tmpi[mi(x-1,y)]+tmpi[mi(x+1,y)]
      ) div 6)div 2{1/3} +
      (tmpi[mi(x-1,y-1)]+tmpi[mi(x-1,y+1)]+tmpi[mi(x+1,y-1)]+tmpi[mi(x
      +1,y-1)]) div 24);
    end;
  end;
end;
procedure makelighttables;
var x,y:longint;
dprod1,dprod2,dprod3,a,b:real;lit,shadow:colorRGB;
begin
  a:=cos(worldparams.sun.altitude);
  b:=sin(worldparams.sun.altitude);
  for y:=-128 to 127 do
  begin
    for x:=-128 to 127 do
    begin
      dprod1:=
      (x*a+derivative_unit*b)/(sqrt
      (x*x+y*y+derivative_unit*derivative_unit));
      dprod2:=derivative_unit/sqrt
      (x*x+y*y+derivative_unit*derivative_unit);
      dprod3:=-dprod1{-(x*a+derivative_unit*b)/(sqrt
      (x*x+y*y+derivative_unit*derivative_unit))};
      if dprod1<0 then dprod1:=0;
      if dprod2<0 then dprod2:=0;
      if dprod3<0 then dprod3:=0;
      with worldparams do
      begin
        shadow.r:=dprod2*abovelightcolor.r+dprod3*reverselightcolor.r;
        shadow.g:=dprod2*abovelightcolor.g+dprod3*reverselightcolor.g;
        shadow.b:=dprod2*abovelightcolor.b+dprod3*reverselightcolor.b;
        lit.r:={shadow.r+}dprod1*sunlightcolor.r;
        lit.g:={shadow.g+}dprod1*sunlightcolor.g;
        lit.b:={shadow.b+}dprod1*sunlightcolor.b;
        sunlight[x and 255+(y and 255)*256].r :=lit.r;
        sunlight[x and 255+(y and 255)*256].g :=lit.g;
        sunlight[x and 255+(y and 255)*256].b :=lit.b;
        shadowlight[x and 255+(y and 255)*256].r :=shadow.r;
        shadowlight[x and 255+(y and 255)*256].g :=shadow.g;
        shadowlight[x and 255+(y and 255)*256].b :=shadow.b;
      end;
    end;
  end;
end;

var n,x,y,z,dx,dy,lowh,highh: longint;
begin
  RandSeed:=worldparams.generator.seed;
  if not initialized_settings.sun then begin
    initialized_settings.sun:=true;
    initialized_settings.SunTables:=false;
    initialized_settings.LandscapeShadows:=false;
    initialized_settings.TextureShadows:=false;
  end;


  {landelevation:=1000;}
  (***fillchar(land,sizeof(land),0);
  fillchar(texture,sizeof(texture),0);
  fillchar(texturebump,sizeof(texturebump),0);***)
  for n:=0 to 31 do amplitudes[n] := 0;

  {for n:=0 to 6 do amplitudes[n]:=1;}
  if not initialized_settings.landscape then begin
    amplitudes[0] := 0.2;
    amplitudes[1] := 0.5;
    amplitudes[2] := 0.5;
    amplitudes[3] := 0.5;
    amplitudes[4] := 0.5;
    amplitudes[5] := 0.5;
    amplitudes[6] := 0.5;
    {amplitudes[5]:=1;}
    write('generating landscape ');
    noiselandscape;

    amplitudes[0] := 0.0;
    amplitudes[1] := 0.03;
    amplitudes[2] := 0.04;
    amplitudes[3] := 0.05;
    amplitudes[4] := 0.05;
    amplitudes[5] := 0.05;
    amplitudes[6] := 0.05 {.2};
    amplitudes[7] := 0.03 {.3};

    noiseth;
    {readln;}

    for y:=0 to landres-1 do begin
      for x:=0 to landres-1 do begin
        n:=mi(x,y);
        // canyons
        // land[mi(x,y)].h:=my_round(land[mi(x,y)].h/sqrt(1+(1/(v_unit*v_unit*1))*sqr(land[mi(x,y)].h)));
        //       x*(1+x/sqrt(x*x+1))
        //glaciate
        // z:=land[n].h;
        // land[n].h:=my_round(z*0.5*(1+z/sqrt(sqr(z)+sqr(v_unit*50))));
      end;
    end;

    UpdateColorScale;
    writeln('done.');
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
        //putpixel(x,y,(land[mi(x,y)].tba)*$010101);
        putpixel(x,y,((land[mi(x,y)].th)div v_unit) *$010101);
      end;
    end;

    for x:=0 to rendered.xcount-1 do CallColumnUpdate(x,x+1);

    {readln;}

    initialized_settings.BumpAmpitudeMap:=true;
  end;

  //readln;
  (***)
  if not initialized_settings.FogLayersData then begin
    write('preparing clouds simulator 4');
    amplitudes[0] := 0.3;
    amplitudes[1] := 0.5;
    amplitudes[2] := 0.75;
    amplitudes[3] := 0.5;
    amplitudes[4] := 0.5;
    amplitudes[5] := 0{.7};
    amplitudes[6] := 0{.7};
    amplitudes[7] := 0;
    amplitudes[8] := 0;
    noisefog(0);
    write(' 3');
    amplitudes[0] := 0.3;
    amplitudes[1] := 0.5;
    amplitudes[2] := 0.6;
    amplitudes[3] := 0.5;
    amplitudes[4] := 0.1;
    amplitudes[5] := 0{.1};
    amplitudes[6] := 0{.1};
    amplitudes[7] := 0;
    amplitudes[8] := 0;
    noisefog(1);
    write(' 2');
    amplitudes[0] := 0.7;
    amplitudes[1] := 0.7;
    amplitudes[2] := 0.7;
    amplitudes[3] := 0.7;
    amplitudes[4] := 0.6;
    amplitudes[5] := 0{.5};
    amplitudes[6] := 0{.4};
    amplitudes[7] := 0{.3};
    amplitudes[8] := 0{.2};
    noisefog(2);
    write(' 1');
    amplitudes[0] := 0.3;
    amplitudes[1] := 0.7;
    amplitudes[2] := 0.7;
    amplitudes[3] := 0.5;
    amplitudes[4] := 0.4;
    amplitudes[5] := 0.1;
    amplitudes[6] := 0.2;
    amplitudes[7] := 0.1;
    amplitudes[8] := 0.1;
    noisefog(3);
    writeln(' 0 ,done');  (***)
    initialized_settings.FogLayersData:=true;
  end;


  {for n:=0 to numfoglayers*2-1 do
  begin
  noisefog(n);
  write('.');
  end;}
  {}

  writeln;
  if not initialized_settings.SunTables then begin
    initialized_settings.TextureShadows:=false;
    write('Calculating lightness tables,');
    makelighttables;
    writeln('done.');
    initialized_settings.SunTables:=true;
  end;

  if not initialized_settings.Texture then
  begin
    initialized_settings.TextureShadows:=false;

    write('generating textures...');
    (*
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
    fillchar(texture,sizeof(texture),255);
    noisetexturemap;
    //should be called before noisetexturebump because uses texturebump as temp

    *)
    amplitudes[0] := 0.01;
    amplitudes[1] := 0.02;
    amplitudes[2] := 0.02;
    amplitudes[3] := 0.02;
    amplitudes[4] := 0.02;
    amplitudes[5] := 0.02;
    amplitudes[6] := 0.02;
    amplitudes[7] := 0.01;
    amplitudes[8] := 0.01;
    amplitudes[9] := 0;
    amplitudes[10] := 0;
    noisebump(0);
    {amplitudes[0] := 0.4;
    amplitudes[1] := 0.5;
    amplitudes[2] := 0.5;
    amplitudes[3] := 0.5;
    amplitudes[4] := 0.5;
    amplitudes[5] := 0.5;
    amplitudes[6] := 0.5;
    amplitudes[7] := 0.5;}

    amplitudes[0] := 0.3;
    amplitudes[1] := 0.4;
    amplitudes[2] := 0.45;
    amplitudes[3] := 0.45;
    amplitudes[4] := 0.5;
    amplitudes[5] := 0.45;
    amplitudes[6] := 0.45;
    amplitudes[7] := 0.45;{}
    amplitudes[8] := 0.45;{}
    amplitudes[9] := 0.45;{}
    amplitudes[10] := 0.25;{}
    noisebump(1);


    {$ifdef testtexture}
    {x arrow}
    for y:=10 to 16 do
    for x:=10 to textureres-10-abs(y-13) do
    begin
      //texture[mi(x,y)] := 128;
      inc(bump[mi(x,y)],v_unit*3);
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
        dx := (bumps[mi(x+1,y)].a[0]-bumps[mi(x-1,y)].a[0])*derivative_unit div (2*
        v_unit);
        dy := (bumps[mi(x,y+1)].a[0]-bumps[mi(x,y-1)].a[0])*derivative_unit div (2*
        v_unit);
        derivatives[x+y*textureres].a[0] := dx and 255 +(dy and 255)*256;
        dx := (bumps[mi(x+1,y)].a[1]-bumps[mi(x-1,y)].a[1])*derivative_unit div (2*
        v_unit);
        dy := (bumps[mi(x,y+1)].a[1]-bumps[mi(x,y-1)].a[1])*derivative_unit div (2*
        v_unit);
        derivatives[x+y*textureres].a[1] := dx and 255 +(dy and 255)*256;
        {z:=round((1/v_unit)*255*sqrt(sqr(dx)+sqr(dy)));
        if z>255 then z:=255;
        bumpselector[x+y*textureres]:=z-128;}
      end;
    end;
    writeln('done.');
    Initialized_settings.Texture:=true;
    Initialized_settings.BumpSelectorSubmap:=false;
  end{};

  {if not initialized_settings.BumpSelectorSubmap then begin
  for y:=0 to textureres-1 do begin
  for x:=0 to textureres-1 do begin
  putpixel(x,y,(128+BumpSelector[mi(x,y)])*$010101);
  end;
  end;
  for x:=0 to rendered.xcount-1 do CallColumnUpdate(x,x+1);
  readln;
  initialized_settings.BumpSelectorSubmap:=true;
  end;}

  if not initialized_settings.TextureColor then begin

    for y:=0 to 255 do begin
      for x:=0 to 255 do begin
        if x>5+(y div 4) then begin
          TextureColor[x+y*256]:=128*$010101+random(5)+random(5)shl 8+random(5) shl 16;{+(((y) and 1)*8)shl 16};
          end else begin
          TextureColor[x+y*256]:=$FFFFFF;
        end;
      end;
    end;
    initialized_settings.TextureColor:=true;
    writeln('Colormap unspecified,using default colormap.');
    //readln;
  end;
  {if not Initialized_settings.Texture}
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


  {draw-time generator params}
  fogX := -24282951;
  dfX := -341;
  fogY := 72920064;
  dfY := 1024;
  foge := 208;
  snownoise := 4;
  texturesize := 10;
  (*
  sunR := 250{450}{200};
  sunG := 150{300}{230}{220};
  sunB := 50{200}{255};

  skyR := {10}0;
  skyG := {20}0;
  skyB := {40}0;
  *)

  worldparams.sunlightcolor.r:=0.9;
  worldparams.sunlightcolor.g:=0.85;
  worldparams.sunlightcolor.b:=0.8;
  worldparams.abovelightcolor.r:=0.1;
  worldparams.abovelightcolor.g:=0.12;
  worldparams.abovelightcolor.b:=0.14;
  worldparams.reverselightcolor.r:=0.14;
  worldparams.reverselightcolor.g:=0.12;
  worldparams.reverselightcolor.b:=0.1;
  setFarDist(256);

  fogcolor.r:=200*256;
  fogcolor.g:=200*256;
  fogcolor.b:=200*256;
  worldparams.FogColor.r:=fogcolor.r/(255*256);
  worldparams.FogColor.g:=fogcolor.g/(255*256);
  worldparams.FogColor.b:=fogcolor.b/(255*256);



  {bumpcolors[0].r:=64*256;
  bumpcolors[0].g:=48*256;
  bumpcolors[0].b:=26*256;
  bumpcolors[1].r:=64*256;
  bumpcolors[1].g:=48*256;
  bumpcolors[1].b:=26*256;}




  {end from include}
  worldparams.sun.heading:=pi/2;
  worldparams.sun.altitude:=0.252;
end;
(******************************)
(*****************v 0.5 Procedures********************)
procedure setSeed(s:longint);
begin
  writeln('vw.setseed ',s,' called');
  worldparams.generator.seed:=s;
end;
procedure init;
begin
  writeln('vw.init called');
  if Rendered.base<>nil then begin
    freemem(rendered.base);fillchar(rendered,sizeof(rendered),0);
  end;
  if light<>nil then begin freemem(light);light:=nil;end;
  if skylight<>nil then begin freemem(skylight);skylight:=nil;end;
  writeln('memory freed');
  rendered.base:=nil;
  InitParams;
  InitTables;
  writeln('fillbyte');
  fillbyte(rendered,sizeof(rendered),0);
  fillbyte(initialized_settings,sizeof(initialized_settings),0);
  LandscapeElevation.base:=@(land[0].h);
  LandscapeElevation.mx:=sizeof(land[0]);
  LandscapeElevation.my:=sizeof(land[0])*landres;
  LandscapeElevation.xcount:=landres;
  LandscapeElevation.ycount:=landres;
  writeln('vw.init done');
end;
procedure uninit;
begin
  writeln('vw.uninit called');
  if Rendered.base<>nil then begin
    freemem(rendered.base);fillchar(rendered,sizeof(rendered),0);
    freemem(light);
    freemem(skylight);
  end;
end;

procedure renderport(projection:n_projections;w,h:longint);
begin

  if phases >= w then phases:=w-1;
  writeln('vw.renderport w=',w,' h=',h,' called');
  if (w<1)or(h<1)then exit;
  case projection of
    n_biconepanoram:runerror(255);(*unsupported*)
    n_stdperspective:begin
      //writeln ('what hell');readln;
      if assigned(rendered.base) then
      freemem(rendered.base);
      rendered.base:=nil;
      if light<>nil then freemem(light);
      light:=nil;
      writeln('allocating image buffer');
      GetMem(rendered.base,w*h*4);
      writeln('allocating light buffer');
      GetMem(light,h*4);(****)
      GetMem(skylight,h*sizeof({skylight^[0]}colorRGB));(****)
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
  UpdateColorScale;
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

  {
  e:=(1/(mult*v_unit))* ( a*mult+ (b-a)*(_Y and(mult-1)) );
  f:=(1/(mult*v_unit))* ( c*mult+ (d-c)*(_Y and(mult-1)) );

  z:=e+  (f-e)*(_X and(mult-1))*(1/mult);
  }
  if((_X and (mult-1)) + (_Y and (mult-1)) < mult) then begin
    z := (a + (b-a)*(_y and (mult-1))*(1.0/mult) + (c-a)*(_x and (mult-1))*(1.0/mult))*(1.0/v_unit);
    end else begin
    z := (d + (b-d)*(1.0-(_X and (mult-1))*(1.0/mult)) + (c-d)*(1.0-(_Y and (mult-1))*(1.0/mult)))*(1.0/v_unit);		
  end;

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
  {
  e:=(1/(mult*v_unit))* ( a*mult+ (b-a)*(_Y and(mult-1)) );
  f:=(1/(mult*v_unit))* ( c*mult+ (d-c)*(_Y and(mult-1)) );

  result:=e+  (f-e)*(_X and(mult-1))*(1/mult);
  }
  if((_X and (mult-1)) + (_Y and (mult-1)) < mult) then begin
    result := (a + (b-a)*(_y and (mult-1))*(1.0/mult) + (c-a)*(_x and (mult-1))*(1.0/mult))*(1.0/v_unit);
    end else begin
    result := (d + (b-d)*(1.0-(_X and (mult-1))*(1.0/mult)) + (c-d)*(1.0-(_Y and (mult-1))*(1.0/mult)))*(1.0/v_unit);		
  end;
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
var land_z,dx,dy:real;
begin
  {$ifdef trep}
  writeln('vw.setcameraposition x=',x:2:2,' y=',y:2:2,' z=',z:2:2,' a=',_a:2:2,' called');
  {$endif}
  worldparams.cam.x:=x;
  worldparams.cam.y:=y;
  worldparams.cam.z:=z;
  worldparams.cam.a:=_a;
  camX:=my_round(x*mult);
  camY:=my_round(y*mult);
  camZ:=my_round(z*mult);
  angle:=_a;	
  z:=0;
  getLandscapePoint(camx/mult,camy/mult,land_z,dx,dy);
  {ifdef trep}  writeln('land_z=',land_z:5:5,' altitude=',camz/mult - land_z);{endif}
end;
procedure SetSunLightColor(r,g,b:real);{0..1}
begin
  worldparams.sunlightcolor.r:=r;
  worldparams.sunlightcolor.g:=g;
  worldparams.sunlightcolor.b:=b;
  initialized_settings.suntables:=false;
end;
procedure SetAboveLightColor(r,g,b:real);{0..1}
begin
  worldparams.abovelightcolor.r:=r;
  worldparams.abovelightcolor.g:=g;
  worldparams.abovelightcolor.b:=b;
  initialized_settings.suntables:=false;
end;
procedure SetReverseLightColor(r,g,b:real);{0..1}
begin
  worldparams.reverselightcolor.r:=r;
  worldparams.reverselightcolor.g:=g;
  worldparams.reverselightcolor.b:=b;
  initialized_settings.suntables:=false;
end;
procedure SetTextureColor(const a:Tcolortable);
begin
  texturecolor:=a;
  initialized_settings.TextureColor:=true;
end;
procedure SetFogColor(r,g,b:real);{0..1}
begin
  if r>1 then r:=1 else if r<0 then r:=0;
  if g>1 then g:=1 else if g<0 then g:=0;
  if b>1 then r:=1 else if b<0 then b:=0;
  FogColor.r:=round(r*255*256);
  FogColor.g:=round(g*255*256);
  FogColor.b:=round(b*255*256);
  worldparams.FogColor.r:=r;
  worldparams.FogColor.g:=g;
  worldparams.FogColor.b:=b;

  if FogColor.r>$FF00 then FogColor.r:=$FF00;
  if FogColor.g>$FF00 then FogColor.g:=$FF00;
  if FogColor.b>$FF00 then FogColor.b:=$FF00;
end;
procedure SetFogTime(t:real);
begin
  worldparams.fogt:=t;
end;
procedure SetFogdtime(dt:real);
begin
  worldparams.fogdt:=Dt;
end;

procedure Sun(d,h:real;r,g,b:real);
begin
end;
procedure SetSunPosition(Heading,Altitude:real);
begin
  worldparams.sun.heading:=Heading;
  worldparams.sun.altitude:=Altitude;
  initialized_settings.LandscapeShadows:=false;
  initialized_settings.SunTables:=false;
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
  {ifdef trep}  writeln('z=',z:5:5,' altitude=',camz/mult - z);{endif}
  z:=(z+0.5)*mult;
  {$ifndef noclip}
  if CamZ<z then CamZ:=my_round(z);
  {$endif}
end;
procedure render;
begin
  Fog100color:=((fogcolor.b shr 8) and $FF)+(fogcolor.g and $FF00) + ((fogcolor.r shl 8)and $FF0000);
  if rendered.base=nil then exit;
  {ifdef trep}writeln('vw.render called');{endif}
  initdefaults;
  {ifdef trep}writeln('camZ=',camZ div mult);{endif}
  case zoommode of
    ZoomDiagonal:cscreendist:=my_round(zoom*sqrt(width*width+height*height));
    ZoomHorisontal:cscreendist:=my_round(zoom*width);
    ZoomVertical:cscreendist:=my_round(zoom*height);
  end;
  FogX:=my_round(dfX*worldparams.fogt);
  FogY:=my_round(dfY*worldparams.fogt);
  _render;

  CallRenderedUpdate;

  worldparams.fogt:=worldparams.fogt+worldparams.fogdt;

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
  worldparams.cam.f:=fd;
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
  if n>0 then phases:=n;
end;
procedure ReinitLand;
begin
  fillchar(initialized_settings,sizeof(initialized_settings),0);
end;
(******************************************************)

begin
  SetFogTime(0);
  SetFogdtime(1);
  worldparams.generator.seed:=0;
  init;
end.
/*

s=(a+b*t)/(c+d*t)
s*(c+d*t)=(a+b*t)
s*c+s*d*t=a+b*t
s*d*t-b*t=a-s*c
t=(a-s*c)/(s*d-b)
*/
