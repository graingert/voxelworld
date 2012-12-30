(*  Voxel World - keyboard and mouse.       (C) 2004-2008 Dmytry Lavrov     *)
(*  http://dmytry.pandromeda.com                                            *)

unit vwkm;
interface
uses vw;
procedure ProcessKey(key:char;excode:longint);
procedure ProcessMouse(x,y : vw.real; k:longint);
{0,0 are center of screen; +1,+Y are upper right and -1,-Y are lower left}
{y may depend to viewport size}
procedure ProcessMouse2(dx,dy,k:longint);{mouse delta as is,in screen pixels}
var dt:real;
const
ExtKey_F1     =$3B;
ExtKey_F2     =$3C;
ExtKey_F3     =$3D;
ExtKey_F4     =$3E;
ExtKey_F5     =$3F;
ExtKey_F6     =$40;
ExtKey_F7     =$41;
ExtKey_F8     =$42;
ExtKey_F9     =$43;
ExtKey_F10    =$44;
ExtKey_F11    =$85;
ExtKey_F12    =$86;
ExtKey_HOME   =71;
ExtKey_UP     =72;
ExtKey_PAGE_UP=73;
ExtKey_LEFT   =75;
ExtKey_RIGHT  =77;
ExtKey_END    =79;
ExtKey_DOWN   =80;
ExtKey_PAGE_DOWN=81;
ExtKey_INSERT   =82;
implementation
var txsize:longint;
const astep=5 *pi/180;
const camh=0.5;
{var cam:record
  x,y,z,a:vw.real;
  vel:record
  x,y,z,a:vw.real;
  end;
end;}
var
camsurf:boolean;
procedure camBounce;
var lh:real;
begin
  lh:=vw.landH(worldparams.cam.x,worldparams.cam.y)+camh;
{
  camsurf:=camsurf or( worldparams.cam.z<lh);
  if camsurf then begin
    worldparams.cam.z:=lh;
  end;
}
  vw.SetCameraPosition4(worldparams.cam.x,worldparams.cam.y,worldparams.cam.z,worldparams.cam.a);
end;
procedure ProcessKey(key:char;excode:longint);
const js=0.1;vjs=0.1;
begin
  case key of
    'r':begin
     vw.SetSeed(vw.GetClocks);
     vw.ReinitLand;
     vw.initialized_settings.TextureColor:=true;
    end;
    't':if txsize<14 then begin
      inc(txsize);
      //vw.setTextureSizeParam(txsize);
    end;
    'T':if txsize>0 then begin
      dec(txsize);
      //vw.setTextureSizeParam(txsize);
    end;
    'g':camSurf:=true;
    #0:with worldparams do begin
      case excode of
        ExtKey_Up:
        begin
          cam.x := cam.x-sin(cam.a)*js;
          cam.y := cam.y+cos(cam.a)*js;
        end;
        ExtKey_End:
        begin
          cam.x := cam.x-cos(cam.a)*js;
          cam.y := cam.y-sin(cam.a)*js;
        end;
        ExtKey_Page_down:
        begin
          {a:=a-0.03;}
          cam.x := cam.x+cos(cam.a)*js;
          cam.y := cam.y+sin(cam.a)*js;
        end;
        ExtKey_Down:
        begin
          cam.x := cam.x+sin(cam.a)*js;
          cam.y := cam.y-cos(cam.a)*js;
        end;
        ExtKey_home:
        begin
          cam.z := cam.z-vjs;
        end;
        ExtKey_Page_Up:
        begin
          cam.z := cam.z+vjs;
          camsurf := false;
        end;
        ExtKey_Left:
        begin
          cam.a:=cam.a+astep;
        end;
        ExtKey_Right:
        begin
          cam.a:=cam.a-astep;
        end;
        else exit;
      end{case excode of};
    end{excode};
    {else:exit;}
  end{case key of};
  camBounce;
  vw.setTextureSizeParam(txsize);
end;
procedure ProcessMouse(x,y : vw.real; k:longint);
const MAS=30*pi/180 ;MLS=10;
var js:real;
begin
  with worldparams do
  case K of
    0:begin
    end;
    1:begin
      js:=MLS*y;
      cam.x:=cam.x-sin(cam.a)*js*dt;
      cam.y:=cam.y+cos(cam.a)*js*dt;
      cam.a:=cam.a-x*MAS*dt;
    end;
    2,4:begin
      js:=MLS*x;
      cam.x:=cam.x+cos(cam.a)*js*dt;
      cam.y:=cam.y+sin(cam.a)*js*dt;
      cam.z:=cam.z+y*MLS*dt;
      camSurf:=false;
    end;
    3:begin
    end;
  end{case};
  CamBounce;
end;
procedure ProcessMouse2(dx,dy,k:longint);
const MAS=0.2*pi/180 ;MLS=0.02{5};
var js:real;
begin
  with worldparams do
  case K of
    0:begin
    end;
    1:begin
      js:=MLS*dy;
      cam.x:=cam.x-sin(cam.a)*js*dt;
      cam.y:=cam.y+cos(cam.a)*js*dt;
      cam.a:=cam.a-dx*MAS*dt;
    end;
    2:begin
      js:=MLS*dx;
      cam.x:=cam.x+cos(cam.a)*js*dt;
      cam.y:=cam.y+sin(cam.a)*js*dt;
      cam.z:=cam.z+dy*MLS*dt;
      camSurf:=false;
    end;
    3:begin
    end;
  end{case};
  CamBounce;
end;
begin
  camSurf:=false;
  dt:=1;
  txsize:=10;
end.
