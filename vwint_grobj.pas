(*  Voxel World interactive viewer, obsolete (directx based).       (C) 2004-2008 Dmytry Lavrov     *)
(*  http://dmytry.pandromeda.com                                                                    *)
{$apptype console}
{$ifdef win32}{$define broken_objscr}{$endif}
uses graphobj,objscr,keyb,msmouse,
vw,
vwkm,
vwscriptu,copr;
{$ifdef broken_objscr}
const maxmode=5;
const modelist:array[1..maxmode] of string =
(
'640x480',
'800x600',
'1024x768',
'1280x1024',
'1600x1200'
);
procedure SelectMode;
var i:longint;rk:string;
label again;
begin
  again:
  Writeln('Select videomode:');
  for i:=1 to maxmode do begin
    writeln(i:2,' ',modelist[i]);
  end;
  writeln('Or press q to quit');
  readln(rk);
  case rk[1] of
    '1':if not initscreen(640,480,32) then goto again;
    '2':if not initscreen(800,600,32) then goto again;
    '3':if not initscreen(1024,768,32) then goto again;
    '4':if not initscreen(1280,1024,32) then goto again;
    '5':if not initscreen(1600,1200,32) then goto again;
    'q':halt(0);
    else goto again;
  end;
end;
{$endif}
function min(x,y:longint):longint;
begin
  if x>y then min:=y else min:=x;
end;
procedure ShowVline(x1,x2:longint;uv:longint);
var i,il,o:vw.puint32;
begin

if (x1<0) or (x1>screen.max.x) then exit;
  i :=puint32(longint(vw.rendered.base)+vw.rendered.mx*x1);
  {il:=pointer(longint(vw.rendered.base)+vw.rendered.mx*x1+vw.rendered.my*min(vw.rendered.ycount-1,screen.max.y));}
  o:=puint32(longint(
  longint(screen.videoptr)+4*x1+screen.lenlineB*min(screen.Max.Y,vw.rendered.ycount-1)
  ));
  repeat
    o^:=i^;
    inc(longint(i),rendered.my);
    dec(longint(o),screen.lenlineB);
  until dword(o)<dword(screen.videoptr);

end;

var rk:char;rkext:longint;
var mX,mY,mK,lmX,lmY,lmK,cx,cy: longint;
Var CSMP{Can Set Mouse Pos?}:boolean;n:longint;
f:text;
inname:string;
begin
  vwscriptu.init;
  if (paramstr(1)='')or(paramstr(1)='--m1') then inname:='default.vws' else inname:=paramstr(1);
  assign(f,inname);
  {$i-}
  reset(f);
  if ioresult=0 then begin
    writeln('"',inname,'" found,executing');
    vwscriptu.executefile(f);
  end else writeln('"',inname,'" is not found!');
  close(f);
  ioresult;
  {$i+}


  {$ifdef broken_objscr}
  selectmode;
  {$else}
  if not initscreen(0,0,32)then begin
    writeln('can''t set videomode');
    halt(1);
  end;
  {$endif}
  {vwscriptu.executeinput;}

  if (screen.max.x=0)or(screen.Max.y=0) then halt(1);
  writeln(screen.max.x,' ',screen.max.y);

  screen.fontback:=1;
  cx:=screen.max.x div 2; cy:=screen.max.y div 2;
  initmouse;
  if paramstr(1)<>'--m1' then begin
  for n:=1 to 5 do begin{to be sure}
    SetMousePos(0,0);GetMouseState(mx,my,mk);
    if (mx=0) and (my=0) then begin
      SetMousePos(cx,cy);
      GetMouseState(mx,my,mk);
      CSMP:=(mx=cx)and(my=cy);
      if CSMP then break;
    end;
  end;
  end else CSMP:=false;
  if not CSMP then begin writeln('Can''t set mouse position,alternative controls used  [press enter to continue]');readln;end;

  vw.RenderPort(n_StdPerspective,screen.max.x+1,screen.max.y+1);
  vw.SetColumnUpdateProc(@ShowVline,0);


  repeat
    vw.render;
    screen.moveto(0,0);
    if keypressed then begin
      rk:=readkey;rkext:=0;
      case rk of
        #13:
        begin
          Writeln('Enter commands for Voxel World Script');
          Writeln('close input by entering "#eof" or natural EOF marker (ascii 26) (Ctrl-z under dos & linux)');
          vwscriptu.executeINPUT;
        end{CRkey};
        #0:
        begin
          rkext:=ord(readkey);
        end;
      end;
      vwkm.ProcessKey(rk,rkext);
    end{if keypressed};
    GetMouseState(mx,my,mk);
    if csmp then
    begin
      if (mx<>cx)or(my<>cy)or(mk<>lmk) then begin
        if (mk<>0)and (lmk=0) then begin
          hidemouse;
          lmx:=mx;lmy:=my;
          setmousepos(cx,cy);
        end else
        if (mk=0)and (lmk<>0) then
        begin
          setmousepos(lmx,lmy);
          showmouse;
          end else if (mk<>0)and(lmk<>0) then begin
          vwkm.ProcessMouse2(mx-cx,cy-my,mk);
          setmousepos(cx,cy);
        end;
        lmk:=mk;
      end;
    end else{csmp}
    begin
      if (mx<>lmx)or(my<>lmy)or(mk<>lmk) then begin
        lmx:=mx;  lmy:=my;  lmk:=mk;
      end;
      vwkm.ProcessMouse((mx*2)/screen.max.x -1 , 1- (my*2)/screen.max.x ,mk );
    end;{csmp}

  until rk=#27;
  writeln(stderr,'Esc pressed');
  vwscriptu.uninit;
end.
