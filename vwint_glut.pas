(*  Voxel World , interactive viewer (GLUT).       (C) 2004-2008 Dmytry Lavrov     *)
(*  http://dmytry.pandromeda.com                                                   *)

{$apptype console}
{define broken_glut}
uses glut,gl,
vw,
vwkm,
vwscriptu{$ifndef FPC},fpcprocs,glutargs{$endif};
{$ifdef broken_glut}
{$define glh_1}
{I haven't downloaded rencent glut headertranslations for windows. So...}
{part of GLUT header}
const
       GLUT_KEY_F1 = 1;
       GLUT_KEY_F2 = 2;
       GLUT_KEY_F3 = 3;
       GLUT_KEY_F4 = 4;
       GLUT_KEY_F5 = 5;
       GLUT_KEY_F6 = 6;
       GLUT_KEY_F7 = 7;
       GLUT_KEY_F8 = 8;
       GLUT_KEY_F9 = 9;
       GLUT_KEY_F10 = 10;
       GLUT_KEY_F11 = 11;
       GLUT_KEY_F12 = 12;
    { directional keys  }
       GLUT_KEY_LEFT  = 100;
       GLUT_KEY_UP    = 101;
       GLUT_KEY_RIGHT = 102;
       GLUT_KEY_DOWN  = 103;
       GLUT_KEY_PAGE_UP   = 104;
       GLUT_KEY_PAGE_DOWN = 105;
       GLUT_KEY_HOME      = 106;
       GLUT_KEY_END = 107;
       GLUT_KEY_INSERT = 108;
const
       GLUT_LEFT_BUTTON = 0;
       GLUT_MIDDLE_BUTTON = 1;
       GLUT_RIGHT_BUTTON = 2;
    { Mouse button  state.  }
       GLUT_DOWN = 0;
       GLUT_UP = 1;

{$macro on}
{$define glutexternal:= stdcall;external 'glut32.dll' }

procedure glutSolidTeapot(size:GLdouble);glutexternal;
procedure glutMouseFunc(func:pointer);glutexternal;
procedure glutSpecialFunc(func:pointer);glutexternal;
//glutMotionFunc(void  (GLUTCALLBACK *func)(int x, int y));
procedure glutMotionFunc(func:pointer);glutexternal;

{$macro off}
{$endif}
{/part of GLUT header}

var screen:array2Ddesc;
function min(x,y:longint):longint;
begin
if x>y then min:=y else min:=x;
end;
procedure putpixel(x,y:longint;c:uint32);
begin
 if (dword(x)<dword(screen.xcount))and(dword(y)<dword(screen.ycount)) then begin
  puint32(longint(screen.base)+screen.mx*x+screen.my*y)^:=c ;
 end;
end;
procedure ShowVline(x1,x2:longint;uv:longint);
var i,il,o:puint32;
begin
if screen.base=nil then exit;
{lRasterPos2d(x1,rendered.ycount-1);
  glDrawPixels(1,rendered.ycount,GL_RGBA,);}
 if dword(x1)<dword(screen.xcount) then begin
 i:=puint32(longint(vw.rendered.base)+vw.rendered.mx*x1);
 o:=puint32(
 longint(screen.base)+screen.mx*x1+screen.my*min(screen.ycount-1,vw.rendered.ycount-1)
 );
 repeat
  o^:=  i^ and $FF00FF00+(i^ shr 16)and $FF +((i^)shl 16)and $FF0000;
  inc(longint(i),rendered.my);
  dec(longint(o),screen.my);
 until dword(o)<dword(screen.base);
 end;
end;

var key_locked:boolean;
procedure notify_MT_bug;
begin
writeln('(multitasking jokes?) .handler called second time before other ended.To avoid bugs,message has been missed.');
readln;
end;
{$ifdef fpc}
{$macro on}
{ note:doesn't guarantere anything without EnterCritical and LeaveCritical pair :( }
{$define bnmt:=if not key_locked then begin key_locked:=true}
{$define enmt:=key_locked:=false;end else notify_MT_bug}
{$else}
procedure bnmt;
begin
if key_locked then notify_MT_bug;
key_locked:=true;
end;
procedure enmt;
begin
key_locked:=false;
end;
{$endif}

procedure reshape(width,height:longint);cdecl;
begin
  bnmt;
    glViewport(0, 0, width, height);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho (0, width, height,0  , -100, 100);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    if assigned(screen.base) then freemem(screen.base);
    screen.base:=nil;
    GetMem(screen.base,width*height*4);
    filldword(screen.base^,width*height,$FFFFFFFF);
    screen.mx:=4;
    screen.my:=screen.mx*width;
    screen.xcount:=width;
    screen.ycount:=height;
    vw.renderport(n_stdPerspective,width,height);
  enmt;
end;

procedure key({$ifdef glh_1}k:char{$else}k:byte{$endif};x,y:longint);cdecl;
begin
  bnmt;
  case {$ifdef glh_1}k{$else}chr(k){$endif} of
    #27:halt(0);
    #13:begin
      vwscriptU.executeINPUT;
      end;
  end;
  vwkm.ProcessKey({$ifdef glh_1}k{$else}chr(k){$endif},0);
  enmt;
end;
procedure skey(k,x,y:longint);cdecl;
var k2:longint;
begin
bnmt;
 case k of
  GLUT_KEY_F1 :k2:=ExtKey_F1;
  GLUT_KEY_F2 :k2:=ExtKey_F2;
  GLUT_KEY_F3 :k2:=ExtKey_F3;
  GLUT_KEY_F4 :k2:=ExtKey_F4;
  GLUT_KEY_F5 :k2:=ExtKey_F5;
  GLUT_KEY_F6 :k2:=ExtKey_F6;
  GLUT_KEY_F7 :k2:=ExtKey_F7;
  GLUT_KEY_F8 :k2:=ExtKey_F8;
  GLUT_KEY_F9 :k2:=ExtKey_F9;
  GLUT_KEY_F10:k2:=ExtKey_F10;
  GLUT_KEY_F11:k2:=ExtKey_F11;
  GLUT_KEY_F12:k2:=ExtKey_F12;
  GLUT_KEY_LEFT:k2:=ExtKey_LEFT;
  GLUT_KEY_UP:k2:=ExtKey_UP;
  GLUT_KEY_RIGHT:k2:=ExtKey_RIGHT;
  GLUT_KEY_DOWN: k2:=ExtKey_DOWN;
  GLUT_KEY_PAGE_UP:k2:=ExtKey_PAGE_UP;
  GLUT_KEY_PAGE_DOWN:k2:=ExtKey_PAGE_DOWN;
  GLUT_KEY_HOME:k2:=ExtKey_HOME;
  GLUT_KEY_END:k2:=ExtKey_END;
  GLUT_KEY_INSERT:k2:=ExtKey_INSERT;
  else exit;
 end;
 vwkm.ProcessKey(#0,k2);
 enmt;
end;
var mx,my,mk:longint;
procedure mous(button,state,x,y:longint);cdecl;
begin
bnmt;
if state=glut_up then begin
{mk:=mk and (-2) shl button}
 if button=GLUT_LEFT_BUTTON then mk:=mk and (not 1) else
 if button=GLUT_RIGHT_BUTTON then mk:=mk and (not 2) else
 if button=GLUT_MIDDLE_BUTTON then mk:=mk and (not 4);
end
else
if state=glut_down then
begin
 if button=GLUT_LEFT_BUTTON then mk:=mk or 1 else
 if button=GLUT_RIGHT_BUTTON then mk:=mk or 2 else
 if button=GLUT_MIDDLE_BUTTON then mk:=mk or 4;
end;
mx:=x;
my:=y;
{vwkm.ProcessMouse(mx*2/screen.xcount-1,1-my*2/screen.ycount,mk);}
enmt;
end;
procedure mousmove(x,y:longint);cdecl;
begin
mx:=x;
my:=y;
{vwkm.ProcessMouse(mx*2/screen.xcount-1,1-my*2/screen.ycount,mk);}
end;

procedure draw;cdecl;
begin
 bnmt;
 if assigned(screen.base) then begin
  if mk<>0 then begin
		if(mk=1)then vwkm.ProcessMouse(0.5*(mx*2/screen.xcount-1),0.1*(1-my*2/screen.ycount),mk) else
		if(mk=2)then vwkm.ProcessMouse(0.1*(mx*2/screen.xcount-1),0.1*(1-my*2/screen.ycount),mk) else
		vwkm.ProcessMouse(0.1*(mx*2/screen.xcount-1),0.1*(1-my*2/screen.ycount),mk);			
	end;
  vw.Render;
  if mk<>0 then begin
   putpixel(screen.xcount div 2 ,screen.ycount div 2 -1,$FFFFFFFF);
   putpixel(screen.xcount div 2 -1,screen.ycount div 2 ,$FFFFFFFF);
   {putpixel(screen.xcount div 2  ,screen.ycount div 2  ,$FFFFFFFF);}
   putpixel(screen.xcount div 2 +1,screen.ycount div 2 ,$FFFFFFFF);
   putpixel(screen.xcount div 2 ,screen.ycount div 2 +1,$FFFFFFFF);
  end;
  writeln('redrawing...');
  {glColor3f(0,1,0);
  glutSolidTeapot(400);}
  glRasterPos2i(0,0);

  glDrawPixels(screen.xcount,screen.ycount,GL_RGBA,GL_UNSIGNED_BYTE,screen.base
  {$ifdef glh_1}^{$endif});
  {glDrawPixels(rendered.xcount,rendered.ycount,GL_RGBA,GL_UNSIGNED_BYTE,rendered.base^);}
  glFlush();
 end else writeln('unassigned screen.');
 enmt;
end;
procedure idle;cdecl;
begin
  bnmt;
  glutPostRedisplay();
  enmt;
end;
procedure _visible(vis:longint);cdecl;
begin
  if (vis=GLUT_VISIBLE) then
    glutIdleFunc(@idle)
  else
    glutIdleFunc(nil);
end;
var f:text;
inname,tmp:string;
begin
  screen.base:=nil;
  key_locked:=false;
  mk:=0;
  vwscriptU.init;
  if paramstr(1)='' then inname:='default.vws' else inname:=paramstr(1);
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

  vw.SetColumnUpdateProc(@ShowVline,0);
  glutInit(@argc, argv);

  glutInitWindowPosition(0, 0);
  glutInitWindowSize(500, 500);
  glutInitDisplayMode(GLUT_SINGLE or GLUT_RGB);
  glutCreateWindow('Voxel World');

  glClear( GL_COLOR_BUFFER_BIT);
  glPixelZoom(1,-1);
  glDisable(gl_blend);
  glFlush();
  glPixelStorei(GL_PACK_ALIGNMENT,   1);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  writeln('initializing events');
  glutDisplayFunc(@draw);
  glutReshapeFunc(@reshape);
  glutVisibilityFunc(@_visible);
  glutKeyboardFunc(@key);
  glutMouseFunc(@mous);
  glutMotionFunc(@mousmove);
  glutSpecialFunc(@skey);
  writeln('entering glut main loop');
  glutMainLoop();
end.
