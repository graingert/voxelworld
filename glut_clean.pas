(* Simplified opengl/glut based interactive viewer for voxelworld. *)
(* (C) 2003-2008 Dmytry Lavrov *)
uses glut,gl,
vw,
vwkm,
vwscriptu;

procedure reshape(width,height:longint);cdecl;
begin

end;
procedure key(k:char;x,y:longint);cdecl;
begin
  case k of
    #27 :
      halt(0);
    'z' :z:=z+0.1;
    'Z' :z:=z-0.1;
  end;
end;
procedure draw;cdecl;
begin
end;
procedure idle;cdecl;
procedure visible(vis:longint);cdecl;
begin
  if (vis=GLUT_VISIBLE) then
    glutIdleFunc(@idle)
  else
    glutIdleFunc(nil);
end;

begin
  glutInitWindowPosition(0, 0);
  glutInitWindowSize(600, 450);
  glutInitDisplayMode(GLUT_RGB or GLUT_DOUBLE or GLUT_DEPTH);
  glutCreateWindow('Voxel World');
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
  glFlush();
  glutSwapBuffers();

  glutDisplayFunc(@draw);
  glutReshapeFunc(@reshape);
  glutVisibilityFunc(@visible);
  glutKeyboardFunc(@key);

  glutMainLoop();
end.
