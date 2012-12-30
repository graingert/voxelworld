(*  Voxel World Script wrapper. (C) 2004-2008 Dmytry Lavrov     *)
(*  http://dmytry.pandromeda.com                                *)
(*  Description: Script wrapper for Voxel World :)              *)
//
//  No warranty at all.
//  You are free to do everything with this file(and this file only
//  as long as first 2 lines remains unchanged
//
// ***********************************************************************
//  Please read Copyright.txt for details
(*
*
* @'filetemplate',
* at 'filetemplate' :
* Set file template string that are combined with all file names after specific templates.
*
* out@'filetemplate';Set output template;
* in@'filetemplate' ;set input template;
*
* Template strings:
* combination of template string and parameter string are defined as
* *
* It template contain '%' char , it's deleted and parameter are inserted ,or,if next
* char in template are also '%', one '%' are left,one are deleted.
* If template contain no place where parameter was inserted,parameter are added
* to the end
* *
*
*
* Example:
* template='/mybasedir/data%.raw' parameter='12'
* '/mybasedir/data12.raw'
*
* If you want to literally specify #,use ##
* Example:
* temlate='/mybasedir/data%%.raw',parameter='12'
* '/mybasedir/data%.raw12'
*
* *
* camera x,y,z,a,fardist
* *
* Load2DHeightfield_Landscape x1,y1,mx,my,xcount,ycount,
* sourcemx,sourcemy,sourceoffset,sourcedatatype,sourceheightoffset,sourceheightunit,
* sourcefilename;
*
* sourcemx,sourcemy,sourceoffset are in bytes .
*)

{$ifdef fpc}
{$mode delphi}
{$endif}
{$apptype console}
{$h+}
{$define at_required}

{define nodebug}{don't define it}
unit vwscriptu;
interface
uses vw,utokenizer,myerrors,copr,dos;
procedure init;(**Danger,initializes VW**)
procedure executeINPUT;
procedure executefile(var f:text);
procedure uninit; (**Danger,uninitializes VW**)
implementation

Function IntToStr(I:Longint):String;
Var S:String;
begin
  Str(I,S);IntToStr:=S;
end;
Function StrToInt(s:string):longint;
Var e:longint;
begin
  val(s,result,e);
  if e<>0 then error('Illegal numeric ');
  {Str(I,S);IntToStr:=S;}
end;
Function FloatToStr(I:real):String;
Var S:String;
begin
  Str(I,S);result:=S;
end;
type integer=longint;
{not more than 1024 symbols are allowed}
const maxparamcount={1024}64;{maximal number of parameters}

procedure warnSkipping;
begin
  warning('Skipping to separator('';'')');
end;

var mytokenizer:Ttokenizer;
const separator:Ttoken=(n:t_symbol;vs:';';original:';';legalident:false;vr:0);
var params:array[0..maxparamcount-1] of Ttoken;
paramscount,curparam:integer;
procedure ReadParams;{including curtoken,stops at ";"}
var i:integer;
begin
  writeln('reading params');
  curparam:=0;
  with mytokenizer do begin
    i:=0;
    paramscount:=0;

    while (CurToken.n<>t_end)and(CurToken.vs<>';') do begin
      if (CurToken.vs='-') then begin
        GetNext;
        if (curtoken.n=t_int) then begin
          curtoken.vi:=-curtoken.vi;
          end else if (curtoken.n=t_flt) then begin
          curtoken.vr:=-curtoken.vr;
        end else error('"-" sign not followed by number');
      end else
      if (CurToken.vs='+') then begin
        GetNext;
        if (curtoken.n<>t_int)and(curtoken.n<>t_flt) then error('"+" sign not followed by number');
      end;
      params[i]:=curtoken;inc(i);
      GetNext;
      if curtoken.vs<>',' then begin
        if curtoken.vs<>';' then begin
          Error('"," or ";" expected ');
          {i:=0;}
        end{if};
        {writeln(', not found');}
        break;
      end{if};
      getnext;
    end{while loop};
  end{with};
  paramscount:=i;
end;

{type typ_file_rec=record
name:real;
pbase:punit8;
bytes_count:integer;
end;}


function getReal(t:ttoken):real;
begin
  if t.n=t_real then result:=t.vr else
  if t.n=t_int then result:=t.vi else begin
    error('Floating value expected,DANGER,using default=1');
    result:=1;
  end;
end;
function getInt(t:ttoken):integer;
begin
  if t.n=t_int then result:=t.vi else begin
    error('Integer value expected,DANGER,using default=1');
    result:=1;
  end;
end;
function NextParam:Ttoken;
begin
  if curparam<=paramscount then begin
    result:=params[curparam];
    inc(curparam);
    end else begin
    error('More parameters expected (or internal script error)');
  end;
end;
Function Template(const t:string;const p: string):string;
var i,ii:longint;
begin
  if t='' then {macro exit(p)}begin result:=(p);exit;end;
  i:=1;ii:=0;result:='';
  while i<length(t) do begin
    if (t[i]='%') then begin
      inc(i);
      if t[i]<>'%' then begin
        result:=result+p;
        inc(ii);
      end;
    end;
    result:=result+t[i];
    inc(i);
  end;
  if t[length(t)]<>'%' then result:=result+t[length(t)];
  if ii=0 then result:=result+p;
end;
(******************  Comands  ***********************)
var FileNameTemplate,InFileNameTemplate,OutFileNameTemplate:string;ImageNumber:integer;
procedure SetFileNameTemplate(s:string);
begin
  FileNameTemplate:=s;
end;
function getfilename(const name:string):string;
begin
  Result:=template(FileNameTemplate,name);
end;
function getINfilename(const name:string):string;
begin
  Result:=getfilename(template(InFileNameTemplate,name));
end;
function getOUTfilename(const name:string):string;
begin
  Result:=getfilename(template(OutFileNameTemplate,name));
end;
procedure RunProcNothing;
begin
end;
procedure RunProcUnsupported;
begin
  Error('command is not supported.');
end;
procedure showprocparams(s:string);
var i:integer;
begin

  {warning}writeln('interpreter debug: '+s+' was called');
  writeln('Parameters:');
  for i:=0 to paramscount-1 do begin
    writeinfo(params[i]);
  end;
  writeln('.');

end;
procedure RunProcTest;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcTest');
end;
procedure RunProcAssign;
{var dest,source:string;mode:integer;}
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcAssign');
  {if paramscount<>3 then begin error('Assign should have 3 parameters');exit;end;
  if params[0].legalident=false then begin error('Ideintifier expected');exit;end;
  if params[1].legalident=false then begin error('Ideintifier expected');exit;end;
  dest:=params[0].vs;
  source:=params[2].vs; }
end;
procedure RunProcCamera;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcCamera');
  Case paramscount of
    5:begin
      {BeginFatals;}
      vw.setCameraPosition4(GetReal(params[0]), GetReal(params[1]), GetReal(params[2]), GetReal(params[3]) );
      vw.setFarDist(round(GetReal(params[4])));
      {EndFatals;}
    end;
    4:begin
      {BeginFatals;}
      vw.setCameraPosition4(GetReal(params[0]), GetReal(params[1]), GetReal(params[2]), GetReal(params[3]) );
      {EndFatals;}
    end;
    else error('invalid parameters list');
  end;
end;
procedure RunProcFAR;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcFAR');
  Case paramscount of
    1:begin
      vw.setFarDist(round(GetReal(params[0])));
    end;
    else error('invalid parameters list');
  end;
end;
(* SetLandscapeHeighxels(const destination:SubArray2Ddesc;const source:array2Ddesc;heightunit,heightoffset:real); *)
procedure RunProcLOAD2DHEIGHTFIELD_LANDSCAPE;(** Contains some dirty pointers stuff **)
(**)
var
d:SubArray2Ddesc;{6}
s:record
  mx,my:integer;
  offset:longint;
  datatype:n_elemtypes;{10}
end;
source:array2Ddesc;

heightunit,heightoffset:real;{12}
var filename:string;{13}
{13 parameters total}
dataunitsize:integer;

var f:file;
fileLen,fileOffset,fileEnd,loadlen:longint;
FileData:pointer;
autoHeight:boolean;
(**)
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcLOAD2DHEIGHTFIELD_LANDSCAPE');
  (**)
  Case paramscount of
    13:
    begin
      BeginFatals;
      with d do begin
        x1:=GetInt(NextParam);
        y1:=GetInt(NextParam);
        mx:=GetInt(NextParam);
        my:=GetInt(NextParam);
        xcount:=GetInt(NextParam);
        ycount:=GetInt(NextParam);
      end;
      with s do begin
        mx:=GetInt(NextParam);
        my:=GetInt(NextParam);

        offset:=GetInt(NextParam);
        if params[curparam].vs = 'UINT8'  then begin datatype:=n_uint8; dataunitsize:=1; end else
        if params[curparam].vs = 'INT8'   then begin datatype:=n_int8;  dataunitsize:=1; end else
        if params[curparam].vs = 'UINT16' then begin datatype:=n_uint16;dataunitsize:=2; end else
if params[curparam].vs = 'INT16'  then begin datatype:=n_int16; dataunitsize:=2; end else error('Supported datatypes: UINT8,INT8,UINT16,INT16  ');
        NextParam;
      end;
      if (params[curparam].vs = '?')or(params[curparam].vs = 'AUTO') then begin
        autoHeight:=true;NextParam;
        end else begin
        autoHeight:=false;
        heightoffset:=GetReal(NextParam);
      end;

      heightunit:=GetReal(NextParam);
      if Params[curparam].n=t_string then begin
        filename:=getInFilename(Params[curparam].vs);
      end else error('String expected');
      //!!!!!!!

      fileOffset:=s.offset;
      if s.mx<0 then fileOffset:=fileoffset+s.mx*(d.xcount-1);
      if s.my<0 then fileOffset:=fileoffset+s.my*(d.ycount-1);
      if fileOffset<0 then error('You can''t read outside file (before file start)');
      fileEnd:=s.offset;
      if s.mx>0 then fileEnd:=fileEnd+s.mx*(d.xcount-1);
      if s.my>0 then fileEnd:=fileEnd+s.my*(d.ycount-1);
      fileend:=fileend+dataunitsize;
      loadlen:=fileend-fileoffset;

      assign(f,filename);
      reset(f,1);
      FileLen:=Filesize(f);
      if fileEnd>FileLen then error('You can''t read outside file (after file end)');

      GetMem(FileData,loadlen);
      seek(f,FileOffset);
      blockread(f,FileData^,loadlen);
      close(f);
      source.mx:=s.mx;
      source.my:=s.my;
      source.xcount:=d.xcount;
      source.ycount:=d.ycount;
      source.base:=pointer(longint(filedata)+s.offset-FileOffset);
      source.datatype:=s.datatype;
      if autoHeight then begin
        heightoffset:=-vw.average(source);
      end;
      Writeln('Calling VW!');
      setLandscapeHeighxels(d,source,HeightOffset,HeightUnit);
      FreeMem(FileData,loadlen);
      EndFatals;
    end;
    else error('invalid parameters list');
  end;
  (**)
end;
procedure RenderToFile(name:string);
var f:file;pbmhead:string;size:integer;x,y:longint;linelen:longint;p:puint32;
type bufftype=packed array[0..maxlongint div 4] of packed record r,g,b:byte;end;
var buff:^bufftype;
t:dword;
begin
  Writeln('Rendering image,name="',name,'"');
  vw.render;
  pbmhead:='P6 '+intToStr(vw.rendered.xcount)+' '+intToStr(vw.rendered.ycount)+' 255'+#10;
  vw.lock_rendered;
  size:=length(pbmhead)+vw.rendered.xcount*vw.rendered.ycount*3;
  writeln('Transponing and saving image ',(size+511) div 1024,' Kbytes total');
  if rendered.datatype<>n_uint32 then begin writeln('Internal bug!');exit; end;
  assign(f,name);
  rewrite(f,1);
  blockwrite(f,pbmhead[1],length(pbmhead));
  linelen:=rendered.xcount*3;
  getmem(buff,linelen);//allocate buffer

  for y:=rendered.ycount-1 downto 0 do begin
    p:=pointer(longint(rendered.base)+y*rendered.my) ;
    for x:=0 to rendered.xcount-1 do begin
      t:=p^;
      {getLandscapePoint(x,y,z,dzdx,dzdy);}
      buff^[x].r:=byte(t shr 16);
      buff^[x].g:=byte(t shr 8);
      buff^[x].b:=byte(t);
      inc(longint(p),rendered.mx);
    end;
    blockwrite(f,buff^,linelen);
  end;
  close(f);
  freemem(buff,linelen);//deallocate
  vw.unlock_rendered;
end;
procedure RunProcRender;
var filename:string;
named:boolean;
begin
  named:=false;
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcRender');
  case paramscount of
    0:begin
    end;
    1:begin
      if params[0].n=t_string then begin
        FileName:=params[0].vs;
        Named:=true;
      end else
      if params[0].n=t_int then begin
        ImageNumber:=params[0].vi;
      end else
      begin
        error('Illegal Parameter');
      end;
    end;
    else begin
      error('illegal parameters list for Render');
    end;
  end;
  if not named then begin
    FileName:=IntToStr(ImageNumber);
    inc(ImageNumber);
  end;
  FileName:=GetOutFileName(FileName);
  RenderToFile(FileName);
end;
procedure RunProcRenderPort;
var
proj:n_projections;
xres,yres:integer;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcRenderPort');
  case paramscount of
    3:begin
      if (params[0].vs='BICONE')or(params[0].vs='BICONEPANORAM') then begin
        proj:=n_biconepanoram;
      end else if (params[0].vs='STD')or(params[0].vs='STDPERSPECTIVE') then
      begin
        proj:=n_stdperspective;
        end else begin
        error('Unknown projection:"'+params[0].vs+'" ,stdPerspective used');
        proj:=n_stdperspective;
      end;
      xres:=GetInt(params[1]);
      yres:=GetInt(params[2]);
      Renderport(proj,xres,yres);
    end;
    2:begin
      xres:=GetInt(params[0]);
      yres:=GetInt(params[1]);
      Renderport(n_stdperspective,xres,yres);
    end;
    else error('Invalid parameters for RenderPort');
  end;
end;
procedure RunProcAT;
begin
  mytokenizer.getnext;
  if mytokenizer.curtoken.n<>t_string then error('String expected') else
  begin
    SetFileNameTemplate(mytokenizer.curtoken.vs);
  end;
  mytokenizer.getnext;
end;
procedure RunProcIN_AT;
begin
  mytokenizer.getnext;
  if (mytokenizer.curtoken.vs='AT')or(mytokenizer.curtoken.vs='@')then begin
    mytokenizer.getnext;
    if mytokenizer.curtoken.n<>t_string then error('String expected') else
    begin
      InFileNameTemplate:=mytokenizer.curtoken.vs;
    end;
    mytokenizer.getnext;
  end else error('use "in@ ''string''" or "in at ''string''"');
end;
procedure RunProcOUT_AT;
begin
  mytokenizer.getnext;
  if (mytokenizer.curtoken.vs='AT')or(mytokenizer.curtoken.vs='@')then begin
    mytokenizer.getnext;
    if mytokenizer.curtoken.n<>t_string then error('String expected') else
    begin
      OutFileNameTemplate:=mytokenizer.curtoken.vs;
    end;
    mytokenizer.getnext;
  end else error('use "out@ ''string''" or "out at ''string''"');
end;
procedure RunProcFOV_D;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcFOV_D');
  if paramscount<>1 then error('illegal parameters list for FOV_D');
  vw.setFOVDiagonal(GetReal(params[0]));
end;
procedure RunProcFOV_H;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcFOV_H');
  if paramscount<>1 then error('illegal parameters list for FOV_H');
  vw.setFOVHorisontal(GetReal(params[0]));
end;
procedure RunProcFOV_V;
begin

  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcFOV_V');
  if paramscount<>1 then error('illegal parameters list for FOV_V');
  vw.setFOVVertical(GetReal(params[0]));
end;
procedure RunProcFogColor;
begin

  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcFogColor');
  if paramscount<>3 then error('illegal parameters list for FogColor');
  vw.setFogColor(GetReal(params[0]),GetReal(params[1]),GetReal(params[2]));
end;
procedure RunProcSunLightColor;
begin

  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcSunLightColor');
  if paramscount<>3 then error('illegal parameters list for sunlightcolor');
  vw.setSunLightColor(GetReal(params[0]),GetReal(params[1]),GetReal(params[2]));
end;
procedure RunProcSunPos;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcSunPos');
  if paramscount<>2 then error('illegal parameters list');
  vw.setSunPosition(GetReal(params[0]),GetReal(params[1]) );
end;
procedure RunProcAboveLightColor;
begin

  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcAboveLightColor');
  if paramscount<>3 then error('illegal parameters list for abovelightcolor');
  vw.setAboveLightColor(GetReal(params[0]),GetReal(params[1]),GetReal(params[2]));
end;
procedure RunProcReverseLightColor;
begin

  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcReverseLightColor');
  if paramscount<>3 then error('illegal parameters list for reverselightcolor');
  vw.setReverseLightColor(GetReal(params[0]),GetReal(params[1]),GetReal(params[2]));
end;
procedure RunProcFogT;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcFogt');
  if paramscount<>1 then error('illegal parameters list for Fogt');
  vw.setFogTime(GetReal(params[0]));
end;
procedure RunProcFogDT;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcFogdt');
  if paramscount<>1 then error('illegal parameters list for Fogdt');
  vw.setFogdTime(GetReal(params[0]));
end;
procedure RunProcInterlacing;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcInterlacing');
  if paramscount<>1 then error('illegal parameters list for Interlacing');
  vw.setInterlacing(GetInt(params[0]));
end;
procedure RunProcTexParam;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcTEXPARAM');
  if paramscount<>1 then error('illegal parameters list for TEXPARAM');
  vw.setTextureSizeParam(GetInt(params[0]));
end;
procedure RunProcSetSeed;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcSetSeed');
  if paramscount<>1 then error('illegal parameters list for SetSeed');
  vw.SetSeed(GetInt(params[0]));
end;
procedure writeparams(filename:string{unexpanded});//appends params to file!
var f:text;
procedure emit(const s:string;const p:array of string);
var i:longint;
begin
 write(f,s+' ');
 i:=0;
 if i<=high(p) then
 repeat
  write(f,p[i]);
  inc(i);
  if i<=high(p) then break;
  write(f,',');
 until false;
 writeln(f,';');
end;
procedure emitf(const s:string;const p:array of real);
var i:longint;
begin
 write(f,s+' ');
 i:=0;
 if i<=high(p) then
 repeat
  write(f,p[i]);
  inc(i);
  if i>high(p) then break;
  write(f,',');
 until false;
 writeln(f,';');
end;
procedure emiti(const s:string;const p:array of longint);
var i:longint;
begin
 write(f,s+' ');
 i:=0;
 if i<=high(p) then
 repeat
  write(f,p[i]);
  inc(i);
  if i>high(p) then break;
  write(f,',');
 until false;
 writeln(f,';');
end;
begin
 assign(f,filename);
 {$i-}
 append(f);
 if ioresult<>0 then begin
  assign(f,filename);
  rewrite(f);
 end;
 {$i+}
 with vw.worldparams do
 begin
  writeln(f,'// autosaved parameters ;');
  emiti('setseed',[generator.seed]);
  emitf('camera',[cam.x,cam.y,cam.z,cam.a,cam.f]);
  emitf('fogt',[fogt]);
  emitf('fogdt',[fogdt]);
  emitf('fogcolor',[fogcolor.r,fogcolor.g,fogcolor.b]);
  emitf('sunlightcolor',[sunlightcolor.r,sunlightcolor.g,sunlightcolor.b]);
  emitf('abovelightcolor',[abovelightcolor.r,abovelightcolor.g,abovelightcolor.b]);
  emitf('reverselightcolor',[reverselightcolor.r,reverselightcolor.g,reverselightcolor.b]);
 end;
 close(f);
end;
procedure RunProcWriteParams;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcWriteParams');
  if (paramscount<>1)or(params[0].n<>t_string) then error('illegal parameters list') else begin
  writeparams(getOutFileName(params[0].vs));
  end;
end;

procedure RunProcExec;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcExec');

  if(1=paramscount) then begin
    if(params[0].n=t_string) then begin
      //dos.exec(params[0].vs,params[1].vs);
      dos.exec(GetEnv('COMSPEC'),'/C '+params[0].vs);
      writeln('Exec: process exit code=',Lo(DosExitCode));
    end else error('illegal parameters list');
  end else
  if(2=paramscount) then begin
    if((params[0].n=t_string)and(params[1].n=t_string))then begin
      dos.exec(params[0].vs,params[1].vs);
      writeln('Exec: process exit code=',Lo(DosExitCode));
    end else error('illegal parameters list');
  end else error('illegal parameters list');
{
  if (paramscount<>2)or(params[0].n<>t_string)or(params[1].n<>t_string) then error('illegal parameters list') else begin
    //dos.exec(params[0].vs,params[1].vs);
  end;
}

end;

procedure RunProcWriteTerragenRaw;
begin
  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcWriteTerragenRaw');
  if paramscount<>0 then error('illegal parameters list for WriteTerragenRaw;');
  vw.WriteTerragenRaw(0,0);
end;
var imgline:packed array[0..255] of packed record r,g,b:byte; end;
colortable:Tcolortable;

procedure RunProcLoadColorMap;
var filename:string;
f:file;
head:array[1..3]of string;
h:longint;
function gethead:boolean;
var c:char;P6:packed array[1..2] of char;
n,i:longint;
label skipspaces;
begin
  blockread(f,P6,sizeof(P6));
  if (P6[1]<>'P') or (P6[2]<>'6') then begin
   error('BAD FILE FORMAT. Only P6 PBM images without comments in the header are supported.');
   {macro exit(false)}begin result:=(false);exit;end;
  end;
  for i:=1 to 3 do head[i]:='';
  i:=1;
  repeat
     n:=0;
     if eof(f) then {macro exit(false)}begin result:=(false);exit;end;
     skipspaces:
     blockread(f,c,1);
     while c in [#10,#13,' ',';',','] do begin
      if eof(f) then {macro exit(false)}begin result:=(false);exit;end;
      blockread(f,c,1);
      inc(n);
      if n>255 then {macro exit(false)}begin result:=(false);exit;end;
     end;
     if c='#' then begin
       repeat
        if eof(f) then {macro exit(false)}begin result:=(false);exit;end;
        blockread(f,c,1);
       until c in[#10,#13];
       goto skipspaces;
     end;
     repeat
      head[i]:=head[i]+c;
      if eof(f) then break;
      blockread(f,c,1);
     until c in [#10,#13,' ',';',','];
    inc(i);
   until i>3;
   if (head[1]<>'256') {or (head[2]<>'1024')} then begin
    error('BAD FILE FORMAT. Image should have 256xXXX dimensions');
   end;
   h:=StrToInt(head[2]);
   if h<>1024 then begin
    warning('wrong image dimensions');
   end;
   {third parameter are ignored.}
end;

var x,y,i:longint;
begin

  mytokenizer.GetNext;
  ReadParams;
  showprocparams('RunProcLoadColorMap');
  if (paramscount<>1)or(params[0].n<>t_string) then begin
   error('illegal parameters list for LoadColorMap');
  end else begin
   filename:=GetInFileName(params[0].vs);

   assign(f,filename);
   reset(f,1);
   if not GetHead then begin
    error('Illegal PBM P6 header');
    exit;
   end;
   {for i:=0 to $FFFF do begin
    colortable[i]:=imgbody[i].r shl 16+imgbody[i].g shl 8+imgbody[i].b;
   end;}

   if h>1024 then h:=1024;
   dec(h);
   for y:=0 to h do begin
    blockread(f,imgline[0],sizeof(imgline));
    for x:=0 to 255 do begin
     {i:=x+(h-y)*256;}
     colortable[x+(1023-y)*256]:=imgline[x].r shl 16 + imgline[x].g shl 8 + imgline[x].b;
    end;
   end;
   vw.setTextureColor(colortable);
   close(f);
  end;
end;


type
CommandRunProc=procedure;
//  CommandHelpProc=procedure(const s:string);
command=record
  name:string;
  run:CommandRunProc;
  //  Showhelp:CommandHelpProc;
  //  HelpStr:string;
end;
const commandscount=29;
commands:array[0..commandscount-1] of command=({should be sorted}
(name:'?' ;run:RunProcNothing ),
(name:'@' ;run:RunProcAT ),
(name:'ABOVELIGHTCOLOR' ;run:RunProcAboveLightColor ),
(name:'ASSIGN' ;run:RunProcAssign ),
(name:'AT' ;run:RunProcAT ),
(name:'CAMERA' ;run:RunProcCamera ),
(name:'EXEC' ;run:RunProcExec ),
(name:'FAR' ;run:RunProcFAR ),
(name:'FOGCOLOR' ;run:RunProcFogColor ),
(name:'FOGDT' ;run:RunProcFogDT ),
(name:'FOGT' ;run:RunProcFogT ),
(name:'FOV_D' ;run:RunProcFov_D ),
(name:'FOV_H' ;run:RunProcFov_H ),
(name:'FOV_V' ;run:RunProcFov_V ),
(name:'IN' ;run:RunProcIN_AT ),
(name:'INTERLACING' ;run:RunProcInterlacing ),
(name:'LOAD2DHEIGHTFIELD_LANDSCAPE' ;run:RunProcLOAD2DHEIGHTFIELD_LANDSCAPE ),
(name:'LOADCOLORMAP' ;run:RunProcLOADCOLORMAP ),
(name:'OUT' ;run:RunProcOUT_AT ),
(name:'RENDER' ;run:RunProcRender ),
(name:'RENDERPORT' ;run:RunProcRenderPort ),
(name:'REVERSELIGHTCOLOR' ;run:RunProcReverseLightColor ),
(name:'SETSEED' ;run:RunProcSetSeed ),
(name:'SUNLIGHTCOLOR' ;run:RunProcSunLightColor ),
(name:'SUNPOS' ;run:RunProcSunPos ),
(name:'TEST' ;run:RunProcTest ),
(name:'TEXPARAM' ;run:RunProcTEXPARAM ),
(name:'WRITEPARAMS' ;run:RunProcWriteParams ),
(name:'WRITETERRAGENRAW' ;run:RunProcWriteTerragenRaw)
);
function ToUpper(const s:string):string;
var i:integer;
begin
  result:=s;
  for i:=1 to length(s) do result[i]:=UpCase(result[i]);
end;
procedure checkCommandsSorted;
var s:string;n:integer;
begin
  s:=#0;
  for n:=0 to CommandsCount-1 do begin
    if commands[n].name=s then begin Writeln('Identical commands found "',s,'",exiting!'); runerror(255);end;
    if commands[n].name<s then begin Writeln('Commands are not sorted ,look at"',s,'" !'); runerror(255);end;
    s:=commands[n].name;
    if s<>ToUpper(s) then begin Writeln('internal interpteter error: commands should all be uppercase');runerror(255);end;
  end;
end;
function commandindex(s:string):integer;
var a,b,i:integer;
begin
  a:=0;
  b:=commandscount-1;
  while a<=b do begin
    i:=(a+b) div 2;
    if s=commands[i].name then begin
      result:=i;
      exit;
    end; {else}
    begin
      if s>commands[i].name then begin
        a:=i+1;
        end else{s<commands[i]..name} begin
        b:=i-1;
      end;
    end
  end;
  error('Command '+s+' is not found');
  result:=0;
end;


procedure Execute;
var ci:integer;
begin
  with mytokenizer do begin
    mytokenizer.reset;
    SkipSpaces;GetNext;
    while CurToken.n<>t_end do begin
      {if (curtoken.n=T_symbol) and (curtoken.vs='//') then begin
        skipto(separator);
        getnext;
        writeln('comment found');
        readline;
        end else} begin
        if (curtoken.n<>T_symbol){or( not curtoken.legalident )} then  begin
          error('Command expected.');warnSkipping;skipto(separator);GetNext;
          end else begin
          ci:=commandindex(curtoken.vs);
          commands[ci].run;
          {getnext;}
          if not TokensEqual(curtoken,separator) then begin
            error('Separator expected.');
            warnSkipping;
            skipto(separator);
          end{not separator};
          getnext;
        end{else ifvalisdymbol};
      end{if};
    end{while};
  end{with};
end;
procedure Init;
begin
  FileNameTemplate:='';InFileNameTemplate:='';OutFileNameTemplate:='';
  ImageNumber:=0;
  vw.init;
end;
procedure executefile(var f:text);
begin
  mytokenizer.f:=@f;
  execute;
  ShowErrorCount;
end;
procedure executeINPUT;
begin
  mytokenizer.f:=@input;
  writeln('Enter "#eof" to close input');
  execute;
  ShowErrorCount;
end;
procedure unInit;
begin
  vw.uninit;
  writeln('end of commands.');
  showerrorcount;
end;

begin
  {$ifndef nodebug}
  checkCommandsSorted;
  {$endif}
  {mytokenizer.init(input);}
end.
(*
26-02-2004 filename needed.
25-02-2004 it's unit now.
24-02-2004 initial version.
*)
