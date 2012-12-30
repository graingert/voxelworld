(*  Voxel World Renderer - input tokeniser.       (C) 2004-2008 Dmytry Lavrov     *)
(*  http://dmytry.pandromeda.com                                                  *)

{Use Pascal Pretty Printer, (C)2001,2002,2003,2004 Dmytry Lavrov }
{$H+}
unit utokenizer;
interface
uses {objects,}myerrors;
type Ttoken=record
  n:longint;
  vs:string;
  original:string;
  Legalident:boolean;
  case byte of
  1:(vi:longint);
  2:(vr:real);
end;

const
T_end=0;
const Dig:set of char=['$','1'..'9','0'];
T_int=1;
T_flt=2;
t_real=T_flt;
const ABC:set of char=['a'..'z','A'..'Z','_'];
{T_abc=3;}
const AllChars:Set Of Char=[#1..#255];{0 is a end of expression}
const Symbols:set of char=['+','-','*','/','(',')','[',']','=','>','<','^','@',';','''',':','!','\','|']{AllChars-dig-abc};
t_symbol=4;
t_string=5;
type TcommentsSkipper=object
  f:^text;
  s:string;
  pos:longint;
  line:longint;
  LinPos:longint;
  CommentLevel:longint;
  LastComments:string;
  constructor init(var fi:text);
  procedure SkipSpaces;
  procedure Error(const err:string);
  procedure ReadLine;
  procedure Reset;
end;

type Ttokenizer=object(TcommentsSkipper)
  CurToken,LastToken:Ttoken;
  procedure GetNext;
  Function GetSymbol:string;
  Function GetIdent:string;
  procedure Skip(_s:string);
  procedure SkipTo(const t:Ttoken);
  procedure DoNum;
  procedure DoString;
end;
function TokensEqual(const a,b:Ttoken):boolean;
procedure writeinfo(const a:Ttoken);
implementation
function TokensEqual(const a,b:Ttoken):boolean;
begin
  if a.n<>b.n then begin result:=false;exit; end;
  case a.n of
    T_end:begin result:=true;exit; end;
    T_int:{exit(a.vi=b.vi);}begin result:=a.vi=b.vi; end;
    T_flt:{exit(a.vr=b.vr);}begin result:=a.vr=b.vr; end;
    t_symbol:{exit(a.vs=b.vs);}begin result:=a.vs=b.vs; end;
    t_string:{exit(a.vs=b.vs);}begin result:=a.vs=b.vs; end;
    else result:=false;
  end;

end;
constructor TCommentsSkipper.init(var fi:text);
begin
f:=@fi;
end;
procedure TCommentsSkipper.SkipSpaces;
label b;
begin
  b:
  if pos>length(s) then ReadLine;
  while (ord(s[pos]) in [1..32]) do begin
    inc(pos);
    if pos>length(s) then ReadLine;
    if s[pos]=#0 then exit;
  end;
  if s[pos]='/' then if length(s)>pos then if s[pos+1]='/' then begin
  readline;
  if s=#0 then exit;
  goto b;
  end;
end;

procedure TCommentsSkipper.ReadLine;
var noteof:boolean;
begin
//  writeln('debug:Checking for eof');
  noteof:=  not eof(f^);
//  writeln('debug:ok,eof=',not noteof);
  if noteof then begin
//  writeln('debug:reading line');
  readln(f^,s);
//  writeln('debug:ok');
  s:=s+' ';
  end else s:=#0;
  if s='#eof ' then s:=#0;
  pos:=1;
  line:=1;
  linpos:=1;
end;
procedure TCommentsSkipper.Reset;
begin
  s:='';
  pos:=1;
  line:=1;
  linpos:=1;
  ReadLine;
end;
procedure TCommentsSkipper.Error(const err:string);
begin
  myerrors.error(err);
end;


procedure TTokenizer.GetNext;
var pos1:longint;
begin
  curtoken.LegalIdent:=false;
  LastToken:=CurToken;
  CurToken.vs:='';
  CurToken.Original:='';
  CurToken.n:=t_end;

  {if s[pos] in #0..#32 then SkipSpaces;}

  if pos>length(s) then begin
    CurToken.n:=T_end;
    exit;
  end;
  SkipSpaces;
  pos1:=pos;
  case s[pos] of
    #255,#0:CurToken.n:=T_end;
    '$','1'..'9','0':DoNum;
    'a'..'z','A'..'Z','_':begin
      while s[pos] in ABC+['0'..'9'] do begin CurToken.vs:=CurToken.vs+UpCase(s[pos]);inc(pos); end;
      CurToken.n:=T_symbol;
      CurToken.Legalident:=true;
    end;
    '''','#':DoString;
    {'.',}'+','-',{'*',}{'/',}'(',')','[',']'{,'=','>','<'},'^','@',';',{':',}'!',',','?','\','|':
    begin
      CurToken.vs:=s[pos];
      CurToken.n:=T_symbol;
      inc(pos);
    end;
    '.':begin
      if s[pos+1]='.' then begin CurToken.vs:='..';inc(pos) end else CurToken.vs:=s[pos];
      {note: there's at least one character after s[pos] if s[pos]<>[#0,#255]}
      CurToken.n:=T_symbol;
      inc(pos);
    end;
    '/':begin
      if s[pos+1]='/' then begin CurToken.vs:='//';inc(pos) end else CurToken.vs:=s[pos];
      CurToken.n:=T_symbol;
      inc(pos);
    end;
    '*':begin
      if s[pos+1]='*' then begin CurToken.vs:='**';inc(pos) end else CurToken.vs:=s[pos];
      CurToken.n:=T_symbol;
      inc(pos);
    end;
    '=':begin
      if s[pos+1]='>' then begin curtoken.vs:='>=';inc(pos) end else
      if s[pos+1]='<' then begin curtoken.vs:='<=';inc(pos)end else CurToken.vs:='=';
      inc(pos);
      CurToken.n:=T_symbol;
    end;
    '>','<':
    begin
      CurToken.vs:=s[pos];
      CurToken.n:=T_symbol;
      inc(pos);
      if s[pos]='=' then begin CurToken.vs:=CurToken.vs+'=';inc(pos) end else begin
        if (s[pos]='>') or (s[pos]='<') then begin
          if s[pos]<>CurToken.vs then begin CurToken.vs:='<>';inc(pos) end;
        end ;
      end;
    end{'>','<'};
    ':':begin
      CurToken.vs:=s[pos];
      CurToken.n:=T_symbol;
      inc(pos);
      if s[pos]='=' then begin CurToken.vs:=':=';inc(pos);end;
    end;
    else error('illegal char');
  end{case};
  curtoken.original:=copy(s,pos1,pos-pos1);
end;
procedure TTokenizer.DoString;
var st:string;aq:boolean;
begin
  st:='';
  aq:=false;
  repeat
    if s[pos]='#'then begin
      inc(pos);
      DoNum;
      if CurToken.n<>t_int then begin error('illegal string constant');end;
      st:=st+chr(CurToken.vi);
      aq:=false;
      end else if s[pos]='''' then begin
      {if s[pos-1]='''' then st:=st+s[pos];}
      if aq=true then st:=st+'''';
      inc(pos);
      while (s[pos]<>'''')and(pos<=length(s)) do begin st:=st+s[pos];inc(pos); end;
      inc(pos);
      aq:=true;
    end else break;
  until false;
  if pos>length(s) then begin
  error('Unexpected EOL');
  readline;
  end;
  CurToken.vs:=st;
  CurToken.N:=t_string;
end;
procedure TTokenizer.DoNum;
var ErrPos,pos2,vi,vr:longint;
isReal,StartD:boolean;
st:string;
procedure DoInt;
begin
  while (s[pos] in dig)or(StartD and(s[pos] in abc)) do inc(pos);
end;
begin
  isReal:=false;
  pos2:=pos;
  StartD:=s[pos]='$';
  DoInt;
  if not startD then begin
    if (s[pos]='.')and(s[pos+1]<>'.') then begin
      if IsReal then error('illegal float ..');
      inc(pos);
      IsReal:=true;
      DoInt;
    end;
    {else }begin
      if (s[pos]='E')or(s[pos]='e') then begin
        isReal:=True;
        inc(Pos);
        {Writeln('Exponent part!');}
        if (s[pos]='+')or(s[pos]='-') then begin inc(pos);{Writeln('plus/minus found');} end;
        DoInt;
      end else begin  end;
    end;
  end;
  st:=copy(s,pos2,pos-pos2);
  if IsReal then begin
    val(st,CurToken.vr,ErrPos);
    if ErrPos<>0 then error('illegal float const:Can''t get value');
    Curtoken.n:=t_flt;
    end else  begin
    val(st,CurToken.vi,ErrPos);
    if ErrPos<>0 then error('illegal integer const:Can''t get value');
    Curtoken.n:=t_int;
  end;
end;
Function TTokenizer.GetSymbol:string;
begin
  if {(CurToken.n<>t_abc)and}(CurToken.n<>t_symbol) then Error('char expected');
  GetSymbol:=CurToken.vs;
end;
Function TTokenizer.Getident:string;
begin
  if not CurToken.LEGALident then Error('Ideintificator expected');
  Getident:=CurToken.vs;
end;
procedure TTokenizer.Skip(_s:string);
begin
  if CurToken.vs<>_s Then Error(_s+' expected ') else GetNext;
end;
procedure TTokenizer.SkipTo(const t:Ttoken);
begin
  while (curtoken.n<>T_end)and(not TokensEqual(curtoken,t))and(not curtoken.n=t_end) do begin
    getnext;
  end;
end;
procedure writeinfo(const a:Ttoken);
begin
  with a do
    case n of
     t_end:begin Writeln('t_end');end;
     t_int:begin Writeln('int,value=',vi);end;
     t_flt:begin Writeln('flt,value=',vr);end;
     t_symbol:begin Writeln('symbol legalident=',legalident,' vs="',vs,'"');end;
     t_string:begin Writeln('string, value="',vs,'"');end;
    end;
end;
begin
end.
