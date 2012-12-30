const frames=750;
const r=128;
var
n:longint;
angle: double;
x,y,z,a: double;
begin
// writeln('// Automatically generated using animation_gen.exe');
// writeln('');
// writeln('out@''Y:/%.pbm'';');
// writeln('interlacing 1;');
// writeln('renderport stdperspective,4000,3000;');
 for n:=0 to frames-1 do begin
   angle:=(n/frames)*2*pi;
   x:=512+r*cos(angle);
   y:=512+r*sin(angle);
   z:=29+20*sin(angle*4);
   a:=angle*2;
   writeln('camera ',x,', ',y,', ',z,', ',a,';');
   writeln('render;');
//convert -quality 90 -filter Sinc -resize 1024x1024 #.# tn_#.#
   writeln('exec ''convert -quality 87 -filter Lanczos -resize 2048x2048 Y:\',n,'.pbm movie\',n,'.jpg '';');
   writeln('exec ''del Y:\',n,'.pbm '';');
 end;
end.