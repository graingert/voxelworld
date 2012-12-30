Voxel World
===========
Introduction
------------
Voxel World is a photo-realistic terrain and volumetric renderer developed by Dmytry Lavrov.
Renderer have volumetric clouds/fog, volumetric lighting, procedural terrain texturing, built-in terrain generator, superscalability, and other features.

The project started in spring 2003 and in summer 2004 I have moved on to develop Volumetrics. 

If you are commericially interested in the VoxelWorld project, technology or simply wish to comment on my work, you can send email to dmytry_lavrov@yahoo.com Skype handle: dmytrylk 

Copyright
---------
Anyone is welcome to maintain this project as open source software. Hereby, you have my permission to set up github project page or whatever. The code is released under GPL. My original copyright must remain on the code, however.

Building
--------
Source code dependencies: `freepascal`, `opengl`, `glut`.

Run: `fpc -Mobjfpc -O3 vwscript.pas` to build command line binary.

Run: `fpc -Mobjfpc -O3 vwint_glut.pas` to build interactive viewer using glut.

No makefiles needed. Pascal supports proper modules, and builds everything on demand automatically.

Notice
-------
Note: I recommend you to get recent antivirus and always check any binary files downloaded from the web or received by e-mail, to prevent virus spreading and possible data losses, or information leaks. 