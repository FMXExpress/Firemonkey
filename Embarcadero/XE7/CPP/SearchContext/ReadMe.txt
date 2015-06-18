This demo covers using Effects, Opacity & UseOSMenu.

------------------
3 Things to check.

1) Move the iconData.cds to the Same folder as the 
   compiled exe for Win32
2) Make sure the iconData.cds is included in the "Project > Deployment" 
   Options for the project to MAC.
3) Make sure Deployment options include Midas.


To Demo... (2 parts)
------------------
1) Using Opacity and Effects 

  start typing in 

    Clock

  Your see the icons fade out (Opacity) and also highlight appropriately (GlowEffect). 
  Once Clock is complete, pause, and then carry on to type 

    Clocking 

  (as in Performance Clocking) so see the Performance Icon Highlight.

  All Search options can be managed via the "Options > Manage Data" link.
  Double click the icon image to add more icons.

2) Use OS Menu

  Run the project to MAC. Show the menu in the app (UseOSMenu = False by default)
  Then update the UserOSMenu property of the menu in the IDE, recompile and run to mac.
  Now you will see the MAC Menu used.
