
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program BindListFMXProject;

uses
  FMX.Forms,
  BindListFMXFormUnit1 in 'BindListFMXFormUnit1.pas' {Form1};

//  BindCompDB in '..\..\..\BindCompDB.pas',
//  BindCompEditors in '..\..\..\BindCompEditors.pas',
//  BindCompEngExt in '..\..\..\BindCompEngExt.pas',
//  BindCompFMXDBEngExt in '..\..\..\BindCompFMXDBEngExt.pas',
//  BindCompFMXEditors in '..\..\..\BindCompFMXEditors.pas',
//  BindCompFMXNavigator in '..\..\..\BindCompFMXNavigator.pas',
//  BindCompFMXResStrs in '..\..\..\BindCompFMXResStrs.pas',
//  BindComponents in '..\..\..\BindComponents.pas',
//  BindCompResStrs in '..\..\..\BindCompResStrs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
