//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
program ListViewVariableHeightItems;



{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {VariableHeight};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  //GlobalUseGPUCanvas := True;
  Application.Initialize;
  Application.CreateForm(TVariableHeight, VariableHeight);
  Application.Run;
end.
