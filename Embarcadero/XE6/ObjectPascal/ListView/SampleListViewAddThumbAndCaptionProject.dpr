//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program SampleListViewAddThumbAndCaptionProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  AddThumbAndCaptionMainFormU in 'AddThumbAndCaptionMainFormU.pas' {Form594};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm594, Form594);
  Application.Run;
end.
