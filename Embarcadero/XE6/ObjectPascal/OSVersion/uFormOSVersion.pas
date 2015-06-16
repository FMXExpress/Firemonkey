
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uFormOSVersion;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo,
  FMX.StdCtrls;

type
  TFormOSVersion = class(TForm)
    ButtonGetOSInfo: TButton;
    PanelTop: TPanel;
    MemoLog: TMemo;
    procedure ButtonGetOSInfoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOSVersion: TFormOSVersion;

implementation

{$R *.fmx}

uses
  uOSVersionUtils;

procedure TFormOSVersion.ButtonGetOSInfoClick(Sender: TObject);
begin
  with MemoLog.Lines do
  begin
    Clear;
    Add(TOSVersion.ToString);
    Add('');
    Add('Architecture: ' + OSArchitectureToStr(TOSVersion.Architecture));
    Add('Platform: ' + OSPlatformToStr(TOSVersion.Platform));
    Add('Build: ' + IntToStr(TOSVersion.Build));
    Add('Major: ' + IntToStr(TOSVersion.Major));
    Add('Minor: ' + IntToStr(TOSVersion.Minor));
    Add('Name: ' + TOSVersion.Name);
    Add('Service Pack - Major: ' + IntToStr(TOSVersion.ServicePackMajor));
    Add('Service Pack - Minor: ' + IntToStr(TOSVersion.ServicePackMinor));
  end;
end;

end.
