
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MonitorForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
  TMonitorGuageForm = class(TForm)
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    Times: TThread.TSystemTimes;
  public
  end;

var
  MonitorGuageForm: TMonitorGuageForm;

implementation

{$R *.fmx}

procedure TMonitorGuageForm.FormCreate(Sender: TObject);
begin
  TThread.GetSystemTimes(Times);
end;

procedure TMonitorGuageForm.FormResize(Sender: TObject);
begin
  ProgressBar1.Width := ClientWidth - 16;
  ProgressBar1.Height := ClientHeight - 48;
  Label1.Position.Y := ProgressBar1.Position.Y + ProgressBar1.Height + 8;
  Label1.Width := ProgressBar1.Width;
end;

procedure TMonitorGuageForm.Timer1Timer(Sender: TObject);
var
  Usage: Integer;
begin
  Usage := TThread.GetCPUUsage(Times);
  ProgressBar1.Value := Usage;
  Label1.Text := IntToStr(Usage) + '%';
end;

end.
