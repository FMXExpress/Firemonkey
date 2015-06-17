
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.StdCtrls,

  System.Sensors, FMX.Controls.Presentation;

type
  TForm4 = class(TForm)
    lblLatitude: TLabel;
    etLatitude: TEdit;
    lblLongitude: TLabel;
    etLongitude: TEdit;
    chkActivateManager: TCheckBox;
    chkActivateSensor: TCheckBox;
    procedure chkActivateManagerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkActivateSensorChange(Sender: TObject);
  private
    { Private declarations }
    FManager: TSensorManager;
    FSensor: TCustomLocationSensor;
  public
    { Public declarations }
    procedure LocationHandler(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

procedure TForm4.chkActivateManagerChange(Sender: TObject);
var
  Sensors: TSensorArray;
  Activated: Boolean;
  Found: Boolean;
begin
  Activated := chkActivateManager.IsChecked;

  FManager.Active := Activated;
  chkActivateSensor.Enabled := Activated;

  Found := Activated;
  if Activated then
  begin
    Sensors := FManager.GetSensorsByCategory(TSensorCategory.Location);

    Found := Length(Sensors) > 0;
    if Found then
    begin
      FSensor := Sensors[0] as TCustomLocationSensor;
      FSensor.OnLocationChanged := LocationHandler;
      FSensor.Optimize := True;
    end;
  end;

  if not Found then
    FSensor := nil;
end;

procedure TForm4.chkActivateSensorChange(Sender: TObject);
begin
  if Assigned(FSensor) then
    if chkActivateSensor.IsChecked then
      FSensor.Start
    else
      FSensor.Stop;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  FManager := TSensorManager.Current;
  chkActivateManager.Enabled := FManager.CanActivate;
end;

procedure TForm4.LocationHandler(Sender: TObject; const OldLocation,
  NewLocation: TLocationCoord2D);
begin
  etLatitude.Text := FloatToStr(NewLocation.Latitude);
  etLongitude.Text := FloatToStr(NewLocation.Longitude);
end;

end.
