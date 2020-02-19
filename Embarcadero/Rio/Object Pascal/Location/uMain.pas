//---------------------------------------------------------------------------

// This software is Copyright (c) 2015-2019 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, System.Sensors, FMX.StdCtrls,
  FMX.Edit, FMX.WebBrowser, FMX.ListBox, FMX.Layouts, System.Sensors.Components,
  FMX.Controls.Presentation, FMX.EditBox, FMX.NumberBox;

type
  TLocationForm = class(TForm)
  published var
    LocationSensor1: TLocationSensor;
    WebBrowser1: TWebBrowser;
    ListBox1: TListBox;
    lbLocationSensor: TListBoxItem;
    swLocationSensorActive: TSwitch;
    lbTriggerDistance: TListBoxItem;
    nbTriggerDistance: TNumberBox;
    Button1: TButton;
    Button2: TButton;
    lbAccuracy: TListBoxItem;
    Button3: TButton;
    Button4: TButton;
    nbAccuracy: TNumberBox;
    lbLatitude: TListBoxItem;
    lbLongitude: TListBoxItem;
    ToolBar1: TToolBar;
    Label1: TLabel;
  published
    procedure LocationSensor1LocationChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure swLocationSensorActiveSwitch(Sender: TObject);
    procedure nbTriggerDistanceChange(Sender: TObject);
    procedure nbAccuracyChange(Sender: TObject);
  end;

implementation

uses
  System.Permissions,
  FMX.DialogService;

{$R *.fmx}

procedure TLocationForm.Button1Click(Sender: TObject);
begin
  nbTriggerDistance.Value := nbTriggerDistance.Value - 1;
end;

procedure TLocationForm.Button2Click(Sender: TObject);
begin
  nbTriggerDistance.Value := nbTriggerDistance.Value + 1;
end;

procedure TLocationForm.Button3Click(Sender: TObject);
begin
  nbAccuracy.Value := nbAccuracy.Value - 1;
end;

procedure TLocationForm.Button4Click(Sender: TObject);
begin
  nbAccuracy.Value := nbAccuracy.Value + 1;
end;

procedure TLocationForm.LocationSensor1LocationChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
const
  GoogleMapsURL: String = 'https://maps.google.com/maps?q=%s,%s';
begin
  var Latitude := NewLocation.Latitude.ToString(ffGeneral, 5, 2, TFormatSettings.Create('en-US'));
  var Longitude := NewLocation.Longitude.ToString(ffGeneral, 5, 2, TFormatSettings.Create('en-US'));

  { convert the location to latitude and longitude }
  lbLatitude.Text := 'Latitude: ' + Latitude;
  lbLongitude.Text := 'Longitude: ' + Longitude;

  { and track the location via Google Maps }
  WebBrowser1.Navigate(Format(GoogleMapsURL, [Latitude, Longitude]));
end;

procedure TLocationForm.nbAccuracyChange(Sender: TObject);
begin
  { set the precision }
  LocationSensor1.Accuracy := nbAccuracy.Value;
end;

procedure TLocationForm.nbTriggerDistanceChange(Sender: TObject);
begin
  { set the triggering distance }
  LocationSensor1.Distance := nbTriggerDistance.Value;
end;

procedure TLocationForm.swLocationSensorActiveSwitch(Sender: TObject);
const
  PermissionAccessFineLocation = 'android.permission.ACCESS_FINE_LOCATION';
begin
{$IFDEF ANDROID}
  PermissionsService.RequestPermissions([PermissionAccessFineLocation],
    procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
        { activate or deactivate the location sensor }
        LocationSensor1.Active := swLocationSensorActive.IsChecked
      else
      begin
        swLocationSensorActive.IsChecked := False;

        TDialogService.ShowMessage('Location permission not granted');
      end;
    end);
{$ELSE}
  LocationSensor1.Active := swLocationSensorActive.IsChecked;
{$ENDIF}
end;

end.
