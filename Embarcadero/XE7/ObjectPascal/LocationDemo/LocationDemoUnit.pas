//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit LocationDemoUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.StdCtrls,
  FMX.ListBox, FMX.Layouts, FMX.WebBrowser, System.Sensors,
  System.Sensors.Components, FMX.MultiView;

type
  TForm1 = class(TForm)
    WebBrowser1: TWebBrowser;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    Switch1: TSwitch;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItemLatitude: TListBoxItem;
    ListBoxItemLongitude: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItemAdminArea: TListBoxItem;
    ListBoxItemCountryCode: TListBoxItem;
    ListBoxItemCountryName: TListBoxItem;
    ListBoxItemFeatureName: TListBoxItem;
    ListBoxItemLocality: TListBoxItem;
    ListBoxItemPostalCode: TListBoxItem;
    ListBoxItemSubAdminArea: TListBoxItem;
    ListBoxItemSubLocality: TListBoxItem;
    ListBoxItemSubThoroughfare: TListBoxItem;
    ListBoxItemThoroughfare: TListBoxItem;
    Label1: TLabel;
    LocationSensor1: TLocationSensor;
    MultiView1: TMultiView;
    ToolBar1: TToolBar;
    Button1: TButton;
    ToolBar2: TToolBar;
    Button2: TButton;
    Layout1: TLayout;
    procedure LocationSensor1LocationChanged(Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D);
    procedure Switch1Switch(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FGeocoder: TGeocoder;
    procedure OnGeocodeReverseEvent(const Address: TCivicAddress);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
procedure TForm1.Switch1Switch(Sender: TObject);
begin
  LocationSensor1.Active := Switch1.IsChecked;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MultiView1.HideMaster;
end;

procedure TForm1.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
var
  URLString: String;
  LSettings: TFormatSettings;
  LDecSeparator : Char;
begin
  LDecSeparator := FormatSettings.DecimalSeparator;
  LSettings := FormatSettings;
  try
    FormatSettings.DecimalSeparator := '.';
    // Show current location
    ListBoxItemLatitude.ItemData.Detail  := Format('%2.6f', [NewLocation.Latitude]);
    ListBoxItemLongitude.ItemData.Detail := Format('%2.6f', [NewLocation.Longitude]);

    // Show Map using Google Maps
    URLString := Format('https://maps.google.com/maps?q=%2.6f,%2.6f', [ NewLocation.Latitude, NewLocation.Longitude]);
  finally
    FormatSettings.DecimalSeparator := LDecSeparator;
  end;
  WebBrowser1.Navigate(URLString);

  // Setup an instance of TGeocoder
  if not Assigned(FGeocoder) then
  begin
    if Assigned(TGeocoder.Current) then
      FGeocoder := TGeocoder.Current.Create;
    if Assigned(FGeocoder) then
      FGeocoder.OnGeocodeReverse := OnGeocodeReverseEvent;
  end;

  // Translate location to address
  if Assigned(FGeocoder) and not FGeocoder.Geocoding then
    FGeocoder.GeocodeReverse(NewLocation);
end;

procedure TForm1.OnGeocodeReverseEvent(const Address: TCivicAddress);
begin
  ListBoxItemAdminArea.ItemData.Detail       := Address.AdminArea;
  ListBoxItemCountryCode.ItemData.Detail     := Address.CountryCode;
  ListBoxItemCountryName.ItemData.Detail     := Address.CountryName;
  ListBoxItemFeatureName.ItemData.Detail     := Address.FeatureName;
  ListBoxItemLocality.ItemData.Detail        := Address.Locality;
  ListBoxItemPostalCode.ItemData.Detail      := Address.PostalCode;
  ListBoxItemSubAdminArea.ItemData.Detail    := Address.SubAdminArea;
  ListBoxItemSubLocality.ItemData.Detail     := Address.SubLocality;
  ListBoxItemSubThoroughfare.ItemData.Detail := Address.SubThoroughfare;
  ListBoxItemThoroughfare.ItemData.Detail    := Address.Thoroughfare;
end;

end.
