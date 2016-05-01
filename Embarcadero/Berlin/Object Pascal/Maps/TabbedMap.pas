//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit TabbedMap;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Maps, FMX.TabControl, FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TForm2 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    MapPiter: TMapView;
    MapFrisco: TMapView;
    CameraInfo: TLabel;
    ZoomOut: TButton;
    ZoomIn: TButton;
    procedure FormShow(Sender: TObject);
    procedure CameraChanged(Sender: TObject);
    procedure ZoomOutClick(Sender: TObject);
    procedure ZoomInClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.ZoomOutClick(Sender: TObject);
begin
  MapPiter.Zoom := MapPiter.Zoom - 1;
  MapFrisco.Zoom := MapFrisco.Zoom - 1;
end;

procedure TForm2.ZoomInClick(Sender: TObject);
begin
  MapPiter.Zoom := MapPiter.Zoom + 1;
  MapFrisco.Zoom := MapFrisco.Zoom + 1;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  MapPiter.Location := TMapCoordinate.Create(59.965, 30.35);
  MapPiter.Zoom := 10;

  MapFrisco.Location := TMapCoordinate.Create(37.82, -122.5);
  MapFrisco.Zoom := 10;
end;

procedure TForm2.CameraChanged(Sender: TObject);
begin
  CameraInfo.Text := Format('Camera at %3.3f, %3.3f Zoom=%2f', [TMapView(Sender).Location.Latitude,
    TMapView(Sender).Location.Longitude, TMapView(Sender).Zoom]);
end;

end.
