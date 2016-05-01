//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Maps;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Edit,
  FMX.Maps;

type
  TForm1 = class(TForm)
    TopToolBar: TToolBar;
    BottomToolBar: TToolBar;
    Label1: TLabel;
    edLat: TEdit;
    edLong: TEdit;
    Button1: TButton;
    MapView1: TMapView;
    Panel1: TPanel;
    GridPanelLayout1: TGridPanelLayout;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    TrackBar1: TTrackBar;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure MapView1MapClick(const Position: TMapCoordinate);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

// -------------------For Normal button -----------------------------------------


procedure TForm1.Button1Click(Sender: TObject);
var
  mapCenter: TMapCoordinate;
begin
  mapCenter := TMapCoordinate.Create(StrToFloat(edLat.Text, TFormatSettings.Invariant),
    StrToFloat(edLong.Text, TFormatSettings.Invariant));
  MapView1.Location := mapCenter;
end;

procedure TForm1.MapView1MapClick(const Position: TMapCoordinate);
var
  MyMarker: TMapMarkerDescriptor;
begin
  MyMarker := TMapMarkerDescriptor.Create(Position, 'MyMarker');
  MyMarker.Draggable := True;
  MyMarker.Visible :=True;
  MapView1.AddMarker(MyMarker);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  MapView1.MapType := TMapType.Normal;
  TrackBar1.Value := 0.0;
end;

// -------------------For Satellite button---------------------------------------

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  MapView1.MapType := TMapType.Satellite;
  TrackBar1.Value := 0.0;
end;

// --------------------For Hybrid button-----------------------------------------

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  MapView1.MapType := TMapType.Hybrid;
  TrackBar1.Value := 0.0;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
MapView1.Bearing := TrackBar1.Value;
end;

end.
