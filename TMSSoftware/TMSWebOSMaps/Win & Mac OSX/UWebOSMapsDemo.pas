unit UWebOSMapsDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Layouts, FMX.Memo, FMX.ListBox, FMX.Edit,
  FMX.TabControl, FMX.TMSWebOSMapsWebBrowser, FMX.TMSWebOSMaps, FMX.TMSWebOSMapsMarkers,
  FMX.TMSWebOSMapsPolyGons, FMX.TMSWebOSMapsPolyLines, FMX.TMSWebOSMapsCommon;

type
  TForm90 = class(TForm)
    Panel1: TPanel;
    lbEvents: TLabel;
    Label1: TLabel;
    TMSFMXWebOSMaps1: TTMSFMXWebOSMaps;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    btAddMarker: TButton;
    edLatitude: TEdit;
    Label5: TLabel;
    rbMarkerDefault: TRadioButton;
    Label8: TLabel;
    edLongitude: TEdit;
    Label6: TLabel;
    rbMarkerImage: TRadioButton;
    cbAddMarker: TCheckBox;
    btClearMarkers: TButton;
    TabItem2: TTabItem;
    btAddPolygon: TButton;
    btClearPolygons: TButton;
    Label9: TLabel;
    Label10: TLabel;
    edPolygonLongitude: TEdit;
    edPolygonLatitude: TEdit;
    Label11: TLabel;
    cbPolygonType: TComboBox;
    Label7: TLabel;
    edPolygonCaption: TEdit;
    Memo1: TMemo;
    TabItem4: TTabItem;
    Label16: TLabel;
    cbMousePositionControl: TCheckBox;
    cbScaleControl: TCheckBox;
    cbOverviewControl: TCheckBox;
    cbZoomControl: TCheckBox;
    cbMapLayers: TCheckBox;
    procedure btAddMarkerClick(Sender: TObject);
    procedure btAddPolygonClick(Sender: TObject);
    procedure btClearMarkersClick(Sender: TObject);
    procedure TMSFMXWebOSMaps1MapClick(Sender: TObject; Latitude,
      Longitude: Double; X, Y: Integer);
    procedure TMSFMXWebOSMaps1MapMoveEnd(Sender: TObject);
    procedure TMSFMXWebOSMaps1MapZoomChange(Sender: TObject; NewLevel: Integer);
    procedure TMSFMXWebOSMaps1MarkerClick(Sender: TObject; IdMarker: Integer);
    procedure TMSFMXWebOSMaps1PolygonClick(Sender: TObject; IdPolygon: Integer);
    procedure TMSFMXWebOSMaps1PolylineClick(Sender: TObject;
      IdPolyline: Integer);
    procedure btClearPolygonsClick(Sender: TObject);
    procedure cbZoomControlChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form90: TForm90;

implementation


const
  MarkerImage = 'http://www.tmssoftware.net/public/iwwebgmaps_pin.png';


{$R *.fmx}

procedure TForm90.btAddMarkerClick(Sender: TObject);
var
  Marker: TMarker;
begin
  if (edLatitude.Text <> '') and (edLongitude.Text <> '') then
  begin
    Marker := TMSFMXWebOSMaps1.Markers.Add;
    Marker.Latitude := StrToFloat(edLatitude.Text);
    Marker.Longitude := StrToFloat(edLongitude.Text);
    if rbMarkerImage.IsChecked then
      Marker.Icon := MarkerImage;
    TMSFMXWebOSMaps1.CreateMapMarker(Marker);
    TMSFMXWebOSMaps1.MapPanTo(Marker.Latitude, Marker.Longitude);
  end;
end;

procedure TForm90.btAddPolygonClick(Sender: TObject);
var
  Path: TPath;
  pi: TPathItem;
  PolygonLatitude, PolygonLongitude: Double;
  PolygonItem: TPolygonItem;
  PolylineItem: TPolylineItem;
  Circle, Rect: TMapPolygon;
begin
  if TryStrToFloat(edPolygonLatitude.Text, PolygonLatitude)
    and TryStrToFloat(edPolygonLongitude.Text, PolygonLongitude) then
  begin
    TMSFMXWebOSMaps1.MapPanTo(PolygonLatitude, PolygonLongitude);

    //Line
    if cbPolygonType.ItemIndex = 0 then
    begin
      Path := TPath.Create;

      pi := Path.Add;
      pi.Latitude := 0;
      pi.Longitude := 0;

      pi := Path.Add;
      pi.Latitude := PolygonLatitude;
      pi.Longitude := PolygonLongitude;

      PolylineItem := TMSFMXWebOSMaps1.Polylines.Add(Path, TAlphaColorRec.Green, 255, 2, True);
      PolylineItem.Polyline.LabelText := edPolygonCaption.Text;

      TMSFMXWebOSMaps1.UpdateMapPolyline(PolylineItem.Polyline);
    end
    //Circle
    else if cbPolygonType.ItemIndex = 1 then
    begin
      PolygonItem := TMSFMXWebOSMaps1.Polygons.Add;
      Circle := PolygonItem.Polygon;
      Circle.LabelText := edPolygonCaption.Text;
      Circle.PolygonType := ptCircle;
      Circle.BackgroundOpacity := 50;
      Circle.BorderWidth := 2;
      Circle.Radius := 75000;
      Circle.Center.Latitude := PolygonLatitude;
      Circle.Center.Longitude := PolygonLongitude;
      TMSFMXWebOSMaps1.CreateMapPolygon(Circle);
    end
    //Square
    else if cbPolygonType.ItemIndex = 2 then
    begin
      PolygonItem := TMSFMXWebOSMaps1.Polygons.Add;
      Rect := PolygonItem.Polygon;
      Rect.LabelText := edPolygonCaption.Text;
      Rect.PolygonType := ptRectangle;
      Rect.BackgroundOpacity := 0;
      Rect.BorderWidth := 2;
      Rect.BorderColor := TAlphaColorRec.Black;
      Rect.BorderOpacity := 100;
      Rect.Bounds.SouthWest.Latitude := PolygonLatitude - 5;
      Rect.Bounds.SouthWest.Longitude := PolygonLongitude - 5;
      Rect.Bounds.NorthEast.Latitude := PolygonLatitude;
      Rect.Bounds.NorthEast.Longitude := PolygonLongitude;
      TMSFMXWebOSMaps1.CreateMapPolygon(Rect);
    end;
  end;
end;

procedure TForm90.btClearMarkersClick(Sender: TObject);
begin
  TMSFMXWebOSMaps1.Markers.Clear;
  TMSFMXWebOSMaps1.DeleteAllMapMarker;
end;

procedure TForm90.btClearPolygonsClick(Sender: TObject);
begin
  TMSFMXWebOSMaps1.Polylines.Clear;
  TMSFMXWebOSMaps1.Polygons.Clear;
  TMSFMXWebOSMaps1.DeleteAllMapPolyline;
  TMSFMXWebOSMaps1.DeleteAllMapPolygon;
end;

procedure TForm90.cbZoomControlChange(Sender: TObject);
begin
  TMSFMXWebOSMaps1.ControlsOptions.PanZoomControl.Visible := cbZoomControl.IsChecked;
  TMSFMXWebOSMaps1.ControlsOptions.ScaleControl.Visible := cbScaleControl.IsChecked;
  TMSFMXWebOSMaps1.ControlsOptions.LayerSwitcher.Visible := cbMapLayers.IsChecked;
  TMSFMXWebOSMaps1.ControlsOptions.OverviewMapControl.Visible := cbOverviewControl.IsChecked;
  TMSFMXWebOSMaps1.ControlsOptions.MousePosition.Visible := cbMousePositionControl.IsChecked;
end;

procedure TForm90.TMSFMXWebOSMaps1MapClick(Sender: TObject; Latitude,
  Longitude: Double; X, Y: Integer);
var
  MarkerIcon: string;
begin
  lbEvents.Text := 'Events: MAP Click: Latitude = ' + FloatToStr(Latitude);

  if cbAddMarker.IsChecked then
  begin
    if rbMarkerImage.IsChecked then
      MarkerIcon := MarkerImage;
    TMSFMXWebOSMaps1.Markers.Add(Latitude, Longitude, 'New Marker', MarkerIcon, false, true);
  end;
end;

procedure TForm90.TMSFMXWebOSMaps1MapMoveEnd(Sender: TObject);
begin
  lbEvents.Text := 'Events: MAP Move End';
end;

procedure TForm90.TMSFMXWebOSMaps1MapZoomChange(Sender: TObject;
  NewLevel: Integer);
begin
  lbEvents.Text := 'Events: MAP Zoom change to level ' + IntToStr(NewLevel);
end;

procedure TForm90.TMSFMXWebOSMaps1MarkerClick(Sender: TObject;
  IdMarker: Integer);
begin
  lbEvents.Text := 'Events: MAP Marker Click: ' + FloatToStr(IdMarker);
end;

procedure TForm90.TMSFMXWebOSMaps1PolygonClick(Sender: TObject;
  IdPolygon: Integer);
begin
  lbEvents.Text := 'Events: MAP Polygon Click: ' + IntToStr(IdPolygon);
end;

procedure TForm90.TMSFMXWebOSMaps1PolylineClick(Sender: TObject;
  IdPolyline: Integer);
begin
  lbEvents.Text := 'Events: MAP Polyline Click: ' + IntToStr(IdPolyline);
end;

end.
