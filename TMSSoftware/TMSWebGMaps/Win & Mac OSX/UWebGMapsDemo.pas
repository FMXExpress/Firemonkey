unit UWebGMapsDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.TMSWebGMapsCommon, FMX.TMSWebGMapsGeocoding, FMX.ListBox,
  FMX.Layouts, FMX.Memo, FMX.Edit, FMX.TabControl, FMX.TMSWebGMapsWebBrowser,
  FMX.TMSWebGMaps, FMX.TMSWebGMapsPolyLines, FMX.TMSWebGMapsPolyGons,
  FMX.TMSWebGMapsDirections, FMX.TMSWebGMapsMarkers;

type
  TForm90 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    lbEvents: TLabel;
    TMSFMXWebGMaps1: TTMSFMXWebGMaps;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    btAddMarker: TButton;
    edLatitude: TEdit;
    Label5: TLabel;
    btAddMarkerAddress: TButton;
    rbMarkerDefault: TRadioButton;
    Label8: TLabel;
    edLongitude: TEdit;
    edMarkerAddress: TEdit;
    Label6: TLabel;
    Label7: TLabel;
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
    Memo1: TMemo;
    TabItem3: TTabItem;
    edTo: TEdit;
    edFrom: TEdit;
    btGetDirections: TButton;
    btClearDirections: TButton;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    lbRoutes: TListBox;
    lbRouteDetails: TListBox;
    TabItem4: TTabItem;
    cbCycle: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    rbRoad: TRadioButton;
    rbSatellite: TRadioButton;
    rbTerrain: TRadioButton;
    rbHybrid: TRadioButton;
    cbPano: TCheckBox;
    cbTraffic: TCheckBox;
    cbStreetView: TCheckBox;
    cbWeather: TCheckBox;
    cbCloud: TCheckBox;
    cbStreetViewControl: TCheckBox;
    cbScaleControl: TCheckBox;
    cbMapType: TCheckBox;
    cbOverviewControl: TCheckBox;
    cbZoomControl: TCheckBox;
    TMSFMXWebGMapsGeocoding1: TTMSFMXWebGMapsGeocoding;
    procedure btAddMarkerAddressClick(Sender: TObject);
    procedure btAddPolygonClick(Sender: TObject);
    procedure btGetDirectionsClick(Sender: TObject);
    procedure cbStreetViewChange(Sender: TObject);
    procedure TMSFMXWebGMaps1MapClick(Sender: TObject; Latitude,
      Longitude: Double; X, Y: Integer);
    procedure TMSFMXWebGMaps1MapMoveEnd(Sender: TObject; Latitude,
      Longitude: Double; X, Y: Integer);
    procedure TMSFMXWebGMaps1MapZoomChange(Sender: TObject; NewLevel: Integer);
    procedure TMSFMXWebGMaps1MarkerClick(Sender: TObject; MarkerTitle: string;
      IdMarker: Integer; Latitude, Longitude: Double);
    procedure TMSFMXWebGMaps1PolygonClick(Sender: TObject; IdPolygon: Integer);
    procedure TMSFMXWebGMaps1PolylineClick(Sender: TObject;
      IdPolyline: Integer);
    procedure btAddMarkerClick(Sender: TObject);
    procedure btClearMarkersClick(Sender: TObject);
    procedure btClearPolygonsClick(Sender: TObject);
    procedure btClearDirectionsClick(Sender: TObject);
    procedure lbRoutesChange(Sender: TObject);
    procedure rbRoadClick(Sender: TObject);
    procedure rbSatelliteClick(Sender: TObject);
    procedure rbTerrainClick(Sender: TObject);
    procedure rbHybridClick(Sender: TObject);
    procedure cbCycleChange(Sender: TObject);
    procedure cbStreetViewControlChange(Sender: TObject);
    procedure TMSFMXWebGMaps1MarkerDrag(Sender: TObject; MarkerTitle: string;
      IdMarker: Integer; Latitude, Longitude: Double);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DisplayRoute;
    procedure DisplayRouteDetails;
    procedure ClearDirections;
  end;

var
  Form90: TForm90;

implementation

const
  MarkerImage = 'http://www.tmssoftware.net/public/iwwebgmaps_pin.png';

{$R *.fmx}

procedure TForm90.btAddMarkerAddressClick(Sender: TObject);
var
  MarkerIcon: string;
begin
  MarkerIcon := '';
  TMSFMXWebGMapsGeocoding1.Address := edMarkerAddress.Text;
  if TMSFMXWebGMapsGeocoding1.LaunchGeocoding = erOk then
  begin
    if rbMarkerImage.IsChecked then
      MarkerIcon := MarkerImage;
    TMSFMXWebGmaps1.Markers.Add(TMSFMXWebGMapsGeocoding1.ResultLatitude,
      TMSFMXWebGMapsGeocoding1.ResultLongitude, edMarkerAddress.Text, MarkerIcon, true, true, true, false, true, 0);
    TMSFMXWebGMaps1.MapPanTo(TMSFMXWebGMapsGeocoding1.ResultLatitude,
      TMSFMXWebGMapsGeocoding1.ResultLongitude);
  end;
end;

procedure TForm90.btAddMarkerClick(Sender: TObject);
var
  Marker: TMarker;
begin
  if (edLatitude.Text <> '') and (edLongitude.Text <> '') then
  begin
    Marker := TMSFMXWebGMaps1.Markers.Add;
    Marker.Latitude := StrToFloat(edLatitude.Text);
    Marker.Longitude := StrToFloat(edLongitude.Text);
    if rbMarkerImage.IsChecked then
      Marker.Icon := MarkerImage;
    Marker.MapLabel.Text := 'New Marker';
    TMSFMXWebGMaps1.CreateMapMarker(Marker);
    TMSFMXWebGMaps1.MapPanTo(Marker.Latitude, Marker.Longitude);
  end;
end;

procedure TForm90.btAddPolygonClick(Sender: TObject);
var
  Path: TPath;
  pi: TPathItem;
  PolygonLatitude, PolygonLongitude: Double;
  PolygonItem: TPolygonItem;
  Circle, Rect: TMapPolygon;
begin
  if TryStrToFloat(edPolygonLatitude.Text, PolygonLatitude) and TryStrToFloat(edPolygonLongitude.Text, PolygonLongitude) then
  begin
    TMSFMXWebGMaps1.MapPanTo(PolygonLatitude, PolygonLongitude);

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

      TMSFMXWebGMaps1.Polylines.Add(True, False, False, nil, Path, TAlphaColorRec.Blue, 255, 2, True, 0);
    end
    //Circle
    else if cbPolygonType.ItemIndex = 1 then
    begin
      PolygonItem := TMSFMXWebGMaps1.Polygons.Add;
      Circle := PolygonItem.Polygon;
      Circle.Clickable := true;
      Circle.PolygonType := ptCircle;
      Circle.BackgroundOpacity := 50;
      Circle.BorderWidth := 2;
      Circle.Radius := 75000;
      Circle.Center.Latitude := PolygonLatitude;
      Circle.Center.Longitude := PolygonLongitude;
      TMSFMXWebGMaps1.CreateMapPolygon(Circle);
    end
    //Square
    else if cbPolygonType.ItemIndex = 2 then
    begin
      PolygonItem := TMSFMXWebGMaps1.Polygons.Add;
      Rect := PolygonItem.Polygon;
      Rect.Clickable := true;
      Rect.PolygonType := ptRectangle;
      Rect.BackgroundOpacity := 0;
      Rect.BorderWidth := 2;
      Rect.BorderColor := TAlphaColorRec.Black;
      Rect.BorderOpacity := 100;
      Rect.Bounds.SouthWest.Latitude := PolygonLatitude - 10;
      Rect.Bounds.SouthWest.Longitude := PolygonLongitude - 10;
      Rect.Bounds.NorthEast.Latitude := PolygonLatitude;
      Rect.Bounds.NorthEast.Longitude := PolygonLongitude;
      TMSFMXWebGMaps1.CreateMapPolygon(Rect);
    end;
  end;

end;

procedure TForm90.btClearDirectionsClick(Sender: TObject);
begin
  ClearDirections;
end;

procedure TForm90.btClearMarkersClick(Sender: TObject);
begin
  TMSFMXWebGMaps1.Markers.Clear;
  TMSFMXWebGMaps1.DeleteAllMapMarker;
end;

procedure TForm90.btClearPolygonsClick(Sender: TObject);
begin
  TMSFMXWebGMaps1.Polylines.Clear;
  TMSFMXWebGMaps1.Polygons.Clear;
  TMSFMXWebGMaps1.DeleteAllMapPolyline;
  TMSFMXWebGMaps1.DeleteAllMapPolygon;
end;

procedure TForm90.btGetDirectionsClick(Sender: TObject);
var
  I, J: Integer;
  TotalDistance, TotalDuration: integer;
  Description: string;
begin
  ClearDirections;

  TMSFMXWebGmaps1.GetDirections(edFrom.Text, edTo.Text, true, tmDriving, usMetric,
    lnDefault, true);

  if TMSFMXWebGmaps1.Directions.Count > 0 then
  begin
    lbRoutes.Items.Clear;
    for I := 0 to TMSFMXWebGmaps1.Directions.Count - 1 do
    begin
      Description := TMSFMXWebGMaps1.Directions[I].Summary + ': ';

      if TMSFMXWebGMaps1.Directions[I].Legs.Count = 1 then
        Description := Description
        + TMSFMXWebGMaps1.Directions[I].Legs[0].DistanceText + ', '
        + TMSFMXWebGMaps1.Directions[I].Legs[0].DurationText
      else
      begin
        TotalDistance := 0;
        TotalDuration := 0;
        for J := 0 to TMSFMXWebGMaps1.Directions[I].Legs.Count - 1 do
        begin
          TotalDistance := TotalDistance + TMSFMXWebGMaps1.Directions[I].Legs[J].Distance;
          TotalDuration := TotalDuration + TMSFMXWebGMaps1.Directions[I].Legs[J].Duration;
        end;
        Description := Description +
          FormatFloat('0.00', TotalDistance / 1000) + ' km, '
          + FormatFloat('0.00', (TotalDuration / 60) / 60) + ' h'
      end;
      lbRoutes.Items.Add(Description);
    end;
    lbRoutes.ItemIndex := 0;

    DisplayRouteDetails;
    DisplayRoute;
  end
  else
    ShowMessage('"From" or "To" location not found.');
end;

procedure TForm90.cbCycleChange(Sender: TObject);
begin
  TMSFMXWebGMaps1.MapOptions.ShowBicycling := cbCycle.IsChecked;
  TMSFMXWebGMaps1.MapOptions.ShowPanoramio := cbPano.IsChecked;
  TMSFMXWebGMaps1.MapOptions.ShowTraffic := cbTraffic.IsChecked;
  TMSFMXWebGMaps1.StreetViewOptions.Visible := cbStreetView.IsChecked;
  TMSFMXWebGMaps1.MapOptions.ShowWeather := cbWeather.IsChecked;
  TMSFMXWebGMaps1.MapOptions.ShowCloud := cbCloud.IsChecked;
end;

procedure TForm90.cbStreetViewChange(Sender: TObject);
begin
  TMSFMXWebGMaps1.StreetViewOptions.Visible := cbStreetView.IsChecked;
end;

procedure TForm90.cbStreetViewControlChange(Sender: TObject);
begin
  TMSFMXWebGMaps1.ControlsOptions.StreetViewControl.Visible := cbStreetViewControl.IsChecked;
  TMSFMXWebGMaps1.ControlsOptions.ScaleControl.Visible := cbScaleControl.IsChecked;
  TMSFMXWebGMaps1.ControlsOptions.MapTypeControl.Visible := cbMapType.IsChecked;
  TMSFMXWebGMaps1.ControlsOptions.OverviewMapControl.Visible := cbOverviewControl.IsChecked;
  TMSFMXWebGMaps1.ControlsOptions.ZoomControl.Visible := cbZoomControl.IsChecked;
end;

procedure TForm90.ClearDirections;
begin
  TMSFMXWebGMaps1.Directions.Clear;
  TMSFMXWebGMaps1.Polylines.Clear;
  TMSFMXWebGMaps1.Polygons.Clear;
  TMSFMXWebGMaps1.Markers.Clear;
  TMSFMXWebGmaps1.DeleteAllMapPolyline;
  TMSFMXWebGmaps1.DeleteAllMapPolygon;
  TMSFMXWebGmaps1.DeleteAllMapMarker;
  TMSFMXWebGMaps1.Directions.Clear;
  lbRoutes.Clear;
  lbRouteDetails.Clear;
end;

procedure TForm90.DisplayRoute;
var
  Route: TRoute;
  Marker: TMarker;
  Circle: TMapPolygon;
  Rect: TMapPolygon;
  PolygonItem: TPolygonItem;
//  I, J: Integer;
begin
  TMSFMXWebGMaps1.DeleteAllMapPolyline;
  TMSFMXWebGMaps1.DeleteAllMapPolygon;

  Route := TMSFMXWebGMaps1.Directions[lbRoutes.ItemIndex];

  //Zoom to directions Bounds
  TMSFMXWebGMaps1.MapZoomTo(Route.Bounds);
  TMSFMXWebGMaps1.CreateMapPolyline(Route.Polyline);

  //Add Markers
  TMSFMXWebGMaps1.DeleteAllMapMarker;

  Marker := TMSFMXWebGMaps1.Markers.Add;
  Marker.Draggable := false;
  Marker.Latitude := Route.Legs[0].StartLocation.Latitude;
  Marker.Longitude := Route.Legs[0].StartLocation.Longitude;
  Marker.Title := 'Start Location';
  Marker.MapLabel.Text := 'Start Location: ' + Route.Legs[0].StartAddress;
  TMSFMXWebGMaps1.CreateMapMarker(Marker);

  Marker := TMSFMXWebGMaps1.Markers.Add;
  Marker.Draggable := false;
  Marker.Latitude := Route.Legs[0].EndLocation.Latitude;
  Marker.Longitude := Route.Legs[0].EndLocation.Longitude;
  Marker.Title := 'Destination';
  Marker.MapLabel.Text := '<b>Destination:</b> ' + Route.Legs[0].EndAddress;
  Marker.MapLabel.Color := TAlphaColorRec.Yellow;
  Marker.MapLabel.BorderColor := TAlphaColorRec.Red;
  Marker.MapLabel.FontColor := TAlphaColorRec.Red;
  Marker.MapLabel.Font.Size := 14;
  TMSFMXWebGMaps1.CreateMapMarker(Marker);

  //Add Polygon Circles
//  if CheckBox1.Checked then
  begin
    PolygonItem := TMSFMXWebGMaps1.Polygons.Add;
    Circle := PolygonItem.Polygon;
    Circle.PolygonType := ptCircle;
    Circle.BackgroundOpacity := 50;
    Circle.BorderWidth := 2;
    Circle.Radius := Integer(Route.Legs[0].Distance div 10);
    Circle.Center.Latitude := Route.Legs[0].StartLocation.Latitude;
    Circle.Center.Longitude := Route.Legs[0].StartLocation.Longitude;
    TMSFMXWebGMaps1.CreateMapPolygon(Circle);

    PolygonItem := TMSFMXWebGMaps1.Polygons.Add;
    Circle := PolygonItem.Polygon;
    Circle.PolygonType := ptCircle;
    Circle.BackgroundOpacity := 50;
    Circle.BorderWidth := 2;
    Circle.Radius := Integer(Route.Legs[0].Distance div 10);
    Circle.Center.Latitude := Route.Legs[0].EndLocation.Latitude;
    Circle.Center.Longitude := Route.Legs[0].EndLocation.Longitude;
    TMSFMXWebGMaps1.CreateMapPolygon(Circle);
  end;

  //Add Polygon Rectangle
//  if CheckBox2.Checked then
  begin
    PolygonItem := TMSFMXWebGMaps1.Polygons.Add;
    Rect := PolygonItem.Polygon;
    Rect.PolygonType := ptRectangle;
    Rect.BackgroundOpacity := 0;
    Rect.BorderWidth := 2;
//    Rect.BorderColor := clBlack;
    Rect.BorderOpacity := 100;
    Rect.Bounds.SouthWest.Latitude := Route.Bounds.SouthWest.Latitude;
    Rect.Bounds.SouthWest.Longitude := Route.Bounds.SouthWest.Longitude;
    Rect.Bounds.NorthEast.Latitude := Route.Bounds.NorthEast.Latitude;
    Rect.Bounds.NorthEast.Longitude := Route.Bounds.NorthEast.Longitude;
    TMSFMXWebGMaps1.CreateMapPolygon(Rect);
  end;
end;

procedure TForm90.DisplayRouteDetails;
var
  I, J: Integer;
begin
  lbRouteDetails.Items.Clear;
  if lbRoutes.ItemIndex >= 0 then
  begin
    for J := 0 to TMSFMXWebGMaps1.Directions[lbRoutes.ItemIndex].Legs.Count - 1 do
    begin
      for I := 0 to TMSFMXWebGMaps1.Directions[lbRoutes.ItemIndex].Legs[J].Steps.Count - 1 do
      begin
        lbRouteDetails.Items.Add(TMSFMXWebGMaps1.Directions[lbRoutes.ItemIndex].Legs[J].Steps[I].Instructions);
      end;
    end;
  end;
end;


procedure TForm90.lbRoutesChange(Sender: TObject);
begin
  DisplayRouteDetails;
  DisplayRoute;
end;

procedure TForm90.rbHybridClick(Sender: TObject);
begin
  TMSFMXWebGMaps1.MapOptions.MapType := mtHybrid;
end;

procedure TForm90.rbRoadClick(Sender: TObject);
begin
  TMSFMXWebGMaps1.MapOptions.MapType := mtDefault;
end;

procedure TForm90.rbSatelliteClick(Sender: TObject);
begin
  TMSFMXWebGMaps1.MapOptions.MapType := mtSatellite;
end;

procedure TForm90.rbTerrainClick(Sender: TObject);
begin
  TMSFMXWebGMaps1.MapOptions.MapType := mtTerrain;
end;

procedure TForm90.TMSFMXWebGMaps1MapClick(Sender: TObject; Latitude,
  Longitude: Double; X, Y: Integer);
var
  MarkerIcon: string;
begin
  lbEvents.Text := 'Events: MAP Click: ' + FloatToStr(X) + ' * ' + FloatToStr(Y);

  if cbAddMarker.IsChecked then
  begin
    MarkerIcon := '';
    if rbMarkerImage.IsChecked then
      MarkerIcon := MarkerImage;
    TMSFMXWebGmaps1.Markers.Add(Latitude, Longitude, 'New Marker', MarkerIcon, true, true, true, false, true, 0);
  end;
end;

procedure TForm90.TMSFMXWebGMaps1MapMoveEnd(Sender: TObject; Latitude,
  Longitude: Double; X, Y: Integer);
begin
  lbEvents.Text := 'Events: MAP Move to: ' + FloatToStr(Latitude);
end;

procedure TForm90.TMSFMXWebGMaps1MapZoomChange(Sender: TObject;
  NewLevel: Integer);
begin
  lbEvents.Text := 'Events: MAP Zoom change to level: ' + IntToStr(NewLevel);
end;

procedure TForm90.TMSFMXWebGMaps1MarkerClick(Sender: TObject;
  MarkerTitle: string; IdMarker: Integer; Latitude, Longitude: Double);
begin
  lbEvents.Text := 'Events: MAP Marker Click: ' + FloatToStr(Latitude);
end;

procedure TForm90.TMSFMXWebGMaps1MarkerDrag(Sender: TObject;
  MarkerTitle: string; IdMarker: Integer; Latitude, Longitude: Double);
begin
  lbEvents.Text := 'Events: MAP Marker Drag: ' + FloatToStr(Latitude);
end;

procedure TForm90.TMSFMXWebGMaps1PolygonClick(Sender: TObject;
  IdPolygon: Integer);
begin
  lbEvents.Text := 'Events: MAP Polygon Click: ' + IntToStr(IdPolygon);
end;

procedure TForm90.TMSFMXWebGMaps1PolylineClick(Sender: TObject;
  IdPolyline: Integer);
begin
  lbEvents.Text := 'Events: MAP Polyline Click: ' + IntToStr(IdPolyline);
end;

end.
