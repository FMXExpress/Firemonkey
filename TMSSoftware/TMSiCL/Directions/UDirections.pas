unit UDirections;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.TMSNativeMKMapView, FMX.TMSNativeUITableView, FMX.TMSNativeUIBaseControl,
  FMX.TMSNativeUIToolBar, FMX.TMSNativeCLGeoCoder,
  FMX.TMSNativeUITextField, FMX.TMSNativeUIView, FMX.TMSNativeUIActivityIndicatorView, Generics.Collections,
  FMX.TMSNativeUILabel, iOSApi.UIKit, FMX.TMSNativeUICore;

type

  TTimeItemNames = array of String;

  TRouteStep = class(TPersistent)
  private
    FLocation: TTMSFMXNativeMKMapLocation;
    FInstructions: String;
    FNotice: String;
  public
    property Instructions: String read FInstructions write FInstructions;
    property Notice: String read FNotice write FNotice;
    property Location: TTMSFMXNativeMKMapLocation read FLocation write FLocation;
  end;

  TRoute = class(TPersistent)
  private
    FName: String;
    FSteps: TList<TRouteStep>;
    FTitle: String;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: String read FName write FName;
    property Title: String read FTitle write FTitle;
    property Steps: TList<TRouteStep> read FSteps write FSteps;
  end;

  TForm1135 = class(TForm)
    TMSFMXNativeUIToolBar1: TTMSFMXNativeUIToolBar;
    TMSFMXNativeUITableView1: TTMSFMXNativeUITableView;
    TMSFMXNativeMKMapView1: TTMSFMXNativeMKMapView;
    TMSFMXNativeUIView1: TTMSFMXNativeUIView;
    TMSFMXNativeUITextField1: TTMSFMXNativeUITextField;
    TMSFMXNativeUITextField2: TTMSFMXNativeUITextField;
    TMSFMXNativeUILabel1: TTMSFMXNativeUILabel;
    TMSFMXNativeUILabel2: TTMSFMXNativeUILabel;
    TMSFMXNativeUIActivityIndicatorView1: TTMSFMXNativeUIActivityIndicatorView;
    TMSFMXNativeUITableView2: TTMSFMXNativeUITableView;
    TMSFMXNativeCLGeoCoder1: TTMSFMXNativeCLGeoCoder;
    procedure TMSFMXNativeUIToolBar1ItemClick(ASender: TObject;
      AItem: TTMSFMXNativeUIToolBarItem);
    procedure TMSFMXNativeMKMapView1GetDirections(Sender: TObject;
      AResponse: TTMSFMXNativeMKDirectionsResponse);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXNativeCLGeoCoder1GetGeocodeLocationError(Sender: TObject;
      AError: string);
    procedure TMSFMXNativeUITextField2ShouldReturn(Sender: TObject;
      var AShouldEdit: Boolean);
    procedure TMSFMXNativeCLGeoCoder1GetGeocodeLocation(Sender: TObject;
      APlacemarks: TArray<FMX.TMSNativeUICore.TTMSFMXNativeMKPlacemark>);
    procedure FormDestroy(Sender: TObject);
    procedure TMSFMXNativeUITableView1GetNumberOfSections(Sender: TObject;
      var ANumberOfSections: Integer);
    procedure TMSFMXNativeUITableView2GetNumberOfSections(Sender: TObject;
      var ANumberOfSections: Integer);
    procedure TMSFMXNativeUITableView2GetNumberOfRowsInSection(Sender: TObject;
      ASection: Integer; var ANumberOfRows: Integer);
    procedure TMSFMXNativeUITableView1GetNumberOfRowsInSection(Sender: TObject;
      ASection: Integer; var ANumberOfRows: Integer);
    procedure TMSFMXNativeUITableView1GetItemText(Sender: TObject; ASection,
      ARow: Integer; var AText: string);
    procedure TMSFMXNativeUITableView2GetItemText(Sender: TObject; ASection,
      ARow: Integer; var AText: string);
    procedure TMSFMXNativeUITableView1GetItemEditingAccessoryType(
      Sender: TObject; ASection, ARow: Integer;
      var AAccessoryType: TTMSFMXNativeUITableViewItemAccessoryType);
    procedure TMSFMXNativeUITableView1GetItemSubDetailView(Sender: TObject;
      ASection, ARow: Integer; var ASubDetailView: TTMSFMXNativeUIBaseControl);
    procedure TMSFMXNativeUITableView1GetItemAccessoryType(Sender: TObject;
      ASection, ARow: Integer;
      var AAccessoryType: TTMSFMXNativeUITableViewItemAccessoryType);
    procedure TMSFMXNativeUITableView1ItemBeforeShowDetailView(Sender: TObject;
      ASection, ARow: Integer);
  private
    { Private declarations }
    FAddress: Integer;
    FActiveRoute: Integer;
    FFirstAddress, FSecondAddress: TTMSFMXNativeMKMapLocation;
    FRouteList: TList<TRoute>;
  public
    procedure CalculateRoute;
    { Public declarations }
  end;

var
  Form1135: TForm1135;

implementation

{$R *.fmx}

procedure TForm1135.CalculateRoute;
begin
  if (TMSFMXNativeUITextField1.Text <> '') and (TMSFMXNativeUITextField2.Text <> '') then
  begin
    TMSFMXNativeUITableView1.NavigationController.popToRootViewControllerAnimated(True);
    TMSFMXNativeUIActivityIndicatorView1.StartAnimating;
    FAddress := 0;
    TMSFMXNativeCLGeoCoder1.GetGeocodeLocation(TMSFMXNativeUITextField1.Text);
  end;
end;

procedure TForm1135.FormCreate(Sender: TObject);
begin
  FAddress := 0;
  FRouteList := TList<TRoute>.Create;
  TMSFMXNativeUITextField1.TextField.setAutoresizingMask(UIViewAutoresizingFlexibleWidth);
  TMSFMXNativeUITextField2.TextField.setAutoresizingMask(UIViewAutoresizingFlexibleWidth);
end;

procedure TForm1135.FormDestroy(Sender: TObject);
begin
  FRouteList.Free;
  FRouteList := nil;
end;

procedure TForm1135.TMSFMXNativeCLGeoCoder1GetGeocodeLocation(Sender: TObject;
  APlacemarks: TArray<FMX.TMSNativeUICore.TTMSFMXNativeMKPlacemark>);
begin
  if FAddress = 0 then
  begin
    FAddress := 1;
    FFirstAddress := APlacemarks[0].Location;
    TMSFMXNativeCLGeoCoder1.GetGeocodeLocation(TMSFMXNativeUITextField2.Text);
  end
  else if FAddress = 1 then
  begin
    FAddress := 2;
    FSecondAddress := APlacemarks[0].Location;
    TMSFMXNativeMKMapView1.GetDirections(FFirstAddress, FSecondAddress, true, ttDirectionsTransportTypeAny);
  end;
end;

procedure TForm1135.TMSFMXNativeCLGeoCoder1GetGeocodeLocationError(
  Sender: TObject; AError: string);
begin
  TMSFMXNativeUIActivityIndicatorView1.StopAnimating;
end;

function SecondsToTimeString(Seconds: Int64): string;
const
  divisors: array [0..5] of Int64 = (SecsPerDay * 365, SecsPerDay * 31, SecsPerDay, SecsPerHour, SecsPerMin, 1);
  itemNames: array[0..5] of String = ('year', 'month', 'day', 'h', 'm', 's');
var
  resCount: integer;
  I: Integer;
  C, V: Int64;
begin
  result := '';
  resCount := 0;
  C := Seconds;
  for I := Low(divisors) to High(divisors) do
  begin
    V := C div divisors[I];
    if V > 0 then
    begin
      if resCount > 0 then
        result := result + ' ';
      result := result + IntToStr(V) + itemNames[I];
      Inc(resCount);
      if resCount > 1 then break;
      C := C mod divisors[I];
    end;
  end;
end;

procedure TForm1135.TMSFMXNativeMKMapView1GetDirections(Sender: TObject;
  AResponse: TTMSFMXNativeMKDirectionsResponse);
var
  I, r: Integer;
  st: TTMSFMXNativeMKRouteStep;
  pl: TTMSFMXNativeMKOverlay;
  rt: TRoute;
  rts: TRouteStep;
begin

  TMSFMXNativeMKMapView1.BeginUpdate;
  TMSFMXNativeMKMapView1.RemoveAllOverlays;
  TMSFMXNativeMKMapView1.RemoveAllAnnotations;

  TMSFMXNativeMKMapView1.AddAnnotation(AResponse.SourceLocation, TMSFMXNativeUITextField1.Text, '');
  TMSFMXNativeMKMapView1.AddAnnotation(AResponse.DestinationLocation, TMSFMXNativeUITextField2.Text, '');

  FRouteList.Clear;
  for r := 0 to Length(AResponse.Routes) - 1  do
  begin
    rt := TRoute.Create;
    rt.Title := 'Route [' +AResponse.Routes[r].Name+']' + ' ' + SecondsToTimeString(Round(AResponse.Routes[r].ExpectedTravelTime));
    rt.Name := AResponse.Routes[r].Name;

    for I := 0 to Length(AResponse.Routes[r].Steps) - 1 do
    begin
      st := AResponse.Routes[r].Steps[I];
      rts := TRouteStep.Create;
      rts.Instructions := st.Instructions;
      rts.Notice := st.Notice;
      if Length(st.Locations) > 0 then
        rts.Location := st.Locations[0];
      rt.Steps.Add(rts);
    end;

    pl := TMSFMXNativeMKMapView1.AddPolyline(AResponse.Routes[r].Locations);
    pl.Opacity := 0.75;
    pl.LineColor := TAlphaColorRec.Red;
    pl.DataString := rt.Name;
    FRouteList.Add(rt);
  end;

  TMSFMXNativeMKMapView1.EndUpdate;
  if FRouteList.Count > 0 then
  begin
    FActiveRoute := 0;
    TMSFMXNativeUITableView1.UpdateTableView;
    TMSFMXNativeUITableView2.UpdateTableView;
  end;

  TMSFMXNativeMKMapView1.ZoomToFitAnnotations(false, 0.5, 0.5);
  TMSFMXNativeUIActivityIndicatorView1.StopAnimating;
end;

procedure TForm1135.TMSFMXNativeUITableView1GetItemAccessoryType(
  Sender: TObject; ASection, ARow: Integer;
  var AAccessoryType: TTMSFMXNativeUITableViewItemAccessoryType);
begin
  AAccessoryType := atTableViewCellAccessoryDisclosureIndicator;
end;

procedure TForm1135.TMSFMXNativeUITableView1GetItemEditingAccessoryType(
  Sender: TObject; ASection, ARow: Integer;
  var AAccessoryType: TTMSFMXNativeUITableViewItemAccessoryType);
begin
  AAccessoryType := atTableViewCellAccessoryDisclosureIndicator;
end;

procedure TForm1135.TMSFMXNativeUITableView1GetItemSubDetailView(
  Sender: TObject; ASection, ARow: Integer;
  var ASubDetailView: TTMSFMXNativeUIBaseControl);
begin
  ASubDetailView := TMSFMXNativeUITableView2;
end;

procedure TForm1135.TMSFMXNativeUITableView1GetItemText(Sender: TObject;
  ASection, ARow: Integer; var AText: string);
begin
  AText := FRouteList[ARow].Title;
end;

procedure TForm1135.TMSFMXNativeUITableView1GetNumberOfRowsInSection(
  Sender: TObject; ASection: Integer; var ANumberOfRows: Integer);
begin
  ANumberOfRows := FRouteList.Count;
end;

procedure TForm1135.TMSFMXNativeUITableView1GetNumberOfSections(Sender: TObject;
  var ANumberOfSections: Integer);
begin
  ANumberOfSections := 1;
end;

procedure TForm1135.TMSFMXNativeUITableView1ItemBeforeShowDetailView(
  Sender: TObject; ASection, ARow: Integer);
var
  I: Integer;
  ol: MKPolylineRenderer;
begin
  FActiveRoute := ARow;
  TMSFMXNativeMKMapView1.BeginUpdate;
  for I := 0 to TMSFMXNativeMKMapView1.Overlays.Count - 1 do
  begin
    if TMSFMXNativeMKMapView1.Overlays[I].Kind = okPolyLine then
    begin
      ol := MKPolylineRenderer(TMSFMXNativeMKMapView1.Overlays[I].OverlayRenderer);
      if Assigned(ol) then
      begin
        if TMSFMXNativeMKMapView1.Overlays[I].DataString = FRouteList[FActiveRoute].Name then
          ol.setStrokeColor(AlphaColorToUIColor(TAlphaColorRec.Blue))
        else
          ol.setStrokeColor(AlphaColorToUIColor(TAlphaColorRec.Red));
      end
    end;
  end;

  TMSFMXNativeMKMapView1.EndUpdate;
  TMSFMXNativeUITableView2.UpdateTableView;
end;

procedure TForm1135.TMSFMXNativeUITableView2GetItemText(Sender: TObject;
  ASection, ARow: Integer; var AText: string);
begin
  AText := FRouteList[FActiveRoute].Steps[ARow].Instructions;
end;

procedure TForm1135.TMSFMXNativeUITableView2GetNumberOfRowsInSection(
  Sender: TObject; ASection: Integer; var ANumberOfRows: Integer);
begin
  ANumberOfRows := FRouteList[FActiveRoute].Steps.Count;
end;

procedure TForm1135.TMSFMXNativeUITableView2GetNumberOfSections(Sender: TObject;
  var ANumberOfSections: Integer);
begin
  ANumberOfSections := 1;
end;

procedure TForm1135.TMSFMXNativeUITextField2ShouldReturn(Sender: TObject;
  var AShouldEdit: Boolean);
begin
  CalculateRoute;
end;

procedure TForm1135.TMSFMXNativeUIToolBar1ItemClick(ASender: TObject;
  AItem: TTMSFMXNativeUIToolBarItem);
begin
  if AItem.Index = 0 then
  begin
    if TMSFMXNativeUITextField1.TextField.isFirstResponder then
      TMSFMXNativeUITextField1.TextField.resignFirstResponder;
    if TMSFMXNativeUITextField2.TextField.isFirstResponder then
      TMSFMXNativeUITextField2.TextField.resignFirstResponder;
    CalculateRoute;
  end;
end;

{ TRoute }

constructor TRoute.Create;
begin
  FSteps := TList<TRouteStep>.Create;
end;

destructor TRoute.Destroy;
begin
  FSteps.Free;
  FSteps := nil;
  inherited;
end;

end.
