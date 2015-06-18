unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Androidapi.JNI.Location, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os, FMX.Layouts, FMX.ListBox, FMX.StdCtrls;

type

  TLocationListener = class;

  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure Button1Click(Sender: TObject);
    { Private declarations }

  private
    FLocationManager : JLocationManager;
    locationListener : TLocationListener;
  public
    destructor Destroy; override;
    { Public declarations }
    procedure onLocationChanged(location: JLocation);
  end;

  TLocationListener = class(TJavaLocal, JLocationListener)
  private
    [weak]
    FParent : TForm1;
  public
    constructor Create(AParent : TForm1);
    procedure onLocationChanged(location: JLocation); cdecl;
    procedure onProviderDisabled(provider: JString); cdecl;
    procedure onProviderEnabled(provider: JString); cdecl;
    procedure onStatusChanged(provider: JString; status: Integer; extras: JBundle); cdecl;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses FMX.Helpers.Android, Androidapi.JNI.GraphicsContentViewText;

{ TLocationListener }

constructor TLocationListener.Create(AParent: TForm1);
begin
  inherited Create;
  FParent := AParent;
end;

procedure TLocationListener.onLocationChanged(location: JLocation);
begin
  FParent.onLocationChanged(location);
end;

procedure TLocationListener.onProviderDisabled(provider: JString);
begin

end;

procedure TLocationListener.onProviderEnabled(provider: JString);
begin

end;

procedure TLocationListener.onStatusChanged(provider: JString; status: Integer;
  extras: JBundle);
begin

end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  LocationManagerService: JObject;
  iter : JIterator;
  location : JLocation;
begin
  if not Assigned(FLocationManager) then
  begin
    LocationManagerService := SharedActivityContext.getSystemService(TJContext.JavaClass.LOCATION_SERVICE);
    FLocationManager := TJLocationManager.Wrap((LocationManagerService as ILocalObject).GetObjectID);
    if not Assigned(locationListener) then
      locationListener := TLocationListener.Create(self);
    FLocationManager.requestLocationUpdates(TJLocationManager.JavaClass.GPS_PROVIDER, 10000, 10, locationListener,
        TJLooper.JavaClass.getMainLooper);
  end;
  iter := FLocationManager.GetAllProviders.Iterator;
  ListBox1.Clear;
  while iter.hasNext do
  begin
    ListBox1.Items.Add(JStringToString(iter.next.ToString));
  end;
  CheckBox1.IsChecked := FLocationManager.isProviderEnabled(TJLocationManager.JavaClass.GPS_PROVIDER);
  CheckBox2.IsChecked := FLocationManager.isProviderEnabled(TJLocationManager.JavaClass.NETWORK_PROVIDER);
  location := FLocationManager.getLastKnownLocation(TJLocationManager.JavaClass.GPS_PROVIDER);
  onLocationChanged(location);
end;

destructor TForm1.Destroy;
begin
  if Assigned(locationListener) then
    FLocationManager.removeUpdates(locationListener);
  inherited;
end;

procedure TForm1.onLocationChanged(location: JLocation);
begin
  Label4.Text := location.getLatitude.ToString;
  Label5.Text := location.getLongitude.ToString;
  Label6.Text := location.getAltitude.ToString;
end;

end.
