unit Android.BarcodeScanner;

interface

// Inspired by http://john.whitham.me.uk/xe5/
// Implemented by Jim McKeeth - jim.mckeeth@embarcadero.com - www.delphi.org

// Must have the ZXing barcode scanner installed
// https://play.google.com/store/apps/details?id=com.google.zxing.client.android
// See the ZXing Google Code page here https://code.google.com/p/zxing/  for more info the library and licensing.

// The barcode is tranfered via the clipboard, but the clipboard is preserved.

uses
  FMX.platform, fmx.helpers.android, System.Rtti, FMX.Types, System.Classes,
  System.SysUtils,
  androidapi.JNI.GraphicsContentViewText,
  androidapi.jni.JavaTypes, FMX.StdCtrls, FMX.Edit;

type
  TAndroidBarcodeScanner = class;
  TAndroidBarcodeEvent = procedure (Sender: TAndroidBarcodeScanner; ABarcode: string) of object;
  TAndroidBarcodeAnonEvent = reference to procedure(ABarcode: string);
  TAndroidBarcodeScanner = class(TComponent)
  private
    FClipService: IFMXClipboardService;
    FOldClipboard: TValue;
    FMonitorClipboard: Boolean;
    FOnBarcode: TAndroidBarcodeEvent;
    FHandler: TAndroidBarcodeAnonEvent;
    const ClipboardCanary = 'waiting';
    procedure CallScan(AScanCmd: string);
    procedure GetBarcodeValue;
  public
    { Public declarations }
    type TBarcodeMode = (bmOneD, bmQRCode, bmProduct, bmDataMatrix);
    type TBarcodeModes = set of TBarcodeMode;
    function HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
    constructor Create(RegisterApplicationEventHandler: Boolean = True);
    procedure Scan; overload;
    procedure Scan(AMode: TBarcodeMode); overload;
    procedure Scan(AModes: TBarcodeModes); overload;
    procedure Scan(AMode: TBarcodeMode; AHandler: TAndroidBarcodeAnonEvent); overload;
    procedure Scan(AModes: TBarcodeModes; AHandler: TAndroidBarcodeAnonEvent); overload;
    const AllBarcodeModes: TBarcodeModes = [bmOneD, bmQRCode, bmProduct, bmDataMatrix];
    property OnBarcode: TAndroidBarcodeEvent read FOnBarcode write FOnBarcode;
  private
    const BarcodeModes: array [bmOneD .. bmDataMatrix] of string =
      ('ONE_D_MODE', 'QR_CODE_MODE', 'PRODUCT_MODE', 'DATA_MATRIX_MODE');
    function GetModeString(AModes: TBarcodeModes): string;
  end;

implementation

{ TAndroidBarcodeScanner }

constructor TAndroidBarcodeScanner.Create(RegisterApplicationEventHandler: Boolean);
var
  aFMXApplicationEventService: IFMXApplicationEventService;
begin
  FMonitorClipboard := false;

  if not TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(FClipService)) then
    FClipService := nil;
  if RegisterApplicationEventHandler then
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(aFMXApplicationEventService)) then
      aFMXApplicationEventService.SetApplicationEventHandler(HandleAppEvent)
    else
      Log.d('Application Event Service is not supported.');
  end;
end;

function TAndroidBarcodeScanner.HandleAppEvent(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
  if FMonitorClipboard and (AAppEvent = aeBecameActive) then
  begin
    GetBarcodeValue;
  end;
end;

procedure TAndroidBarcodeScanner.Scan(AMode: TBarcodeMode);
begin
  Scan([AMode]);
end;

procedure TAndroidBarcodeScanner.Scan;
begin
  Scan(AllBarcodeModes);
end;

procedure TAndroidBarcodeScanner.Scan(AModes: TBarcodeModes);
begin
  CallScan(GetModeString(AModes));
end;

procedure TAndroidBarcodeScanner.Scan(AModes: TBarcodeModes;
  AHandler: TAndroidBarcodeAnonEvent);
begin
  FHandler := AHandler;
  Scan(AModes);
end;

procedure TAndroidBarcodeScanner.Scan(AMode: TBarcodeMode;
  AHandler: TAndroidBarcodeAnonEvent);
begin
  FHandler := AHandler;
  Scan([AMode]);
end;
 
procedure TAndroidBarcodeScanner.CallScan(AScanCmd: string);
var
  intent: JIntent;
begin
  if assigned(FClipService) then
  begin
    FOldClipboard := FClipService.GetClipboard;
    FMonitorClipboard := True;
    FClipService.SetClipboard(ClipboardCanary);
    intent := tjintent.Create;
    intent.setAction(stringtojstring('com.google.zxing.client.android.SCAN'));
    intent.putExtra(tjintent.JavaClass.EXTRA_TEXT, stringtojstring(AScanCmd));
    sharedactivity.startActivityForResult(intent, 0);
  end;
end;

procedure TAndroidBarcodeScanner.GetBarcodeValue;
var
  value: String;
begin
  FMonitorClipboard := false;
  if (FClipService.GetClipboard.ToString <> ClipboardCanary) then
  begin
    value := FClipService.GetClipboard.ToString;
    if assigned(FHandler) then
      FHandler(value)
    else if assigned(FOnBarcode) then
      FOnBarcode(Self, FClipService.GetClipboard.ToString);
    FHandler := nil;
  end;
  FClipService.SetClipboard(FOldClipboard);
end;

function TAndroidBarcodeScanner.GetModeString(AModes: TBarcodeModes): string;
var
  mode: TBarcodeMode;
begin
  Result := '';
  for mode in AModes do
  begin
    Result := Result + ',' + BarcodeModes[mode];  
  end;

  Result := StringReplace(Result, ',', '"SCAN_MODE","', []) + '"';
                  
end;

end.
