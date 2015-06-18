unit BarCodeReader;

interface

// iOS code implemented by TMS software
// Android code implemented by Jim McKeeth

uses
  System.Classes,
  System.Rtti,
  FMX.Types,
  FMX.Platform,
  FMX.StdCtrls
{$IFDEF IOS}
    , MacApi.ObjectiveC, iOSApi.CocoaTypes, iOSApi.Foundation, iOSApi.UIKit,
  iOSApi.QuartzCore, iOSApi.CoreMedia, iOSApi.CoreVideo, iOSApi.AVFoundation
{$ENDIF}
{$IFDEF ANDROID}
    , FMX.helpers.android, androidapi.JNI.GraphicsContentViewText,
  androidapi.JNI.JavaTypes
{$ENDIF}
    ;

type
  TBarCodeReader = class;

{$IFDEF IOS}
  zbar_symbol_type_t = Cardinal;
  zbar_config_t = Cardinal;
  zbar_symbol_s = Pointer;
  zbar_symbol_set_s = Pointer;
  zbar_symbol_t = zbar_symbol_s;
  zbar_symbol_set_t = zbar_symbol_set_s;

  ZBarSymbolClass = interface(NSObjectClass)
    ['{35FAE888-4088-4C6D-8564-E65E728653E4}']
  end;

  ZBarSymbol = interface(NSObject)
    ['{D5B645FC-1CF7-4966-BA44-17787FADD524}']
    function data: NSString; cdecl;
    function initWithSymbol(symbol: zbar_symbol_t): Pointer; cdecl;
  end;

  TZBarSymbol = class(TOCGenericImport<ZBarSymbolClass, ZBarSymbol>)
  end;

  ZBarSymbolSetClass = interface(NSObjectClass)
    ['{A285BD5D-32AD-43F9-BA75-3F002DF4D7B2}']
  end;

  ZBarSymbolSet = interface(NSObject)
    ['{1A6960EA-B523-4057-81F6-0DB227FA8CA5}']
    function count: Integer; cdecl;
    function ZBarSymbolSet: Pointer; cdecl;
  end;

  TZBarSymbolSet = class(TOCGenericImport<ZBarSymbolSetClass, ZBarSymbolSet>)
  end;

  ZBarReaderDelegate = interface(IObjectiveC)
    ['{2B97F7C6-8FA8-4BC3-B8B1-EB913D0A8F77}']
    procedure imagePickerController(reader: UIImagePickerController;
      didFinishPickingMediaWithInfo: NSDictionary); cdecl;
  end;

  TZBarReaderDelegate = class(TOCLocal, ZBarReaderDelegate)
  private
    FBarCodeReader: TBarCodeReader;
  public
    procedure imagePickerController(reader: UIImagePickerController;
      didFinishPickingMediaWithInfo: NSDictionary); cdecl;
  end;

  ZBarImageScannerClass = interface(NSObjectClass)
    ['{A162FBBE-1BC8-451F-B783-302BDDF4E58B}']
  end;

  ZBarImageScanner = interface(NSObject)
    ['{779D918F-9CCF-4E4B-AA8E-C0461D3B6060}']
    procedure setSymbology(symbology: zbar_symbol_type_t; config: zbar_config_t;
      _to: NSInteger); cdecl;
  end;

  TZBarImageScanner = class(TOCGenericImport<ZBarImageScannerClass,
    ZBarImageScanner>)
  end;

  ZBarReaderViewControllerClass = interface(UIViewControllerClass)
    ['{A23DB840-F5FE-44FF-B80E-FA251FFE78B7}']
  end;

  ZBarReaderViewController = interface(UIViewController)
    ['{B9431ED6-C67D-46D8-ABB7-7CCAB9DB8703}']
    procedure setReaderDelegate(newValue: Pointer); cdecl;
    function readerDelegate: Pointer; cdecl;
    procedure setSupportedOrientationsMask(supportedOrientationsMask
      : NSUInteger); cdecl;
    function scanner: ZBarImageScanner; cdecl;
    procedure start; cdecl;
    procedure stop; cdecl;
  end;

  TZBarReaderViewController = class
    (TOCGenericImport<ZBarReaderViewControllerClass, ZBarReaderViewController>)
  end;
{$ENDIF}

  TZBarReaderGetResult = procedure(Sender: TBarCodeReader; ABarcode: string)
    of object;

  [ComponentPlatformsAttribute(pidiOSDevice)]
  TBarCodeReader = class(TComponent)
  private
    FOnGetResult: TZBarReaderGetResult;
{$IFDEF IOS}
    FZBarReaderDelegate: TZBarReaderDelegate;
    FZBarReaderViewController: ZBarReaderViewController;
{$ENDIF}
{$IFDEF ANDROID}
    FClipService: IFMXClipboardService;
    FOldClipboard: TValue;
    FMonitorClipboard: Boolean;

  const
    ClipboardCanary = 'waiting';
    procedure CallScan(s: string);
    procedure GetBarcodeValue;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show(bQRCode: Boolean = False);
{$IFDEF ANDROID}
    function HandleAppEvent(AAppEvent: TApplicationEvent;
      AContext: TObject): Boolean;
{$ENDIF}
  published
    property OnGetResult: TZBarReaderGetResult read FOnGetResult
      write FOnGetResult;
  end;

{$IFDEF IOS}
{$O-}

function lzld: ZBarReaderViewController; cdecl;
  external 'libzbar.a' name 'OBJC_CLASS_$_ZBarReaderViewController';
{$O+}
function zbar_symbol_set_first_symbol(symbols: zbar_symbol_set_t)
  : zbar_symbol_t; cdecl;
  external 'libzbar.a' name 'zbar_symbol_set_first_symbol';
{$ENDIF}

implementation

{$IFDEF IOS}

function GetSharedApplication: UIApplication;
begin
  Result := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
end;
{$ENDIF}
{ TTMSFMXZBarReader }

constructor TBarCodeReader.Create(AOwner: TComponent);
{$IFDEF ANDROID}
var
  aFMXApplicationEventService: IFMXApplicationEventService;
{$ENDIF}
begin
  inherited;
{$IFDEF IOS}
  FZBarReaderDelegate := TZBarReaderDelegate.Create;
  FZBarReaderDelegate.FBarCodeReader := Self;
  FZBarReaderViewController := TZBarReaderViewController.Wrap
    (TZBarReaderViewController.Wrap(TZBarReaderViewController.OCClass.
    alloc).init);
  FZBarReaderViewController.setReaderDelegate(FZBarReaderDelegate.GetObjectID);
  FZBarReaderViewController.setSupportedOrientationsMask
    (UIInterfaceOrientationMaskAll);
{$ENDIF}
{$IFDEF ANDROID}
  FMonitorClipboard := False;
  if not TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService,
    IInterface(FClipService)) then
    FClipService := nil;
  if TPlatformServices.Current.SupportsPlatformService
    (IFMXApplicationEventService, IInterface(aFMXApplicationEventService)) then
    aFMXApplicationEventService.SetApplicationEventHandler(HandleAppEvent)
  else
    Log.d('Application Event Service is not supported.');
{$ENDIF}
end;

destructor TBarCodeReader.Destroy;
begin
{$IFDEF IOS}
  if Assigned(FZBarReaderDelegate) then
  begin
    FZBarReaderDelegate.Free;
    FZBarReaderDelegate := nil;
  end;
  if Assigned(FZBarReaderViewController) then
  begin
    FZBarReaderViewController.release;
    FZBarReaderViewController := nil;
  end;
{$ENDIF}
  inherited;
end;

procedure TBarCodeReader.Show(bQRCode: Boolean = False);
{$IFDEF IOS}
var
  app: UIApplication;
{$ENDIF}
begin
{$IFDEF IOS}
  app := GetSharedApplication;
  if Assigned(app) and Assigned(app.keyWindow) then
    app.keyWindow.rootViewController.presentModalViewController
      (FZBarReaderViewController, True);
{$ENDIF}
{$IFDEF ANDROID}
  if bQRCode then
    CallScan('"SCAN_MODE","ONE_D_MODE,QR_CODE_MODE,PRODUCT_MODE,DATA_MATRIX_MODE"')
  else
    CallScan('"SCAN_MODE", "CODE_39"');
{$ENDIF}
end;

{$IFDEF IOS}
{ TZBarReaderDelegate }

procedure TZBarReaderDelegate.imagePickerController
  (reader: UIImagePickerController;
  didFinishPickingMediaWithInfo: NSDictionary);
var
  val: ZBarSymbolSet;
  symbol: ZBarSymbol;
  sym: zbar_symbol_t;
  res: NSString;
begin
  val := TZBarSymbolSet.Wrap(didFinishPickingMediaWithInfo.objectForKey
    ((NSStr('ZBarReaderControllerResults') as ILocalObject).GetObjectID));
  sym := zbar_symbol_set_first_symbol(val.ZBarSymbolSet);
  symbol := TZBarSymbol.Wrap(TZBarSymbol.Wrap(TZBarSymbol.OCClass.alloc)
    .initWithSymbol(sym));
  res := symbol.data;
  symbol.release;
  symbol := nil;

  if Assigned(FBarCodeReader.OnGetResult) then
    FBarCodeReader.OnGetResult(FBarCodeReader, UTF8ToString(res.UTF8String));

  if not FBarCodeReader.FZBarReaderViewController.isBeingDismissed then
    FBarCodeReader.FZBarReaderViewController.
      dismissModalViewControllerAnimated(True);
end;

{$ENDIF}
{$IFDEF ANDROID}

function TBarCodeReader.HandleAppEvent(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
  Result := True;
  if FMonitorClipboard and (AAppEvent = aeBecameActive) then
  begin
    GetBarcodeValue;
  end;
end;

procedure TBarCodeReader.CallScan(s: string);
var
  intent: JIntent;
begin
  if Assigned(FClipService) then
  begin
    FMonitorClipboard := True;
    FOldClipboard := FClipService.GetClipboard;
    FClipService.SetClipboard(ClipboardCanary);
    intent := tjintent.Create;
    intent.setAction(stringtojstring('com.google.zxing.client.android.SCAN'));
    intent.putExtra(tjintent.JavaClass.EXTRA_TEXT, stringtojstring(s));
    sharedactivity.startActivityForResult(intent, 0);
  end;
end;

procedure TBarCodeReader.GetBarcodeValue;
begin
  if (FClipService.GetClipboard.ToString <> ClipboardCanary) then
  begin
    if Assigned(FOnGetResult) then
      FOnGetResult(Self, FClipService.GetClipboard.ToString);
  end;
  FMonitorClipboard := False;
  FClipService.SetClipboard(FOldClipboard);
end;

{$ENDIF}

end.
