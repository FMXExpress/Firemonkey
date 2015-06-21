unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  FMX.StdCtrls, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope,
  FMX.Layouts, FMX.Memo, IdBaseComponent, IdZLibCompressorBase,
  IdCompressorZLib, REST.Types, AnonThread,
  FMX.Controls.Presentation, FMX.Edit, FMX.ScrollBox, FMX.WebBrowser,
  FMX.TMSWebBrowser
{$IFDEF IOS}
    , iOSApi.UIKit
{$ENDIF}
    ;

type
  TForm1 = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    Button1: TButton;
    Memo1: TMemo;
    ToolBar1: TToolBar;
    Zlib: TIdCompressorZLib;
    Button2: TButton;
    Edit1: TEdit;
    WebBrowser1: TWebBrowser;
    Edit2: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
{$IFDEF IOS}
    UIApp: UIApplication;
{$ENDIF}
    function DecodeGZIPContent(RawBytes: System.TArray<System.Byte>;
      ContentEncoding: String): String;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button2Click(Sender: TObject);
var
  SL: TStringList;
begin

  SL := TStringList.Create;

  if RESTResponse1.ContentEncoding <> '' then
  begin
    // decode and do something with the content
    SL.Text := DecodeGZIPContent(RESTResponse1.RawBytes,
      RESTResponse1.ContentEncoding);
  end
  else
    SL.Text := RESTResponse1.Content; // do something with the content

  WebBrowser1.LoadFromStrings(SL.Text, 'about:blank');
  Memo1.BeginUpdate;
  Memo1.Lines.Append(SL.Text);
  Memo1.EndUpdate;

  SL.Free;
end;

function TForm1.DecodeGZIPContent(RawBytes: System.TArray<System.Byte>; ContentEncoding: String): String;
var
  MSI: TMemoryStream;
  MSO: TStringStream;
{$IF NOT DEFINED(MACOS)}
{$ELSE}
  I: LongInt;
  W: Word;
{$ENDIF}
begin
  MSI := TMemoryStream.Create;
  MSO := TStringStream.Create;
  MSI.WriteData(RawBytes, Length(RawBytes));
{$IF NOT DEFINED(MACOS)}
  MSI.Seek(0, 0);
{$ELSE}
  MSI.Seek(I, W);
{$ENDIF}
  try
{$IF NOT DEFINED(MACOS)}
    // Zlib is a TIdCompressorZlib
    if ContentEncoding = 'gzip' then
      Zlib.DecompressGZipStream(MSI, MSO);
    if ContentEncoding = 'deflate' then
      Zlib.DecompressHTTPDeflate(MSI, MSO);
{$ENDIF}
  except
    on e: Exception do
    begin
      showmessage(e.Message);

    end;
  end;
{$IF DEFINED(MACOS)}
  MSO.LoadFromStream(MSI);
{$ENDIF}
  MSI.DisposeOf;
{$IF NOT DEFINED(MACOS)}
  MSO.Seek(0, 0);
{$ELSE}
  MSO.Seek(I, W);
{$ENDIF}
  Result := MSO.DataString;
  MSO.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  { This defines the default active tab at runtime }
{$IFDEF IOS}
  UIApp := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
{$ENDIF}
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  FThread: TAnonymousThread<Boolean>;
begin
  Button2.Enabled := False;
  RESTClient1.BaseURL := Edit2.Text;

  RESTRequest1.AcceptEncoding := Edit1.Text;

{$IFDEF IOS}
  UIApp.setIdleTimerDisabled(True);
{$ENDIF}
  FThread := TAnonymousThread<Boolean>.Create(
    function: Boolean
    begin
      RESTRequest1.Execute;
      Result := True;
    end,
    procedure(AResult: Boolean)
    begin

      Memo1.Lines.Append(IntToStr(RESTResponse1.StatusCode) + ' ' +
        RESTResponse1.StatusText + ' ' + RESTResponse1.ContentEncoding);
      Application.ProcessMessages;
      Button2.Enabled := True;

{$IFDEF IOS}
      UIApp.setIdleTimerDisabled(False);
{$ENDIF}
    end,
    procedure(AException: Exception)
    begin
      // Runs in main thread
      // do something if there is an exception
      Memo1.Lines.Append(AException.Message);

{$IFDEF IOS}
      UIApp.setIdleTimerDisabled(False);
{$ENDIF}
    end, False);

end;

end.
