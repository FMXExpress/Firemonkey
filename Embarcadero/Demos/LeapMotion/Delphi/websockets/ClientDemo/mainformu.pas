unit MainFormU;

{$mode delphi}{$H+}

interface

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  blcksock, websocket2, customserver2,
  StdCtrls, ComCtrls, ExtCtrls, ActnList;

type

  { TTestWebSocketClientConnection }

  TTestWebSocketClientConnection = class(TWebSocketClientConnection)
    fFramedText: string;
    fFramedStream: TMemoryStream;

    fPing: string;
    fPong: string;

    procedure ProcessText(aFinal, aRes1, aRes2, aRes3: boolean; aData: string); override;
    procedure ProcessTextContinuation(aFinal, aRes1, aRes2, aRes3: boolean; aData: string); override;

    procedure ProcessStream(aFinal, aRes1, aRes2, aRes3: boolean; aData: TMemoryStream); override;
    procedure ProcessStreamContinuation(aFinal, aRes1, aRes2, aRes3: boolean; aData: TMemoryStream); override;

    procedure ProcessPing(aData: string); override;
    procedure ProcessPong(aData: string); override;


    procedure SyncTextFrame;
    procedure SyncBinFrame;

    procedure SyncPing;
    procedure SyncPong;

  public
    constructor Create(aHost, aPort, aResourceName: string;
    aOrigin: string = '-'; aProtocol: string = '-'; aExtension: string = '-';
    aCookie: string = '-'; aVersion: integer = 8); override;
    destructor Destroy; override;
    property ReadFinal: boolean read fReadFinal;
    property ReadRes1: boolean read fReadRes1;
    property ReadRes2: boolean read fReadRes2;
    property ReadRes3: boolean read fReadRes3;
    property ReadCode: integer read fReadCode;
    property ReadStream: TMemoryStream read fReadStream;

    property WriteFinal: boolean read fWriteFinal;
    property WriteRes1: boolean read fWriteRes1;
    property WriteRes2: boolean read fWriteRes2;
    property WriteRes3: boolean read fWriteRes3;
    property WriteCode: integer read fWriteCode;
    property WriteStream: TMemoryStream read fWriteStream;

  end;

  { TMainForm }

  TMainForm = class(TForm)
    SendAction: TAction;
    EndAction: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    EndButton: TButton;
    GroupBox1: TGroupBox;
    HostCombo: TComboBox;
    Image1: TImage;
    InfoMemo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    LastReceivedMemo: TMemo;
    LastSentMemo: TMemo;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel8: TPanel;
    PortCombo: TComboBox;
    SendMemo: TMemo;
    FrameReceiveMemo: TMemo;
    ServerErrorLabel: TLabel;
    Splitter1: TSplitter;
    Splitter4: TSplitter;
    SSLCheck: TCheckBox;
    StartButton: TButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure EndActionExecute(Sender: TObject);
    procedure EndActionUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure GroupBox1Click(Sender: TObject);
    procedure SendActionExecute(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
  private
    fClient: TWebSocketClientConnection;
    fFrameString: string;

    procedure OnOpen(aSender: TWebSocketCustomConnection);
    procedure OnRead(aSender: TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
    procedure OnWrite(aSender: TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
    procedure OnClose(aSender: TWebSocketCustomConnection; aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
    procedure OnConnectionSocket(Sender: TObject; Reason: THookSocketReason; const Value: String);
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation
uses StrUtils, synachar, synautil, Math, TypInfo;

{ TMainForm }

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (EndButton.Enabled) then
  begin
    EndActionExecute(EndAction);
  end;
end;

procedure TMainForm.GroupBox1Click(Sender: TObject);
begin

end;

procedure TMainForm.SendActionExecute(Sender: TObject);
begin
  fClient.SendText(CharsetConversion(SendMemo.Text, GetCurCP, UTF_8));
end;

procedure TMainForm.StartButtonClick(Sender: TObject);
begin
  fClient := TTestWebSocketClientConnection.Create(HostCombo.text, PortCombo.Text, '/');

  fClient.OnRead := OnRead;
  fClient.OnWrite := OnWrite;
  fClient.OnClose := OnClose;
  fClient.OnOpen := OnOpen;
  //fClient.Socket.OnSyncStatus := OnConnectionSocket;

  fClient.SSL := boolean(SslCheck.Checked);
  fClient.Start;
end;

procedure TMainForm.EndActionExecute(Sender: TObject);
begin
  //fClient.TerminateThread;
  fClient.Close(wsCloseNormal, 'bye bye');
  //sleep(2000);
end;

procedure TMainForm.EndActionUpdate(Sender: TObject);
begin
  TAction(sender).Enabled := not StartButton.Enabled;
end;

procedure TMainForm.OnOpen(aSender: TWebSocketCustomConnection);
begin
  InfoMemo.Lines.Insert(0, Format('OnOpen %d', [aSender.Index]));
  StartButton.Enabled := false;
end;

Var
  FS : TFileStream;
  FCount : Integer;

procedure TMainForm.OnRead(aSender: TWebSocketCustomConnection; aFinal, aRes1,
  aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
var s: string;
    c: TTestWebSocketClientConnection;
begin
  c := TTestWebSocketClientConnection(aSender);
  InfoMemo.Lines.Insert(0, Format('OnRead %d, final: %d, ext1: %d, ext2: %d, ext3: %d, type: %d, length: %d', [aSender.Index, ord(aFinal), ord(aRes1), ord(aRes2), ord(aRes3), aCode, aData.Size]));
  if (FS=Nil) then
    begin
    FS:=TFileStream.Create('/tmp/leap-data.txt',fmCreate);
    end;
  inc(FCount);
  S:=Format('Packet %d',[FCount])+sLineBreak;
  FS.Write(S[1],Length(S));
  s := ReadStrFromStream(c.ReadStream, min(c.ReadStream.size, 10 * 1024));
  if Length(S)>0 then
    FS.Write(S[1],Length(S));
  if (c.ReadCode = wsCodeText) then
    LastReceivedMemo.Lines.text := CharsetConversion(s, UTF_8, GetCurCP)
  else
    LastReceivedMemo.Lines.text := s;
end;

procedure TMainForm.OnWrite(aSender: TWebSocketCustomConnection; aFinal, aRes1,
  aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
var s: string;
    c: TTestWebSocketClientConnection;
begin
  c := TTestWebSocketClientConnection(aSender);
  InfoMemo.Lines.Insert(0, Format('OnWrite %d, final: %d, ext1: %d, ext2: %d, ext3: %d, type: %d, length: %d', [aSender.Index, ord(aFinal), ord(aRes1), ord(aRes2), ord(aRes3), aCode, aData.Size]));

  s := ReadStrFromStream(c.WriteStream, min(c.WriteStream.size, 10 * 1024));
  if (c.ReadCode = wsCodeText) then
    LastSentMemo.Lines.text := CharsetConversion(s, UTF_8, GetCurCP)
  else
    LastSentMemo.Lines.text := s;

end;

procedure TMainForm.OnClose(aSender: TWebSocketCustomConnection;
  aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
begin
  InfoMemo.Lines.Insert(0, Format('OnClose %d, %d, %s, %s', [aSender.Index, aCloseCode, aCloseReason, IfThen(aClosedByPeer, 'closed by peer', 'closed by me')]));
  EndButton.Enabled := false;
  StartButton.Enabled := true;
end;

procedure TMainForm.OnConnectionSocket(Sender: TObject;
  Reason: THookSocketReason; const Value: String);
begin
  InfoMemo.Lines.Insert(0, Format('OnConnectionSocket %d, %s, %s', [
    TTCPCustomConnectionSocket(Sender).Connection.Index,
    GetEnumName(TypeInfo(THookSocketReason), ord(Reason)),
    value
  ]));
end;

{ TTestWebSocketClientConnection }

procedure TTestWebSocketClientConnection.ProcessText(aFinal, aRes1, aRes2,
  aRes3: boolean; aData: string);
begin
  fFramedText := aData;
end;

procedure TTestWebSocketClientConnection.ProcessTextContinuation(aFinal, aRes1,
  aRes2, aRes3: boolean; aData: string);
begin
  fFramedText := fFramedText + aData;
  if (aFinal) then
  begin
    Synchronize(SyncTextFrame);
  end;
end;

procedure TTestWebSocketClientConnection.ProcessStream(aFinal, aRes1, aRes2,
  aRes3: boolean; aData: TMemoryStream);
begin
  fFramedStream.Size := 0;
  fFramedStream.CopyFrom(aData, aData.Size);
  MainForm.InfoMemo.Lines.Insert(0, Format('ProcessStream %d, %d, %d, %d, %d, %d ', [Index, ord(aFinal), ord(aRes1), ord(aRes2), ord(aRes3), aData.Size]));
  if (aFinal) then
  begin
    Synchronize(SyncBinFrame);
  end;
end;

procedure TTestWebSocketClientConnection.ProcessStreamContinuation(aFinal,
  aRes1, aRes2, aRes3: boolean; aData: TMemoryStream);
begin
  fFramedStream.CopyFrom(aData, aData.Size);
  MainForm.InfoMemo.Lines.Insert(0, Format('ProcessStreamContinuation %d, %d, %d, %d, %d, %d ', [Index, ord(aFinal), ord(aRes1), ord(aRes2), ord(aRes3), aData.Size]));
  if (aFinal) then
  begin
    Synchronize(SyncBinFrame);
  end;
end;

procedure TTestWebSocketClientConnection.ProcessPing(aData: string);
begin
  Pong(aData);
  fPing := aData;
  Synchronize(SyncPing);
end;

procedure TTestWebSocketClientConnection.ProcessPong(aData: string);
begin
  fPong := aData;
  Synchronize(SyncPong);
end;

procedure TTestWebSocketClientConnection.SyncTextFrame;
begin
  MainForm.FrameReceiveMemo.Text :=  CharsetConversion(fFramedText, UTF_8, GetCurCP);
end;

procedure TTestWebSocketClientConnection.SyncBinFrame;
var png : TPortableNetworkGraphic;
begin
  MainForm.InfoMemo.Lines.Insert(0, Format('SyncBinFrame %d', [fFramedStream.Size]));
  png := TPortableNetworkGraphic.Create;
  fFramedStream.Position := 0;
  png.LoadFromStream(fFramedStream);
  MainForm.Image1.Picture.Assign(png);
  png.Free;
end;

procedure TTestWebSocketClientConnection.SyncPing;
begin
  MainForm.InfoMemo.Lines.Insert(0, Format('SyncPing %s', [fPing]));
end;

procedure TTestWebSocketClientConnection.SyncPong;
begin
  MainForm.InfoMemo.Lines.Insert(0, Format('SyncPong %s', [fPong]));
end;

constructor TTestWebSocketClientConnection.Create(aHost, aPort, aResourceName: string;
    aOrigin: string = '-'; aProtocol: string = '-'; aExtension: string = '-';
    aCookie: string = '-'; aVersion: integer = 8);
begin
  inherited;
  fFramedText := '';
  fFramedStream := TMemoryStream.Create;
end;

destructor TTestWebSocketClientConnection.Destroy;
begin
  fFramedStream.free;
  inherited;
end;

initialization
  {$I mainformu.lrs}
finalization
  FreeAndNil(FS);

end.

