unit MainFormU;

{$mode DELPHI}{$H+}

interface

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  blcksock, customserver2, WebSocket2,
  ExtCtrls, StdCtrls, ActnList, ComCtrls, ExtDlgs, Menus, Math;

type

  { TMainForm }

  TMainForm = class(TForm)
    CloseAction: TAction;
    MenuItem1: TMenuItem;
    PingAction: TAction;
    PongAction: TAction;
    LoadImageAction: TAction;
    OpenPictureDialog: TOpenPictureDialog;
    PopupMenu1: TPopupMenu;
    SendFramesAction: TAction;
    BroadcastAction: TAction;
    SendTextAction: TAction;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    EndAction: TAction;
    Image1: TImage;
    InfoMemo: TMemo;
    ConnectionList: TListView;
    SendSelectedMemo: TMemo;
    BroadcastMemo: TMemo;
    ConnectionInfoMemo: TMemo;
    LastSentMemo: TMemo;
    LastReceivedMemo: TMemo;
    SendSelectedMemo2: TMemo;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    ServerErrorLabel: TLabel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    StartAction: TAction;
    ActionList1: TActionList;
    StartButton: TButton;
    EndButton: TButton;
    SSLCheck: TCheckBox;
    HostCombo: TComboBox;
    PortCombo: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure BroadcastActionExecute(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure ConnectionListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure EndActionExecute(Sender: TObject);
    procedure EndActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadImageActionExecute(Sender: TObject);
    procedure PingActionExecute(Sender: TObject);
    procedure PongActionExecute(Sender: TObject);
    procedure SendFramesActionExecute(Sender: TObject);
    procedure StartActionExecute(Sender: TObject);
    procedure SendTextActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fServer: TWebSocketServer;

    procedure OnAfterAddConnection(Server: TCustomServer; aConnection: TCustomConnection);
    procedure OnBeforeAddConnection(Server: TCustomServer; aConnection: TCustomConnection; var CanAdd: boolean);
    procedure OnAfterRemoveConnection(Server: TCustomServer; aConnection: TCustomConnection);
    procedure OnBeforeRemoveConnection(Server: TCustomServer; aConnection: TCustomConnection);
    procedure OnServerSocketError(Server: TCustomServer; Socket: TTCPBlockSocket);

    procedure OnOpen(aSender: TWebSocketCustomConnection);
    procedure OnRead(aSender: TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
    procedure OnWrite(aSender: TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
    procedure OnClose(aSender: TWebSocketCustomConnection; aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
    procedure OnConnectionSocket(Sender: TObject; Reason: THookSocketReason; const Value: String);

    function ListItemByIndex(aConnectionIndex: integer): TListItem;
  public
    { public declarations }
  end;

  TTestWebSocketServerConnection = class;

  { TTestWebSocketServer }

  TTestWebSocketServer = class(TWebSocketServer)
  public
    function GetWebSocketConnectionClass(
      Socket: TTCPCustomConnectionSocket;
      Header: TStringList;
      ResourceName, Host, Port, Origin, Cookie: string;
      out HttpResult: integer;
      var Protocol, Extensions: string
    ): TWebSocketServerConnections; override;
  end;

  TTestWebSocketServerConnection = class(TWebSocketServerConnection)
  public
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

var
  MainForm: TMainForm;

implementation
uses synsock, synachar, StrUtils, synautil,  TypInfo;

{ TMainForm }

procedure TMainForm.StartActionExecute(Sender: TObject);
begin
  ServerErrorLabel.Caption := '';
  fServer := TTestWebSocketServer.Create(HostCombo.text, PortCombo.Text);

  fServer.OnAfterAddConnection := OnAfterAddConnection;
  fServer.OnBeforeAddConnection := OnBeforeAddConnection;
  fServer.OnAfterRemoveConnection := OnAfterRemoveConnection;
  fServer.OnBeforeRemoveConnection := OnBeforeRemoveConnection;
  fServer.OnSocketError := OnServerSocketError;

  fServer.SSL := boolean(SslCheck.Checked);
  fServer.SSLCertificateFile := ExtractFilePath(ParamStr(0)) + 'test.crt';
  fServer.SSLPrivateKeyFile := ExtractFilePath(ParamStr(0)) + 'test.key';
  fServer.Start;
  StartButton.Enabled := false;
end;

procedure TMainForm.SendTextActionExecute(Sender: TObject);
var i: integer;
begin
  fServer.LockTermination;
  for I := 0 to ConnectionList.Items.Count - 1 do
  begin
    if (ConnectionList.Items[i].Selected) then
    begin
      TWebSocketCustomConnection(ConnectionList.Items[i]. Data).SendText(CharsetConversion(SendSelectedMemo.Text, GetCurCP, UTF_8));
      //ConnectionList.Items[i].Selected := false;
    end;
  end;
  fServer.UnLockTermination;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (EndButton.Enabled) then
  begin
    EndActionExecute(EndAction);
  end;
end;



procedure TMainForm.OnAfterAddConnection(Server: TCustomServer;
  aConnection: TCustomConnection);
begin
  InfoMemo.Lines.Insert(0, Format('OnAfterAddConnection %d', [aConnection.Index]));

  with ConnectionList.Items.Add do
  begin
    Data := aConnection;
    Caption := (Format('(%d) %s:%d', [aConnection.Index, aConnection.Socket.GetRemoteSinIP, aConnection.Socket.GetLocalSinPort]));
    SubItems.add('0');
    SubItems.add('0');

    TWebSocketServerConnection(aConnection).OnWrite := OnWrite;
    TWebSocketServerConnection(aConnection).OnRead := OnRead;
    TWebSocketServerConnection(aConnection).OnClose := OnClose;
    TWebSocketServerConnection(aConnection).OnOpen := OnOpen;
    //TWebSocketServerConnection(aConnection).Socket.OnSyncStatus := OnConnectionSocket;

  end;
end;

procedure TMainForm.OnBeforeAddConnection(Server: TCustomServer;
  aConnection: TCustomConnection; var CanAdd: boolean);
begin
  InfoMemo.Lines.Insert(0, Format('OnBeforeAddConnection %d', [aConnection.Index]));
end;

procedure TMainForm.OnAfterRemoveConnection(Server: TCustomServer;
  aConnection: TCustomConnection);
begin
  InfoMemo.Lines.Insert(0, Format('OnAfterRemoveConnection %d', [aConnection.Index]));
end;

procedure TMainForm.OnBeforeRemoveConnection(Server: TCustomServer;
  aConnection: TCustomConnection);
var i: integer;
begin
  InfoMemo.Lines.Insert(0, Format('OnBeforeRemoveConnection %d', [aConnection.Index]));
  for i := 0 to ConnectionList.Items.Count - 1 do
  begin
    if (TCustomConnection(ConnectionList.Items[i].Data).Index = aConnection.Index) then
    begin
      ConnectionList.Items.Delete(i);
      break;
    end;
  end;

end;

procedure TMainForm.OnServerSocketError(Server: TCustomServer; Socket: TTCPBlockSocket);
begin
  ServerErrorLabel.Caption := Format('%s - %d (%s)', [FormatDateTime('yyyy-mm-dd hh:nn:ss', now), Socket.LastError, Socket.LastErrorDesc]);
  InfoMemo.Lines.Insert(0, ServerErrorLabel.Caption);
  ServerErrorLabel.Repaint;
end;

procedure TMainForm.OnOpen(aSender: TWebSocketCustomConnection);
begin
  InfoMemo.Lines.Insert(0, Format('OnOpen %d', [aSender.Index]));
end;

procedure TMainForm.OnRead(aSender: TWebSocketCustomConnection; aFinal, aRes1,
  aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
var item: TListItem;
begin
  InfoMemo.Lines.Insert(0, Format('OnRead %d, final: %d, ext1: %d, ext2: %d, ext3: %d, type: %d, length: %d', [aSender.Index, ord(aFinal), ord(aRes1), ord(aRes2), ord(aRes3), aCode, aData.Size]));

  item := ListItemByIndex(aSender.Index);
  item.SubItems[0] := IntToStr(StrToInt(Item.SubItems[0]) + 1);

end;

procedure TMainForm.OnWrite(aSender: TWebSocketCustomConnection; aFinal, aRes1,
  aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
var item: TListItem;
begin
  InfoMemo.Lines.Insert(0, Format('OnWrite %d, final: %d, ext1: %d, ext2: %d, ext3: %d, type: %d, length: %d', [aSender.Index, ord(aFinal), ord(aRes1), ord(aRes2), ord(aRes3), aCode, aData.Size]));
  item := ListItemByIndex(aSender.Index);
  item.SubItems[1] := IntToStr(StrToInt(Item.SubItems[1]) + 1);

end;

procedure TMainForm.OnClose(aSender: TWebSocketCustomConnection;
  aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
begin
  InfoMemo.Lines.Insert(0, Format('OnClose %d, %d, %s, %s', [aSender.Index, aCloseCode, aCloseReason, IfThen(aClosedByPeer, 'closed by peer', 'closed by me')]));
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

function TMainForm.ListItemByIndex(aConnectionIndex: integer): TListItem;
var i: integer;
begin
  fServer.LockTermination;
  result := nil;
  for i := 0 to ConnectionList.Items.Count - 1 do
  begin
    if (TCustomConnection(ConnectionList.Items[i].Data).Index = aConnectionIndex) then
    begin
      result := ConnectionList.Items[i];
      break;
    end;
  end;
  fServer.UnLockTermination;
end;

procedure TMainForm.EndActionExecute(Sender: TObject);
begin
  ServerErrorLabel.Caption := '';
  fServer.TerminateThread;
  {$IFDEF WIN32}
  WaitForSingleObject(fServer.Handle, 60 * 1000);
  {$ELSE WIN32}
  sleep(2000);
  {$ENDIF WIN32}
  ConnectionList.Items.Clear;
  StartButton.Enabled := true;
end;

procedure TMainForm.BroadcastActionExecute(Sender: TObject);
begin
  fServer.BroadcastText(CharsetConversion(BroadcastMemo.Text, GetCurCP, UTF_8));
end;

procedure TMainForm.CloseActionExecute(Sender: TObject);
var i: integer;
begin
  fServer.LockTermination;
  for I := 0 to ConnectionList.Items.Count - 1 do
  begin
    if (ConnectionList.Items[i].Selected) then
    begin
      ConnectionList.Items[i].Selected := false;
      TWebSocketCustomConnection(ConnectionList.Items[i]. Data).Close(wsCloseNormal, 'closing connection');
    end;
  end;
  fServer.UnLockTermination;
end;

procedure TMainForm.ConnectionListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var s: string;
    c: TTestWebSocketServerConnection;
begin
  LastSentMemo.Lines.Text := '';
  LastReceivedMemo.Lines.Text := '';
  ConnectionInfoMemo.Text := '';
  if (Change = ctState) and (Item.Selected) and (ConnectionList.SelCount = 1) then
  begin
    //TWebSocketCustomConnection(ConnectionList.Items[i]. Data).SendText(SendSelectedMemo.Text)
    c := TTestWebSocketServerConnection(Item.data);
    s := ReadStrFromStream(c.ReadStream, min(c.ReadStream.size, 10 * 1024));
    if (c.ReadCode = wsCodeText) then
      LastReceivedMemo.Lines.text := CharsetConversion(s, UTF_8, GetCurCP)
    else
      LastReceivedMemo.Lines.text := s;

    s := ReadStrFromStream(c.WriteStream, min(c.WriteStream.size, 10 * 1024));
    if (c.WriteCode = wsCodeText) then
      LastSentMemo.Lines.text := CharsetConversion(s, UTF_8, GetCurCP)
    else
      LastSentMemo.Lines.text := s;
    c.ReadStream.Position := 0;
    c.WriteStream.Position := 0;

    ConnectionInfoMemo.Text := c.Header.Text;

  end;
end;

procedure TMainForm.EndActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not StartButton.Enabled;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin

end;

procedure TMainForm.LoadImageActionExecute(Sender: TObject);
var ms1, ms2, ms3: TMemoryStream;
    fs: TFileStream;
    l, i: integer;
    c: TWebSocketCustomConnection;
begin
  OpenPictureDialog.Filter := 'All (*.png;*.bmp;*.ico;*.emf;*.wmf)|*.png;*.bmp;*.ico;*.emf;*.wmf|PNG Image File (*.png)|*.png|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf';
  if (OpenPictureDialog.Execute) then
  begin
    Image1.Picture.LoadFromFile(OpenPictureDialog.FileName);

    ms1 := TMemoryStream.Create;
    ms2 := TMemoryStream.Create;
    ms3 := TMemoryStream.Create;
    fs := TFileStream.Create(OpenPictureDialog.FileName, fmOpenRead);
    try
      fs.Position := 0;
      l := fs.Size div 3;
      ms1.CopyFrom(fs, l);
      ms2.CopyFrom(fs, l);
      ms3.CopyFrom(fs, fs.Size - 2 * l);


      fServer.LockTermination;
      for I := 0 to ConnectionList.Items.Count - 1 do
      begin
        if (ConnectionList.Items[i].Selected) then
        begin
          c := TWebSocketCustomConnection(ConnectionList.Items[i]. Data);
          c.SendBinary(ms1, false);
          c.SendBinaryContinuation(ms2, false);
          c.SendBinaryContinuation(ms3, true);

          {
          fs.Position := 0;
          c.SendBinary(fs, true);
          }
        end;
      end;
      fServer.UnLockTermination;


    finally
      ms1.Free;
      ms2.Free;
      ms3.Free;
      fs.Free;
    end;

  end;
end;

procedure TMainForm.PingActionExecute(Sender: TObject);
var i: integer;
begin
  fServer.LockTermination;
  for I := 0 to ConnectionList.Items.Count - 1 do
  begin
    if (ConnectionList.Items[i].Selected) then
    begin
      TWebSocketCustomConnection(ConnectionList.Items[i]. Data).Ping('ping you');
    end;
  end;
  fServer.UnLockTermination;
end;

procedure TMainForm.PongActionExecute(Sender: TObject);
var i: integer;
begin
  fServer.LockTermination;
  for I := 0 to ConnectionList.Items.Count - 1 do
  begin
    if (ConnectionList.Items[i].Selected) then
    begin
      TWebSocketCustomConnection(ConnectionList.Items[i]. Data).Pong('pong you');
    end;
  end;
  fServer.UnLockTermination;
end;

procedure TMainForm.SendFramesActionExecute(Sender: TObject);
var i, l: integer;
    s, s1, s2, s3: string;
    c: TWebSocketCustomConnection;
    us: UnicodeString;
begin
  s := CharsetConversion(SendSelectedMemo2.text, GetCurCP, UTF_8);
  us := UTF8Decode(s);
  l := length(us) div 3;
  s1 := copy(us, 1, l);
  s2 := copy(us, l + 1, l);
  s3 := copy(us, 2 * l + 1, length(s));
  s1 := UTF8Encode(s1);
  s2 := UTF8Encode(s2);
  s3 := UTF8Encode(s3);

  fServer.LockTermination;
  for I := 0 to ConnectionList.Items.Count - 1 do
  begin
    if (ConnectionList.Items[i].Selected) then
    begin
      c := TWebSocketCustomConnection(ConnectionList.Items[i]. Data);
      c.SendText(s1, false);
      c.SendTextContinuation(s2, false);
      c.SendTextContinuation(s3, true);
    end;
  end;
  fServer.UnLockTermination;

end;

{ TTestWebSocketServer }

function TTestWebSocketServer.GetWebSocketConnectionClass(
      Socket: TTCPCustomConnectionSocket;
      Header: TStringList;
      ResourceName, Host, Port, Origin, Cookie: string;
      out HttpResult: integer;
      var Protocol, Extensions: string
    ): TWebSocketServerConnections;
begin
  Result:= TTestWebSocketServerConnection;
end;

initialization
  {$I mainformu.lrs}

end.

