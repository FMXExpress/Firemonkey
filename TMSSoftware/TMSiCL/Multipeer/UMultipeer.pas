unit UMultipeer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.TMSNativeUIButton, FMX.TMSNativeUITextField, FMX.TMSNativeUIView, FMX.TMSNativeUITabBarController,
  FMX.TMSNativeUIBaseControl, FMX.TMSNativeMultipeerConnectivity, IOUtils,
  FMX.TMSNativeUITableView, iOSApi.UIKit, FMX.TMSNativeUILabel, FMX.TMSNativeUICore;

type
  TSendMode = (smMessage, smMyInfo, smNone);

  TMyInfo = class(TComponent)
  private
    FEmail: String;
    FLastName: String;
    FCompany: String;
    FPhone: String;
    FFirstName: String;
  public
    constructor Create(AFirstName, ALastName, AEmail, ACompany, APhone: String); reintroduce; overload;
    constructor Create; reintroduce; overload;
  published
    property FirstName: String read FFirstName write FFirstName;
    property LastName: String read FLastName write FLastName;
    property Email: String read FEmail write FEmail;
    property Company: String read FCompany write FCompany;
    property Phone: String read FPhone write FPhone;
  end;

  TForm1175 = class(TForm)
    TMSFMXNativeMultipeerConnectivity1: TTMSFMXNativeMultipeerConnectivity;
    TMSFMXNativeUITabBarController1: TTMSFMXNativeUITabBarController;
    TMSFMXNativeUITabBarItem1: TTMSFMXNativeUITabBarItem;
    TMSFMXNativeUITabBarItem2: TTMSFMXNativeUITabBarItem;
    TMSFMXNativeUITabBarItem3: TTMSFMXNativeUITabBarItem;
    TMSFMXNativeUITextField1: TTMSFMXNativeUITextField;
    TMSFMXNativeUIView1: TTMSFMXNativeUIView;
    TMSFMXNativeUIButton1: TTMSFMXNativeUIButton;
    TMSFMXNativeUITableView1: TTMSFMXNativeUITableView;
    TMSFMXNativeUILabel1: TTMSFMXNativeUILabel;
    TMSFMXNativeUITextField2: TTMSFMXNativeUITextField;
    TMSFMXNativeUILabel2: TTMSFMXNativeUILabel;
    TMSFMXNativeUITextField3: TTMSFMXNativeUITextField;
    TMSFMXNativeUILabel3: TTMSFMXNativeUILabel;
    TMSFMXNativeUITextField4: TTMSFMXNativeUITextField;
    TMSFMXNativeUILabel4: TTMSFMXNativeUILabel;
    TMSFMXNativeUITextField5: TTMSFMXNativeUITextField;
    TMSFMXNativeUILabel5: TTMSFMXNativeUILabel;
    TMSFMXNativeUITextField6: TTMSFMXNativeUITextField;
    TMSFMXNativeUIButton2: TTMSFMXNativeUIButton;
    TMSFMXNativeUITextField7: TTMSFMXNativeUITextField;
    TMSFMXNativeUILabel6: TTMSFMXNativeUILabel;
    TMSFMXNativeUILabel7: TTMSFMXNativeUILabel;
    TMSFMXNativeUITextField8: TTMSFMXNativeUITextField;
    TMSFMXNativeUITextField9: TTMSFMXNativeUITextField;
    TMSFMXNativeUILabel8: TTMSFMXNativeUILabel;
    TMSFMXNativeUILabel9: TTMSFMXNativeUILabel;
    TMSFMXNativeUITextField10: TTMSFMXNativeUITextField;
    TMSFMXNativeUITextField11: TTMSFMXNativeUITextField;
    TMSFMXNativeUILabel10: TTMSFMXNativeUILabel;
    procedure TMSFMXNativeUIButton1Click(Sender: TObject);
    procedure TMSFMXNativeMultipeerConnectivity1BrowserViewControllerDidFinish(
      Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXNativeMultipeerConnectivity1DidReceiveString(
      Sender: TObject; AValue: string;
      APeer: TTMSFMXNativeMultipeerConnectivityPeer);
    procedure TMSFMXNativeUITableView1GetItemStyle(Sender: TObject; ASection,
      ARow: Integer; var AStyle: TTMSFMXNativeUITableViewItemStyle);
    procedure TMSFMXNativeUIButton2Click(Sender: TObject);
    procedure TMSFMXNativeMultipeerConnectivity1DidReceiveObject(
      Sender: TObject; AValue: TMemoryStream;
      APeer: TTMSFMXNativeMultipeerConnectivityPeer);
    procedure TMSFMXNativeUITextField2Changed(Sender: TObject);
    procedure TMSFMXNativeMultipeerConnectivity1DidChangeState(Sender: TObject;
      APeer: TTMSFMXNativeMultipeerConnectivityPeer;
      AState: TTMSFMXNativeMultipeerConnectivitySessionState);
  private
    { Private declarations }
    FSendMode: TSendMode;
    procedure CustomizeCell(Sender: TObject; ACell: UITableViewCell; AItemStyle: TTMSFMXNativeUITableViewItemStyle; ASection, ARow: Integer);
    procedure SendMessage;
    procedure SendMyInfo;
    procedure ReceiveFriendsInfo(AValue: TMemoryStream);
    procedure SaveMyInfo;
    procedure LoadMyInfo;
    procedure ReceiveMessage(APeer: TTMSFMXNativeMultipeerConnectivityPeer; AMessage: String);
  public
    { Public declarations }
  end;

var
  Form1175: TForm1175;

implementation

{$R *.fmx}

procedure ShowMessageEx(AMessage: String);
var
  al: UIAlertView;
begin
  al := TUIAlertView.Wrap(TUIAlertView.Wrap(TUIAlertView.OCClass.alloc).initWithTitle(NSStrEx(''),
    NSStrEx(AMessage), nil, NSStrEx('OK'), nil));
  al.show;
  al.release;
end;

procedure TForm1175.CustomizeCell(Sender: TObject; ACell: UITableViewCell;
  AItemStyle: TTMSFMXNativeUITableViewItemStyle; ASection, ARow: Integer);
begin
  if Assigned(ACell) then
  begin
    if TMSFMXNativeUITableView1.Sections[ASection].Items[ARow].Tag = 0 then
    begin
      ACell.textLabel.setTextAlignment(UITextAlignmentLeft);
      ACell.textLabel.setTextColor(TUIColor.Wrap(TUIColor.OCClass.greenColor));
    end;
    if TMSFMXNativeUITableView1.Sections[ASection].Items[ARow].Tag = 1 then
    begin
      ACell.textLabel.setTextAlignment(UITextAlignmentRight);
      ACell.textLabel.setTextColor(TUIColor.Wrap(TUIColor.OCClass.redColor));
    end;
  end;
end;

procedure TForm1175.FormCreate(Sender: TObject);
begin
  TMSFMXNativeMultipeerConnectivity1.MaximumNumberOfPeers := 2;
  TMSFMXNativeUITableView1.OnItemCustomizeCell := CustomizeCell;
  LoadMyInfo;
end;

procedure TForm1175.LoadMyInfo;
var
  sf: TStringList;
begin
  if not TFile.Exists(TPath.GetDocumentsPath + '/MyInfo.txt') then
  begin
    TMSFMXNativeUITextField2.Text := 'tms';
    TMSFMXNativeUITextField3.Text := 'software';
    TMSFMXNativeUITextField4.Text := 'info@tmssoftware.com';
    TMSFMXNativeUITextField5.Text := 'tmssoftware.com';
    TMSFMXNativeUITextField6.Text := '0123456789';
    Exit;
  end;

  sf := nil;
  try
    sf := TStringList.Create;
    sf.LoadFromFile(TPath.GetDocumentsPath + '/MyInfo.txt');
    if sf.Count = 5 then
    begin
      TMSFMXNativeUITextField2.Text := sf[0];
      TMSFMXNativeUITextField3.Text := sf[1];
      TMSFMXNativeUITextField4.Text := sf[2];
      TMSFMXNativeUITextField5.Text := sf[3];
      TMSFMXNativeUITextField6.Text := sf[4];
    end;
  finally
    if Assigned(sf) then
      sf.Free;
  end;
end;

procedure TForm1175.ReceiveMessage(
  APeer: TTMSFMXNativeMultipeerConnectivityPeer; AMessage: String);
var
  it: TTMSFMXNativeUITableViewItem;
begin
  it := TMSFMXNativeUITableView1.Sections[0].Items.Add;
  it.Text := APeer.DisplayName + ': ' + AMessage;
  it.Tag := 1;
end;

procedure TForm1175.ReceiveFriendsInfo(AValue: TMemoryStream);
var
  c: TMyInfo;
begin
  if not Assigned(AValue) then
    Exit;

  try
    c := TMyInfo.Create;
    AValue.ReadComponent(c);
    TMSFMXNativeUITextField7.Text := c.Phone;
    TMSFMXNativeUITextField8.Text := c.Company;
    TMSFMXNativeUITextField9.Text := c.Email;
    TMSFMXNativeUITextField10.Text := c.LastName;
    TMSFMXNativeUITextField11.Text := c.FirstName;
    ShowMessageEx('Friends card info received');
    TMSFMXNativeUITabBarController1.SelectedItemIndex := 1
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

procedure TForm1175.SaveMyInfo;
var
  sf: TStringList;
begin
  sf := nil;
  try
    sf := TStringList.Create;
    sf.Add(TMSFMXNativeUITextField2.Text);
    sf.Add(TMSFMXNativeUITextField3.Text);
    sf.Add(TMSFMXNativeUITextField4.Text);
    sf.Add(TMSFMXNativeUITextField5.Text);
    sf.Add(TMSFMXNativeUITextField6.Text);
    sf.SaveToFile(TPath.GetDocumentsPath + '/MyInfo.txt');
  finally
    if Assigned(sf) then
      sf.Free;
  end;
end;

procedure TForm1175.SendMessage;
var
  it: TTMSFMXNativeUITableViewItem;
begin
  if TMSFMXNativeUITextField1.Text <> '' then
  begin
    TMSFMXNativeMultipeerConnectivity1.SendStringToAllPeers(TMSFMXNativeUITextField1.Text);
    it := TMSFMXNativeUITableView1.Sections[0].Items.Add;
    it.Text := UTF8ToString(TMSFMXNativeMultipeerConnectivity1.PeerID.displayName.UTF8String) + ': ' + TMSFMXNativeUITextField1.Text;
    it.Tag := 0;
    TMSFMXNativeUITextField1.Text := '';
  end;
end;

procedure TForm1175.SendMyInfo;
var
  c: TMyInfo;
  ms: TMemoryStream;
begin
  try
    c := TMyInfo.Create(TMSFMXNativeUITextField2.Text, TMSFMXNativeUITextField3.Text,
      TMSFMXNativeUITextField4.Text, TMSFMXNativeUITextField5.Text, TMSFMXNativeUITextField6.Text);

    ms := TMemoryStream.Create;
    ms.WriteComponent(c);
    TMSFMXNativeMultipeerConnectivity1.SendObjectToAllPeers(ms);
    ShowMessageEx('My card info sent');
  finally
    if Assigned(ms) then
      ms.Free;
    if Assigned(c) then
      c.Free;
  end;
end;

procedure TForm1175.TMSFMXNativeMultipeerConnectivity1BrowserViewControllerDidFinish(
  Sender: TObject);
begin
  case FSendMode of
    smMessage: SendMessage;
    smMyInfo: SendMyInfo;
  end;
  FSendMode := smNone;
end;

procedure TForm1175.TMSFMXNativeMultipeerConnectivity1DidChangeState(
  Sender: TObject; APeer: TTMSFMXNativeMultipeerConnectivityPeer;
  AState: TTMSFMXNativeMultipeerConnectivitySessionState);
begin
  case AState of
    ssSessionStateNotConnected: ShowMessageEx('Connection to ' + APeer.DisplayName + ' is lost');
  end;
end;

procedure TForm1175.TMSFMXNativeMultipeerConnectivity1DidReceiveObject(
  Sender: TObject; AValue: TMemoryStream;
  APeer: TTMSFMXNativeMultipeerConnectivityPeer);
begin
  ReceiveFriendsInfo(AValue);
end;

procedure TForm1175.TMSFMXNativeMultipeerConnectivity1DidReceiveString(
  Sender: TObject; AValue: string;
  APeer: TTMSFMXNativeMultipeerConnectivityPeer);
begin
  ReceiveMessage(APeer, AValue);
end;

procedure TForm1175.TMSFMXNativeUIButton1Click(Sender: TObject);
begin
  if TMSFMXNativeMultipeerConnectivity1.PeerCount = 0 then
  begin
    FSendMode := smMessage;
    TMSFMXNativeMultipeerConnectivity1.SearchForPeers;
  end
  else
    SendMessage;
end;

procedure TForm1175.TMSFMXNativeUIButton2Click(Sender: TObject);
begin
  if TMSFMXNativeMultipeerConnectivity1.PeerCount = 0 then
  begin
    FSendMode := smMyInfo;
    TMSFMXNativeMultipeerConnectivity1.SearchForPeers;
  end
  else
    SendMyInfo;
end;

procedure TForm1175.TMSFMXNativeUITableView1GetItemStyle(Sender: TObject;
  ASection, ARow: Integer; var AStyle: TTMSFMXNativeUITableViewItemStyle);
begin
  AStyle := isTableViewCellStyleDefault;
end;

procedure TForm1175.TMSFMXNativeUITextField2Changed(Sender: TObject);
begin
  SaveMyInfo;
end;

{ TMyInfo }

constructor TMyInfo.Create(AFirstName, ALastName, AEmail, ACompany,
  APhone: String);
begin
  inherited Create(nil);
  FFirstName := AFirstName;
  FLastName := ALastName;
  FEmail := AEmail;
  FCompany := ACompany;
  FPhone := APhone;
end;

constructor TMyInfo.Create;
begin
  inherited Create(nil);
end;

end.
