unit UPushOverDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSCloudBase, FMX.TMSCloudPushOver, FMX.Edit, FMX.ListBox, FMX.Layouts,
  FMX.Memo;

type
  TForm82 = class(TForm)
    edUser: TEdit;
    Button1: TButton;
    TMSFMXCloudPushOver1: TTMSFMXCloudPushOver;
    edTitle: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edMemo: TMemo;
    Label4: TLabel;
    edURL: TEdit;
    Label5: TLabel;
    edDevice: TEdit;
    edSound: TComboBox;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form82: TForm82;

implementation

{$R *.fmx}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  APPID = 'xxxxxxxxx';
//  USERID = 'yyyyyyyyy';

{$I APPIDS.INC}

procedure TForm82.Button1Click(Sender: TObject);
var
  ms: TMessageSound;
begin
  // set the PushOver application ID
  TMSFMXCloudPushOver1.App.Key := APPID;
  // fill in the message details
  TMSFMXCloudPushOver1.PushOverMessage.User := edUser.Text;
  TMSFMXCloudPushOver1.PushOverMessage.Title := edTitle.Text;
  TMSFMXCloudPushOver1.PushOverMessage.Message := edMemo.Lines.Text;
  TMSFMXCloudPushOver1.PushOverMessage.URL := edURL.Text;
  TMSFMXCloudPushOver1.PushOverMessage.Device := edDevice.Text;

  ms := msDefault;
  case edSound.ItemIndex of
  1: ms := msBike;
  2: ms := msBugle;
  3: ms := msCashRegister;
  4: ms := msCosmic;
  5: ms := msIntermission;
  6: ms := msPianoBar;
  7: ms := msSiren;
  8: ms := msAlien;
  9: ms := msEcho;
  10: ms := msPersistent;
  11: ms := msNone;
  end;
  TMSFMXCloudPushOver1.PushOverMessage.Sound := ms;
  // Send the message
  TMSFMXCloudPushOver1.PushMessage(TMSFMXCloudPushOver1.PushOverMessage);
end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  edUser.Text := USERID;

  edSound.Items.Add('Default');
  edSound.Items.Add('Bike');
  edSound.Items.Add('Bugle');
  edSound.Items.Add('CashRegister');
  edSound.Items.Add('Cosmic');
  edSound.Items.Add('Intermission');
  edSound.Items.Add('PianoBar');
  edSound.Items.Add('Siren');
  edSound.Items.Add('Alien');
  edSound.Items.Add('Echo');
  edSound.Items.Add('Persistent');
  edSound.Items.Add('None');
  edSound.ItemIndex := 0;
end;

end.
