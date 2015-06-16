unit UPushOverDemo;

interface

uses
  FMX.Forms,FMX.TMSCloudBase, FMX.TMSCloudPushOver, FMX.StdCtrls, FMX.ListBox,
  FMX.Layouts, FMX.Memo, FMX.Edit, System.Classes, FMX.Types, FMX.Controls;


type
  TForm4 = class(TForm)
    TMSFMXCloudPushOver1: TTMSFMXCloudPushOver;
    edUser: TEdit;
    GroupBox1: TGroupBox;
    edTitle: TEdit;
    edMemo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    edSound: TComboBox;
    edDevice: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    edURL: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.FMX}

uses
  DBXJSON;

{$I APPIDS.INC}

procedure TForm4.Button1Click(Sender: TObject);
begin
  // set the PushOver application ID
  TMSFMXCloudPushOver1.App.Key := APPID;
  // fill in the message details
  TMSFMXCloudPushOver1.PushOverMessage.User := edUser.Text;
  TMSFMXCloudPushOver1.PushOverMessage.Title := edTitle.Text;
  TMSFMXCloudPushOver1.PushOverMessage.Message := edMemo.Lines.Text;
  TMSFMXCloudPushOver1.PushOverMessage.URL := edURL.Text;
  TMSFMXCloudPushOver1.PushOverMessage.Device := edDevice.Text;
  TMSFMXCloudPushOver1.PushOverMessage.Sound := TMessageSound(integer(edSound.Items.Objects[edSound.ItemIndex]));
  // Send the message
  TMSFMXCloudPushOver1.PushMessage(TMSFMXCloudPushOver1.PushOverMessage);
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  edUser.Text := USERID;

  edSound.Items.AddObject('Default',TObject(msDefault));
  edSound.Items.AddObject('Bike',TObject(msBike));
  edSound.Items.AddObject('Bugle',TObject(msBugle));
  edSound.Items.AddObject('CashRegister',TObject(msCashRegister));
  edSound.Items.AddObject('Cosmic',TObject(msCosmic));
  edSound.Items.AddObject('Intermission',TObject(msIntermission));
  edSound.Items.AddObject('PianoBar',TObject(msPianoBar));
  edSound.Items.AddObject('Siren',TObject(msSiren));
  edSound.Items.AddObject('Alien',TObject(msAlien));
  edSound.Items.AddObject('Echo',TObject(msEcho));
  edSound.Items.AddObject('Persistent',TObject(msPersistent));
  edSound.Items.AddObject('None',TObject(msNone));
  edSound.ItemIndex := 0;
end;




end.
