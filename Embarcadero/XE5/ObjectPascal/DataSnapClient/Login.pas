unit Login;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.Layouts,
  FMX.Filter.Effects, FMX.Effects, FMX.Objects, DMMain, FMX.StdCtrls;

type
  TFrmLogin = class(TForm)
    Layout1: TLayout;
    edLogin: TClearingEdit;
    LbLogin: TLabel;
    edPassword: TClearingEdit;
    lbPassword: TLabel;
    Panel1: TPanel;
    Button2: TButton;
    edDSHost: TClearingEdit;
    Label1: TLabel;
    Label2: TLabel;
    nbPort: TNumberBox;
    seLogin: TShadowEffect;
    sePassword: TShadowEffect;
    seDSHost: TShadowEffect;
    sePort: TShadowEffect;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure edFieldChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FValid: Boolean;
    { Private declarations }
  public
    { Public declarations }
    property Valid: Boolean read FValid;
    function DoLogin( mDM : TDM) : Boolean;
  end;

var
  FrmLogin: TFrmLogin;

implementation

{$R *.fmx}

procedure TFrmLogin.Button1Click(Sender: TObject);
begin

  FValid := true;
  edFieldChange(edLogin);
  edFieldChange(edPassword);
  edFieldChange(edDSHost);

  if Valid then
    ModalResult := mrOk
  else
    ModalResult := mrNone;

end;

function TFrmLogin.DoLogin( mDM : TDM) : Boolean;
var
  User, Pass, Server, Port: String;
begin

    result :=  false;
    if ShowModal = mrOk then
    begin

      Server := edDSHost.Text;
      User := edLogin.Text;
      Pass := edPassword.Text;
      Port := nbPort.Text;
      try
        mDM.Connect(Server, Port, User, Pass);
        result :=  true;
      except
        on E: Exception do
        begin
          ShowMessage(E.Message);
        end;
      end;
    end;
end;

procedure TFrmLogin.edFieldChange(Sender: TObject);
var
  i: Integer;
  Edit: TCustomEdit;
begin

  Edit := TCustomEdit(Sender);

  for i := 0 to Edit.ChildrenCount - 1 do
  begin
    if Edit.Children[i] is TEffect and (Edit.Text = EmptyStr) then
    begin
      TEffect(Edit.Children[i]).Enabled := (Edit.Text = EmptyStr);
      if FValid then
        FValid := Edit.Text <> EmptyStr;
    end;
  end;
end;

procedure TFrmLogin.FormCreate(Sender: TObject);
begin
  FValid := False;
end;

end.
