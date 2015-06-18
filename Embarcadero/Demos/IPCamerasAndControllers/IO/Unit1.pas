unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP;

type
  TForm1 = class(TForm)
    IdHTTP: TIdHTTP;
    btnActivate: TButton;
    Button1: TButton;
    Button2: TButton;
    procedure btnActivateClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnActivateClick(Sender: TObject);
begin

  IdHTTP.Request.BasicAuthentication := TRUE;
  IdHTTP.Request.Username            := 'convidado';
  IdHTTP.Request.Password            := 'convidado';
  IdHTTP.Get('http://10.1.10.10:8601/Interface/GlobalEvents/TriggerGlobalEvent?Event=Saida-Acionadores-1-Ligado');

end;

procedure TForm1.Button1Click(Sender: TObject);
begin

  IdHTTP.Request.BasicAuthentication := TRUE;
  IdHTTP.Request.Username            := 'convidado';
  IdHTTP.Request.Password            := 'convidado';
  IdHTTP.Get('http://10.1.10.10:8601/Interface/GlobalEvents/TriggerGlobalEvent?Event=Saida-Acionadores-1-Desligado');

end;

procedure TForm1.Button2Click(Sender: TObject);
begin

  IdHTTP.Request.BasicAuthentication := TRUE;
  IdHTTP.Request.Username            := 'convidado';
  IdHTTP.Request.Password            := 'convidado';
  IdHTTP.Get('http://10.1.10.10:8601/Interface/GlobalEvents/TriggerGlobalEvent?Event=Saida-Acionadores-Cooler');

end;

end.
