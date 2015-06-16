unit UIPEditDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSBaseControl, FMX.TMSIPEdit, FMX.Edit, FMX.TMSEdit;

type
  TForm7 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TMSFMXIPEditv4: TTMSFMXIPEdit;
    TMSFMXIPEditv6: TTMSFMXIPEdit;
    TMSFMXIPEditMac: TTMSFMXIPEdit;
    btGet: TButton;
    btSet: TButton;
    TMSFMXEditv4: TTMSFMXEdit;
    TMSFMXEditv6: TTMSFMXEdit;
    TMSFMXEditMac: TTMSFMXEdit;
    procedure btGetClick(Sender: TObject);
    procedure btSetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InitValues;
  end;

var
  Form7: TForm7;

implementation

{$R *.fmx}

procedure TForm7.btGetClick(Sender: TObject);
begin
  InitValues;
end;

procedure TForm7.btSetClick(Sender: TObject);
begin
  TMSFMXIPEditv4.IPAddress := TMSFMXEditv4.Text;
  TMSFMXIPEditv6.IPAddress := TMSFMXEditv6.Text;
  TMSFMXIPEditMac.IPAddress := TMSFMXEditMac.Text;
end;

procedure TForm7.FormCreate(Sender: TObject);
begin
  InitValues;
end;

procedure TForm7.InitValues;
begin
  TMSFMXEditv4.Text := TMSFMXIPEditv4.IPAddress;
  TMSFMXEditv6.Text := TMSFMXIPEditv6.IPAddress;
  TMSFMXEditMac.Text := TMSFMXIPEditMac.IPAddress;
end;

end.
