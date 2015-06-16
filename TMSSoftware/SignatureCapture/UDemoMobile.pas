unit UDemoMobile;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Colors, FMX.Objects, FMX.TMSSignatureCapture;

type
  TForm99 = class(TForm)
    Panel1: TPanel;
    ColorPanel1: TColorPanel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    TMSFMXSignatureCapture1: TTMSFMXSignatureCapture;
    procedure ColorPanel1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    root: string;
  public
    { Public declarations }
  end;

var
  Form99: TForm99;

implementation

{$R *.fmx}

procedure TForm99.Button2Click(Sender: TObject);
begin
  TMSFMXSignatureCapture1.LoadFromFile(root + 'fmxtest.fmxsig');
end;

procedure TForm99.Button3Click(Sender: TObject);
begin
  TMSFMXSignatureCapture1.SaveToFile(root + 'fmxtest.fmxsig');
end;

procedure TForm99.Button4Click(Sender: TObject);
begin
  TMSFMXSignatureCapture1.Clear;
end;

procedure TForm99.Button5Click(Sender: TObject);
begin
  if Button5.Text = 'Hide Options' then
  begin
    Button5.Text := 'Show Options';
    Panel1.Visible := False;
  end
  else
  begin
    Button5.Text := 'Hide Options';
    Panel1.Visible := True;
  end;
end;

procedure TForm99.ColorPanel1Change(Sender: TObject);
begin
  TMSFMXSignatureCapture1.Stroke.Color := ColorPanel1.Color;
end;

procedure TForm99.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;
  ColorPanel1.Color := TMSFMXSignatureCapture1.Stroke.Color;
  {$IFDEF ANDROID}
  root := GetHomePath + '/';
  {$ELSE}
  root := ExtractFilePath(ParamStr(0));
  {$ENDIF}
end;

end.
