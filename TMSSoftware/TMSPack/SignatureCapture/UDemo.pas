unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.TMSSignatureCapture, FMX.Colors;

type
  TForm98 = class(TForm)
    Button2: TButton;
    Button1: TButton;
    SaveDialog1: TSaveDialog;
    Button3: TButton;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    ColorPanel1: TColorPanel;
    TMSFMXSignatureCapture1: TTMSFMXSignatureCapture;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ColorPanel1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form98: TForm98;

implementation

{$R *.fmx}

procedure TForm98.Button1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    TMSFMXSignatureCapture1.SaveToImageFile(SaveDialog1.FileName);
end;

procedure TForm98.Button2Click(Sender: TObject);
begin
  TMSFMXSignatureCapture1.Clear;
end;

procedure TForm98.Button3Click(Sender: TObject);
var
  extension: string;
  namesize, lastpoint: integer;
  I: Integer;
begin
  if SaveDialog1.Execute then
  begin
    lastpoint := -1;
    extension := '';
    namesize := Length(SaveDialog1.FileName);
    for I := 0 to namesize do
    begin
      if SaveDialog1.FileName[I] = '.' then
      begin
        lastpoint := I;
        extension := '.';
      end
      else if lastpoint > -1 then
        extension := extension + SaveDialog1.FileName[I];
    end;
    TMSFMXSignatureCapture1.SaveToFile(SaveDialog1.FileName, extension = '.sig');
  end;
end;

procedure TForm98.Button4Click(Sender: TObject);
var
  extension: string;
  namesize, lastpoint: integer;
  I: Integer;
begin
  if OpenDialog1.Execute then
  begin
    lastpoint := -1;
    extension := '';
    namesize := Length(OpenDialog1.FileName);
    for I := 0 to namesize do
    begin
      if OpenDialog1.FileName[I] = '.' then
      begin
        lastpoint := I;
        extension := '.';
      end
      else if lastpoint > -1 then
        extension := extension + OpenDialog1.FileName[I];
    end;
    TMSFMXSignatureCapture1.LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TForm98.ColorPanel1Change(Sender: TObject);
begin
  TMSFMXSignatureCapture1.Stroke.Color := ColorPanel1.Color;
end;

procedure TForm98.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  ColorPanel1.Color := TMSFMXSignatureCapture1.Stroke.Color;
end;

end.
