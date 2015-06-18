// This software is Copyright (c) 2014 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of one of Embarcadero's developer tools products.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls;

type
  TForm36 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Button2: TButton;
    Edit3: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form36: TForm36;

implementation

{$R *.fmx}

uses
  Androidapi.Helpers,
  Android.JNI.Base64Coder;

procedure TForm36.Button1Click(Sender: TObject);
begin
  Edit2.Text := JStringToString(TJBase64Coder.JavaClass.encodeString(StringToJString(Edit1.Text)));
end;

procedure TForm36.Button2Click(Sender: TObject);
begin
  Edit3.Text := JStringToString(TJBase64Coder.JavaClass.decodeString(StringToJString(Edit2.Text)));

end;

end.
