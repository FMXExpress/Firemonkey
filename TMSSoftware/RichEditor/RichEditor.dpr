program RichEditor;

uses
  FMX.Forms,
  URichEditor in 'URichEditor.pas' {Form1185};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1185, Form1185);
  Application.Run;
end.
