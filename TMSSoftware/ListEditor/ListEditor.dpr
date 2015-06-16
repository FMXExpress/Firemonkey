program ListEditor;

uses
  FMX.Forms,
  UListEditor in 'UListEditor.pas' {Form1175};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1175, Form1175);
  Application.Run;
end.
