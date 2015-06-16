program PickerDemo;

uses
  FMX.Forms,
  UPickerDemo in 'UPickerDemo.pas' {Form5};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
