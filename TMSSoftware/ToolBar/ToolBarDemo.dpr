program ToolBarDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UToolBarDemo in 'UToolBarDemo.pas' {Form88};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm88, Form88);
  Application.Run;
end.
