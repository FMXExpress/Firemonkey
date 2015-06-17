program TeeChart_FastLine_Speed_FMX;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  FMX.Types,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
//  GlobalUseGPUCanvas:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
