program FlexDocs;

uses
  System.StartUpCopy,
  FMX.Forms,
  UFlexDocs in 'UFlexDocs.pas' {Form13};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm13, Form13);
  Application.Run;
end.
