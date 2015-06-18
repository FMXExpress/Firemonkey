program ListCollectionsSampleProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  ListExpressionsSampleFormUnit1 in 'ListExpressionsSampleFormUnit1.pas' {Form1},
  SampleCollections in 'SampleCollections.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
