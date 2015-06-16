program CollectionView;

uses
  System.StartUpCopy,
  FMX.Forms,
  UCollectionView in 'UCollectionView.pas' {Form1130};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1130, Form1130);
  Application.Run;
end.
