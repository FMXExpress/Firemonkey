program CustomPreview;

uses
  FMX.Forms,
  UCustomPreview in 'UCustomPreview.pas' {FCustomPreview},
  UPasswordDialog in 'UPasswordDialog.pas' {PasswordDialog},
  UPdfExporting in 'UPdfExporting.pas',
  UPrinting in 'UPrinting.pas',
  UProgressThread in 'UProgressThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFCustomPreview, FCustomPreview);
  Application.Run;
end.
