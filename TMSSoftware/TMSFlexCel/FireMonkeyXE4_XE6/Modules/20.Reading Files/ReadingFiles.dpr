program ReadingFiles;

uses
  FMX.Forms,
  UReadingFiles in 'UReadingFiles.pas' {FReadingFiles};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFReadingFiles, FReadingFiles);
  Application.Run;
end.
