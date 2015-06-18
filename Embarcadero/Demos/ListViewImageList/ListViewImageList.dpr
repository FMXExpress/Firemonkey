program ListViewImageList;

uses
  System.StartUpCopy,
  FMX.Forms,
  ImageListForm in 'ImageListForm.pas' {ImageListDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TImageListDemo, ImageListDemo);
  Application.Run;
end.
