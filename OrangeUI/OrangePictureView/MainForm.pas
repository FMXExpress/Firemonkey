unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  uUIFunction,
  ViewPictureListFrame;

type
  TfrmMain = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.FormShow(Sender: TObject);
begin
  ShowFrame(TFrame(GlobalViewPictureListFrame),TFrameViewPictureList,Self,nil,nil,nil);
  GlobalViewPictureListFrame.FrameHistroy:=CurrentFrameHistroy;
  GlobalViewPictureListFrame.ShowPicture(0);
end;

end.
