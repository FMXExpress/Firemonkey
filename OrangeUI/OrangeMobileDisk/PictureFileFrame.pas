unit PictureFileFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Layouts, FMX.Memo,uFileManage, FMX.ExtCtrls,
  uUIFunction,
  uSkinFireMonkeyLabel, uSkinFireMonkeyButton, uSkinFireMonkeyControl,
  uSkinFireMonkeyImage, uSkinFireMonkeyPanel;

type
  TFramePictureFile = class(TFrame)
    imgPicture: TImageViewer;
    pnlToolBar: TSkinFMXPanel;
    btnReturn: TSkinFMXButton;
    lblFileName: TSkinFMXLabel;
    procedure btnReturnClick(Sender: TObject);
  private
    FFileItem:TFileItem;
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    procedure OnReturnFrame(FromFrame:TFrame);
  public
    function LoadFile(AFileItem:TFileItem):Boolean;
    { Public declarations }
  end;

var
  GlobalPictureFileFrame:TFramePictureFile;

implementation

{$R *.fmx}

{ TFramePictureFile }


procedure TFramePictureFile.btnReturnClick(Sender: TObject);
begin
  HideFrame(Self);
  ReturnFrame(FrameHistroy);
  FreeAndNil(GlobalPictureFileFrame);
end;

function TFramePictureFile.LoadFile(AFileItem: TFileItem): Boolean;
begin
  Result:=False;

  FFileItem:=AFileItem;
  lblFileName.Text:=FFileItem.FileName;
  Self.imgPicture.Bitmap.LoadFromFile(FFileItem.FilePath);

  Result:=True;
end;

procedure TFramePictureFile.OnReturnFrame(FromFrame: TFrame);
begin
end;

end.

