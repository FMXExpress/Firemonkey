unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.UIView,
  DPF.iOS.UIButton,
  DPF.iOS.UIImageView;

type
  TForm3 = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFButton1: TDPFButton;
    DPFImageView1: TDPFImageView;
    procedure FormShow( Sender: TObject );
    procedure DPFButton1Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.DPFButton1Click( Sender: TObject );
begin
  // Save Bitmap In Album
  {
    DPFImageView1.UpdateBitmap;
    SaveBitmapInAlbum( DPFImageView1.Bitmap );
  }

  // Save UIImage In Album
  SaveImageInAlbum( DPFImageView1.GetUIImage );
end;

procedure TForm3.FormShow( Sender: TObject );
begin
  DPFImageView1.LoadFromFile( GetDocumentsFolder + 'DPF1.png' );
end;

end.
