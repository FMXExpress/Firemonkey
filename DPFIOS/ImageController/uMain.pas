unit uMain;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
{$IFDEF DELPHIXE5}FMX.Graphics, {$ENDIF}
  DPF.iOS.BaseControl,
  DPF.iOS.UIView,
  DPF.iOS.UIToolbar,
  DPF.iOS.UIImagePickerController,
  DPF.iOS.UIButton,
  DPF.iOS.UIImageView;

type
  TFImageController = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFToolbar1: TDPFToolbar;
    DPFUIImagePickerController1: TDPFUIImagePickerController;
    DPFImageView1: TDPFImageView;
    procedure DPFToolbar1BarItems0Click( Sender: TObject );
    procedure DPFUIImagePickerController1didFinishPickingImage( Sender: TObject; BitMap: TBitmap );
    procedure DPFToolbar1BarItems2Click( Sender: TObject );
    procedure DPFToolbar1BarItems4Click( Sender: TObject );
    procedure DPFUIImagePickerController1DidFinishPickingMovie( Sender: TObject; const FileSize: Int64; var FileSavedPath: string );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FImageController: TFImageController;

implementation

{$R *.fmx}

// ------------------------------------------------------------------------------
procedure TFImageController.DPFToolbar1BarItems0Click( Sender: TObject );
begin
  DPFUIImagePickerController1.EditingType := TDPFEditingType.etOriginalImage;
  if not DPFUIImagePickerController1.TakeImageFromCamera then
    ShowMessage( 'Not Support' );
end;

// ------------------------------------------------------------------------------
procedure TFImageController.DPFToolbar1BarItems2Click( Sender: TObject );
begin
  if not DPFUIImagePickerController1.TakeImageFromLibrary then
    ShowMessage( 'Not Support' );
end;

// ------------------------------------------------------------------------------
procedure TFImageController.DPFToolbar1BarItems4Click( Sender: TObject );
begin
  DPFUIImagePickerController1.EditingType := TDPFEditingType.etMediaType;
  if not DPFUIImagePickerController1.TakeImageFromCamera then
    ShowMessage( 'Not Support' );
end;

// ------------------------------------------------------------------------------
procedure TFImageController.DPFUIImagePickerController1didFinishPickingImage( Sender: TObject; BitMap: TBitmap );
begin
  DPFImageView1.SetImage( BitMap );
end;

procedure TFImageController.DPFUIImagePickerController1DidFinishPickingMovie( Sender: TObject; const FileSize: Int64; var FileSavedPath: string );
begin
  ShowMessage( FileSize.ToString );
end;

// ------------------------------------------------------------------------------
procedure TFImageController.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

// ------------------------------------------------------------------------------
end.
