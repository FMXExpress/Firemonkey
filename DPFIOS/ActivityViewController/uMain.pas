unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIView, DPF.iOS.UIActivityViewController, DPF.iOS.UIToolbar;

type
  TFActivityViewController = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFActivityViewController1: TDPFActivityViewController;
    DPFToolbar1: TDPFToolbar;
    procedure DPFToolbar1BarItems0Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FActivityViewController: TFActivityViewController;

implementation

{$R *.fmx}

procedure TFActivityViewController.DPFToolbar1BarItems0Click( Sender: TObject );
begin
  DPFActivityViewController1.ShowActivity( nil, 'Sample text to share', nil, [ { atMail, atPostToFacebook, atPostToTwitter, atMessage, } atPrint, atPostToWeibo, atAssignToContact, atSaveToCameraRoll, atCopyToPasteboard] );
end;

procedure TFActivityViewController.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
