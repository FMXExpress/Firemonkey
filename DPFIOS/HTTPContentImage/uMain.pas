unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.HTTP,
  DPF.iOS.UIImageView, DPF.iOS.UIButton, DPF.iOS.BaseControl,
  DPF.iOS.UITextField;

type
  TFHTTPContent = class( TForm )
    DPFHttp1: TDPFHttp;
    DPFTextField1: TDPFTextField;
    DPFButton1: TDPFButton;
    DPFImageView1: TDPFImageView;
    DPFButton2: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure DPFHttp1ReceiveError( Sender: TObject; Error: string; var isFree: Boolean );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FHTTPContent: TFHTTPContent;

implementation

{$R *.fmx}

procedure TFHTTPContent.DPFButton1Click( Sender: TObject );
begin
{$IFDEF IOS}
  DPFImageView1.setImage( DPFHttp1.GetUrlContentImage( DPFTextField1.Text, [], false ) );
{$ENDIF}
end;

procedure TFHTTPContent.DPFButton2Click( Sender: TObject );
begin
  DPFImageView1.SetAnimationTransition( TDPFViewAnimationTransition( RandomRange( 1, 4 ) ), 1.0 );
end;

procedure TFHTTPContent.DPFHttp1ReceiveError( Sender: TObject; Error: string; var isFree: Boolean );
begin
  ShowMessage( Error );
end;

procedure TFHTTPContent.FormCreate( Sender: TObject );
begin
  Randomize;
end;

procedure TFHTTPContent.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
