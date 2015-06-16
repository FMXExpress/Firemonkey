unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.UIAlertView,
  DPF.iOS.UIToolbar,
  DPF.iOS.UIView,
  DPF.iOS.UIButton,
  DPF.iOS.UITextField;

type
  TFToolbar = class( TForm )
    DPFToolbar1: TDPFToolbar;
    DPFToolbar2: TDPFToolbar;
    DPFToolbar3: TDPFToolbar;
    DPFToolbar4: TDPFToolbar;
    DPFToolbar5: TDPFToolbar;
    DPFButton1: TDPFButton;
    DPFToolbar6: TDPFToolbar;
    DPFAlertView1: TDPFAlertView;
    DPFUIView1: TDPFUIView;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFToolbar1Items0Click( Sender: TObject );
    procedure DPFToolbar2BarItems0Click( Sender: TObject );
    procedure DPFToolbar3BarItems0Click( Sender: TObject );
    procedure DPFToolbar4BarItems0Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FToolbar: TFToolbar;

implementation

{$R *.fmx}

procedure TFToolbar.DPFButton1Click( Sender: TObject );
begin
  ShowMessage( 'You clicked a normal Button!' );
end;

procedure TFToolbar.DPFToolbar1Items0Click( Sender: TObject );
begin
  ShowMessage( 'Clicked Imaged Button' );
end;

procedure TFToolbar.DPFToolbar2BarItems0Click( Sender: TObject );
begin
  ShowMessage( 'Clicked System Button: ' + IntToStr( Integer( TDPFToolbarItem( Sender ).ButtonItemStyle ) ) );
end;

procedure TFToolbar.DPFToolbar3BarItems0Click( Sender: TObject );
begin
  ShowMessage( 'Clicked Titled Button 1' );
end;

procedure TFToolbar.DPFToolbar4BarItems0Click( Sender: TObject );
begin
  ShowMessage( 'Clicked Titled Button 2' );
end;

procedure TFToolbar.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
