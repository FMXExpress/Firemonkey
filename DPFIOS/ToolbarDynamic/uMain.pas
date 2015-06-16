unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  DPF.iOS.UIAlertView,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIToolbar, DPF.iOS.UIView, DPF.iOS.UIButton, DPF.iOS.UITextField;

type
  TFiOSToolbarDyn = class( TForm )
    DPFToolbar1: TDPFToolbar;
    DPFAlertView1: TDPFAlertView;
    DPFUIView1: TDPFUIView;
    procedure DPFToolbar1BarItems0Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
    procedure ItemClick( Sender: TObject );
  public
    { Public declarations }
  end;

var
  FiOSToolbarDyn: TFiOSToolbarDyn;

implementation

{$R *.fmx}

procedure TFiOSToolbarDyn.DPFToolbar1BarItems0Click( Sender: TObject );
begin
  // Dont use create item only use this: BarItems.Add
  with DPFToolbar1.BarItems.Add do
  begin
    ButtonSystemItem := bbsiTrash;
    OnClick          := ItemClick;
    AddToToolBar; // <--------- Important to add button into toolbar
  end;
end;

procedure TFiOSToolbarDyn.ItemClick( Sender: TObject );
begin
  TDPFToolbarItem( Sender ).DisposeOf;
  // Or
  // DPFToolbar1.BarItems.Items[TDPFToolbarItem( Sender ).Index].DisposeOf;
  // Or :
  // DPFToolbar1.BarItems.Delete( TDPFToolbarItem( Sender ).Index );
end;

procedure TFiOSToolbarDyn.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
