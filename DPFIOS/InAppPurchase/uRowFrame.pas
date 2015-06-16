unit uRowFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  DPF.iOS.UIButton, DPF.iOS.BaseControl, DPF.iOS.UILabel, DPF.iOS.UIView, uMain;

type
  TFrameRow = class( TFrame )
    DPFLabelTitle: TDPFLabel;
    DPFLabelDesc: TDPFLabel;
    DPFLabelPrice: TDPFLabel;
    DPFButton1: TDPFButton;
    DPFUIViewMain: TDPFUIView;
    procedure DPFButton1Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
    SectionNo, RowNo: Integer;
    constructor create( aowner: TComponent );
  end;

implementation

{$R *.fmx}

constructor TFrameRow.create( aowner: TComponent );
begin
  inherited create( aowner );
  SectionNo := -1;
  RowNo     := -1;
end;

procedure TFrameRow.DPFButton1Click( Sender: TObject );
begin
  if ( SectionNo > -1 ) and ( RowNo > -1 ) then
    FInAppPurchase.DPFInAppPurchase1.PurchaseProduct( FInAppPurchase.DPFUITableView1.Row[SectionNo, RowNo].TagStr );
end;

end.
