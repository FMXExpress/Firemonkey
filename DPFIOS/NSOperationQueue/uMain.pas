unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.UIButton,
  DPF.iOS.UILabel,
  DPF.iOS.UIView,
  DPF.iOS.UIProgressView,
  DPF.iOS.NSOperationQueue,
  DPF.iOS.UISwitch;

type
  TFNSOperationQueue = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFUIView2: TDPFUIView;
    DPFLabel1: TDPFLabel;
    DPFProgress1: TDPFProgress;
    DPFButtonAddOp: TDPFButton;
    DPFNSOperationQueue1: TDPFNSOperationQueue;
    DPFSwitch1: TDPFSwitch;
    DPFLabel2: TDPFLabel;
    DPFButtonCancelAll: TDPFButton;
    DPFLabelOPCount: TDPFLabel;
    procedure DPFButtonAddOpClick( Sender: TObject );
    procedure DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
    procedure DPFButtonCancelAllClick( Sender: TObject );
  private
    { Private declarations }
    prg: Integer;
    procedure Op1;
    procedure Op2;
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FNSOperationQueue: TFNSOperationQueue;

implementation

{$R *.fmx}

const
  opCount = 100;

  // ------------------------------------------------------------------------------
procedure TFNSOperationQueue.DPFButtonAddOpClick( Sender: TObject );
begin
  DPFNSOperationQueue1.AddOperationBlock( op1 );
end;

// ------------------------------------------------------------------------------
procedure TFNSOperationQueue.DPFButtonCancelAllClick( Sender: TObject );
begin
  DPFNSOperationQueue1.CancelAllOperations;
end;

// ------------------------------------------------------------------------------
procedure TFNSOperationQueue.DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
begin
  DPFNSOperationQueue1.SetSuspended( ISON );
end;

// ------------------------------------------------------------------------------
procedure TFNSOperationQueue.Op1;
var
  I: Integer;
begin
  prg   := 0;
  for I := 1 to opCount do
  begin
    FNSOperationQueue.DPFNSOperationQueue1.AddOperationBlockToMainQueue( op2 );
    Sleep( 100 );
  end;

end;

// ------------------------------------------------------------------------------
procedure TFNSOperationQueue.Op2;
begin
  Inc( prg );
  FNSOperationQueue.DPFLabel1.Text        := prg.ToString( );
  FNSOperationQueue.DPFProgress1.Progress := prg / opCount;
  FNSOperationQueue.DPFLabelOPCount.Text  := 'OP Count: ' + FNSOperationQueue.DPFNSOperationQueue1.GetOperationCount.ToString( );
  if prg = opCount then
    prg := 1;
end;

procedure TFNSOperationQueue.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
