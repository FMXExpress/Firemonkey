unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.HTTP,
  DPF.iOS.BaseControl, DPF.iOS.UITextField, DPF.iOS.UIButton, DPF.iOS.UITextView;

type
  TFHTTPContent = class( TForm )
    DPFHttp1: TDPFHttp;
    DPFTextField1: TDPFTextField;
    DPFTextView1: TDPFTextView;
    DPFButton2: TDPFButton;
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFHttp1ReceiveData( Sender: TObject; Data: string; var isFree: Boolean );
    procedure DPFHttp1ReceiveError( Sender: TObject; Error: string; var isFree: Boolean );
    procedure FormShow( Sender: TObject );
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

procedure TFHTTPContent.DPFButton2Click( Sender: TObject );
begin
  DPFTextView1.Text := DPFHttp1.GetUrlContentString( DPFTextField1.Text, [], True );
end;

procedure TFHTTPContent.DPFHttp1ReceiveData( Sender: TObject; Data: string; var isFree: Boolean );
begin
  DPFTextView1.Text := Data;
end;

procedure TFHTTPContent.DPFHttp1ReceiveError( Sender: TObject; Error: string; var isFree: Boolean );
begin
  DPFTextView1.Text := Error;
end;

procedure TFHTTPContent.FormShow( Sender: TObject );
begin
  DPFButton2Click( nil );
end;

procedure TFHTTPContent.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
