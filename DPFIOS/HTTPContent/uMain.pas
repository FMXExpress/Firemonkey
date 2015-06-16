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
    DPFButton1: TDPFButton;
    DPFTextView1: TDPFTextView;
    DPFButton2: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
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

procedure TFHTTPContent.DPFButton1Click( Sender: TObject );
begin
{$IFDEF IOS}
  // DPFTextView1.Text := DPFHttp1.GetUrl( DPFTextField1.Text, [BuildHeaderRecord( 'Content-Type', 'application/json' )], false );
  DPFTextView1.Text := DPFHttp1.GetUrlContentString( DPFTextField1.Text, [BuildHeaderRecord( 'content-type', 'application/json' ), BuildHeaderRecord( 'Accept', 'application/json' )], false );
{$ENDIF}
end;

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
