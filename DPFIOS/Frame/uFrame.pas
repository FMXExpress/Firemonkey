unit uFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, DPF.iOS.BaseControl, DPF.iOS.UIButton, DPF.iOS.UIView,
  DPF.iOS.HTTP, DPF.iOS.UIImageView, uFrame2;

type
  TFrame1 = class( TFrame )
    DPFHttp1: TDPFHttp;
    DPFImageView1: TDPFImageView;
    DPFUIView1: TDPFUIView;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFHttp1ReceiveData( Sender: TObject; Data: string; var isFree: boolean );
    procedure DPFImageView1Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
  end;

implementation

{$R *.fmx}

constructor TFrame1.Create( AOwner: TComponent );
begin
  inherited;
  DPFHttp1.GetUrlContentString( 'http://shahid.ifilmtv.ir/query/englishcurrentshows', [], true );
  DPFUIView1.Frame := TFrame2;
end;

procedure TFrame1.DPFButton1Click( Sender: TObject );
begin
  DisposeOf;
end;

procedure TFrame1.DPFHttp1ReceiveData( Sender: TObject; Data: string; var isFree: boolean );
begin
  { }
end;

procedure TFrame1.DPFImageView1Click( Sender: TObject );
begin
  DPFImageView1.SetAnimationTransition( vatDown );
end;

end.
