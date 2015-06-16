unit uMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,

  iOSapi.OpenGLES,
  iOSapi.GLKit,

  DPF.iOS.BaseControl,
  DPF.iOS.Common,
  DPF.iOS.GLKView;

type
  TFGLKView = class( TForm )
    DPFGLKView1: TDPFGLKView;
    procedure DPFGLKView1DrawRect( Sender: TObject; Rect: DPFNSRect );
    procedure FormCreate( Sender: TObject );
  private
    { Private declarations }
    _increasing: Boolean;
    _curRed    : Single;

  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FGLKView: TFGLKView;

implementation

{$R *.fmx}

const

  gCubeVertexData: array [0 .. 216 - 1] of GLfloat = (
    // x      y         z               nx     ny     nz
    1.0, -1.0, -1.0, 1.0, 0.0, 0.0, 1.0, 1.0, -1.0, 1.0, 0.0, 0.0, 1.0, -1.0, 1.0, 1.0, 0.0, 0.0, 1.0, -1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, -1.0, 1.0, 0.0, 0.0,

    1.0, 1.0, -1.0, 0.0, 1.0, 0.0, -1.0, 1.0, -1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 0.0, -1.0, 1.0, -1.0, 0.0, 1.0, 0.0, -1.0, 1.0, 1.0, 0.0, 1.0, 0.0,

    -1.0, 1.0, -1.0, -1.0, 0.0, 0.0, -1.0, -1.0, -1.0, -1.0, 0.0, 0.0, -1.0, 1.0, 1.0, -1.0, 0.0, 0.0, -1.0, 1.0, 1.0, -1.0, 0.0, 0.0, -1.0, -1.0, -1.0, -1.0, 0.0, 0.0, -1.0, -1.0, 1.0, -1.0, 0.0, 0.0,

    -1.0, -1.0, -1.0, 0.0, -1.0, 0.0, 1.0, -1.0, -1.0, 0.0, -1.0, 0.0, -1.0, -1.0, 1.0, 0.0, -1.0, 0.0, -1.0, -1.0, 1.0, 0.0, -1.0, 0.0, 1.0, -1.0, -1.0, 0.0, -1.0, 0.0, 1.0, -1.0, 1.0, 0.0, -1.0, 0.0,

    1.0, 1.0, 1.0, 0.0, 0.0, 1.0, -1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, -1.0, 1.0, 0.0, 0.0, 1.0, 1.0, -1.0, 1.0, 0.0, 0.0, 1.0, -1.0, 1.0, 1.0, 0.0, 0.0, 1.0, -1.0, -1.0, 1.0, 0.0, 0.0, 1.0,

    1.0, -1.0, -1.0, 0.0, 0.0, -1.0, -1.0, -1.0, -1.0, 0.0, 0.0, -1.0, 1.0, 1.0, -1.0, 0.0, 0.0, -1.0, 1.0, 1.0, -1.0, 0.0, 0.0, -1.0, -1.0, -1.0, -1.0, 0.0, 0.0, -1.0, -1.0, 1.0, -1.0, 0.0, 0.0, -1.0 );

  // ------------------------------------------------------------------------------
procedure TFGLKView.DPFGLKView1DrawRect( Sender: TObject; Rect: DPFNSRect );
begin

  if ( _increasing ) then
  begin
    _curRed := _curRed + 0.01;
  end
  else
  begin
    _curRed := _curRed - 0.01;
  end;
  if ( _curRed >= 1.0 ) then
  begin
    _curRed     := 1.0;
    _increasing := false;
  end;
  if ( _curRed <= 0.0 ) then
  begin
    _curRed     := 0.0;
    _increasing := true;
  end;

  glClearColor( _curRed, 0.0, 0.0, 1.0 );
  glClear( GL_COLOR_BUFFER_BIT );

end;

// ------------------------------------------------------------------------------
procedure TFGLKView.FormCreate( Sender: TObject );
begin
  _increasing := true;
  _curRed     := 0.0;
end;

procedure TFGLKView.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

// ------------------------------------------------------------------------------
end.
