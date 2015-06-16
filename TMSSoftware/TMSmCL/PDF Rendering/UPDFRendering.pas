unit UPDFRendering;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSNativeNSBaseControl, FMX.TMSNativeWebView, FMX.TMSNativeMacGraphicsLib,
  FMX.TMSNativeMacPDFLib, FMX.TMSNativeMacRichTextLib, IOUtils;

type
  TForm1147 = class(TForm)
    TMSFMXNativeMacPDFLib1: TTMSFMXNativeMacPDFLib;
    TMSFMXNativeWebView1: TTMSFMXNativeWebView;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1147: TForm1147;

implementation

{$R *.fmx}

procedure TForm1147.FormCreate(Sender: TObject);
var
  fn: string;
  lorem: String;
  r, rs, rss: TRectF;
  str: String;
  ostr: String;
  st: TPointF;
  x, y: Single;
  I: Integer;
  rad: Single;
  angle: Single;
begin
  lorem := 'Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry''s standard dummy text ever since '+
  'the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also '+
  'the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem '+
  'Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum. It is a long established fact that'+
  ' a reader will be distracted by the readable content of a page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less normal'+
  ' distribution of letters, as opposed to using ''Content here, content here'', making it look like readable English. '+
  'Many desktop publishing packages and web page editors now use Lorem Ipsum as their default model text, and a search for ''lorem ipsum'' '+
  'will uncover many web sites still in their infancy. Various versions have evolved over the years, sometimes by accident, sometimes on purpose (injected humour and the like).';

  fn := TPath.GetDocumentsPath + '/sample.pdf';
  r := TMSFMXNativeMacPDFLib1.MediaBox;
  InflateRect(r, -50, -40);

  TMSFMXNativeMacPDFLib1.Author := 'Your Company';
  TMSFMXNativeMacPDFLib1.Creator := 'Your Name';
  TMSFMXNativeMacPDFLib1.Title := 'PDF Rendering Library Sample';
  TMSFMXNativeMacPDFLib1.Subject := 'Demonstration of the PDF Rendering Library';
  TMSFMXNativeMacPDFLib1.Keywords.Add('PDF Rendering Library');
  TMSFMXNativeMacPDFLib1.Keywords.Add('Mac OSX');
  TMSFMXNativeMacPDFLib1.Keywords.Add('Lorem Ipsum');

  TMSFMXNativeMacPDFLib1.BeginDocument(fn);
  TMSFMXNativeMacPDFLib1.Header := 'PDF Rendering Library';
  TMSFMXNativeMacPDFLib1.Footer := 'Page 1 / 2';
  TMSFMXNativeMacPDFLib1.NewPage;

  TMSFMXNativeMacPDFLib1.RichText.Text := ' Text flow in multiple columns'#13#10;
  TMSFMXNativeMacPDFLib1.RichText.SetFontSize(14);
  TMSFMXNativeMacPDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Steelblue);
  TMSFMXNativeMacPDFLib1.RichText.SetForegroundColor(TAlphaColorRec.White);
  rs := TMSFMXNativeMacPDFLib1.DrawRichText(RectF(R.Left, R.Top, r.Right, r.Top + 20));
  TMSFMXNativeMacPDFLib1.FillColor := TAlphaColorRec.Black;
  TMSFMXNativeMacPDFLib1.DrawText(lorem, RectF(r.Left, R.Top + rs.Height, r.Right, R.Top + rs.Height + 250), 3);
  TMSFMXNativeMacPDFLib1.FillColor := TAlphaColorRec.Steelblue;
  TMSFMXNativeMacPDFLib1.RichText.Text := ' Rich text'#13#10;
  TMSFMXNativeMacPDFLib1.RichText.SetFontSize(14);
  TMSFMXNativeMacPDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Steelblue);
  TMSFMXNativeMacPDFLib1.RichText.SetForegroundColor(TAlphaColorRec.White);
  rs := TMSFMXNativeMacPDFLib1.DrawRichText(RectF(R.Left, R.Top + rs.Height + 250, r.Right, R.Top + rs.Height + 270));
  ostr := 'Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry''s standard dummy text ever since.';
  TMSFMXNativeMacPDFLib1.RichText.Text := ostr;
  str := 'dummy';
  TMSFMXNativeMacPDFLib1.RichText.SetFontSize(28, Pos(str, ostr) - 1, Length(str));
  str := 'typesetting';
  TMSFMXNativeMacPDFLib1.RichText.SetForegroundColor(TAlphaColorRec.Red, Pos(str, ostr) - 1, Length(str));
  str := 'industry''s';
  TMSFMXNativeMacPDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Yellowgreen, Pos(str, ostr) - 1, Length(str));
  TMSFMXNativeMacPDFLib1.RichText.SetForegroundColor(TAlphaColorRec.Darkgreen, Pos(str, ostr) - 1, Length(str));
  str := 'standard dummy';
  TMSFMXNativeMacPDFLib1.RichText.SetFont('Cochin', 18, Pos(str, ostr) - 1, Length(str));
  TMSFMXNativeMacPDFLib1.RichText.SetBold(True, Pos(str, ostr) - 1, Length(str));
  TMSFMXNativeMacPDFLib1.RichText.SetItalic(True, Pos(str, ostr) - 1, Length(str));
  rs := TMSFMXNativeMacPDFLib1.DrawRichText(RectF(rs.Left, rs.Bottom - 10, rs.Left + 300, rs.Bottom + 290));

  TMSFMXNativeMacPDFLib1.RichText.Text := ' Text shadow with color and blur'#13#10;
  TMSFMXNativeMacPDFLib1.RichText.SetFontSize(14);
  TMSFMXNativeMacPDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Steelblue);
  TMSFMXNativeMacPDFLib1.RichText.SetForegroundColor(TAlphaColorRec.White);
  rs := TMSFMXNativeMacPDFLib1.DrawRichText(RectF(R.Left, rs.Bottom + 10, r.Right, rs.Bottom + 30));

  TMSFMXNativeMacPDFLib1.DrawSaveState;
  TMSFMXNativeMacPDFLib1.DrawAddShadow(PointF(5, 5), 3);
  TMSFMXNativeMacPDFLib1.Font.Size := 28;
  TMSFMXNativeMacPDFLib1.DrawText('Hello World', PointF(rs.Left + 30, rs.Bottom));
  TMSFMXNativeMacPDFLib1.DrawAddShadow(PointF(5, 5), 0, TAlphaColorRec.Red);
  rs := TMSFMXNativeMacPDFLib1.DrawText('Hello World', PointF(rs.Left + 300, rs.Bottom));
  TMSFMXNativeMacPDFLib1.DrawRestoreState;
  TMSFMXNativeMacPDFLib1.Font.Size := 14;

  TMSFMXNativeMacPDFLib1.RichText.Text := ' Combinations'#13#10;
  TMSFMXNativeMacPDFLib1.RichText.SetFontSize(14);
  TMSFMXNativeMacPDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Steelblue);
  TMSFMXNativeMacPDFLib1.RichText.SetForegroundColor(TAlphaColorRec.White);
  rs := TMSFMXNativeMacPDFLib1.DrawRichText(RectF(R.Left, rs.Bottom + 10, r.Right, rs.Bottom + 30));

  TMSFMXNativeMacPDFLib1.DrawSaveState;
  TMSFMXNativeMacPDFLib1.DrawAddShadow(PointF(5, 5), 3, TAlphaColorRec.Silver);
  TMSFMXNativeMacPDFLib1.RichText.Text := lorem;
  TMSFMXNativeMacPDFLib1.RichText.SetFontSize(10);
  str := 'standard dummy';
  TMSFMXNativeMacPDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Yellow, Pos(str, lorem) - 1, Length(str));
  str := 'electronic';
  TMSFMXNativeMacPDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Yellow, Pos(str, lorem) - 1, Length(str));
  str := 'Aldus PageMaker';
  TMSFMXNativeMacPDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Yellow, Pos(str, lorem) - 1, Length(str));
  str := 'established fact';
  TMSFMXNativeMacPDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Yellow, Pos(str, lorem) - 1, Length(str));
  str := 'packages';
  TMSFMXNativeMacPDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Yellow, Pos(str, lorem) - 1, Length(str));

  str := 'Lorem Ipsum';
  TMSFMXNativeMacPDFLib1.RichText.SetStrikethrough([usUnderlineStyleSingle], Pos(str, lorem) - 1, Length(str));
  TMSFMXNativeMacPDFLib1.RichText.SetStrikethroughColor(TAlphaColorRec.Blue, Pos(str, lorem) - 1, Length(str));
  TMSFMXNativeMacPDFLib1.RichText.SetBold(True, Pos(str, lorem) - 1, Length(str));

  str := 'Various';
  TMSFMXNativeMacPDFLib1.RichText.SetStrikethrough([usUnderlineStyleSingle], Pos(str, lorem) - 1, Length(str));
  TMSFMXNativeMacPDFLib1.RichText.SetStrikethroughColor(TAlphaColorRec.Blue, Pos(str, lorem) - 1, Length(str));
  TMSFMXNativeMacPDFLib1.RichText.SetBold(True, Pos(str, lorem) - 1, Length(str));

  str := 'accident';
  TMSFMXNativeMacPDFLib1.RichText.SetForegroundColor(TAlphaColorRec.Red, Pos(str, lorem) - 1, Length(str));
  TMSFMXNativeMacPDFLib1.RichText.SetFontSize(20, Pos(str, lorem) - 1, Length(str));
  TMSFMXNativeMacPDFLib1.RichText.SetBold(True, Pos(str, lorem) - 1, Length(str));

  TMSFMXNativeMacPDFLib1.DrawRichText(RectF(r.Left, rs.Bottom, r.Right, rs.Bottom + 190), 4);
  TMSFMXNativeMacPDFLib1.DrawRestoreState;

  TMSFMXNativeMacPDFLib1.RichText.SetFontSize(14);

  TMSFMXNativeMacPDFLib1.Footer := 'Page 2 / 2';
  TMSFMXNativeMacPDFLib1.Orientation := poLandscape;
  TMSFMXNativeMacPDFLib1.NewPage;
  r := TMSFMXNativeMacPDFLib1.MediaBox;
  InflateRect(r, -50, -50);

  TMSFMXNativeMacPDFLib1.RichText.Text := ' Image drawing and quality control'#13#10;
  TMSFMXNativeMacPDFLib1.RichText.SetFontSize(14);
  TMSFMXNativeMacPDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Steelblue);
  TMSFMXNativeMacPDFLib1.RichText.SetForegroundColor(TAlphaColorRec.White);
  rs := TMSFMXNativeMacPDFLib1.DrawRichText(RectF(R.Left, R.Top, r.Right, R.Top + 20));

  str := ExtractFilePath(ParamStr(0)) + 'family.jpg';
  rs := RectF(rs.Left, rs.Bottom + 10, rs.Left + 150, rs.Bottom + 110);
  TMSFMXNativeMacPDFLib1.DrawImageFromFile(str, rs);
  rs := RectF(rs.Right + 20, rs.Top, rs.Right + 20 + rs.Width, rs.Bottom);
  TMSFMXNativeMacPDFLib1.DrawImageFromFile(str, rs, itJPG, 0.1);
  str := ExtractFilePath(ParamStr(0)) + 'tulips.jpg';
  rs := RectF(r.Left, rs.Bottom + 10, r.Left + 150, rs.Bottom + 110);
  TMSFMXNativeMacPDFLib1.DrawImageFromFile(str, rs);
  rs := RectF(rs.Right + 20, rs.Top, rs.Right + 20 + rs.Width, rs.Bottom);
  TMSFMXNativeMacPDFLib1.DrawImageFromFile(str, rs, itJPG, 0.25);

  TMSFMXNativeMacPDFLib1.RichText.Text := ' Graphics'#13#10;
  TMSFMXNativeMacPDFLib1.RichText.SetFontSize(14);
  TMSFMXNativeMacPDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Steelblue);
  TMSFMXNativeMacPDFLib1.RichText.SetForegroundColor(TAlphaColorRec.White);
  rs := TMSFMXNativeMacPDFLib1.DrawRichText(RectF(R.Left, rs.Bottom + 10, r.Right, rs.Bottom + 30));

  rs := RectF(rs.Left, rs.Bottom + 20, rs.Left + 200, rs.Bottom + 150);
  TMSFMXNativeMacPDFLib1.FillColor := TAlphaColorRec.Lightsteelblue;
  TMSFMXNativeMacPDFLib1.StrokeColor := TAlphaColorRec.Steelblue;
  TMSFMXNativeMacPDFLib1.DrawRectangle(rs);

  rss := RectF(rs.Right + 50, rs.Top, rs.Right + 150, rs.Top + 100);
  TMSFMXNativeMacPDFLib1.DrawSaveState;
  TMSFMXNativeMacPDFLib1.FillColor := TAlphaColorRec.Orange;
  TMSFMXNativeMacPDFLib1.StrokeColor := TAlphaColorRec.Null;
  TMSFMXNativeMacPDFLib1.LineWidth := 2;
  TMSFMXNativeMacPDFLib1.DrawPathBegin;
  TMSFMXNativeMacPDFLib1.DrawPathAddEllipse(rss);
  TMSFMXNativeMacPDFLib1.DrawAddShadow(PointF(5, 5), 3);
  TMSFMXNativeMacPDFLib1.DrawPathEnd(dmPathFill);
  TMSFMXNativeMacPDFLib1.DrawRestoreState;
  TMSFMXNativeMacPDFLib1.DrawSaveState;
  TMSFMXNativeMacPDFLib1.StrokeColor := TAlphaColorRec.Darkorange;
  TMSFMXNativeMacPDFLib1.FillColor := TAlphaColorRec.Null;
  TMSFMXNativeMacPDFLib1.DrawEllipse(rss);
  TMSFMXNativeMacPDFLib1.DrawRestoreState;

  TMSFMXNativeMacPDFLib1.DrawSaveState;
  TMSFMXNativeMacPDFLib1.FillColor := TAlphaColorRec.Red;
  TMSFMXNativeMacPDFLib1.FillColorTo := TAlphaColorRec.Blue;
  TMSFMXNativeMacPDFLib1.DrawPathBegin;

  st := PointF(rss.Left + 250, rss.CenterPoint.Y);
  rad := 75;
  angle := 0;
  x := rad * Sin(angle * Pi / 5) + st.X;
  y := rad * Cos(angle * PI / 5) + st.Y;
  TMSFMXNativeMacPDFLib1.DrawPathMoveToPoint(PointF(x, y));

  for I := 1 to 5 do
  begin
    x := rad * sin((i * 4 * pi + angle) / 5) + st.X;
    y := rad * cos((i * 4 * pi + angle) / 5) + st.Y;
    TMSFMXNativeMacPDFLib1.DrawPathAddLineToPoint(PointF(x, y));
  end;

  TMSFMXNativeMacPDFLib1.DrawPathClose;
  TMSFMXNativeMacPDFLib1.DrawPathBeginClip;
  TMSFMXNativeMacPDFLib1.DrawPathEndLinearGradient(PointF(st.X - 100, st.Y - 100), PointF(st.X + 100, st.Y + 100));
  TMSFMXNativeMacPDFLib1.DrawPathEndClip;
  TMSFMXNativeMacPDFLib1.DrawRestoreState;

  rss := rs;
  InflateRect(rss, -25, -25);
  rss.Left := rs.Left + 5;
  rss.Top := rs.Top + 5;
  TMSFMXNativeMacPDFLib1.FillColor := TAlphaColorRec.Seagreen;
  TMSFMXNativeMacPDFLib1.StrokeColor := TAlphaColorRec.Darkgreen;
  TMSFMXNativeMacPDFLib1.DrawRoundedRectangle(rss, 10);

  TMSFMXNativeMacPDFLib1.EndDocument;
  TMSFMXNativeWebView1.LoadFile(fn);
end;

end.
