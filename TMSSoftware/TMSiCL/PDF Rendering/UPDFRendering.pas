unit UPDFRendering;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TMSNativeUIBaseControl, FMX.TMSNativeUIWebView, FMX.TMSNativeUICore,
  FMX.TMSNativeGraphicsLib, FMX.TMSNativePDFLib, FMX.TMSNativeRichTextLib;

type
  TForm1147 = class(TForm)
    TMSFMXNativeUIWebView1: TTMSFMXNativeUIWebView;
    TMSFMXNativePDFLib1: TTMSFMXNativePDFLib;
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

  fn := ExtractFilePath(ParamStr(0)).Replace('PDFRendering.app/', '') + 'Documents/sample.pdf';
  r := TMSFMXNativePDFLib1.MediaBox;
  InflateRect(r, -50, -50);

  TMSFMXNativePDFLib1.Author := 'Your Company';
  TMSFMXNativePDFLib1.Creator := 'Your Name';
  TMSFMXNativePDFLib1.Title := 'PDF Rendering Library Sample';
  TMSFMXNativePDFLib1.Subject := 'Demonstration of the PDF Rendering Library';
  TMSFMXNativePDFLib1.Keywords.Add('PDF Rendering Library');
  TMSFMXNativePDFLib1.Keywords.Add('iOS');
  TMSFMXNativePDFLib1.Keywords.Add('Lorem Ipsum');

  TMSFMXNativePDFLib1.BeginDocument(fn);
  TMSFMXNativePDFLib1.Header := 'PDF Rendering Library';
  TMSFMXNativePDFLib1.Footer := 'Page 1 / 2';
  TMSFMXNativePDFLib1.NewPage;

  TMSFMXNativePDFLib1.RichText.Text := ' Text flow in multiple columns'#13#10;
  TMSFMXNativePDFLib1.RichText.SetFontSize(14);
  TMSFMXNativePDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Steelblue);
  TMSFMXNativePDFLib1.RichText.SetForegroundColor(TAlphaColorRec.White);
  rs := TMSFMXNativePDFLib1.DrawRichText(RectF(R.Left, R.Top, r.Right, r.Top + 20));
  TMSFMXNativePDFLib1.FillColor := TAlphaColorRec.Black;
  if isIOS7 then
    TMSFMXNativePDFLib1.DrawText(lorem, RectF(r.Left, R.Top + rs.Height, r.Right, R.Top + rs.Height + 250), 3)
  else
    TMSFMXNativePDFLib1.DrawText(lorem, RectF(r.Left, R.Top + rs.Height, r.Right, R.Top + rs.Height + 250));

  TMSFMXNativePDFLib1.FillColor := TAlphaColorRec.Steelblue;
  TMSFMXNativePDFLib1.RichText.Text := ' Rich text'#13#10;
  TMSFMXNativePDFLib1.RichText.SetFontSize(14);
  TMSFMXNativePDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Steelblue);
  TMSFMXNativePDFLib1.RichText.SetForegroundColor(TAlphaColorRec.White);
  rs := TMSFMXNativePDFLib1.DrawRichText(RectF(R.Left, R.Top + rs.Height + 270, r.Right, R.Top + rs.Height + 310));
  ostr := 'Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry''s standard dummy text ever since.';
  TMSFMXNativePDFLib1.RichText.Text := ostr;
  str := 'dummy';
  TMSFMXNativePDFLib1.RichText.SetFontSize(28, Pos(str, ostr) - 1, Length(str));
  str := 'typesetting';
  TMSFMXNativePDFLib1.RichText.SetForegroundColor(TAlphaColorRec.Red, Pos(str, ostr) - 1, Length(str));
  str := 'industry''s';
  TMSFMXNativePDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Yellowgreen, Pos(str, ostr) - 1, Length(str));
  TMSFMXNativePDFLib1.RichText.SetForegroundColor(TAlphaColorRec.Darkgreen, Pos(str, ostr) - 1, Length(str));
  str := 'standard dummy';
  TMSFMXNativePDFLib1.RichText.SetFont('Cochin', 18, Pos(str, ostr) - 1, Length(str));
  TMSFMXNativePDFLib1.RichText.SetBold(True, Pos(str, ostr) - 1, Length(str));
  TMSFMXNativePDFLib1.RichText.SetItalic(True, Pos(str, ostr) - 1, Length(str));
  rs := TMSFMXNativePDFLib1.DrawRichText(RectF(rs.Left, rs.Bottom, rs.Left + 300, rs.Bottom + 300));

  TMSFMXNativePDFLib1.RichText.Text := ' Text shadow with color and blur'#13#10;
  TMSFMXNativePDFLib1.RichText.SetFontSize(14);
  TMSFMXNativePDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Steelblue);
  TMSFMXNativePDFLib1.RichText.SetForegroundColor(TAlphaColorRec.White);
  rs := TMSFMXNativePDFLib1.DrawRichText(RectF(R.Left, rs.Bottom + 10, r.Right, rs.Bottom + 30));

  TMSFMXNativePDFLib1.DrawSaveState;
  TMSFMXNativePDFLib1.DrawAddShadow(PointF(5, 5), 3);
  TMSFMXNativePDFLib1.Font.Size := 28;
  TMSFMXNativePDFLib1.DrawText('Hello World', PointF(rs.Left + 30, rs.Bottom + 10));
  TMSFMXNativePDFLib1.DrawAddShadow(PointF(5, 5), 0, TAlphaColorRec.Red);
  rs := TMSFMXNativePDFLib1.DrawText('Hello World', PointF(rs.Left + 300, rs.Bottom + 10));
  TMSFMXNativePDFLib1.DrawRestoreState;
  TMSFMXNativePDFLib1.Font.Size := 14;

  TMSFMXNativePDFLib1.RichText.Text := ' Combinations'#13#10;
  TMSFMXNativePDFLib1.RichText.SetFontSize(14);
  TMSFMXNativePDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Steelblue);
  TMSFMXNativePDFLib1.RichText.SetForegroundColor(TAlphaColorRec.White);
  rs := TMSFMXNativePDFLib1.DrawRichText(RectF(R.Left, rs.Bottom + 10, r.Right, rs.Bottom + 30));

  TMSFMXNativePDFLib1.DrawSaveState;
  TMSFMXNativePDFLib1.DrawAddShadow(PointF(5, 5), 3, TAlphaColorRec.Silver);
  TMSFMXNativePDFLib1.RichText.Text := lorem;
  TMSFMXNativePDFLib1.RichText.SetFontSize(10);
  str := 'standard dummy';
  TMSFMXNativePDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Yellow, Pos(str, lorem) - 1, Length(str));
  str := 'electronic';
  TMSFMXNativePDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Yellow, Pos(str, lorem) - 1, Length(str));
  str := 'Aldus PageMaker';
  TMSFMXNativePDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Yellow, Pos(str, lorem) - 1, Length(str));
  str := 'established fact';
  TMSFMXNativePDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Yellow, Pos(str, lorem) - 1, Length(str));
  str := 'packages';
  TMSFMXNativePDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Yellow, Pos(str, lorem) - 1, Length(str));

  str := 'Lorem Ipsum';
  if isIOS7 then
  begin
    TMSFMXNativePDFLib1.RichText.SetStrikethrough([usUnderlineStyleSingle], Pos(str, lorem) - 1, Length(str));
    TMSFMXNativePDFLib1.RichText.SetStrikethroughColor(TAlphaColorRec.Blue, Pos(str, lorem) - 1, Length(str));
  end;
  TMSFMXNativePDFLib1.RichText.SetBold(True, Pos(str, lorem) - 1, Length(str));

  str := 'Various';
  if isIOS7 then
  begin
    TMSFMXNativePDFLib1.RichText.SetStrikethrough([usUnderlineStyleSingle], Pos(str, lorem) - 1, Length(str));
    TMSFMXNativePDFLib1.RichText.SetStrikethroughColor(TAlphaColorRec.Blue, Pos(str, lorem) - 1, Length(str));
  end;
  TMSFMXNativePDFLib1.RichText.SetBold(True, Pos(str, lorem) - 1, Length(str));

  str := 'accident';
  TMSFMXNativePDFLib1.RichText.SetForegroundColor(TAlphaColorRec.Red, Pos(str, lorem) - 1, Length(str));
  TMSFMXNativePDFLib1.RichText.SetFontSize(20, Pos(str, lorem) - 1, Length(str));
  TMSFMXNativePDFLib1.RichText.SetBold(True, Pos(str, lorem) - 1, Length(str));

  if isIOS7 then
    TMSFMXNativePDFLib1.DrawRichText(RectF(r.Left, rs.Bottom + 10, r.Right, rs.Bottom + 200), 4)
  else
    TMSFMXNativePDFLib1.DrawRichText(RectF(r.Left, rs.Bottom + 10, r.Right, rs.Bottom + 200));

  TMSFMXNativePDFLib1.DrawRestoreState;

  TMSFMXNativePDFLib1.RichText.SetFontSize(14);

  TMSFMXNativePDFLib1.Footer := 'Page 2 / 2';
  TMSFMXNativePDFLib1.Orientation := poLandscape;
  TMSFMXNativePDFLib1.NewPage;
  r := TMSFMXNativePDFLib1.MediaBox;
  InflateRect(r, -50, -50);

  TMSFMXNativePDFLib1.RichText.Text := ' Image drawing and quality control'#13#10;
  TMSFMXNativePDFLib1.RichText.SetFontSize(14);
  TMSFMXNativePDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Steelblue);
  TMSFMXNativePDFLib1.RichText.SetForegroundColor(TAlphaColorRec.White);
  rs := TMSFMXNativePDFLib1.DrawRichText(RectF(R.Left, R.Top, r.Right, R.Top + 20));

  str := ExtractFilePath(ParamStr(0)) + 'family.jpg';
  rs := RectF(rs.Left, rs.Bottom + 10, rs.Left + 150, rs.Bottom + 110);
  TMSFMXNativePDFLib1.DrawImageFromFile(str, rs);
  rs := RectF(rs.Right + 20, rs.Top, rs.Right + 20 + rs.Width, rs.Bottom);
  TMSFMXNativePDFLib1.DrawImageFromFile(str, rs, itJPG, 0.1);
  str := ExtractFilePath(ParamStr(0)) + 'tulips.jpg';
  rs := RectF(r.Left, rs.Bottom + 10, r.Left + 150, rs.Bottom + 110);
  TMSFMXNativePDFLib1.DrawImageFromFile(str, rs);
  rs := RectF(rs.Right + 20, rs.Top, rs.Right + 20 + rs.Width, rs.Bottom);
  TMSFMXNativePDFLib1.DrawImageFromFile(str, rs, itJPG, 0.25);

  TMSFMXNativePDFLib1.RichText.Text := ' Graphics'#13#10;
  TMSFMXNativePDFLib1.RichText.SetFontSize(14);
  TMSFMXNativePDFLib1.RichText.SetBackgroundColor(TAlphaColorRec.Steelblue);
  TMSFMXNativePDFLib1.RichText.SetForegroundColor(TAlphaColorRec.White);
  rs := TMSFMXNativePDFLib1.DrawRichText(RectF(R.Left, rs.Bottom + 10, r.Right, rs.Bottom + 30));

  rs := RectF(rs.Left, rs.Bottom + 20, rs.Left + 200, rs.Bottom + 150);
  TMSFMXNativePDFLib1.FillColor := TAlphaColorRec.Lightsteelblue;
  TMSFMXNativePDFLib1.StrokeColor := TAlphaColorRec.Steelblue;
  TMSFMXNativePDFLib1.DrawRectangle(rs);

  rss := RectF(rs.Right + 50, rs.Top, rs.Right + 150, rs.Top + 100);
  TMSFMXNativePDFLib1.DrawSaveState;
  TMSFMXNativePDFLib1.FillColor := TAlphaColorRec.Orange;
  TMSFMXNativePDFLib1.StrokeColor := TAlphaColorRec.Null;
  TMSFMXNativePDFLib1.LineWidth := 2;
  TMSFMXNativePDFLib1.DrawPathBegin;
  TMSFMXNativePDFLib1.DrawPathAddEllipse(rss);
  TMSFMXNativePDFLib1.DrawAddShadow(PointF(5, 5), 3);
  TMSFMXNativePDFLib1.DrawPathEnd(dmPathFill);
  TMSFMXNativePDFLib1.DrawRestoreState;
  TMSFMXNativePDFLib1.DrawSaveState;
  TMSFMXNativePDFLib1.StrokeColor := TAlphaColorRec.Darkorange;
  TMSFMXNativePDFLib1.FillColor := TAlphaColorRec.Null;
  TMSFMXNativePDFLib1.DrawEllipse(rss);
  TMSFMXNativePDFLib1.DrawRestoreState;

  TMSFMXNativePDFLib1.DrawSaveState;
  TMSFMXNativePDFLib1.FillColor := TAlphaColorRec.Red;
  TMSFMXNativePDFLib1.FillColorTo := TAlphaColorRec.Blue;
  TMSFMXNativePDFLib1.DrawPathBegin;

  st := PointF(rss.Left + 250, rss.CenterPoint.Y);
  rad := 75;
  angle := 0;
  x := rad * Sin(angle * Pi / 5) + st.X;
  y := rad * Cos(angle * PI / 5) + st.Y;
  TMSFMXNativePDFLib1.DrawPathMoveToPoint(PointF(x, y));

  for I := 1 to 5 do
  begin
    x := rad * sin((i * 4 * pi + angle) / 5) + st.X;
    y := rad * cos((i * 4 * pi + angle) / 5) + st.Y;
    TMSFMXNativePDFLib1.DrawPathAddLineToPoint(PointF(x, y));
  end;

  TMSFMXNativePDFLib1.DrawPathClose;
  TMSFMXNativePDFLib1.DrawPathBeginClip;
  TMSFMXNativePDFLib1.DrawPathEndLinearGradient(PointF(st.X - 100, st.Y - 100), PointF(st.X + 100, st.Y + 100));
  TMSFMXNativePDFLib1.DrawPathEndClip;
  TMSFMXNativePDFLib1.DrawRestoreState;

  rss := rs;
  InflateRect(rss, -25, -25);
  rss.Left := rs.Left + 5;
  rss.Top := rs.Top + 5;
  TMSFMXNativePDFLib1.FillColor := TAlphaColorRec.Seagreen;
  TMSFMXNativePDFLib1.StrokeColor := TAlphaColorRec.Darkgreen;
  TMSFMXNativePDFLib1.DrawRoundedRectangle(rss, 10);

  TMSFMXNativePDFLib1.EndDocument;
  TMSFMXNativeUIWebView1.LoadFile(fn);
end;

end.
