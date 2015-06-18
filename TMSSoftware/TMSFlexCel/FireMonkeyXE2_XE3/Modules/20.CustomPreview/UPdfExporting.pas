unit UPdfExporting;

interface
uses UProgressThread, SysUtils, FMX.FlexCel.Core, FlexCel.Render, FlexCel.Pdf;

type
  TPdfThread = class(TProgressThread)
  private
    procedure ShowProgress(const sender: TObject; const e: TPageEventArgs);
  protected
    procedure Execute; override;
  end;

implementation
uses Classes, IOUtils;

{ TPdfThread }

procedure TPdfThread.Execute;
var
  pdf: TFlexCelPdfExport;
  fs: TFileStream;
begin
  pdf := TFlexCelPdfExport.Create(Xls, true);
  try
    pdf.AfterGeneratePage := ShowProgress;

    if AllSheets then
    begin
      fs := TFileStream.Create(FileName, fmCreate);
      try
        pdf.BeginExport(fs);
        pdf.PageLayout := TPageLayout.Outlines;
        pdf.ExportAllVisibleSheets(false, TPath.GetFileNameWithoutExtension(FileName));
        pdf.EndExport;
      finally
        FreeAndNil(fs);
      end;
    end else
    begin
      pdf.Export(FileName);
    end;
  finally
    FreeAndNil(pdf);
  end;
end;

procedure TPdfThread.ShowProgress(const sender: TObject; const e: TPageEventArgs);
var
  Prog: TFlexCelPdfExportProgress;
  Percent: Integer;
begin
  Prog := (Sender as TFlexCelPdfExport).Progress;
  if (Prog.TotalPage = 0) then Percent := 100 else Percent := Round(Prog.Page * 100.0 / Prog.TotalPage);

  Synchronize(
  procedure
  begin
    ProgressFeedback(Percent, 'Page ' + IntToStr(Prog.Page) + ' of ' + IntToStr(Prog.TotalPage));
  end);
end;

end.
