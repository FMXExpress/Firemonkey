unit uAsynchLoad;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  REST.Client, REST.Types,
  FMX.TMSBitmapContainer, FMX.Graphics;

type
    TUpdateProc = procedure (bitmap : TBitmap; url : string) of object;
      AsynchLoad = class(TThread)
      protected
        procedure Execute; override;
      private
        AURl : string;
        RESTResponseImage: TRESTResponse;
        RESTRequestImage: TRESTRequest;
        RESTClientImage: TRESTClient;
        FUpdateProc : TUpdateProc;
      public
        constructor Create(url : string; updateProc : TUpdateProc = nil);
      end;

implementation
uses
    formMain;
{ AsynchLoad }
{******************************************************************************}
constructor AsynchLoad.Create(url: string; updateProc: TUpdateProc);
begin
  inherited Create(true);
    if url.IsEmpty or (not Assigned(updateProc)) then
        Terminate;
    AURl := url;
    FUpdateProc := updateProc;
    RESTResponseImage := TRESTResponse.Create(nil);
    RESTRequestImage := TRESTRequest.Create(nil);
    RESTClientImage := TRESTClient.Create(nil);
    RESTClientImage.BaseURL := AURl;
    RESTRequestImage.Client := RESTClientImage;
    RESTRequestImage.Response := RESTResponseImage;
end;
{******************************************************************************}
procedure AsynchLoad.Execute;
var
    Image : TBitmap;
    mStream : TMemoryStream;
begin
    NameThreadForDebugging('AsynchLoad');
    try
        fRSS.Semafore.Acquire;
        repeat
            RESTRequestImage.Execute;
            Image := TBitmap.Create;
            mStream := TMemoryStream.Create;
            try
                mStream.WriteData(RESTResponseImage.RawBytes,
                    Length(RESTResponseImage.RawBytes));

                mStream.Seek(0,0);
                Image.LoadFromStream(mStream);
                Synchronize
                (
                    procedure
                    begin
                        FUpdateProc(Image, AURl);
                    end
                );
            finally
                Terminate;
                mStream.Free;
                RESTResponseImage.FreeOnRelease;
                RESTRequestImage.FreeOnRelease;
                RESTClientImage.FreeOnRelease;
            end;

        until Terminated;
    finally
        fRSS.Semafore.Release;
    end;
end;
{******************************************************************************}
end.
