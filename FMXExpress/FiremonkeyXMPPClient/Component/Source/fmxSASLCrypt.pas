unit fmxSASLCrypt;

interface

uses classes, sysutils,IdCoderMIME, IdGlobal,IdHashMessageDigest, strutils, system.IOUtils;

//Decode jabber SASL message for authentification
function GetSASLResponse(AStr: string; AUsername,APassword,AServer : string): string;


implementation

// convert string to commatext
procedure parseNameValues(list: TStringlist; str: String);
begin
  str := AnsiReplaceStr(str,'"','');
  str := AnsiReplaceStr(str,',','","');
  str := '"'+str+'"';
  list.CommaText := str;
end;


  //Get SASL RESPONSE (as described in the jabber protocole)
function GetSASLResponse(AStr: string; AUsername,APassword,AServer : string): string;
var
  _hasher: TIdHashMessageDigest5;
  _decoder: TIdDecoderMime;
  _encoder: TIdEncoderMime;
  _nc: integer;
  _realm: string;
  _nonce: string;
  _cnonce: string;
  // -------
  azjid: string;
  resp, pass, serv, uname, uri, az, dig, a2, p1, p2, e, c: string;
  a1: String;
  pairs: TStringlist;
  a1s: TMemoryStream;
  tmpstr: string;
  tmp: TIdBytes;
  wa1Bytes : TIdBytes;
  i : integer;
begin
  uname := AUserName;
  serv := AServer;
  pass := APassword;

  _decoder := TIdDecoderMIME.Create(nil);
  c := _decoder.DecodeString(AStr);
  freeandnil(_decoder);

  pairs := TStringlist.Create();
  parseNameValues(pairs, c);
  _nc := 1;
  _realm := pairs.Values['realm'];
  _nonce := pairs.Values['nonce'];
  tmpstr := copy(c, 1, 7);
  if tmpstr = 'rspauth' then begin
    Result := '';
    Exit
  end;

  _realm := serv;
  // Start the insanity.............
  e := '1234567890123456789012345678930';
  _encoder := TIdEncoderMIME.Create(nil);
  e := _encoder.Encode(e);
  _hasher := TIdHashMessageDigest5.Create;
  _cnonce := Lowercase(_hasher.HashStringAsHex(e));

  azjid := uname + '@' + serv;
  uri := 'xmpp/' + serv;

  resp := 'username="' + uname + '",';
  resp := resp + 'realm="' + _realm + '",';
  resp := resp + 'nonce="' + _nonce + '",';
  resp := resp + 'cnonce="' + _cnonce + '",';
  resp := resp + 'nc=' + Format('%8.8d', [_nc]) + ',';
  resp := resp + 'qop=auth,';
  resp := resp + 'digest-uri="' + uri + '",';
  resp := resp + 'charset=utf-8,';

  // actually calc the response...
  e := uname + ':' + _realm + ':' + pass;

  tmp := _hasher.HashString(e);
  // NB: H(A1) is just 16 bytes, not HEX(H(A1))
  a1s := TMemoryStream.Create();
  a1s.Write(tmp[0], 16);

  if (az <> '') then
      a1 := ':' + _nonce + ':' + _cnonce + ':' + az
  else
      a1 := ':' + _nonce + ':' + _cnonce;
//  a1 := tmp + a1;
  SetLength(wa1Bytes, Length(a1));
  for i := 0 to Length(wa1Bytes) -1 do
  begin
    {$ifdef WIN32}
    wa1Bytes[i] := Ord(a1[i+1]);
    {$ELSE}
    wa1Bytes[i] := Ord(a1[i]);
    {$ENDIF}
  end;

  a1s.Write(Pointer(wa1Bytes)^, Length(wa1Bytes));
  a1s.Seek(0, soFromBeginning);
//  ha1 := _hasher.HashValue(a1s);

   a1s.SaveToFile(TPath.Combine(TPath.GetSharedDownloadsPath , 'test.bin'));

  a2 := 'AUTHENTICATE:' + uri;
  p1 := Lowercase(_hasher.HashStreamAsHex(a1s));
  FreeAndNil(a1s);
  p2 := Lowercase(_hasher.HashStringAsHex(a2));

  e := p1 + ':' + _nonce + ':' + Format('%8.8d', [_nc]) + ':' + _cnonce + ':auth:' +
       p2;
  dig := Lowercase(_hasher.HashStringAsHex(e));

  if (az <> '') then
      resp := resp + 'authzid="' + az + '",';
  resp := resp + 'response=' + dig;

  Result := _encoder.Encode(resp);
  FreeAndNil(_encoder);
  FreeAndNil(_hasher);
end;

end.

