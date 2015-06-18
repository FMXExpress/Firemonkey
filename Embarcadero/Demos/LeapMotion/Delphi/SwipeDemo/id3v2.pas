{$mode objfpc}
{$H+}
unit ID3v2;

interface

uses
   Classes, SysUtils;

const
   TAG_VERSION_2_3 = 3;                               { Code for
ID3v2.3.0 tag }

type
   { Class TID3v2 }
   TID3v2 = class(TObject)
     private
       { Private declarations }
       FExists: Boolean;
       FVersionID: Byte;
       FSize: Integer;
       FTitle: string;
       FArtist: string;
       FAlbum: string;
       FTrack: Byte;
       FYear: string;
       FGenre: string;
       FComment: string;
     public
       { Public declarations }
       constructor Create;                                     { Create
object }
       procedure ResetData;                                   { Reset
all data }
       function ReadFromFile(const FileName: string): Boolean;      {
Load tag }
       property Exists: Boolean read FExists;              { True if tag
found }
       property VersionID: Byte read FVersionID;                {
Version code }
       property Size: Integer read FSize;                     { Total
tag size }
       property Title: string read FTitle;                        { Song
title }
       property Artist: string read FArtist;                     {
Artist name }
       property Album: string read FAlbum;                        {
Album name }
       property Track: Byte read FTrack;                        { Track
number }
       property Year: string read FYear;
{ Year }
       property Genre: string read FGenre;                        {
Genre name }
       property Comment: string read FComment;                       {
Comment }
   end;

implementation

const
   { Max. number of supported tag frames }
   ID3V2_FRAME_COUNT = 7;

   { Names of supported tag frames }
   ID3V2_FRAME: array [1..ID3V2_FRAME_COUNT] of string =
     ('TIT2', 'TPE1', 'TALB', 'TRCK', 'TYER', 'TCON', 'COMM');

type
   { ID3v2 frame header }
   FrameHeader = record
     ID: array [1..4] of AnsiChar;                                      {
Frame ID }
     Size: Integer;                                    { Size excluding
header }
     Flags: Word;                                                      {
Flags }
   end;

   { ID3v2 header data - for internal use }
   TagInfo = record
     { Real structure of ID3v2 header }
     ID: array [1..3] of AnsiChar;                                  { Always
"ID3" }
     Version: Byte;                                           { Version
number }
     Revision: Byte;                                         { Revision
number }
     Flags: Byte;                                               { Flags
of tag }
     Size: array [1..4] of Byte;                   { Tag size excluding
header }
     { Extended data }
     FileSize: Integer;                                    { File size
(bytes) }
     Frame: array [1..ID3V2_FRAME_COUNT] of string;  { Information from
frames }
   end;

{ ********************* Auxiliary functions & procedures
******************** }

function ReadHeader(const FileName: string; var Tag: TagInfo): Boolean;
var
   SourceFile: file;
   Transferred: Integer;
begin
   try
     Result := true;
     { Set read-access and open file }
     AssignFile(SourceFile, FileName);
     FileMode := 0;
     Reset(SourceFile, 1);
     { Read header and get file size }
     BlockRead(SourceFile, Tag, 10, Transferred);
     Tag.FileSize := FileSize(SourceFile);
     CloseFile(SourceFile);
     { if transfer is not complete }
     if Transferred < 10 then Result := false;
   except
     { Error }
     Result := false;
   end;
end;

{
---------------------------------------------------------------------------

}

function GetVersionID(const Tag: TagInfo): Byte;
begin
   { Get tag version from header }
   Result := Tag.Version;
end;

{
---------------------------------------------------------------------------

}

function GetTagSize(const Tag: TagInfo): Integer;
begin
   { Get total tag size }
   Result :=
     Tag.Size[1] * $200000 +
     Tag.Size[2] * $4000 +
     Tag.Size[3] * $80 +
     Tag.Size[4] + 10;
   if Result > Tag.FileSize then Result := 0;
end;

{
---------------------------------------------------------------------------

}

procedure SetTagItem(const ID, Data: string; var Tag: TagInfo);
var
   Iterator: Byte;
begin
   { Set tag item if supported frame found }
   for Iterator := 1 to ID3V2_FRAME_COUNT do
     if ID3V2_FRAME[Iterator] = ID then Tag.Frame[Iterator] := Data;
end;

{
---------------------------------------------------------------------------

}

function Swap32(const Figure: Integer): Integer;
var
   ByteArray: array [1..4] of Byte absolute Figure;
begin
   { Swap 4 bytes }
   Result :=
     ByteArray[1] * $100000000 +
     ByteArray[2] * $10000 +
     ByteArray[3] * $100 +
     ByteArray[4];
end;

{
---------------------------------------------------------------------------

}

procedure ReadFrames(const FileName: string; var Tag: TagInfo);
var
   SourceFile: file;
   Frame: FrameHeader;
   Data: array [1..250] of AnsiChar;
   DataPosition: Integer;
begin
   try
     { Set read-access, open file }
     AssignFile(SourceFile, FileName);
     FileMode := 0;
     Reset(SourceFile, 1);
     Seek(SourceFile, 10);
     while (FilePos(SourceFile) < GetTagSize(Tag)) and (not
EOF(SourceFile)) do
     begin
       FillChar(Data, SizeOf(Data), 0);
       { Read frame header }
       BlockRead(SourceFile, Frame, 10);
       DataPosition := FilePos(SourceFile);
       { Read frame data and set tag item if frame supported }
       BlockRead(SourceFile, Data, Swap32(Frame.Size) mod SizeOf(Data));
       SetTagItem(Frame.ID, Data, Tag);
       Seek(SourceFile, DataPosition + Swap32(Frame.Size));
     end;
     CloseFile(SourceFile);
   except
   end;
end;

{
---------------------------------------------------------------------------

}

function GetTrack(const TrackString: string): Byte;
var
   Index, Value, Code: Integer;
begin
   { Extract track from string }
   Index := Pos('/', TrackString);
   if Index = 0 then Val(Trim(TrackString), Value, Code)
   else Val(Copy(Trim(TrackString), 1, Index), Value, Code);
   if Code = 0 then Result := Value
   else Result := 0;
end;

{
---------------------------------------------------------------------------

}

function GetGenre(const GenreString: string): string;
begin
   { Extract genre from string }
   Result := Trim(GenreString);
   if Pos(')', Result) > 0 then Delete(Result, 1, LastDelimiter(')',
Result));
end;

{ ********************** Public functions & procedures
********************** }

constructor TID3v2.Create;
begin
   inherited;
   ResetData;
end;

{
---------------------------------------------------------------------------

}

procedure TID3v2.ResetData;
begin
   FExists := false;
   FVersionID := 0;
   FSize := 0;
   FTitle := '';
   FArtist := '';
   FAlbum := '';
   FTrack := 0;
   FYear := '';
   FGenre := '';
   FComment := '';
end;

{
---------------------------------------------------------------------------

}

function TID3v2.ReadFromFile(const FileName: string): Boolean;
var
   Tag: TagInfo;
begin
   { Reset data and load header from file to variable }
   ResetData;
   Result := ReadHeader(FileName, Tag);
   { Process data if loaded and header valid }
   if (Result) and (Tag.ID = 'ID3') then
   begin
     FExists := true;
     { Fill properties with header data }
     FVersionID := GetVersionID(Tag);
     FSize := GetTagSize(Tag);
     { Get information from frames if version supported }
     if (FVersionID = TAG_VERSION_2_3) and (FSize > 0) then
     begin
       ReadFrames(FileName, Tag);
       { Fill properties with data from frames }
       FTitle :=  Trim(Tag.Frame[1]);
       FArtist := Trim(Tag.Frame[2]);
       FAlbum := Trim(Tag.Frame[3]);
       FTrack := GetTrack(Tag.Frame[4]);
       FYear := Trim(Tag.Frame[5]);
       FGenre := GetGenre(Tag.Frame[6]);
       FComment := Trim(Copy(Tag.Frame[7], 5, Length(Tag.Frame[7]) - 4));
     end;
   end;
end;

end.

