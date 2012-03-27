unit Common;

interface

uses
  Classes, SysUtils, Windows, Graphics, Forms;

const
  GDefaultCaption = '공디스크 업로더';
  GDefaultServerURL = 'http://www.gongdisk.net';
  GGongDiskWebPage = 'http://www.gongdisk.net';
  GUploadImageFolder = 'image';
  GTimeOut = 15000;

var
  GSaveDirectory: string = '';
  GSaveDirectorySetting: Boolean = False;

function MD5(const FileName: string): string;
function CreateBitmapRgn(Bitmap: TBitmap): HRGN;
function HTTPGet(const AURL: string): string;
procedure ParseURI(AURI: string; var VHost, VPort, VUserName, VPassword, VPath,
  VDoc: string);
function GetMyDocumentsPath: string;
function FileSizeToString(ASize: Int64): string;
function ShutDown: Boolean;
procedure Delay(dwMilliseconds: Longint);
function GetVersionInfo(AFilename: string): string;
function ExtractLongPathName(const ShortName: string): string;
function GetFileKind(const AFileName: string): string;

implementation

uses
  IdHashMessageDigest, IdURI, IdHTTP, ShlObj, ShellApi, StrUtils;

function MD5(const FileName: string): string;
var
  MD5: TIdHashMessageDigest5;
  LStream: TFileStream;
begin
  MD5 := TIdHashMessageDigest5.Create;
  LStream := TFileStream.Create(FileName, fmOpenRead OR fmShareDenyWrite);
  try
    Result := MD5.HashStreamAsHex(LStream);
  finally
    FreeAndNil(LStream);
    FreeAndNil(MD5);
  end;
end;

const
  DEFAULTRECTCOUNT = 50;

function CreateBitmapRgn(Bitmap: TBitmap): HRGN;
type
  TPixels08 = array [0 .. 0] of Byte;
  TPixels16 = array [0 .. 0] of Word;
  TPixels24 = array [0 .. 0] of TRGBTriple;
  TPixels32 = array [0 .. 0] of Cardinal;
  TIsTransparent = function(X: Integer): Boolean;
var
  PixelFormat: TPixelFormat;
  Pixels: Pointer;
  TransparentPixel: Pointer;
  RegionData: PRgnData;
  RegionBufferSize: Integer;
  RectCount, NewRectLeft: Integer;
  X, Y: Integer;
  procedure GetTransparentPixel;
  begin
    GetMem(TransparentPixel, 4);
    case PixelFormat of
      pf8Bit:
        PByte(TransparentPixel)^ := TPixels08(Pixels^)[0];
      pf16Bit:
        PWord(TransparentPixel)^ := TPixels16(Pixels^)[0];
      pf24Bit:
        PRGBTriple(TransparentPixel)^ := TPixels24(Pixels^)[0];
      pf32Bit:
        PCardinal(TransparentPixel)^ := TPixels32(Pixels^)[0];
    end;
  end;
  function IsTransparent(X: Integer): Boolean;
  begin
    case PixelFormat of
      pf8Bit:
        Result := TPixels08(Pixels^)[X] = PByte(TransparentPixel)^;
      pf16Bit:
        Result := TPixels16(Pixels^)[X] = PWord(TransparentPixel)^;
      pf24Bit:
        Result := (TPixels24(Pixels^)[X].rgbtRed = PRGBTriple(TransparentPixel)
          ^.rgbtRed) and
          (TPixels24(Pixels^)[X].rgbtGreen = PRGBTriple(TransparentPixel)
          ^.rgbtGreen) and
          (TPixels24(Pixels^)[X].rgbtBlue = PRGBTriple(TransparentPixel)
          ^.rgbtBlue);
      pf32Bit:
        Result := TPixels32(Pixels^)[X] = PCardinal(TransparentPixel)^;
    else
      Result := False;
    end;
  end;
  procedure AddRect;
  type
    PRectBuffer = ^TRectBuffer;
    TRectBuffer = array [0 .. 0] of TRect;
  begin
    if (RegionBufferSize div SizeOf(TRect)) = RectCount then
    begin
      Inc(RegionBufferSize, SizeOf(TRect) * DEFAULTRECTCOUNT);
      ReallocMem(RegionData, SizeOf(TRgnDataHeader) + RegionBufferSize + 3);
    end;

    PRectBuffer(@RegionData^.Buffer)^[RectCount].Left := NewRectLeft;
    PRectBuffer(@RegionData^.Buffer)^[RectCount].Top := Y;
    PRectBuffer(@RegionData^.Buffer)^[RectCount].Right := X;
    PRectBuffer(@RegionData^.Buffer)^[RectCount].Bottom := Y + 1;

    Inc(RectCount);
    NewRectLeft := -1;
  end;

begin
  PixelFormat := Bitmap.PixelFormat;
  Pixels := Bitmap.ScanLine[0];
  GetTransparentPixel;

  RectCount := 0;
  RegionBufferSize := SizeOf(TRect) * DEFAULTRECTCOUNT;
  GetMem(RegionData, SizeOf(TRgnDataHeader) + RegionBufferSize + 3);
  try

    for Y := 0 to Bitmap.Height - 1 do
    begin
      Pixels := Bitmap.ScanLine[Y];
      NewRectLeft := -1;
      for X := 0 to Bitmap.Width - 1 do
        if IsTransparent(X) then
        begin
          if NewRectLeft >= 0 then
            AddRect;
        end
        else
        begin
          if NewRectLeft = -1 then
            NewRectLeft := X;
          if X = Bitmap.Width - 1 then
            AddRect;
        end;
    end;

    RegionData^.rdh.dwSize := SizeOf(TRgnDataHeader);
    RegionData^.rdh.iType := RDH_RECTANGLES;
    RegionData^.rdh.nCount := RectCount;
    RegionData^.rdh.nRgnSize := RectCount * SizeOf(TRect);
    Result := ExtCreateRegion(nil, RegionData^.rdh.dwSize +
      RegionData^.rdh.nRgnSize, RegionData^);

  finally
    FreeMem(RegionData);
    FreeMem(TransparentPixel);
  end;
end;

function HTTPGet(const AURL: string): string;
var
  IdHTTP: TIdHTTP;
  LStream: TBytesStream;
begin
  Result:='';
  LStream := TBytesStream.Create;
  IdHTTP := TIdHTTP.Create(nil);
  try
    IdHTTP.ConnectTimeout := GTimeOut;
    IdHTTP.ReadTimeout := GTimeOut;
    IdHTTP.Get(AURL, LStream, []);
    Result:=TEncoding.UTF8.GetString(LStream.Bytes, 0, LStream.Size);
  finally
    FreeAndNil(IdHTTP);
    FreeAndNil(LStream);
  end;
end;

procedure ParseURI(AURI: string; var VHost, VPort, VUserName, VPassword, VPath,
  VDoc: string);
var
  URI: TIdURI;
begin
  URI := TIdURI.Create(AURI);
  try
    VHost := URI.Host;
    VPort := URI.Port;
    VUserName := URI.Username;
    VPassword := URI.Password;
    VPath := URI.Path;
    VDoc := URI.Document;
  finally
    FreeAndNil(URI);
  end;
end;

function GetMyDocumentsPath: string;
var
  Path: array[0..MAX_PATH + 1] of Char;
begin
  if SHGetSpecialFolderPath(0, Path, CSIDL_PERSONAL, False) then
    Result := Path
  else Result := '';
end;

function FileSizeToString(ASize: Int64): string;
begin
  if ASize >= 1024*1024*1024 then
    Result := FloatToStrF(ASize / (1024*1024*1024), ffFixed, 6, 2) + 'GB'
  else if ASize >= 1024*1024 then
    Result := FloatToStrF(ASize / (1024*1024), ffFixed, 6, 1) + 'MB'
  else if ASize >= 1024 then
    Result := FloatToStrF(ASize / (1024), ffFixed, 6, 0) + 'KB'
  else
    Result := Format('%d Bytes', [ASize]);
end;

function ShutDown: Boolean;
var
  TTokenHd: THandle;
  TTokenPvg: TTokenPrivileges;
  cbtpPrevious: DWORD;
  rTTokenPvg: TTokenPrivileges;
  pcbtpPreviousRequired: DWORD;
  tpResult: Boolean;
const
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    tpResult := OpenProcessToken(GetCurrentProcess(),
      TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, TTokenHd);
    if tpResult then
    begin
      tpResult := LookupPrivilegeValue(nil, SE_SHUTDOWN_NAME,
        TTokenPvg.Privileges[0].Luid);
      TTokenPvg.PrivilegeCount := 1;
      TTokenPvg.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      cbtpPrevious := SizeOf(rTTokenPvg);
      pcbtpPreviousRequired := 0;
      if tpResult then
        Windows.AdjustTokenPrivileges(TTokenHd, False, TTokenPvg, cbtpPrevious,
          rTTokenPvg, pcbtpPreviousRequired);
    end;
  end;
  Result := ExitWindowsEx(EWX_POWEROFF or EWX_FORCE, 0);
end;

procedure Delay(dwMilliseconds: Longint);
var
  iStart, iStop: DWORD;
begin
  iStart := GetTickCount;
  repeat
    iStop := GetTickCount;
    Application.ProcessMessages;
    Sleep(1);
  until (iStop - iStart) >= dwMilliseconds;
end;

function GetVersionInfo(AFilename: string): string;
var
  InfoSize: DWORD;
  Wnd: DWORD;
  VerBuf: Pointer;
  VerSize: DWORD;
  FI: PVSFixedFileInfo;
  LMajor, LMinor, LRelease, LBuild: Integer;
begin
  Result := '';
  if (AFilename = '') or (not FileExists(AFilename)) then Exit;
  InfoSize := GetFileVersionInfoSize(PChar(AFilename), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(AFilename), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          LMajor := HiWord(FI^.dwFileVersionMS);
          LMinor := LoWord(FI^.dwFileVersionMS);
          LRelease := HiWord(FI^.dwFileVersionLS);
          LBuild := LoWord(FI^.dwFileVersionLS);
          Result := IntToStr(LMajor) + '.' +
                    IntToStr(LMinor) + '.' +
                    IntToStr(LRelease) + '.' +
                    IntToStr(LBuild);
        end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

function GetLongPathName(ShortPathName: PChar; LongPathName: PChar;
  cchBuffer: Integer): Integer; stdcall; external kernel32 name 'GetLongPathNameW';

function ExtractLongPathName(const ShortName: string): string;
begin
  SetLength(Result, GetLongPathName(PChar(ShortName), nil, 0));
  SetLength(Result, GetLongPathName(PChar(ShortName), PChar(Result), Length(Result)));
end;

function GetFileKind(const AFileName: string): string;
var
  S: string;
begin
  S := LowerCase(ExtractFileExt(AFileName));
  if MatchText(S, ['.htm', '.html', '.log', '.doc', '.txt', '.xls', '.csv',
    '.rtf', '.asp', '.pdf', '.fdf', '.ppt', '.dwg', '.msg', '.xml', '.sdxl',
    '.xdp']) then
      Result := '문서'
  else if MatchText(S, ['.tiff', '.tif', '.gif', '.jpg', '.jpeg', '.bmp']) then
    Result := '이미지'
  else if MatchText(S, ['.3gp', '.asf', '.avi', '.flv', '.mp4', '.mov', '.mkv',
    '.mpg', '.mpeg', '.ogm', '.swf', '.rm', '.ts', '.vob', '.wmv']) then
      Result := '동영상'
  else if MatchText(S, ['.smi', '.srt', '.idx', '.sub']) then
    Result := '자막'
  else if MatchText(S, ['.wav', '.mp3', '.ogg', '.flac']) then
    Result := '음악'
  else
    Result := '파일';
end;

end.
