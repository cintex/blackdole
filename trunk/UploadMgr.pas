unit UploadMgr;

interface

uses
  Classes, SysUtils, Forms, IdThreadSafe, IdComponent, IdThread, IdHTTP, IdFTP,
  IdFTPCommon, IdFTPList, IdGlobal, IdAntiFreeze;

type
  TPathType = (ptFile, ptFolder);
  TUploadState = (dsReady, dsUpload, dsError, dsFinished);
  TFileInfo = class
    ArticleID: string;
    UserID: string;
    FileName: string;
    Dir: string;
    Size: Int64;
    FTPURL: string;
    FTPUser: string;
    FTPPass: string;
    FTPPort: string;
    FTPPath: string;
    RelativePath: string;
    Retry: Integer;
    IsRealUp: string;
    State: TUploadState;
    Active: Boolean;
  end;

  TArticleInfo = class
    ArticleID: string;
    UserID: string;
    Active: Boolean;
  end;

  TUploadMgr = class;

  TUploadList = class(TObject)
  private
    FList: TIdThreadSafeList;
    FArticleList: TIdThreadSafeList;
    FOwner: TUploadMgr;
    procedure DoGetArticleList;
    procedure DoGetFileList;
    function GetActiveFileInfo: TFileInfo;
    function GetItemCount: Integer;
    function GetItems(Index: Integer): TFileInfo;
    procedure SetItems(Index: Integer; const Value: TFileInfo);
    function GetActiveArticleInfo: TArticleInfo;
    function GetArticles(Index: Integer): TArticleInfo;
    procedure RequestFileInfo;
    procedure SetArticles(Index: Integer; const Value: TArticleInfo);
    procedure UpFileDone;
  public
    constructor Create(AOwner: TUploadMgr); reintroduce;
    destructor Destroy; override;
    procedure ClearItems;
    procedure ClearArticleList;
    function Add(Item: Pointer): Integer;
    function AddArticle(Item: Pointer): Integer;
    procedure Delete(Index: Integer);
    function IndexOf(AFileName, ADir: string): Integer;
    property Articles[Index: Integer]: TArticleInfo read GetArticles write SetArticles;
    property Items[Index: Integer]: TFileInfo read GetItems write SetItems;
    property ItemCount: Integer read GetItemCount;
    property ActiveFileInfo: TFileInfo read GetActiveFileInfo;
    property ActiveArticleInfo: TArticleInfo read GetActiveArticleInfo;
  end;

  TUploadEvent = procedure(ASender: TObject; APosition: Int64) of object;
  TUploadBeginEvent = procedure(ASender: TObject; AMaximum: Int64) of object;
  TUploadEndEvent = procedure(ASender: TObject) of object;

  TUploadThread = class(TThread)
  private
    FOwner: TUploadMgr;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TUploadMgr); reintroduce;
    destructor Destroy; override;
  end;

  TUploadMgr = class(TComponent)
  private
    FThread: TUploadThread;
    FList: TUploadList;
    FUserID: string;
    FArticleID: string;
    FAverageSpeed: Double;
    FRemainTime: Integer;
    FServerURL: string;
    FStopped: Boolean;
    FIdFTP: TIdFTP;
    FIdAntiFreeze: TIdAntiFreeze;
    FCurrentPosition: Int64;
    FCurrentMaximum: Int64;
    FStartTime: TDateTime;
    FTransferrignData: Boolean;
    FOnUpload: TUploadEvent;
    FOnUploadBegin: TUploadBeginEvent;
    FOnUploadComplete: TNotifyEvent;
    FOnUploadEnd: TUploadEndEvent;
    FOnGetFileList: TNotifyEvent;
    FFTPPath: string;
    FIsRealUp: string;
    FCode: Integer;
    FCodeMsg: string;
    FFTPPort: string;
    FFTPPass: string;
    FFTPURL: string;
    FFTPUser: string;
    FLocalPath: string;
    FPathType: TPathType;
    procedure DoFileUpload;
    procedure DoFileUploadComplete;
    procedure DoWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure DoWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    function GetActiveFileInfo: TFileInfo;
    function GetFiles(Index: Integer): TFileInfo;
    function GetFileCount: Integer;
    function GetTotalSize: Int64;
    function GetTotalPosition: Int64;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteQueue(const Index: Integer);
    procedure RequestInfo;
    procedure Resume;
    procedure Start;
    procedure Stop;
    property ActiveFileInfo: TFileInfo read GetActiveFileInfo;
    property ArticleID: string read FArticleID write FArticleID;
    property AverageSpeed: Double read FAverageSpeed write FAverageSpeed;
    property Code: Integer read FCode write FCode;
    property CodeMsg: string read FCodeMsg write FCodeMsg;
    property CurrentPosition: Int64 read FCurrentPosition;
    property CurrentMaximum: Int64 read FCurrentMaximum;
    property IsRealUp: string read FIsRealUp write FIsRealUp;
    property FTPUser: string read FFTPUser write FFTPUser;
    property FTPPass: string read FFTPPass write FFTPPass;
    property FTPURL: string read FFTPURL write FFTPURL;
    property FTPPort: string read FFTPPort write FFTPPort;
    property FTPPath: string read FFTPPath write FFTPPath;
    property Files[Index: Integer]: TFileInfo read GetFiles;
    property FileCount: Integer read GetFileCount;
    property LocalPath: string read FLocalPath write FLocalPath;
    property PathType: TPathType read FPathType write FPathType;
    property RemainTime: Integer read FRemainTime write FRemainTime;
    property ServerURL: string read FServerURL write FServerURL;
    property Stopped: Boolean read FStopped;
    property TotalPosition: Int64 read GetTotalPosition;
    property TotalSize: Int64 read GetTotalSize;
    property UserID: string read FUserID write FUserID;
    property OnUpload: TUploadEvent read FOnUpload write FOnUpload;
    property OnUploadBegin: TUploadBeginEvent read FOnUploadBegin write FOnUploadBegin;
    property OnUploadComplete: TNotifyEvent read FOnUploadComplete write FOnUploadComplete;
    property OnUploadEnd: TUploadEndEvent read FOnUploadEnd write FOnUploadEnd;
    property OnGetFileList: TNotifyEvent read FOnGetFileList write FOnGetFileList;
  end;

implementation

uses
  Dialogs,
  CodeSiteLogging, HTTPApp,
  NativeXml, JclFileUtils,
  Common;

function TUploadList.Add(Item: Pointer): Integer;
begin
  with FList.LockList do
  try
    Result := Add(Item);
  finally
    FList.UnlockList;
  end;
end;

function TUploadList.AddArticle(Item: Pointer): Integer;
begin
  with FArticleList.LockList do
  try
    Result := Add(Item);
  finally
    FArticleList.UnlockList;
  end;
end;

procedure TUploadList.ClearArticleList;
begin
  FArticleList.ClearAndFree;
end;

procedure TUploadList.ClearItems;
begin
  FList.ClearAndFree;
end;

constructor TUploadList.Create(AOwner: TUploadMgr);
begin
  FList := TIdThreadSafeList.Create;
  FArticleList := TIdThreadSafeList.Create;
  FOwner := AOwner;
end;

procedure TUploadList.Delete(Index: Integer);
begin
  with FList.LockList do
  try
    Delete(Index);
  finally
    FList.UnlockList;
  end;
end;

destructor TUploadList.Destroy;
begin
  ClearItems;
  ClearArticleList;
  FreeAndNil(FList);
  FreeAndNil(FArticleList);
  inherited Destroy;
end;

procedure TUploadList.DoGetFileList;
var
  FI: TFileInfo;

  procedure RecurseFolder(APath: string);
  var
    SR: TSearchRec;
    LDir: string;
  begin
    LDir := IncludeTrailingPathDelimiter(APath);
    if FindFirst(LDir + '*.*', faAnyFile, SR) = 0 then
    begin
      repeat
        if (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          if (SR.Attr and faDirectory) = faDirectory then
            RecurseFolder(LDir + SR.Name + DirDelimiter)
          else
          begin
            if Pos(IncludeTrailingPathDelimiter(FOwner.LocalPath)  //이미지 폴더는 목록에 추가하지 않음.
              + GUploadImageFolder, APath) <> 0 then Continue;
            FI := TFileInfo.Create;
            FI.ArticleID := FOwner.ArticleID;
            FI.UserID := FOwner.UserID;
            FI.FileName := SR.Name;
            FI.Dir := LDir;
            FI.Size := FileGetSize(LDir + SR.Name);
            FI.FTPURL := FOwner.FTPURL;
            FI.FTPUser := FOwner.FTPUser;
            FI.FTPPass := FOwner.FTPPass;
            FI.FTPPort := FOwner.FTPPort;
            FI.FTPPath := FOwner.FTPPath;
            FI.RelativePath := StringReplace(StringReplace(LDir, FOwner.LocalPath, '', [rfReplaceAll]), '\', '/', [rfReplaceAll]);
            FI.Retry := 0;
            FI.State := dsReady;
            FI.Active := True;
            FList.Add(FI);
          end;
        end;
      until FindNext(SR) <> 0;
      SysUtils.FindClose(SR);
    end;
  end;
begin
  case FOwner.PathType of
    ptFile:
      begin
        if FileExists(FOwner.LocalPath) then
        begin
          FI := TFileInfo.Create;
          FI.ArticleID := FOwner.ArticleID;
          FI.UserID := FOwner.UserID;
          FI.FileName := ExtractFileName(FOwner.LocalPath);
          FI.Dir := ExtractFilePath(FOwner.LocalPath);
          FI.Size := FileGetSize(FOwner.LocalPath);
          FI.FTPURL := FOwner.FTPURL;
          FI.FTPUser := FOwner.FTPUser;
          FI.FTPPass := FOwner.FTPPass;
          FI.FTPPort := FOwner.FTPPort;
          FI.FTPPath := FOwner.FTPPath;
          FI.RelativePath := '/';
          FI.Retry := 0;
          FI.State := dsReady;
          FI.Active := True;
          FList.Add(FI);
        end;
      end;
    ptFolder:
      begin
        if DirectoryExists(FOwner.LocalPath) then
          RecurseFolder(FOwner.LocalPath);
      end;
  end;
end;

function TUploadList.GetActiveArticleInfo: TArticleInfo;
var
  I: Integer;
  AI: TArticleInfo;
begin
  Result := nil;
  with FArticleList.LockList do
  try
    for I := 0 to Count - 1 do
    begin
      AI := TArticleInfo(Items[I]);
      if Assigned(AI) and AI.Active then
      begin
        Result := AI;
        Exit;
      end;
    end;
  finally
    FArticleList.UnlockList;
  end;
end;

function TUploadList.GetActiveFileInfo: TFileInfo;
var
  FI: TFileInfo;
  I: Integer;
begin
  Result := nil;
  with FList.LockList do
  try
    for I := 0 to Count - 1 do
    begin
      FI := TFileInfo(Items[I]);
      if Assigned(FI) and FI.Active then
      begin
        Result := FI;
        Exit;
      end;
    end;
  finally
    FList.UnlockList;
  end;
end;

function TUploadList.GetArticles(Index: Integer): TArticleInfo;
begin
  with FArticleList.LockList do
  try
    Result := TArticleInfo(Items[Index]);
  finally
    FArticleList.UnlockList;
  end;
end;

function TUploadList.GetItemCount: Integer;
begin
  with FList.LockList do
  try
    Result := Count;
  finally
    FList.UnlockList;
  end;
end;

function TUploadList.GetItems(Index: Integer): TFileInfo;
begin
  with FList.LockList do
  try
    Result := TFileInfo(Items[Index]);
  finally
    FList.UnlockList;
  end;
end;

function TUploadList.IndexOf(AFileName, ADir: string): Integer;
var
  I: Integer;
  FI: TFileInfo;
begin
  Result := -1;
  with FList.LockList do
  try
    for I := 0 to Count - 1 do
    begin
      FI := TFileInfo(Items[I]);
      if Assigned(FI) and SameText(LowerCase(FI.FileName), LowerCase(AFileName))
        and SameText(LowerCase(FI.Dir), LowerCase(ADir)) then
      begin
        Result := I;
        Break;
      end;
    end;
  finally
    FList.UnlockList;
  end;
end;

procedure TUploadList.DoGetArticleList;
var
  AI: TArticleInfo;
begin
  Assert(FOwner <> nil);
  if FOwner.UserID = '' then Exit;
  if FOwner.ArticleID = '' then Exit;
  AI := TArticleInfo.Create;
  AI.ArticleID := FOwner.ArticleID;
  AI.UserID := FOwner.UserID;
  AI.Active := True;
  AddArticle(AI);
end;

procedure TUploadList.RequestFileInfo;
var
  S: string;
  LXMLDoc: TNativeXml;
  I: Integer;
begin
  Assert(FOwner <> nil);
  if FOwner.UserID = '' then Exit;
  if FOwner.ArticleID = '' then Exit;
  DoGetArticleList;
  LXMLDoc := TNativeXml.Create(nil);
  try
    S := HTTPGet(FOwner.ServerURL + '/ws/wsftpupdown.asmx/WsUpFile?UserID=' + FOwner.UserID + '&ArticleID='
                 + FOwner.ArticleID);
    LXMLDoc.ExternalEncoding := seUTF8;
    LXMLDoc.XmlFormat := xfReadable;
    LXMLDoc.ReadFromString(S);
    if Assigned(LXMLDoc.Root) then
      with LXMLDoc.Root do
        for I := 0 to NodeCount - 1 do
        begin
          if SameText(Nodes[I].Name, 'Code') then
            FOwner.Code := StrToIntDef(Nodes[I].Value, 0)
          else if SameText(Nodes[I].Name, 'CodeMsg') then
            FOwner.CodeMsg := Nodes[I].Value
          else if SameText(Nodes[I].Name, 'IsRealUp') then
            FOwner.IsRealUp := Nodes[I].Value
          else if SameText(Nodes[I].Name, 'FtpUser') then
            FOwner.FTPUser := Nodes[I].Value
          else if SameText(Nodes[I].Name, 'FtpPass') then
            FOwner.FTPPass := Nodes[I].Value
          else if SameText(Nodes[I].Name, 'FtpUrl') then
            FOwner.FTPURL := Nodes[I].Value
          else if SameText(Nodes[I].Name, 'FtpPort') then
            FOwner.FTPPort := Nodes[I].Value
          else if SameText(Nodes[I].Name, 'FtpPath') then
            FOwner.FTPPath := UTF8ToString(HTTPDecode(UTF8String(Nodes[I].Value)));
        end;
    DoGetFileList;
  finally
    FreeAndNil(LXMLDoc);
  end;
end;

procedure TUploadList.SetArticles(Index: Integer; const Value: TArticleInfo);
begin
  with FArticleList.LockList do
  try
    Items[Index] := Value;
  finally
    FArticleList.UnlockList;
  end;
end;

procedure TUploadList.SetItems(Index: Integer; const Value: TFileInfo);
begin
  with FList.LockList do
  try
    Items[Index] := Value;
  finally
    FList.UnlockList;
  end;
end;

procedure TUploadList.UpFileDone;

  function CheckFinish(const ArticleID: string): Boolean;
  var
    FI: TFileInfo;
    I: Integer;
  begin
    Result := False;
    with FList.LockList do
    try
      for I := 0 to Count - 1 do
      begin
        FI := TFileInfo(Items[I]);
        if Assigned(FI) and SameText(FI.ArticleID, ArticleID) and FI.Active then
        begin
          Result := True;
          Exit;
        end;
      end;
    finally
      FList.UnlockList;
    end;
  end;

  function IsFail(const ArticleID: string): Boolean;
  var
    FI: TFileInfo;
    I: Integer;
  begin
    Result := False;
    with FList.LockList do
    try
      for I := 0 to Count - 1 do
      begin
        FI := TFileInfo(Items[I]);
        if Assigned(FI) and SameText(FI.ArticleID, ArticleID) and (FI.State <> dsFinished) then
        begin
          Result := True;
          Exit;
        end;
      end;
    finally
      FList.UnlockList;
    end;
  end;

  procedure DoUpFileDone(AUserID, AArticleID, ARstCode: string);
  var
    S: string;
    LXMLDoc: TNativeXml;
    I: Integer;
  begin
    LXMLDoc := TNativeXml.Create(nil);
    try
      S := HTTPGet(FOwner.ServerURL + '/ws/wsftpupdown.asmx/WsUpFileDone?UserID=' + AUserID + '&ArticleID='
                       + AArticleID + '&RstCode=' + ARstCode);
      LXMLDoc.ExternalEncoding := seUTF8;
      LXMLDoc.XmlFormat := xfReadable;
      LXMLDoc.ReadFromString(S);
      if Assigned(LXMLDoc.Root) then
        with LXMLDoc.Root do
          for I := 0 to NodeCount - 1 do
          begin
            if SameText(Nodes[I].Name, 'Code') then
              FOwner.Code := StrToIntDef(Nodes[I].Value, 0)
            else if SameText(Nodes[I].Name, 'CodeMsg') then
              FOwner.CodeMsg := Nodes[I].Value
          end;
    finally
      FreeAndNil(LXMLDoc);
    end;
  end;
var
  AI: TArticleInfo;
begin
  AI := GetActiveArticleInfo;
  if Assigned(AI) then
  begin
    if CheckFinish(AI.ArticleID) then Exit;
    AI.Active := False;
    Delay(3000);  //해당 3초는 절대 빼지말것..
    if IsFail(AI.ArticleID) then  //실패
      DoUpFileDone(AI.UserID, AI.ArticleID, '2')
    else  //성공
      DoUpFileDone(AI.UserID, AI.ArticleID, '1');
  end;
end;

{ TUploadThread }

constructor TUploadThread.Create(AOwner: TUploadMgr);
begin
  inherited Create(False);
  FOwner := AOwner;
end;

destructor TUploadThread.Destroy;
begin
  inherited Destroy;
end;

procedure TUploadThread.Execute;
begin
  while not Terminated do
  try
    if not FOwner.Stopped then
      Synchronize(FOwner.DoFileUpload);
  except end;
end;

{ TUploadMgr }

constructor TUploadMgr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnUploadBegin := nil;
  FOnUpload := nil;
  FOnUploadEnd := nil;
  FOnUploadComplete := nil;
  FLocalPath := '';
  FPathType := ptFile;
  FStopped := False;
  FIdFTP := TIdFTP.Create(Self);
  FIdFTP.ConnectTimeout := GTimeOut;
  FIdFTP.ReadTimeout := GTimeOut;
  FIdFTP.TransferTimeout := GTimeOut;
  FIdFTP.Passive := True;
  FIdFTP.TransferType := ftBinary;
  FIdFTP.OnWork := DoWork;
  FIdFTP.OnWorkBegin := DoWorkBegin;
  FIdFTP.OnWorkEnd := DoWorkEnd;
  FIdAntiFreeze := TIdAntiFreeze.Create(Self);
  FList := TUploadList.Create(Self);
  FThread := TUploadThread.Create(Self);
end;

procedure TUploadMgr.DeleteQueue(const Index: Integer);
begin
  Assert(FList <> nil);
  FList.Delete(Index);
end;

destructor TUploadMgr.Destroy;
begin
  FreeAndNil(FThread);
  FreeAndNil(FList);
  FreeAndNil(FIdFTP);
  FreeAndNil(FIdAntiFreeze);
  inherited Destroy;
end;

procedure TUploadMgr.DoFileUpload;

  procedure DoChangeDir(APath: string);
  var
    SL: TStringList;
    S: string;
    I, Len: Integer;
  begin
    S := APath;
    Len := Length(S);
    if S[1] = '/' then
      S := Copy(S, 2, Len-1);
    if (Len > 0) and (S[Len] = '/') then
      S := Copy(S, 0, Len-1);
    SL := TStringList.Create;
    try
      ExtractStrings(['/'], [], PChar(S), SL);
      for I := 0 to SL.Count - 1 do
      begin
        try
          FIdFTP.MakeDir(SL.Strings[I]);
        except end;
        FIdFTP.ChangeDir(SL.Strings[I]);
      end;
    finally
      FreeAndNil(SL);
    end;
  end;

  procedure CheckFileSize(FI: TFileInfo);
  var
    FTP: TIdFTP;
    S: string;
  begin
    FTP := TIdFTP.Create(nil);
    with FTP do
    try
      Username := FI.FTPUser;
      Password := FI.FTPPass;
      Host := FI.FTPURL;
      Port := StrToInt(FI.FTPPort);
      try
        Connect;
        if FI.RelativePath <> '/' then
          S := FI.FTPPath + FI.RelativePath
        else S := FI.FTPPath;
        S := S + '/' + FI.FileName;
        S := StringReplace(S, '//', '/', [rfReplaceAll]);
        if Size(S) = FI.Size then
          FI.State := dsFinished
        else
          FI.State := dsError;
        FI.Active := False;
      except
        FI.State := dsError;
        FI.Active := False;
      end;
    finally
      try
        Disconnect;
      except end;
      FreeAndNil(FTP);
    end;
  end;
var
  FI: TFileInfo;
begin
  FI := GetActiveFileInfo;
  if not Assigned(FI) then
  begin
    Stop;
    if Assigned(FOnUploadComplete) then
      FOnUploadComplete(Self);
    Exit;
  end;
  with FIdFTP do
  try
    Username := FI.FTPUser;
    Password := FI.FTPPass;
    Host := FI.FTPURL;
    Port := StrToInt(FI.FTPPort);
    try
      Connect;
      ChangeDir(FI.FTPPath);
      if FI.RelativePath <> '/' then
        DoChangeDir(FI.RelativePath);
      Put(IncludeTrailingPathDelimiter(FI.Dir) + FI.FileName, FI.FileName, False);
    except
      if not FStopped and (FI.State <> dsFinished) then
      begin
        //Inc(FI.Retry);
        //if FI.Retry > 2 then
        //begin
          FI.State := dsError;
          FI.Active := False;
          Exit;
        //end;
      end;
    end;
    Delay(1000);
    if FI.Size > 2147483648 then
      Delay(4000);
    if FI.Size > 5368709120 then
      Delay(3000);
    CheckFileSize(FI);
  finally
    DoFileUploadComplete;
    Disconnect;
  end;
end;

procedure TUploadMgr.DoFileUploadComplete;
begin
  if Assigned(FList) then
    FList.UpFileDone;
end;

procedure TUploadMgr.DoWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
var
  TotalTime: TDateTime;
  Hour, Min, Sec, MSec: Word;
  DLTime: Double;
begin
  TotalTime := Now - FStartTime;
  DecodeTime(TotalTime, Hour, Min, Sec, MSec);
  Sec := Sec + Min * 60 + Hour * 3600;
  DLTime := Sec + MSec / 1000;
  if DLTime > 0 then
    FAverageSpeed := AWorkCount / DLTime;

  if FAverageSpeed > 0 then
    FRemainTime := Trunc((GetTotalSize - (GetTotalPosition + AWorkCount)) / FAverageSpeed);

  FCurrentPosition := AWorkCount;
  Application.ProcessMessages;
  if Assigned(FOnUpload) then
    FOnUpload(Self, FCurrentPosition);
end;

procedure TUploadMgr.DoWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
var
  FI: TFileInfo;
begin
  FI := GetActiveFileInfo;
  Assert(FI <> nil);

  FI.State := dsUpload;
  FTransferrignData := True;
  FAverageSpeed := 0;
  FStartTime := Now;

  FCurrentPosition := 0;
  if AWorkCountMax > 0 then FCurrentMaximum := AWorkCountMax
  else FCurrentMaximum := FI.Size;

  Application.ProcessMessages;
  if Assigned(FOnUploadBegin) then
    FOnUploadBegin(Self, FCurrentMaximum);
end;

procedure TUploadMgr.DoWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
var
  FI: TFileInfo;
begin
  FI := GetActiveFileInfo;
  Assert(FI <> nil);

  FI.Active := False;
  if FCurrentPosition = FCurrentMaximum then
    FI.State := dsFinished;

  FTransferrignData := False;
  FAverageSpeed := 0;
  if Assigned(FOnUploadEnd) then
    FOnUploadEnd(Self);
end;

function TUploadMgr.GetActiveFileInfo: TFileInfo;
begin
  Assert(FList <> nil);
  Result := FList.ActiveFileInfo;
end;

function TUploadMgr.GetFileCount: Integer;
begin
  Assert(FList <> nil);
  Result := FList.ItemCount;
end;

function TUploadMgr.GetFiles(Index: Integer): TFileInfo;
begin
  Assert(FList <> nil);
  Result := FList.Items[Index];
end;

function TUploadMgr.GetTotalPosition: Int64;
var
  FI: TFileInfo;
  I, LActiveIndex: Integer;
begin
  Result := 0;
  if Assigned(FList) then
  begin
    LActiveIndex := -1;
    for I := 0 to FList.ItemCount - 1 do
    begin
      FI := FList.Items[I];
      if Assigned(FI) and FI.Active then
      begin
        LActiveIndex := I;
        Break;
      end;
    end;
    for I := 0 to LActiveIndex - 1 do
    begin
      FI := FList.Items[I];
      if Assigned(FI) then
        Result := Result + FI.Size;
    end;
  end;
end;

function TUploadMgr.GetTotalSize: Int64;
var
  I: Integer;
begin
  Result := 0;
  if Assigned(FList) then
    for I := 0 to FList.ItemCount - 1 do
      Result := Result + FList.Items[I].Size;
end;

procedure TUploadMgr.RequestInfo;
begin
  if Assigned(FList) then
  begin
    FList.RequestFileInfo;
    if Assigned(FOnGetFileList) then
      FOnGetFileList(Self);
  end;
end;

procedure TUploadMgr.Resume;
var
  I: Integer;
begin
  if Assigned(FList) then
    for I := 0 to FList.ItemCount - 1 do
      if FList.Items[I].State = dsError then
      begin
        FList.Items[I].Retry := 0;
        FList.Items[I].State := dsReady;
        FList.Items[I].Active := True;
      end;
  Start;
end;

procedure TUploadMgr.Start;
begin
  FStopped := False;
end;

procedure TUploadMgr.Stop;
begin
  FStopped := True;
  try
    if FIdFTP.Connected then
    begin
      if FTransferrignData then
        FIdFTP.Abort;
      FIdFTP.Disconnect;
    end;
  except end;
end;

end.
