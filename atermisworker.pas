unit atermisworker;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, Graphics, winsock2,
  StdCtrls, Types, LCLIntf, LCLType, SysUtils, Windows, Clipbrd,
  zlibex,
  TCPWorkerThread
  ;

const
  DATASIZE = 1048 * 1024;
  FILEBLOCKS = 16;
  REQUEST_LOGIN = 10;
  REQUEST_SCREEN = 20;
  FULL_SCREEN = 21;
  PART_SCREEN = 22;
  STOP_SCREEN = 23;
  SEND_SCREEN = 29;
  REQUEST_AUDIO = 30;
  AUDIO_DATA = 31;
  REQUEST_RECEIVE_FILE = 50;
  CONFIRM_REQUEST_RECEIVE_FILE = 51;
  FILE_CONTENT = 52;
  NEXT_BLOCK_FILE_DATA = 53;
  REJECT_REQUEST_RECEIVE_FILE = 54;
  CLIPBOARD_DATA = 60;
  MOUSE_Move = 70;
  MOUSE_ACTION = 71;
  KEYBOARD_ACTION = 80;

type
  TShowScreenEvent = procedure(EventID, Idx: Integer) of object;
  TReMoveEvent = procedure(Idx: Integer) of object;
  TAddClientEvent = procedure(sIP, sPort: string; Idx: Integer) of object;
  PAtermisWorker = ^TAtermisWorker;
  TShowScreenEvent2 = procedure(EventID: Integer; Source: PAtermisWorker) of object;

  TAtermisWorker = class (TTCPWorkerThread)
  protected
    procedure CheckDisconnect(r: Integer);
  private
    NeedCmd: Boolean;
    FPayloadSize,
    FStatusID: Integer;
    iDownloadFileHandle: Integer;
    iUploadFileHandle: Integer;
    FFileBlockCount: Integer;
    FRemoteBlockCount: Integer;
    FNextBytes: Integer;
    //curbytes-->means the start of databuf to be use
    FCurBytes: Integer;
    FBufTail: Integer;
    FCmdBuf: array [0..4] of byte;
    FDataBuf: PByte;
    FOutBuf: PByte;
    FActualFileSize: Integer;
    FReceivedFileSize: Integer;
    FOutBufSize: Integer;
    FSendCSec: TRTLCriticalSection;
    FOnShowScreen: TShowScreenEvent;
    FOnReMove: TReMoveEvent;
    FOnNewConnect: TAddClientEvent;
    procedure ShowScreen;
    procedure ReInit();
    function SetCmd(cmd, size: Integer): Integer;
    function FillData(p: PByte; size: Integer): Integer;
    function recvFileRequest(FileName: string; AFileSize: Integer): Integer;
    function shiftInputFile(APos: Integer): Integer;
    function writeData2File(APayload: PByte): Integer;
  public
    mmsin: TMemoryStream;
    mmsout: TMemoryStream;
    isLogined: Boolean;
    NeedScreen: Boolean;
    passwd: string;
    DownloadFileName: string;
    RemoteIp: string;
    RemotePort: string;
    AtermisIdx: Integer;
    GridIndx: Integer;
    MouseX: Integer;
    MouseY: Integer;
    MouseClicked: TMouseButton;
    UpAndDown: Integer;
    MouseScroll: Integer;
    SState: TShiftState;
    AKey: Word;
    RemoteErrCode: Integer;
    UploadFileSize: Integer;
    UploadCurrentFileSize: Integer;
    FilePercent: Integer;
    function SendStopScreen(): Integer;
    function SendGetScreen(): Integer;
    function SendScreen(iType: Integer; p: PByte; ScreenSize: Integer): Integer;
    function SendFile(FName: string): Integer;
    function SendFileData(): Integer;
    function SendMouseMove(x, y: Integer): Integer;
    function SendMouseClick(x, y, Delta, UpDown: Integer; Button: TMouseButton; Shift: TShiftState): Integer;
    function SendKey(k: word; Shift: TShiftState; UpDown: Integer): Integer;
    function SendRecvScreen(): Integer;
    function FileRejected(): Integer;
    function SendClipboard(s: string): Integer;
    function SendLogin(s: string): Integer;
    procedure Event(SocketEvent: Integer; iRead: Integer; RcvBuf: PByte); override;
    constructor Create(B: Boolean; LogFile: string);
    destructor Destroy;
    procedure Disconnect(); override;
    procedure ShowClientInfo;
    property OnShowScreen: TShowScreenEvent read FOnShowScreen write FOnShowScreen;
    property OnReMove: TReMoveEvent read FOnReMove write FOnReMove;
    property OnNewConnect: TAddClientEvent read FOnNewConnect write FOnNewConnect;
  end;

implementation

constructor TAtermisWorker.Create(B: Boolean; LogFile: string);
begin
  inherited Create(B, LogFile);
  InitializeCriticalSection(FSendCSec);
  FDataBuf := allocmem(DATASIZE);
  FOutBuf := allocmem(DATASIZE);
  ReInit();
  mmsin := TMemoryStream.Create;
  mmsout := TMemoryStream.Create;
end;

destructor TAtermisWorker.Destroy;
begin
  mmsin.Free;
  mmsout.Free;
  freemem(FDataBuf);
  freemem(FOutBuf);
  DeleteCriticalSection(FSendCSec);
  Disconnect();
  inherited;
end;

function TAtermisWorker.SendClipboard(s: string): Integer;
var
  i, j, k: Integer;
begin
  EnterCriticalSection(FSendCSec);
  try
    if Length(s) < DATASIZE then
      Result := SetCmd(CLIPBOARD_DATA, Length(s))
    else
      Result := SetCmd(CLIPBOARD_DATA, DATASIZE);
    if Result < 0 then
      Exit;
    if Length(s) < DATASIZE then
      Result := FillData(@s[1], Length(s))
    else
      Result := FillData(@s[1], DATASIZE);
    if FOutBufSize > 4096 then
    begin
      i := 0;
      k := FOutBufSize div 4096;
      j := FOutBufSize mod 4096;
      repeat
        Result := Send(Skt, FOutBuf[i], 4096, 0);
        i := i + 4096;
        Dec(k);
      until (k < 1) or (Result < 0);
      if (Result > 0) and (j > 0) then
      begin
        Result := Send(Skt, FOutBuf[i], j, 0);
      end;
    end
    else
    begin
      Result := Send(Skt, FOutBuf[0], FOutBufSize, 0);
    end;
    CheckDisconnect(Result);
  finally
    LeaveCriticalSection(FSendCSec);
  end;
end;

function TAtermisWorker.SendLogin(s: string): Integer;
begin
  EnterCriticalSection(FSendCSec);
  try
    Result := SetCmd(REQUEST_LOGIN, Length(s));
    if Result < 0 then
      Exit;
    Result := FillData(@s[1], Length(s));
    Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
    CheckDisconnect(Result);
  finally
    LeaveCriticalSection(FSendCSec);
  end;
end;

function TAtermisWorker.SendStopScreen(): Integer;
begin
  EnterCriticalSection(FSendCSec);
  try
    Result := SetCmd(STOP_SCREEN, 0);
    Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
    CheckDisconnect(Result);
  finally
    LeaveCriticalSection(FSendCSec);
  end;
end;

function TAtermisWorker.SendGetScreen(): Integer;
begin
  EnterCriticalSection(FSendCSec);
  try
    Result := SetCmd(REQUEST_SCREEN, 0);
    Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
    CheckDisconnect(Result);
  finally
    LeaveCriticalSection(FSendCSec);
  end;
end;

function TAtermisWorker.SendRecvScreen(): Integer;
begin
  EnterCriticalSection(FSendCSec);
  try
    Result := SetCmd(SEND_SCREEN, 0);
    Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
    CheckDisconnect(Result);
  finally
    LeaveCriticalSection(FSendCSec);
  end;
end;

function TAtermisWorker.SendScreen(iType: Integer; p: PByte; ScreenSize: Integer): Integer;
var
  i, j, k: Integer;
begin
  EnterCriticalSection(FSendCSec);
  try
    Result := SetCmd(iType, ScreenSize);
    if Result < 0 then
      Exit;
    Result := FillData(p, ScreenSize);
    if FOutBufSize > 4096 then
    begin
      i := 0;
      k := FOutBufSize div 4096;
      j := FOutBufSize mod 4096;
      repeat
        Result := send(Skt, FOutBuf[i], 4096, 0);
        i := i + 4096;
        Dec(k);
      until (k < 1) or (Result < 0);
      if (Result > 0) and (j > 0) then
      begin
        Result := send(Skt, FOutBuf[i], j, 0);
      end;
    end
    else
    begin
      Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
    end;
    CheckDisconnect(Result);
  finally
    LeaveCriticalSection(FSendCSec);
  end;
end;

procedure TAtermisWorker.CheckDisconnect(r: Integer);
begin
  if r < 0 then
    Disconnect();
end;

procedure TAtermisWorker.Disconnect();
begin
  if Skt = -1 then
    Exit;
  CloseSocket(Skt);
  Skt := -1;
  if iDownloadFileHandle > 0 then
  begin
    FileClose(iDownloadFileHandle);
    iDownloadFileHandle := -2;
  end;
  if iUploadFileHandle > 0 then
  begin
    FileClose(iUploadFileHandle);
    iUploadFileHandle := -2;
  end;
  FStatusID := -1;
  if Assigned(FOnReMove) then
  begin
    FOnReMove(GridIndx);
  end;
  ReInit();
end;

procedure TAtermisWorker.ShowClientInfo;
begin
  if Assigned(FOnNewConnect) then
  begin
    FOnNewConnect(RemoteIp, RemotePort, GridIndx);
  end;
end;

procedure TAtermisWorker.ShowScreen;
//https://wiki.freepascal.org/Multithreaded_Application_Tutorial
// in mainthread : atermisworker.OnShowStatus := @ShowStatus; ShowStatus is an function with Integer parameter
// this method is executed by the mainthread and can therefore access all GUI elements.
begin
  if Assigned(FOnShowScreen) then
  begin
    FOnShowScreen(FStatusID, AtermisIdx);
  end;
end;

procedure TAtermisWorker.ReInit();
begin
  isWaiting := True;
  NeedScreen := False;
  isLogined := False;
  NeedCmd := True;
  FNextBytes := 5;
  FCurBytes := 0;
  FBufTail := 0;
  iUploadFileHandle := -1;
  iDownloadFileHandle := -1;
end;

function TAtermisWorker.FillData(p: PByte; size: Integer): Integer;
begin
  Move(p^, FOutBuf[FOutBufSize], size);
  FOutBufSize := FOutBufSize + size;
  Result := 0;
end;

function TAtermisWorker.SetCmd(cmd, size: Integer): Integer;
begin
  FOutBuf[0] := cmd;
  //Log('sendout:'+inttostr(cmd)+' payloadsize='+inttostr(size)+' bytes.');
  //for intel little encdian
  Move(SwapEndian(size), FOutBuf[1], 4);
  FOutBufSize := 5;
  Result := 0;
end;

procedure TAtermisWorker.Event(SocketEvent: Integer; iRead: Integer; RcvBuf: PByte);
var
  j, iRemain: Integer;
  s: string;
  ppayload: PByte;
begin
  case SocketEvent of
    1://connected
    begin
      RemoteIp := GetRemoteSocketAddress(Skt);
      RemotePort := IntToStr(GetRemoteSocketPort(Skt));
    end;
    2://seDisconnect :
    begin
      Disconnect();
    end;
    3://seRead :
    begin
      Move(RcvBuf[0], FDataBuf[FBufTail], iRead);
      FBufTail := FBufTail + iRead;
      iRemain := FBufTail - FCurBytes;
      Log('imcoming:' + IntToStr(iRead) + ' bytes.');
      while FCurBytes < FBufTail do
      begin
        if NeedCmd then
        begin
          if iRemain < FNextBytes then
          begin
            break;
          end
          else
          begin
            Move(FDataBuf[FCurBytes], FCmdBuf[0], FNextBytes);
            FCurBytes := FCurBytes + FNextBytes;
            FNextBytes := 0;
            iRemain := FBufTail - FCurBytes;
          end;
          NeedCmd := False;
          FPayloadSize := SwapEndian(pInteger(@FCmdBuf[1])^);
          Log('cmd:' + IntToStr(FCmdBuf[0]) + ' payload:' + IntToStr(FPayloadSize));
          if FPayloadSize > DATASIZE then
            Exit;

        end;

        if (FPayloadSize > 0) and (FPayloadSize > iRemain) then
        begin
          //not enough for payload
          Log('not enough for payload:' + IntToStr(FPayloadSize) + ' remain:' + IntToStr(iRemain));
          if FCurBytes > 0 then
          begin
            Move(FDataBuf[FCurBytes], FDataBuf[0], iRemain);
            FBufTail := iRemain;
            FCurBytes := 0;
          end;
          break;
        end;
        //now data is ready in FDataBuf with size=FPayloadSize
        FStatusID := FCmdBuf[0];
        ppayload := @FDataBuf[FCurBytes];
        if REQUEST_LOGIN = FCmdBuf[0] then//login with password Length
        begin
          Log('cmd:loging');
          setstring(passwd, pansichar(ppayload), FPayloadSize);
          isLogined := True;
          Synchronize(@ShowScreen);
          if isLogined = False then
          begin
            Disconnect();
            Exit;
          end;
          NeedCmd := True;
          FNextBytes := 5;
          FCurBytes := FCurBytes + FPayloadSize;
          iRemain := FBufTail - FCurBytes;
          FPayloadSize := 0;
          continue;
        end;
        if (isLogined = False) and (AtermisIdx > -1) then
        begin
          Disconnect();
          Exit;
        end;

        case FCmdBuf[0] of
          REQUEST_SCREEN://request server send out screen
            begin
              //start a thread to send out screen capture
              Log('cmd:request screen');

              Synchronize(@ShowScreen);
            end;
          SEND_SCREEN://request server receive screen
            begin
            end;
          FULL_SCREEN, PART_SCREEN://screenshot received
            begin
              Log('cmd:receive screen');
              //load to a bitmap and show on view form
              //j:=FileCreate('d:\ttj3.jpg',fmOpenWrite);
              //FileWrite(j,ppayload[0],FPayloadSize);
              //FileClose(j);
              mmsin.Clear;
              mmsin.Write(ppayload[0], FPayloadSize);
              mmsin.Position := 0;
              mmsout.Clear;

              //ZDecompressStream(mmsin, mmsout);
              mmsout.Position := 0;
              //mmsin.SaveTofile('in.bin');
              Synchronize(@ShowScreen);
            end;
          STOP_SCREEN:
          begin
            NeedScreen := False;
          end;
          REQUEST_AUDIO://request audio
          begin
            //start mic-in thread for send out audio payload
            Log('cmd:request audio');
          end;
          AUDIO_DATA://receive and audio data
          begin
            Log('cmd:receive audio');
            //decode audio data and play it
          end;
          REQUEST_RECEIVE_FILE://request receive file name+4 byte file size
          begin
            setstring(s, pansichar(ppayload), FPayloadSize - 4);

            j := SwapEndian(pInteger(@ppayload[FPayloadSize - 4])^);
            Log('cmd:request file:' + s + ' filesize:' + IntToStr(j));
            if recvFileRequest(s, j) < 0 then
            begin
              Disconnect();
            end;

          end;
          CONFIRM_REQUEST_RECEIVE_FILE://file write confirm, with 4 bytes position,4 bytes block count
          begin
            j := SwapEndian(pInteger(ppayload)^);
            FRemoteBlockCount := SwapEndian(pInteger(@ppayload[4])^);
            Log('cmd:file write confirm,shift=' + IntToStr(j) + ',remoteBlockCount=' + IntToStr(FRemoteBlockCount));
            shiftInputFile(j);
            RemoteErrCode := SendFileData();
            Synchronize(@ShowScreen);
          end;
          NEXT_BLOCK_FILE_DATA://confirm next  read with block count;
          begin

            FRemoteBlockCount := SwapEndian(pInteger(ppayload)^);
            Log('cmd:file next  read count:' + IntToStr(FRemoteBlockCount));
            RemoteErrCode := SendFileData();
            Synchronize(@ShowScreen);
          end;
          REJECT_REQUEST_RECEIVE_FILE://file write reject, with 4 bytes error code
          begin
            Log('cmd:file write reject');
            RemoteErrCode := SwapEndian(pInteger(ppayload)^);
            Synchronize(@ShowScreen);
          end;
          FILE_CONTENT://request receive file content
          begin
            if writeData2File(ppayload) < 0 then
            begin
              Disconnect();
            end;
          end;
          CLIPBOARD_DATA://request share clipboard data
          begin
            Log('cmd:file clipboard data');
            setstring(s, pansichar(ppayload), FPayloadSize);
            Clipboard.AsText := s;
          end;
          //https://wiki.freepascal.org/MouseAndKeyInput
          MOUSE_Move://mouse action : Move, fixed 8 bytes for x,y
          begin
            //SetCursorPos(cmd^.X, cmd^.Y);
            MouseX := SwapEndian(pInteger(ppayload)^);
            MouseY := SwapEndian(pInteger(@ppayload[4])^);
            Synchronize(@ShowScreen);
            Log('cmd:mouse Move:X=' + IntToStr(MouseX) + ',y=' + IntToStr(MouseY));
          end;
          MOUSE_ACTION://mouse action : click fixed 4 bytes for button left,middle,right, 0 or 1 for clicked. and wheel scrolling(255 degree)
          begin
            //1 bytes:left most byte:shift,alt,ctrl,left,middle,right,wheel,up/down,
            //4 bytes: delta of wheel
            //8 bytes: x=4 bytes, y = 4 bytes
            SState := [];
            if ppayload[0] and $80 > 0 then
            begin
              include(SState, ssShift);
              Log('ssShift clicked');
            end;
            if ppayload[0] and $40 > 0 then
            begin
              include(SState, ssAlt);
              Log('ssAlt clicked');
            end;

            if ppayload[0] and $20 > 0 then
            begin
              include(SState, ssCtrl);
              Log('ssCtrl clicked');
            end;

            if ppayload[0] and $10 > 0 then
            begin
              include(SState, ssLeft);
              Log('ssLeft clicked');
            end;

            if ppayload[0] and $8 > 0 then
            begin
              include(SState, ssRight);
              Log('ssRight clicked');
            end;

            if ppayload[0] and $4 > 0 then
            begin
              include(SState, ssMiddle);
              Log('ssMiddle clicked');
            end;

            if ppayload[0] and $2 > 0 then
            begin
              //mouse wheel scroll
              if SwapEndian(pInteger(@ppayload[1])^) > 0 then
              begin
                //scroll down
                MouseScroll := 2;
              end
              else
              begin
                //scroll up
                MouseScroll := 1;
              end;
            end;
            MouseX := SwapEndian(pInteger(@ppayload[5])^);
            MouseY := SwapEndian(pInteger(@ppayload[9])^);
            if ppayload[0] and $10 > 0 then
              MouseClicked := mbLeft;

            if ppayload[0] and $8 > 0 then
              MouseClicked := mbRight;

            if ppayload[0] and $4 > 0 then
              MouseClicked := mbMiddle;
            if ppayload[0] and $1 > 0 then
            begin
              Log('cmd:mouse down:X=' + IntToStr(MouseX) + ',y=' + IntToStr(MouseY));
              //mouse down
              UpAndDown := 2;

            end
            else
            begin
              Log('cmd:mouse up:X=' + IntToStr(MouseX) + ',y=' + IntToStr(MouseY));
              UpAndDown := 1;
            end;
            Synchronize(@ShowScreen);
          end;
          KEYBOARD_ACTION://for keyboard type fixed 4 bytes,key code,shift(0,1 left ,2 right),ctrl,alt
          begin
            //4 bytes:
            //1 bytes:left most byte:shift,alt,ctrl,left,right,middle,
            //1 byte : reserve
            //2 bytes:keycode
            SState := [];
            if ppayload[0] and $80 > 0 then
            begin
              include(SState, ssShift);
              Log('ssShift clicked');
            end;
            if ppayload[0] and $40 > 0 then
            begin
              include(SState, ssAlt);
              Log('ssAlt clicked');
            end;

            if ppayload[0] and $20 > 0 then
            begin
              include(SState, ssCtrl);
              Log('ssCtrl clicked');
            end;

            if ppayload[0] and $10 > 0 then
            begin
              include(SState, ssLeft);
              Log('ssLeft clicked');
            end;

            if ppayload[0] and $8 > 0 then
            begin
              include(SState, ssRight);
              Log('ssRight clicked');
            end;

            if ppayload[0] and $4 > 0 then
            begin
              include(SState, ssMiddle);
              Log('ssMiddle clicked');
            end;
            AKey := SwapEndian(pword(@ppayload[2])^);
            if ppayload[0] and $1 > 0 then
            begin
              //key down
              UpAndDown := 2;
              Log('cmd:key Down:=' + IntToStr(AKey));
            end
            else
            begin
              UpAndDown := 1;
              Log('cmd:key up:=' + IntToStr(AKey));
            end;
            Synchronize(@ShowScreen);
          end;
          else
          begin
            Disconnect();
            Exit;
          end;
        end;//end case
        FCurBytes := FCurBytes + FPayloadSize;
        iRemain := FBufTail - FCurBytes;
        NeedCmd := True;
        FNextBytes := 5;
        FPayloadSize := 0;
      end;//end while < FBufTail
      if FCurBytes > 0 then
      begin
        Move(FDataBuf[FCurBytes], FDataBuf[0], iRemain);
        FBufTail := iRemain;
        FCurBytes := 0;
      end;
    end;//end READ
  end;
end;

function TAtermisWorker.SendMouseMove(x, y: Integer): Integer;
var
  axisarray: array[0..7] of byte;
  tmp: Integer;
begin
  EnterCriticalSection(FSendCSec);
  try
    Result := SetCmd(MOUSE_Move, 8);
    if Result < 0 then
      Exit;
    tmp := SwapEndian(x);
    Move(tmp, axisarray[0], 4);
    tmp := SwapEndian(y);
    Move(tmp, axisarray[4], 4);
    FillData(@axisarray[0], 8);
    Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
    CheckDisconnect(Result);
  finally
    LeaveCriticalSection(FSendCSec);
  end;
end;

function TAtermisWorker.SendMouseClick(x, y, Delta, UpDown: Integer; Button: TMouseButton; Shift: TShiftState): Integer;
var
  AxisArray: array[0..7] of byte;
  MouseAction: array[0..3] of byte;
  Btn: byte;
  tmp: Integer;
begin
  EnterCriticalSection(FSendCSec);
  try
    Result := SetCmd(MOUSE_ACTION, 13);
    if Result < 0 then
      Exit;
    Btn := 0;
    //1 bytes:left most byte:shift,alt,ctrl,left,middle,right,wheel,up/down,
    //4 bytes: Delta of wheel
    //8 bytes: x=4 bytes, y = 4 bytes
    if ssShift in Shift then
      Btn := Btn or $80;
    if ssAlt in Shift then
      Btn := Btn or $40;
    if ssCtrl in Shift then
      Btn := Btn or $20;
    if ssLeft in Shift then
      Btn := Btn or $10;
    if ssRight in Shift then
      Btn := Btn or $8;
    if ssMiddle in Shift then
      Btn := Btn or $4;

    if Button = mbLeft then
      Btn := Btn or $10;

    if Button = mbRight then
      Btn := Btn or $8;
    if Button = mbMiddle then
      Btn := Btn or $4;
    if Delta > 0 then
      Btn := Btn or $2;
    if UpDown > 1 then
      Btn := Btn or 1;
    FillData(@Btn, 1);
    tmp := SwapEndian(Delta);
    FillData(@tmp, 4);
    if Result < 0 then
      Exit;
    tmp := SwapEndian(x);
    Move(tmp, AxisArray[0], 4);
    tmp := SwapEndian(y);
    Move(tmp, AxisArray[4], 4);
    FillData(@AxisArray[0], 8);
    Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
    CheckDisconnect(Result);
  finally
    LeaveCriticalSection(FSendCSec);
  end;
end;

function TAtermisWorker.SendKey(k: word; Shift: TShiftState; UpDown: Integer): Integer;
var
  Btn: byte;
  lkey: word;
  KeyArray: array[0..3] of byte;
begin
  EnterCriticalSection(FSendCSec);
  try
    Result := SetCmd(KEYBOARD_ACTION, 4);
    if Result < 0 then
      Exit;
    Btn := 0;
    //1 bytes:left most byte:shift,alt,ctrl,left,right,middle,0,up/down
    //1 byte : reserve
    //2 bytes:keycode
    if ssShift in Shift then
      Btn := Btn or $80;
    if ssAlt in Shift then
      Btn := Btn or $40;
    if ssCtrl in Shift then
      Btn := Btn or $20;
    if ssLeft in Shift then
      Btn := Btn or $10;
    if ssRight in Shift then
      Btn := Btn or $8;
    if ssMiddle in Shift then
      Btn := Btn or $4;
    if UpDown > 1 then
      Btn := Btn or 1;
    lkey := SwapEndian(k);
    KeyArray[0] := Btn;
    Move(lkey, KeyArray[2], 2);
    FillData(@KeyArray[0], 4);
    Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
    CheckDisconnect(Result);
  finally
    LeaveCriticalSection(FSendCSec);
  end;
end;

function TAtermisWorker.shiftInputFile(APos: Integer): Integer;
begin
  if APos = 0 then
    Exit(0);
  Result := FileSeek(iUploadFileHandle, APos, fsFromBeginning);

  UploadCurrentFileSize := APos;
  FilePercent := round(double(UploadCurrentFileSize) / double(UploadFileSize) * 100);
end;

function TAtermisWorker.recvFileRequest(FileName: string; AFileSize: Integer): Integer;
var
  F: file of byte;
  CurrentFileSize: Integer;
begin
  if AFileSize > 0 then
  begin
    Result := 0;
    if FileExists(FileName) then
    begin
      Assignfile(F, FileName);
      Reset(F);
      CurrentFileSize := FileSize(F);
      Closefile(F);
      if AFileSize > CurrentFileSize then
      begin
        iDownloadFileHandle := FileOpen(FileName, fmOpenWrite);
        if iDownloadFileHandle = -1 then
        begin
          SetCmd(REJECT_REQUEST_RECEIVE_FILE, 4);
          CurrentFileSize := 1;
          CurrentFileSize := SwapEndian(CurrentFileSize);
          FillData(@CurrentFileSize, 4);
          Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
          Exit;
        end
        else
        begin
          FileSeek(iDownloadFileHandle, CurrentFileSize, fsFromBeginning);
        end;
        SetCmd(CONFIRM_REQUEST_RECEIVE_FILE, 8);
        FillData(@CurrentFileSize, 4);
        FFileBlockCount := FILEBLOCKS;
        CurrentFileSize := SwapEndian(FFileBlockCount);
        FillData(@CurrentFileSize, 4);
        Result := Send(Skt, FOutBuf[0], FOutBufSize, 0);
        FActualFileSize := AFileSize;
        FReceivedFileSize := CurrentFileSize;
        DownloadFileName := FileName;
        Synchronize(@ShowScreen);
        Exit;
      end
      else
      begin
        SetCmd(REJECT_REQUEST_RECEIVE_FILE, 4);
        CurrentFileSize := 1;
        CurrentFileSize := SwapEndian(CurrentFileSize);
        FillData(@CurrentFileSize, 4);
        Result := Send(Skt, FOutBuf[0], FOutBufSize, 0);
        CheckDisconnect(Result);
        Exit;
      end;
    end;
  end;
  iDownloadFileHandle := FileCreate(FileName, fmOpenWrite);
  if iDownloadFileHandle = -1 then
  begin
    SetCmd(REJECT_REQUEST_RECEIVE_FILE, 4);
    CurrentFileSize := 1;
    CurrentFileSize := SwapEndian(CurrentFileSize);
    FillData(@CurrentFileSize, 4);
    Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
    CheckDisconnect(Result);
  end
  else
  begin
    SetCmd(CONFIRM_REQUEST_RECEIVE_FILE, 8);
    CurrentFileSize := 0;
    FillData(@CurrentFileSize, 4);
    FFileBlockCount := FILEBLOCKS;
    CurrentFileSize := SwapEndian(FFileBlockCount);
    FillData(@CurrentFileSize, 4);
    Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
    FActualFileSize := AFileSize;
    FReceivedFileSize := 0;
    CheckDisconnect(Result);
  end;
end;

function TAtermisWorker.SendFile(FName: string): Integer;
var
  Str: string;
  CurrentFileSize,
  j: Integer;
  F: file of Byte;
begin
  EnterCriticalSection(FSendCSec);
  try
    Str := ExtractFilename(FName);

    if iUploadFileHandle < 0 then
    begin
      Assignfile(F, FName);
      Reset(F);
      CurrentFileSize := FileSize(F);
      Closefile(F);
      iUploadFileHandle := FileOpen(FName, fmOpenRead);
      if iUploadFileHandle = -1 then
      begin
        Exit(-100);
      end;
    end;
    Result := SetCmd(REQUEST_RECEIVE_FILE, Length(Str) + 4);

    Result := FillData(@Str[1], Length(Str));
    UploadFileSize := CurrentFileSize;
    CurrentFileSize := SwapEndian(CurrentFileSize);
    Result := FillData(@CurrentFileSize, 4);
    FilePercent := 0;
    Result := Send(Skt, FOutBuf[0], FOutBufSize, 0);
    UploadCurrentFileSize := 0;
    CheckDisconnect(Result);
  finally
    LeaveCriticalSection(FSendCSec);
  end;
  Result := 0;
end;

function TAtermisWorker.FileRejected(): Integer;
begin
  FileClose(iUploadFileHandle);
  iUploadFileHandle := -2;
  Result := 0;
end;

function TAtermisWorker.writeData2File(APayload: PByte): Integer;
var
  j: Integer;
begin
  if iDownloadFileHandle < 0 then
    Exit;

  j := FileWrite(iDownloadFileHandle, APayload[0], FPayloadSize);
  if FPayloadSize <> j then
  begin
    Log('file write error');
    FileClose(iDownloadFileHandle);
    iDownloadFileHandle := -2;
    SetCmd(REJECT_REQUEST_RECEIVE_FILE, 4);
    j := SwapEndian(j);
    FillData(@j, 4);
    Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
    CheckDisconnect(Result);
  end;
  Dec(FFileBlockCount);
  FReceivedFileSize := FReceivedFileSize + FPayloadSize;
  Log('file receive wrote:' + IntToStr(FReceivedFileSize));
  if FReceivedFileSize = FActualFileSize then
  begin
    Log('file receive done.');
    FileClose(iDownloadFileHandle);

  end
  else
    if FFileBlockCount = 0 then
    begin
      FFileBlockCount := FILEBLOCKS;
      SetCmd(NEXT_BLOCK_FILE_DATA, 4);
      j := FILEBLOCKS;
      FillData(@j, 4);
      Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
      CheckDisconnect(Result);
    end;
end;

function TAtermisWorker.SendFileData(): Integer;
var
  j: Integer;
begin
  if FRemoteBlockCount = 0 then
    Exit(0);
  EnterCriticalSection(FSendCSec);
  try
    repeat
      j := FileRead(iUploadFileHandle, FOutBuf[5], BUFSIZE);
      if (j = -1) or (j = 0) then
      begin
        FileClose(iUploadFileHandle);
        iUploadFileHandle := -2;
        Exit(-101);
      end;
      Result := SetCmd(FILE_CONTENT, j);
      if Result < 0 then
        Exit(-102);
      //result := FillData( @FOutBuf[0], j );
      FOutBufSize := FOutBufSize + j;
      Result := send(Skt, FOutBuf[0], FOutBufSize, 0);
      CheckDisconnect(Result);
      if Result < 0 then
        Exit(-102);
      Dec(FRemoteBlockCount);
      UploadCurrentFileSize := UploadCurrentFileSize + j;
      FilePercent := round(double(UploadCurrentFileSize) / double(UploadFileSize) * 100);
    until (j < BUFSIZE) or (FRemoteBlockCount = 0);
    if (j < BUFSIZE) then
    begin
      Log('file send done!!');
      FileClose(iUploadFileHandle);
      iUploadFileHandle := -2;
    end;
  finally
    LeaveCriticalSection(FSendCSec);
  end;
  Result := 0;
end;

end.
