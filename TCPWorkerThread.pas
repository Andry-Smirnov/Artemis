unit TCPWorkerThread;
{$mode objfpc}{$H+}
interface

uses
  Types,
  {$ifdef unix}cthreads, {$endif}
  {$IFDEF WINDOWS}
  winsock2,windows,
  {$ENDIF}
  SysUtils, Classes, dateutils, syncobjs;

const
  BUFSIZE = 4096 * 2;
  {$IFDEF unix}
    {$DEFINE TSOCKET := Integer}
  	{$DEFINE closesocket:=close}
  	INVALID_SOCKET = -1;
  	SOCKET_ERROR = -1;
  {$ENDIF}

type
  TTCPWorkerThread = class (TThread)
  protected
    mEvent: TEventObject;

    Skt: Integer;
    BufRev: PByte;//buffer
    Logger: TextFile;
    procedure Log(MessageString: string);
    procedure Execute; override;
    procedure Disconnect(); virtual; abstract;
  public
    isWaiting: boolean;
    constructor Create(B: Boolean; LogFile: string);
    destructor Destroy; override;
    procedure DoWork(ClientSocket: Integer);
    procedure Event(SocketEvent: Integer; iRead: Integer; RcvBuf: pbyte); virtual; abstract;
    procedure DoTerminate();
  end;

function GetRemoteSocketAddress(ASocket: TSocket): string;
function GetRemoteSocketPort(ASocket: TSocket): Integer;
function GetLocalSocketAddress(ASocket: TSocket): string;
function GetLocalSocketPort(s: TSocket): Integer;

implementation

function GetIPByName(const Name: string): string;
var
  r: PHostEnt;
  a: TInAddr;
begin
  Result := '';
  r := GetHostByName(PChar(Name));
  if Assigned(r) then
  begin
    a := PInAddr(r^.h_Addr_List^)^;
    Result := inet_ntoa(a);
  end;
end;

destructor TTCPWorkerThread.Destroy;
begin
  //DoTerminate;
  mEvent.Free;
  inherited; // Also called parent class destroyer
end;

constructor TTCPWorkerThread.Create(B: Boolean; LogFile: string);
var
  i: Integer;
begin
  inherited Create(B);
  FreeOnTerminate := True;
  mEvent := TEventObject.Create(nil, True, False, '');
  isWaiting := True;
  Skt := -1;
  AssignFile(Logger, LogFile);
  ReWrite(Logger);
end;

procedure TTCPWorkerThread.Execute;
var
  AResult: Integer;
begin
  Log('TTCPWorkerThread is working');
  BufRev := allocmem(BUFSIZE);
  if BufRev = nil then
  begin
    Log('BufRev is nil');
    exit;
  end;
  if Skt > 0 then
    Event(1, 0, BufRev);
  while Terminated = False do
  begin
    while (Skt > 0) do
    begin
      AResult := Recv(Skt, BufRev, BUFSIZE, 0);
      if (AResult > 0) then
      begin
        Event(3, AResult, BufRev);
      end
      else
        if (AResult <= 0) then
        begin
          Log('socket error:' + IntToStr(AResult));
          Event(2, AResult, BufRev);
        end;
    end;
    Skt := -1;
    isWaiting := True;
    mEvent.WaitFor(INFINITE);
    mEvent.ResetEvent;
  end;//end while true
  FreeMem(BufRev);
  Log('worker done.');
  CloseFile(Logger);
end;

procedure TTCPWorkerThread.DoWork(ClientSocket: Integer);
begin
  Skt := ClientSocket;
  Event(1, 0, nil);
  mEvent.SetEvent;
  isWaiting := False;
end;

procedure TTCPWorkerThread.DoTerminate();
begin
  mEvent.SetEvent;
  Terminate;
  Disconnect();
end;

procedure TTCPWorkerThread.Log(MessageString: string);
begin
  WriteLn(Logger, MessageString);
end;

//You can use the GetSockName function for the local port and address and the GetPeerName for the remote port like so
function GetLocalSocketPort(s: TSocket): Integer;
var
  Addr: TSockAddrIn;
  Size: Integer;
begin
  Size := SizeOf(Addr);
  GetSockName(s, Addr, Size);
  Result := ntohs(Addr.sin_port);
end;

function GetLocalSocketAddress(ASocket: TSocket): string;
var
  Addr: TSockAddrIn;
  Size: Integer;
begin
  Size := SizeOf(Addr);
  GetSockName(ASocket, Addr, Size);
  Result := inet_ntoa(Addr.sin_addr);
end;


function GetRemoteSocketPort(ASocket: TSocket): Integer;
var
  Addr: TSockAddrIn;
  Size: Integer;
begin
  Size := SizeOf(Addr);
  GetPeerName(ASocket, Addr, Size);
  Result := ntohs(Addr.sin_port);
end;

function GetRemoteSocketAddress(ASocket: TSocket): string;
var
  Addr: TSockAddrIn;
  Size: Integer;
begin
  Size := SizeOf(Addr);
  GetPeerName(ASocket, Addr, Size);
  Result := inet_ntoa(Addr.sin_addr);
end;
{
例子代码：(关键是使用winsock2单元)
var
  stLclAddr, stDstAddr: sockaddr_in;
  stMreq: ip_mreq;
  hSocket: TSOCKET;
  stWSAData: TWSADATA;

procedure TForm1.FormCreate(Sender: TObject);
var
  nRet:Integer;
  fFlag:Boolean;
begin
  // Init WinSock
  nRet := WSAStartup($0202, stWSAData);
  if nRet<>0 then
  begin
    StatusBar.SimpleText := Format('WSAStartup failed: %d', [nRet]);
    Exit;
  end;
  // Multicast Group Address and Port setting
  StatusBar.SimpleText := Format('Multicast Address:%s, Port:%d, IP TTL:%d, Interval:%d.',[achMCAddr, nPort, lTTL, nInterval]);
  // Get a datagram socket
  hSocket := socket(AF_INET,SOCK_DGRAM,0);
  if (hSocket = INVALID_SOCKET) then
  begin
    StatusBar.SimpleText := Format('socket() failed, Err: %d', [WSAGetLastError]);
    Exit;
  end;
  // Bind the socket
  stLclAddr.sin_family := AF_INET;
  stLclAddr.sin_addr.s_addr := htonl(INADDR_ANY); // any interface
  stLclAddr.sin_port := 0; //any port
  nRet := bind(hSocket,stLclAddr,SizeOf(stLclAddr));
  if (nRet = SOCKET_ERROR) then
  begin
    StatusBar.SimpleText := Format('bind() port: %d failed, Err: %d', [nPort,WSAGetLastError]);
    Exit;
  end;
  // Join the multicast group
  stMreq.imr_multiaddr.s_addr := inet_addr(achMCAddr);
  stMreq.imr_interface.s_addr := INADDR_ANY;
  nRet := setsockopt(hSocket,IPPROTO_IP,IP_ADD_MEMBERSHIP,@stMreq,SizeOf(stMreq));
  if (nRet = SOCKET_ERROR) then
  begin
    StatusBar.SimpleText := Format('setsockopt() IP_ADD_MEMBERSHIP address %s failed, Err: %d',[achMCAddr, WSAGetLastError]);
    Exit;
  end;
  // Set IP TTL to traverse up to multiple routers
  nRet := setsockopt(hSocket,IPPROTO_IP,IP_MULTICAST_TTL,@lTTL,SizeOf(lTTL));
  if (nRet = SOCKET_ERROR) then
  begin
    StatusBar.SimpleText := Format('setsockopt() IP_MULTICAST_TTL failed, Err: %d',[WSAGetLastError]);
    Exit;
  end;
  // Disable loopback
  fFlag := False;
    nRet := setsockopt(hSocket,IPPROTO_IP,IP_MULTICAST_LOOP,@fFlag,SizeOf(fFlag));
  if (nRet = SOCKET_ERROR) then
  begin
    StatusBar.SimpleText := Format('setsockopt() IP_MULTICAST_LOOP failed, Err: %d',[WSAGetLastError]);
  end;
  SndTimer.Enabled := True;
end;

procedure TForm1.SndTimerTimer(Sender: TObject);
var
  nRet: Integer;
  SndStr: String;
begin
  //Get System (UTC) Time
  GetSystemTime(stSysTime);

  //Assign our destination address
  stDstAddr.sin_family := AF_INET;
  stDstAddr.sin_addr.s_addr := inet_addr(achMCAddr);
  stDstAddr.sin_port := htons(nPort);
  // Send the time to our multicast group!
  nRet := sendto(hSocket,stSysTime,SizeOf(stSysTime),0,stDstAddr,SizeOf(stDstAddr));
  if (nRet < 0) then
  begin
    StatusBar.SimpleText := Format('sendto() failed, Error: %d', [WSAGetLastError]);
    Exit;
  end
  else
  begin
    SndStr:=Format('Sent UTC Time %02d:%02d:%02d:%03d ',[stSysTime.wHour,stSysTime.wMinute,stSysTime.wSecond,stSysTime.wMilliseconds]);
    SndStr:=SndStr+Format('Date: %02d-%02d-%02d to: %s:%d',[stSysTime.wMonth,stSysTime.wDay,stSysTime.wYear,inet_ntoa(stDstAddr.sin_addr),ntohs(stDstAddr.sin_port)]);
    TimeLog.Lines.Add(SndStr);
  end;
end;

}
end.
