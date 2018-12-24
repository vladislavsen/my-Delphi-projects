unit Com32;

interface

uses
  Windows, Classes, SysUtils;

{ =========================================================================== }

type
  TCommPort = class(TObject)
  private
    FPortNum : integer;
    FBaudRate : integer;
    FPortStream : THandleStream;
    FPortHandle : THandle;
    function  GetOpened: boolean;
    procedure SetPortNum(v: integer);
    procedure SetBaudRate(v: integer);
    procedure ApplyCOMSettings;
    procedure FlushBuffers(inBuf, outBuf: boolean);
  public
    constructor Create;
    destructor  Destroy; override;
  public
    property  PortNumber : integer read FPortNum write SetPortNum;
    property  BaudRate : integer read FBaudRate write SetBaudRate;
    property  Opened : boolean read GetOpened;
    procedure Open;
    procedure Close;
    procedure SendBuff(Buff: PChar; Len: integer);
    procedure SendStr(const s: string);
  end;

{ --------------------------------------------------------------------------- }

function ScanPorts(Lst: TStrings; MaxNumber: integer; TillFail: boolean): integer;

{ =========================================================================== }

implementation

{ =========================================================================== }

function ScanPorts(Lst: TStrings; MaxNumber: integer; TillFail: boolean): integer;
var i: integer; PortName: string; hPort: THandle;
begin
  Result := 0;
  i := 1;
  if (MaxNumber < i) then MaxNumber := i;
  if Assigned(Lst) then Lst.Clear;
  while (i <= MaxNumber) do begin
    PortName := Format('COM%d', [i]);
    hPort := CreateFile(PChar(PortName),
                        GENERIC_READ or GENERIC_WRITE,
                        0,
                        nil,
                        OPEN_EXISTING,
                        FILE_ATTRIBUTE_NORMAL,
                        0);
    if (hPort <> INVALID_HANDLE_VALUE) then begin
      CloseHandle(hPort);
      Inc(Result);
      if Assigned(Lst) then Lst.Add(Format('%d', [i]));
    end else
    if TillFail then Break;
    Inc(i);
  end;
end;

{ =========================================================================== }
{ TCommPort }

constructor TCommPort.Create;
begin
  FPortHandle := INVALID_HANDLE_VALUE;
  FPortNum := 1;
  SetBaudRate(9600);
end;

destructor TCommPort.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TCommPort.SetPortNum(v: integer);
begin
  if (v < 1) then v := 1;
  if (v > 255) then v := 255;
  if (FPortNum <> v) then begin
    FPortNum := v;
    if Opened then begin
      Close;
      Open;
    end;
  end;
end;

procedure TCommPort.SetBaudRate(v: integer);
begin
  if (v < 110) then v := 110;
  if (v > 256000) then v := 256000;
  if (FBaudRate <> v) then begin
    FBaudRate := v;
    if Opened then begin
      Close;
      Open;
    end;
  end;
end;

function TCommPort.GetOpened: boolean;
begin
  Result := (FPortHandle <> INVALID_HANDLE_VALUE);
end;

procedure TCommPort.Open;
var
  PortName: string;
  tms: TCOMMTIMEOUTS;
begin
  PortName := Format('COM%d', [FPortNum]);
  FPortHandle := CreateFile(PChar(PortName),
                            GENERIC_READ or GENERIC_WRITE,
                            0,
                            nil,
                            OPEN_EXISTING,
                            FILE_ATTRIBUTE_NORMAL,
                            0);
  Win32Check(FPortHandle <> INVALID_HANDLE_VALUE);
  FPortStream := THandleStream.Create(FPortHandle);
  try
    ApplyCOMSettings;
    tms.ReadIntervalTimeout := 1;
    tms.ReadTotalTimeoutMultiplier := 0;
    tms.ReadTotalTimeoutConstant := 1;
    tms.WriteTotalTimeoutMultiplier := 100;
    tms.WriteTotalTimeoutConstant := 1000;
    Win32Check(SetCommTimeOuts(FPortHandle, tms));
  except
    Close;
    raise;
  end;
end;

procedure TCommPort.Close;
begin
  if (FPortHandle <> INVALID_HANDLE_VALUE) then begin
    FPortStream.Free;
    FPortStream := nil;
    CloseHandle(FPortHandle);
    FPortHandle := INVALID_HANDLE_VALUE;
  end;
end;

procedure TCommPort.SendBuff(Buff: PChar; Len: integer);
begin
  FPortStream.WriteBuffer(Buff^, Len);
end;

procedure TCommPort.SendStr(const s: string);
begin
  SendBuff(@s[1], Length(s));
end;

const
  dcb_Binary              = $00000001;
  dcb_ParityCheck         = $00000002;
  dcb_OutxCtsFlow         = $00000004;
  dcb_OutxDsrFlow         = $00000008;
  dcb_DtrControlMask      = $00000030;
    dcb_DtrControlDisable   = $00000000;
    dcb_DtrControlEnable    = $00000010;
    dcb_DtrControlHandshake = $00000020;
  dcb_DsrSensivity        = $00000040;
  dcb_TXContinueOnXoff    = $00000080;
  dcb_OutX                = $00000100;
  dcb_InX                 = $00000200;
  dcb_ErrorChar           = $00000400;
  dcb_NullStrip           = $00000800;
  dcb_RtsControlMask      = $00003000;
    dcb_RtsControlDisable   = $00000000;
    dcb_RtsControlEnable    = $00001000;
    dcb_RtsControlHandshake = $00002000;
    dcb_RtsControlToggle    = $00003000;
  dcb_AbortOnError        = $00004000;
  dcb_Reserveds           = $FFFF8000;

procedure TCommPort.ApplyCOMSettings;
var dcb: TDCB;
begin
  if not Opened then Exit;
  FillChar(dcb, sizeof(dcb), 0);
  dcb.DCBLength := sizeof(dcb);
  dcb.BaudRate := FBaudRate;
  dcb.Flags := dcb_Binary;
  dcb.Flags := dcb.Flags or dcb_DtrControlEnable;
//  dcb.Flags := dcb.Flags or dcb_OutxCtsFlow or dcb_RtsControlHandshake;
//  dcb.Flags := dcb.Flags or dcb_OutX or dcb_InX;
  dcb.XONLim  := 256;
  dcb.XOFFLim := 1;
  dcb.ByteSize := 8;
  dcb.Parity   := 0;
  dcb.StopBits := 0;
  dcb.XONChar  := #17;
  dcb.XOFFChar := #19;
  Win32Check(SetCommState(FPortHandle, dcb));
  FlushBuffers(true, true);
  Win32Check(SetupComm(FPortHandle, 1024, 1024));
end;

procedure TCommPort.FlushBuffers(inBuf, outBuf: boolean);
var dwAction: DWORD;
begin
  if not Opened then Exit;
  dwAction := 0;
  if outBuf then
    dwAction := dwAction or PURGE_TXABORT or PURGE_TXCLEAR;
  if inBuf then
    dwAction := dwAction or PURGE_RXABORT or PURGE_RXCLEAR;
  Win32Check(PurgeComm(FPortHandle, dwAction));
end;

{ =========================================================================== }

end.
