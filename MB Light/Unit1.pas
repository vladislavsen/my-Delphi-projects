unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Com32, StdCtrls, ExtCtrls;

type

    TMyThread = class(TThread)
    widthZoneStart, widthZoneEnd, heightZone  : Integer;
    PBmp: ^TBitmap;
    result : TColor;
    private
    protected
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    cbxPortList: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    Panel1: TPanel;
    Timer1: TTimer;
    Label2: TLabel;
    edtColorChangeAccur: TEdit;
    Label3: TLabel;
    edtRereadDelay: TEdit;
    Label4: TLabel;
    edtHeightZone: TEdit;
    Timer2: TTimer;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
   

    procedure FormCreate(Sender: TObject);


    procedure WriteToComPort(s: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure edtHeightZoneChange(Sender: TObject);
    procedure edtColorChangeAccurChange(Sender: TObject);
    procedure edtRereadDelayChange(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
  public
     bmp: TBitmap;
     DC: HDC;
     shotZone : TRect;
     c: TCanvas;
     averageColour: TColor;
     ir, ig, ib, r, g, b, oldR, oldG, oldB, size, ledsCount: Integer;
     rgbColour: LongInt;
     heightZone, rereadDelay, colourAccur: Integer;
     averageColourArray: array[1..20] of TColor; getColourProcArr: array[1..10] of TMyThread;
  end;




var
  Form1: TForm1;
  FComPort : TCommPort;
  FCbxList : TList;
  GetColourThread: TMyThread;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  heightZone:= StrToInt(edtheightZone.Text);
  size := heightZone * Screen.Width;
  colourAccur:= StrToInt(edtColorChangeAccur.Text);
  rereadDelay:= StrToInt(edtRereadDelay.Text);
  Timer1.Interval:= rereadDelay;
  oldR:= 0; oldG:=0; oldB:=0;
  ledsCount:=4;
  FComPort := TCommPort.Create;
  ScanPorts(cbxPortList.Items, 25, False);
  if (cbxPortList.Items.Count = 0) then
  begin
    ShowMessage('COM портов не найдено');
  end
  else
  begin
  //  ShowMessage('Готов');
  end;
  FComPort.BaudRate := 9600;
  StrToIntDef(cbxPortList.Text, 1);
  cbxPortList.ItemIndex:=0;
  FComPort.PortNumber := StrToIntDef(cbxPortList.Text, 1);
//  ShowMessage(cbxPortList.Text);
   WriteToComPort('0G0B0*');
   oldR :=0; oldG:=0; oldB :=0;
end;



procedure Tform1.WriteToComPort(s: string);
begin
  FComPort.PortNumber := StrToIntDef(cbxPortList.Text, 1);
  if not FComPort.Opened then
  begin
    if (cbxPortList.Items.Count = 0) then Exit;
    try
      FComPort.Open;
    except
        ShowMessage('Ошибка открытия COM порта ');
    end;
  end;

  if FComPort.Opened then
  try
    FComPort.SendStr(s);
  except
      ShowMessage('Ошибка записи в COM порт');
  end;

end;
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    WriteToComPort(IntToStr(0) + 'G' + IntToStr(0)+ 'B' +inttostr(0) + '*');
end;


procedure TForm1.Timer1Timer(Sender: TObject);
var i,j :Integer;
begin
size:= heightZone * Screen.Width;
bmp:=TBitmap.Create;
bmp.Height:=heightZone;
bmp.Width:=Screen.Width;
DC:=GetDC(0);
bitblt(bmp.Canvas.Handle, 0, 0, Screen.Width, Screen.Height, DC, 0, Screen.Height-heightZone, SRCCOPY);
//bmp.SaveToFile('D:/Screen.jpg');
 r :=0; g:=0; b:=0; ir:=0; ig:=0; ib:=0; i:=0; j:=0;
for  i:=0 to round(heightZone/2) do
 begin
  for j:=0 to Round(Screen.Width/2) do
  begin
      averageColour := bmp.Canvas.Pixels[2*j,2*i];
      rgbColour:= ColorToRGB(averageColour);

      r:= r + GetRValue(rgbColour);
      g:= g + GetGValue(rgbColour);
      b:= b + GetBValue(rgbColour);
     // inc(j);
  end;
  //Inc(i);
 end;

   r := Round(r / (size/4));
   g := Round(g / (size/4));
   b := Round(b / (size/4));
   if (abs(r - oldr) > colourAccur)or(abs(g - oldg) > colourAccur)or(abs(b - oldb) > colourAccur) then
   begin
   WriteToComPort(IntToStr(r) + 'G' + IntToStr(g)+ 'B' +inttostr(b) + '*');
   Panel1.Caption := IntToStr(r) + 'G' + IntToStr(g)+ 'B' +inttostr(b);
   Panel1.Color:= RGB(r,g,b);
   oldR:=r; oldG:=g; oldB:=b;
   end;
ReleaseDC(0, DC);
bmp.Free;
end;

procedure TForm1.edtHeightZoneChange(Sender: TObject);
begin
 try
   if StrToInt(edtheightZone.Text) > 0 then
   heightZone:= StrToInt(edtheightZone.Text)
   else
   heightZone:=1;
 except
   ShowMessage('Введите целое значение');
end;
end;

procedure TForm1.edtColorChangeAccurChange(Sender: TObject);
begin
  try
   colourAccur:= StrToInt(edtColorChangeAccur.Text);
  except
   ShowMessage('Введите целое значение');
end;

end;

procedure TForm1.edtRereadDelayChange(Sender: TObject);
begin
  try
    rereadDelay:= StrToInt(edtRereadDelay.Text);
     except
   ShowMessage('Введите целое значение');
end;
end;

procedure TMyThread.Execute;
var    i, j, r, g, b, size: Integer;
begin
r :=0; g:=0; b:=0; size:=(widthZoneEnd-widthZoneStart)*heightZone;
for  i:=0 to heightZone do
 begin
  for j:=widthZoneStart to widthZoneEnd do
  begin
      r:= r + GetRValue(ColorToRGB(PBmp^.Canvas.Pixels[j,i]));
      g:= g + GetGValue(ColorToRGB(PBmp^.Canvas.Pixels[j,i]));
      b:= b + GetBValue(ColorToRGB(PBmp^.Canvas.Pixels[j,i]));

  end;

 end;

   r := Round(r / size);
   g := Round(g / size);
   b := Round(b / size);

   result:= RGB(r,g,b);

end;




procedure TForm1.Timer2Timer(Sender: TObject);
var i,j :Integer; isReady: boolean;
begin
size:= heightZone * Screen.Width;
  DC:=GetDC(0);
  bmp:=TBitmap.Create;
  bmp.Height:=heightZone;
  bmp.Width:=Screen.Width;
bitblt(bmp.Canvas.Handle, 0, 0, Screen.Width, heightZone, DC, 0, Screen.Height-heightZone, SRCCOPY);
//bmp.SaveToFile('D:/Screen.jpg');

for i:= 1 to ledsCount do
  begin
     getColourProcArr[i] := TMyThread.Create(true);
     getColourProcArr[i].widthZoneStart:= (Round(Screen.Width/ledsCount))*i - (Round(Screen.Width/ledsCount));
     getColourProcArr[i].widthZoneEnd:= (Round(Screen.Width/ledsCount))* i;
     getColourProcArr[i].PBmp := @bmp;
     getColourProcArr[i].heightZone:= heightZone;
     getColourProcArr[i].Priority:=tpNormal;
     getColourProcArr[i].Execute;
  end;

  while not(isReady) do
  begin
    isReady := getColourProcArr[1].Terminated;
    for i:=1 to  ledsCount do
    begin
         isReady :=isReady AND getColourProcArr[i].Terminated;
    end;
  end;

  for i:=1 to  ledsCount do
  begin
       averageColourArray[i] := getColourProcArr[i].result;
       getColourProcArr[i].Destroy;
  end;

  Panel2.Color:=  averageColourArray[1];
  Panel3.Color:=  averageColourArray[2];
  Panel4.Color:=  averageColourArray[3];
  Panel5.Color:=  averageColourArray[4];



ReleaseDC(0, DC);
bmp.Free;
end;

end.
