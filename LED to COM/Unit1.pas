unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Com32, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    RedLevel: TTrackBar;
    cbxPortList: TComboBox;
    LabelRed: TLabel;
    GreenLevel: TTrackBar;
    BlueLevel: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelGreen: TLabel;
    LabelBlue: TLabel;
    Label1: TLabel;
    ColorDialog1: TColorDialog;
    Button1: TButton;
    Panel1: TPanel;
   
    
    procedure FormCreate(Sender: TObject);
    procedure RedLevelChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure WriteToComPort(s: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Panel1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  FComPort : TCommPort;
  FCbxList : TList;
  r, g, b: Byte;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FComPort := TCommPort.Create;
  ScanPorts(cbxPortList.Items, 25, False);
  if (cbxPortList.Items.Count = 0) then begin
    ShowMessage('COM ������ �� �������');
  end else begin
  //  ShowMessage('�����');
  end;
  FComPort.BaudRate := 9600;
  StrToIntDef(cbxPortList.Text, 1);
  cbxPortList.ItemIndex:=0;
  FComPort.PortNumber := StrToIntDef(cbxPortList.Text, 1);
//  ShowMessage(cbxPortList.Text);
   WriteToComPort('0G0B0*');
end;


procedure TForm1.RedLevelChange(Sender: TObject);
begin
    WriteToComPort(IntToStr(RedLevel.Position) + 'G' + IntToStr(GreenLevel.Position)+ 'B' +inttostr(BlueLevel.Position) + '*');
    Label1.Caption :='R' + IntToStr(RedLevel.Position) + ' G' + IntToStr(GreenLevel.Position)+ ' B' +inttostr(BlueLevel.Position) + '*';
    LabelRed.Caption := IntToStr(RedLevel.Position);
    LabelGreen.Caption := IntToStr(GreenLevel.Position);
    LabelBlue.Caption := IntToStr(BlueLevel.Position);
    Panel1.Color := RGB(RedLevel.Position,GreenLevel.Position, BlueLevel.Position);
end;

procedure TForm1.Button1Click(Sender: TObject);
  var
  Color: Longint;
begin
   if ColorDialog1.Execute then
   begin
        Color := ColorDialog1.Color;
        r := Color;
        g := Color shr 8;
        b := Color shr 16;
   end;
   WriteToComPort(IntToStr(r) + 'G' + IntToStr(g)+ 'B' +inttostr(b) + '*');
   GreenLevel.Position  := g;
   RedLevel.Position    := r;
   BlueLevel.Position   := b;
   Panel1.Color := RGB(r,g,b);
end;

procedure Tform1.WriteToComPort(s: string);
begin
  FComPort.PortNumber := StrToIntDef(cbxPortList.Text, 1);
  if not FComPort.Opened then begin
    if (cbxPortList.Items.Count = 0) then Exit;
    try
      FComPort.Open;
    except
        ShowMessage('������ �������� COM ����� ');
    end;
  end;

  if FComPort.Opened then try
    FComPort.SendStr(s);
  except
      ShowMessage('������ ������ � COM ����');
  end;

end;
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    WriteToComPort(IntToStr(0) + 'G' + IntToStr(0)+ 'B' +inttostr(0) + '*');
end;

procedure TForm1.Panel1Click(Sender: TObject);
var
  Color: Longint;
begin
   if ColorDialog1.Execute then
   begin
        Color := ColorDialog1.Color;
        r := Color;
        g := Color shr 8;
        b := Color shr 16;
   end;
   WriteToComPort(IntToStr(r) + 'G' + IntToStr(g)+ 'B' +inttostr(b) + '*');
   GreenLevel.Position  := g;
   RedLevel.Position    := r;
   BlueLevel.Position   := b;
   Panel1.Color := RGB(r,g,b);
end;

end.
