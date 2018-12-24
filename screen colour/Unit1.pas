unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
     bmp: TBitmap;
     DC: HDC;
     shotZone : TRect;
     c: TCanvas;
     averageColour: TColor;
     ir, ig, ib, r, g, b: Integer;
     rgbColour: LongInt;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var i,j :Integer;
begin
bmp:=TBitmap.Create;
bmp.Height:=100;
bmp.Width:=Screen.Width;
DC:=GetDC(0);
bitblt(bmp.Canvas.Handle, 0, 0, Screen.Width, Screen.Height, DC, 0, Screen.Height-100, SRCCOPY);
bmp.SaveToFile('D:/IMG001.jpg');
 r :=0; g:=0; b:=0; ir:=0; ig:=0; ib:=0;
for i:= 0 to 70 do
  for j:= 0 to Screen.Width do
  begin

      averageColour := bmp.Canvas.Pixels[j,i];
      rgbColour:= ColorToRGB(averageColour);
      if GetRValue(rgbColour) > 50 then
      begin
      r:= r + GetRValue(rgbColour);
      Inc(ir);
      end;
      if  GetGValue(rgbColour) > 50 then
      begin
      g:= g + GetGValue(rgbColour);
      Inc(ig);
      end;
      if GetBValue(rgbColour)> 50 then
      begin
      b:= b + GetBValue(rgbColour);
      Inc(ib);
      end;
  end;
   r := Round(r / (ir+1));
   g := Round(g / (ig+1));
   b := Round(b / (ib+1));
   Panel1.Color:= RGB(r,g,b);
ReleaseDC(0, DC);
bmp.Free;
end;
end.



