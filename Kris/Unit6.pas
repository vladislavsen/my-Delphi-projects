unit Unit6;

//==============================================================================
// Salut Demo 1.0c                                                             |
// 17.07.2008                                                                   |
//==============================================================================

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Engine_beta1, Explosions, Math2D, ExtCtrls;

type
  TForm6 = class(TForm)
    Timer1: TTimer;
    Timer2: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

  Buf: TBitmap;

implementation

uses Unit1;

{$R *.dfm}

procedure ShowSalut(pexp: PExplosion);
var
  i: word;
  alpha: real;
begin
 // if random * 150 < (pexp^).Counter then
 // begin
    if (pexp^).Life >= 210 then       // For rocket fire
    begin
      AddExp((pexp^).X, (pexp^).Y, -(pexp^).MoveX + (random - random) * 0.5, -(pexp^).MoveY + (random - random) * 0.5,
        100 + random(60), 0, nil);
      if (pexp^).Counter = 1 then
      begin
        for i := 0 to 512 + random(512) do
        begin
          alpha := random * pi * 2;
          AddExp((pexp^).X, (pexp^).Y, cos(alpha) * random * 5, sin(alpha) * random * 5, 40 + random(40), 0, ShowSalut);
        end;
      end;
    end else
    begin                             // For bombs fire
      if (pexp^).Counter = 1 then
        for i := 0 to 12 + random(4) do
        begin
          alpha := random * pi * 2;
          AddExp((pexp^).X, (pexp^).Y, cos(alpha) * random * 5, sin(alpha) * random * 5, 0 + random(3), 0, nil);
        end;
    end;
 // end;
 { if ((pexp^).Life >= 210) and ((pexp^).Counter = 1) then
  begin
    for i := 0 to 16 + random(16) do
    begin
      alpha := random * pi * 2;
      AddExp((pexp^).X, (pexp^).Y, cos(alpha) * random * 5, sin(alpha) * random * 5, 20 + random(10), 0, ShowSalut);
    end;
  end else
    if ((pexp^).Life >= 8) and ((pexp^).Counter = 1) then
    begin
      for i := 0 to 3 do
      begin
        alpha := random * pi * 2;
        AddExp((pexp^).X, (pexp^).Y, cos(alpha) * random * 15, sin(alpha) * random * 15, random(10), 0, ShowSalut);
      end;
    end;      }
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
   randomize;
  SetMaxAngle(360);
  InitSinCosTables;
  CalcSinCosTables;

  Buf := TBitmap.Create;
  Buf.Width := Form6.ClientWidth;
  Buf.Height := Form6.ClientHeight;

  InitExp;

end;

procedure TForm6.FormDestroy(Sender: TObject);
begin
  Buf.Free;
  DestroyExp;
end;

procedure TForm6.Timer1Timer(Sender: TObject);
begin
  PatBlt(Buf.Canvas.Handle, 0, 0, Buf.Width, Buf.Height, BLACKNESS);

  ClearOldExp;
  DrawExp(Buf);

  BitBlt(Form6.Canvas.Handle, 0, 0, Buf.Width, Buf.Height, Buf.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TForm6.Timer2Timer(Sender: TObject);
var
  alpha: real;
begin
  alpha := pi / 4 + random * pi / 2;
  AddExp(20 + random(Form6.ClientWidth - 20), Form6.ClientHeight - 20,
    cos(alpha) * random * 3, -9 - random * 3, 210 + random(100), 0, ShowSalut);
  Timer2.Interval := 2000 + random(2000);
end;

procedure TForm6.FormResize(Sender: TObject);
begin
  Buf.Width := Form6.ClientWidth;
  Buf.Height := Form6.ClientHeight;
end;

procedure TForm6.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (key = #27)
  then begin
  Form6.Close;
  Form1.close;
  end;
end;

initialization


end.
