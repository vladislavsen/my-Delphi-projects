unit GameGraph;

interface

uses
  Windows, Graphics, Math2D, Classes;

type
  TRGB = record
  b, g, r: byte;
end;
  ARGB = array [0..1] of TRGB;
  PARGB = ^ARGB;

procedure InternalDrawTransBmpRect(Dest: TCanvas; X, Y: Integer;
  Bitmap: TBitmap; TransparentColor: TColor; GetMask: boolean);
procedure RotateImage(var Dest, Src: TBitmap; RotType: TRotate; RotXAxes, RotYAxes, Angle: word; TransColor: TColor);
procedure DrawSprite(Buffer: HDC; X, Y, Width, Height: integer; Sprite, Mask: HDC; SprX, SprY: integer);
procedure DrawRect(X1, Y1, X2, Y2: integer; Color: TColor; Buffer: TBitmap);

var
  RotKoef: real = 0.5;

implementation

const
{ TBitmap.GetTransparentColor from GRAPHICS.PAS use this value }
  TransparentMask = $02000000;   

procedure DrawTransparentBitmapRect(DC: HDC; Bitmap: HBitmap; xStart, yStart: integer;
  TransparentColor: TColorRef; GetMask: boolean);
var
{$IFDEF WIN32}
  BM: Windows.TBitmap;
{$ELSE}
  BM: WinTypes.TBitmap;
{$ENDIF}
  cColor: TColorRef;
  bmAndBack, bmAndObject, bmAndMem, bmSave: HBitmap;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: HBitmap;
  hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave: HDC;
  ptSize, ptRealSize, ptBitSize, ptOrigin: TPoint;
begin
  hdcTemp := CreateCompatibleDC(DC);  //Контекст устройства
  SelectObject(hdcTemp, Bitmap);  //Занесение в hdcTemp Bitmap
  GetObject(Bitmap, SizeOf(BM), @BM);
  ptRealSize.x := 0;
  ptRealSize.y := 0;
  DPtoLP(hdcTemp, ptRealSize, 1);
  ptOrigin.x := 0;
  ptOrigin.y := 0;
  DPtoLP(hdcTemp, ptOrigin, 1);       { Convert from device  }
                                      { to logical points    }
  ptBitSize.x := BM.bmWidth;  //Ширина Bitmap
  ptBitSize.y := BM.bmHeight;  //Высота Bitmap
  DPtoLP(hdcTemp, ptBitSize, 1);
  if (ptRealSize.x = 0) or (ptRealSize.y = 0) then begin
    ptSize := ptBitSize;
    ptRealSize := ptSize;
  end
  else ptSize := ptRealSize;

  { Create some DCs to hold temporary data }
  hdcBack   := CreateCompatibleDC(DC);
  hdcObject := CreateCompatibleDC(DC);
  hdcMem    := CreateCompatibleDC(DC);
  hdcSave   := CreateCompatibleDC(DC);
  { Create a bitmap for each DC. DCs are required for a number of }
  { GDI functions                                                 }
  { Monochrome DC }
  bmAndBack   := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndMem    := CreateCompatibleBitmap(DC, ptSize.x, ptSize.y);
  bmSave      := CreateCompatibleBitmap(DC, ptBitSize.x, ptBitSize.y);
  { Each DC must select a bitmap object to store pixel data }
  bmBackOld   := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld    := SelectObject(hdcMem, bmAndMem);
  bmSaveOld   := SelectObject(hdcSave, bmSave);
  { Set proper mapping mode }
  SetMapMode(hdcTemp, GetMapMode(DC));
  { Save the bitmap sent here, because it will be overwritten }
  BitBlt(hdcSave, 0, 0, ptBitSize.x, ptBitSize.y, hdcTemp, 0, 0, SRCCOPY);
  { Set the background color of the source DC to the color,         }
  { contained in the parts of the bitmap that should be transparent }
  cColor := SetBkColor(hdcTemp, TransparentColor);
  {Создание обьектной маски для Bitmap при помощи BitBlt}
  {путём занесения исходного Bitmap в монохромный Bitmap}
  BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, ptOrigin.x, ptOrigin.y,
    SRCCOPY);
  { Set the background color of the source DC back to the original  }
  { color                                                           }
  SetBkColor(hdcTemp, cColor);
  {Создание инверсии обьектной маски}
  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0,
    NOTSRCCOPY);
  if not GetMask then
  begin
    {Копирование бэкграунда главного DC в hdcMem}
    BitBlt(hdcMem, 0, 0, ptRealSize.x, ptRealSize.y, DC, xStart, yStart,
      SRCCOPY);
    {Маска выводится на место где находится Bitmap}
    BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);
    BitBlt(hdcTemp, ptOrigin.x, ptOrigin.y, ptSize.x, ptSize.y, hdcBack, 0, 0,
      SRCAND);
    { XOR the bitmap with the background on the destination DC }
    BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, ptOrigin.x, ptOrigin.y,
      SRCPAINT);
    {Копирование результата на экран}
    BitBlt(DC, xStart, yStart, ptRealSize.x, ptRealSize.y,
      hdcMem, 0, 0, SRCCOPY);
  end else
    BitBlt(DC, xStart, yStart, ptRealSize.x, ptRealSize.y,
      hdcBack, 0, 0, SRCCOPY);
  { Place the original bitmap back into the bitmap sent here }
  BitBlt(hdcTemp, 0, 0, ptBitSize.x, ptBitSize.y, hdcSave, 0, 0, SRCCOPY);
  {Удаление Bitmap'ов}
  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  DeleteObject(SelectObject(hdcSave, bmSaveOld));
  {Удаление DCs'ов}
  DeleteDC(hdcMem);
  DeleteDC(hdcBack);
  DeleteDC(hdcObject);
  DeleteDC(hdcSave);
  DeleteDC(hdcTemp);
end;

procedure InternalDrawTransBmpRect(Dest: TCanvas; X, Y: Integer;
  Bitmap: TBitmap; TransparentColor: TColor; GetMask: boolean);
begin
  DrawTransparentBitmapRect(Dest.Handle, Bitmap.Handle, X, Y,
  ColorToRGB(TransparentColor and not TransparentMask), GetMask);
end;

procedure RotateImage(var Dest, Src: TBitmap; RotType: TRotate; RotXAxes, RotYAxes, Angle: word; TransColor: TColor);
label
  Inc;
var
  sx, sy, x1, y1: real;
  i, j, rsx, rsy, rx1, ry1: integer;
  p1, p2: PARGB;
  CosA, SinA: real;
  RValue, GValue, BValue: byte;
begin
  sx := 0;
  sy := 0;
  CosA := CosTable[Angle];
  SinA := SinTable[Angle];
  RValue := GetRValue(TransColor);
  GValue := GetGValue(TransColor);
  BValue := GetBValue(TransColor);
  for i := 0 to Dest.Height - 1 do
  begin
    p1 := Dest.ScanLine[i];
    for j := 0 to Dest.Width - 1 do
    begin
      p1[j].r := RValue;
      p1[j].g := GValue;
      p1[j].b := BValue;
    end;
  end;
  while sy < Src.Height - 1 do
    begin
      rsy := round(sy);
      p2 := Src.ScanLine[rsy];
      while sx < Src.Width - 1 do
      begin
        rsx := round(sx);
        if
        (p2[rsx].r <> RValue) or
        (p2[rsx].g <> GValue) or
        (p2[rsx].b <> BValue) then
        begin
          if RotType = rXY then
          begin
            x1 := CosA * (sx - RotXAxes) - SinA * (sy - RotYAxes) + RotXAxes;
            y1 := SinA * (sx - RotXAxes) + CosA * (sy - RotYAxes) + RotYAxes;
          end;
          if RotType = rX then
          begin
            x1 := CosA * (sx - RotXAxes) + RotXAxes;
            y1 := sy;
          end;
          if RotType = rY then
          begin
            x1 := sx;
            y1 := SinA * (sy - RotYAxes) + RotYAxes;
          end;
          rx1 := round(x1);
          ry1 := round(y1);
          if (ry1 >= Src.Height - 1) or (ry1 < 0) or
          (rx1 >= Src.Width - 1) or (rx1 < 0) then goto inc;
          p1 := Dest.ScanLine[ry1];
          p1[rx1].r := p2[rsx].r;
          p1[rx1].g := p2[rsx].g;
          p1[rx1].b := p2[rsx].b;
        end;
        Inc: sx := sx + RotKoef;
      end;
      sx := 0;
      sy := sy + RotKoef;
    end;
end;

procedure DrawSprite(Buffer: HDC; X, Y, Width, Height: integer; Sprite, Mask: HDC; SprX, SprY: integer);
begin
  BitBlt(Buffer, X, Y, Width, Height, Mask, SprX, SprY, SRCPAINT);
  BitBlt(Buffer, X, Y, Width, Height, Sprite, SprX, SprY, SRCAND);
end;

procedure DrawRect(X1, Y1, X2, Y2: integer; Color: TColor; Buffer: TBitmap);
var
  i, j: integer;
  p: PARGB;
begin
  for i := Y1 to Y2 do
  begin
    if (i < 0) or (i >= Buffer.Height) then exit;
    p := Buffer.ScanLine[i];
    for j := X1 to X2 do
    begin
      if (j < 0) or (j >= Buffer.Width) then continue;
      p[j].r := GetRValue(Color);
      p[j].g := GetGValue(Color);
      p[j].b := GetBValue(Color);
    end;
  end;
end;


end.
