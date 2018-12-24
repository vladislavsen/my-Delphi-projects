unit Engine_beta1;

interface

uses
  Windows, Graphics, jpeg, SysUtils, GameGraph, Math2D;

type
  TOrient = (onUnknow, onTop, onLeft, onBottom, onRight);

type
  TCollisionParams = record
  Collisioned: boolean;
  Orient: TOrient;
end;

type
  TSprite = class(TObject)
private
  FRes: array of TBitmap;  //Ресурсы
  FMask: array of TBitmap;  //Маски ресурсов
  FProgMask: boolean;  //Использовать программную маску?
  FRotSprite, FTempSprite: TBitmap;    //"Кручёный" спрайт, Результирующий спрайт
  FWidth, FHeight: word;  //Ширина, высота спрайта
  FX, FY: real;  //Координаты спрайта
  FSwapX, FSwapY: boolean;  //Залетел в один конец,а вылетел из другого
  FMoveAngle: real;  //Угол вектора направления
  FMoveSpeed: real;  //Скорость перемещения
  FRotated: boolean;  //При коллизии производит чтение или из FRotSprite или из FRes[FFramePos]
  FRotAngle: real;  //Угол вращения изображения
  FRotX, FRotY: word; //Координаты осей вращения изображения ИМХО (RotX >= 0) or (RotX <= FWidth) и тд.
  FAnimated: boolean;  //Анимировать?
  FNumFrames: word;  //Кол-во кадров
  FFramePos: integer;  //Текущий кадр, предыдущий кадр
  FAnimStart, FAnimEnd: word;  //Кадры начала и конца анимации
  FAnimSleep, FAnimCount: byte;  //Замедление и счётчик анимации
  FAnimStep: shortint;  //Шаг анимации
  FTransparentColor: TColor;  //Прозрачный цвет
  FPixelCollision: boolean;  //Использовать попиксельную коллизию?
  FLastOrient: TOrient;  //Предыдущее столкновение
  FVisible: boolean;  //Прорисовывать?
  FID: word;  //Индивидуальный номер
  procedure SetWidth(W: word);
  procedure SetHeight(H: word);
  procedure SetMaskLen(Len: word);
  procedure SetResLen(Len: word);
  procedure SetNumFrames(NumFrames: word);
  procedure SetFramePos(FramePos: integer);
  procedure SetAnimSleep(Sleep: byte);
  procedure SetAngle(var DestAngle: real; Angle: real);
  procedure SetRotAngle(Angle: real);
  procedure SetMoveAngle(Angle: real);
  procedure BmpInSpr(Pos: word; Bmp: TBitmap; InRes: boolean);  Overload;
  procedure BmpInSpr(Pos: word; FileName: string; InRes: boolean); Overload;
  function MaskLen: word;
  function ResLen: word;
public
  constructor Create;
  property ProgMask: boolean read FProgMask write FProgMask;
  property RotSprite: TBitmap read FRotSprite write FRotSprite;
  property TempSprite: TBitmap read FTempSprite write FTempSprite;
  property Width: word read FWidth write SetWidth;
  property Height: word read FHeight write SetHeight;
  property X: real read FX write FX;
  property Y: real read FY write FY;
  property SwapX: boolean read FSwapX write FSwapX;
  property SwapY: boolean read FSwapY write FSwapY;
  property MoveAngle: real read FMoveAngle write SetMoveAngle;
  property MoveSpeed: real read FMoveSpeed write FMoveSpeed;
  property Rotated: boolean read FRotated write FRotated;
  property RotAngle: real read FRotAngle write SetRotAngle;
  property RotX: word read FRotX write FRotX;
  property RotY: word read FRotY write FRotY;
  property MaskLength: word read MaskLen write SetMaskLen;
  property ResLength: word read ResLen write SetResLen;
  property Animated: boolean read FAnimated write FAnimated;
  property NumFrames: word read FNumFrames write SetNumFrames;
  property FramePos: integer read FFramePos write SetFramePos;
  property AnimStart: word read FAnimStart write FAnimStart;
  property AnimEnd: word read FAnimEnd write FAnimEnd;
  property AnimStep: shortint read FAnimStep write FAnimStep;
  property AnimSleep: byte read FAnimSleep write SetAnimSleep;
  property TransparentColor: TColor read FTransparentColor write FTransparentColor;
  property PixelCollision: boolean read FPixelCollision write FPixelCollision;
  property Visible: boolean read FVisible write FVisible;
  property ID: word read FID write FID;
  procedure SetParams(ResFileName, MaskFileName: string; Width, Height: word; TransColor: TColor; bID: byte);
  procedure CreateMask(Pos: word; Bmp: TBitmap);
  procedure BmpInMask(Pos: word; Bmp: TBitmap);  Overload;
  procedure BmpInMask(Pos: word; MaskFileName: string); Overload;
  procedure BmpInRes(Pos: word; Bmp: TBitmap);  Overload;
  procedure BmpInRes(Pos: word; ResFileName: string); Overload;
  procedure Draw(Buffer: TBitmap);
  procedure Move;  //Передвинуть
  procedure Rotate(RotType: TRotate);  //Вращение спрайта
  function Res(Pos: word): TBitmap;
  function Mask(Pos: word): TBitmap;
  function Collision(Spr: TSprite): TCollisionParams;  //Проверка коллизий
  destructor Destroy; Override;
end;

type
  TSpriteList = class
private
  SprList: array of TSprite;  //Массив спрайтов
  FListLength: word;
  FNumSpr: word;
  procedure SetListLength(ListLength: word);
public
  constructor Create(ListLength: word);
  property NumSpr: word read FNumSpr;
  property ListLength: word read FListLength write SetListLength;
  function Spr(Pos: word): TSprite;
  procedure SprCreate(Pos: word);
  procedure SprDestroy(Pos: word);
  procedure Draw(Buffer: TBitmap);
  destructor Destroy; Override;
end;

implementation

{ TSpriteList }

constructor TSpriteList.Create(ListLength: word);
begin
  FListLength := ListLength;
  SetLength(SprList, ListLength + 1);
end;

destructor TSpriteList.Destroy;
var
  i: integer;
begin
  for i := 0 to FListLength do
    if FNumSpr > 0 then
    begin
      if SprList[i] <> nil then
      begin
        SprList[i].Destroy;
        dec(FNumSpr);
      end;
    end else break;
  inherited;
end;

procedure TSpriteList.Draw(Buffer: TBitmap);
var
  i: integer;
  Counter: word;
begin
  Counter := 0;
  for i := 0 to FListLength do
    if Counter < FNumSpr then
    begin
      if SprList[i] <> nil then
      begin
        SprList[i].Draw(Buffer);
        inc(Counter);
      end;
    end else break;
end;

function TSpriteList.Spr(Pos: word): TSprite;
begin
  Result := SprList[Pos];
end;

procedure TSpriteList.SprCreate(Pos: word);
begin
  if SprList[Pos] = nil then
  begin
    SprList[Pos] := TSprite.Create;
    inc(FNumSpr);
  end;
end;

procedure TSpriteList.SprDestroy(Pos: word);
begin
  if SprList[Pos] <> nil then
  begin
    SprList[Pos].Destroy;
    SprList[Pos] := nil;
    dec(FNumSpr);
  end;
end;

{ TSprite }

constructor TSprite.Create;
begin
  FRotSprite := TBitmap.Create;
  FRotSprite.PixelFormat := pf24bit;
  FTempSprite := TBitmap.Create;
  FVisible := true;
  FAnimStep := 1;
  FAnimSleep := 1;
end;

destructor TSprite.Destroy;
var
  i: word;
begin
  if High(FRes) > 0 then
    for i := 0 to High(FRes) do
      FRes[i].Free;
  if High(FMask) > 0 then
    for i := 0 to High(FMask) do
      FMask[i].Free;
  FRotSprite.Free;
  FTempSprite.Free;
  inherited;
end;

procedure TSprite.SetFramePos(FramePos: integer);
var
  TempAnimStart,
  TempAnimEnd: word;
begin
  FFramePos := FramePos;
  if FAnimStart <= FAnimEnd then
  begin
    TempAnimStart := FAnimStart;
    TempAnimEnd := FAnimEnd;
  end else
  begin
    TempAnimStart := FAnimEnd;
    TempAnimEnd := FAnimStart;
  end;
  if TempAnimEnd - TempAnimStart <> 0 then
  begin
    if FramePos < TempAnimStart then
      FFramePos := TempAnimEnd + (FramePos - TempAnimStart) + 1
    else
      if FramePos > TempAnimEnd then
        FFramePos := TempAnimStart + (FramePos - TempAnimEnd) - 1
      else
        FFramePos := FramePos;
  end else FFramePos := TempAnimStart;
  BitBlt(FTempSprite.canvas.Handle, 0, 0, FWidth, FHeight,
    FRes[FFramePos].canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TSprite.Draw(Buffer: TBitmap);
var
  rX, rY: integer;
begin
  rX := round(FX);
  rY := round(FY);
  if FVisible then
  begin
    if FRotated then
      BitBlt(FTempSprite.canvas.handle, 0, 0, FWidth, FHeight,
        FRotSprite.canvas.handle, 0, 0, SRCCOPY);
    if FTransparentColor <> clNone then
    begin
      if FProgMask = true then
        InternalDrawTransBmpRect(Buffer.canvas, rX, rY, FTempSprite, FTransparentColor, false)
      else
        DrawSprite(Buffer.canvas.handle, rX, rY, FWidth, FHeight,
          FTempSprite.canvas.handle, FMask[FFramePos].canvas.handle, 0, 0);
    end else BitBlt(Buffer.canvas.handle, rX, rY, FWidth, FHeight, FTempSprite.canvas.handle, 0, 0, SrcCopy);
    if FSwapX = true then
    begin
      if rX >= Buffer.Width then FX := 0;
      if rX <= - FWidth then FX := Buffer.Width - FWidth;
      if rX + FWidth > Buffer.Width then
        if FTransparentColor <> clNone then
        begin
          if FProgMask = true then
            InternalDrawTransBmpRect(Buffer.canvas, - (Buffer.Width - rX), rY, FTempSprite, FTransparentColor, false)
          else
            DrawSprite(Buffer.canvas.handle, - (Buffer.Width - rX), rY, FWidth, FHeight,
              FTempSprite.canvas.handle, FMask[FFramePos].canvas.handle, 0, 0);
        end else BitBlt(Buffer.canvas.handle, - (Buffer.Width - rX), rY, FWidth, FHeight, FTempSprite.canvas.handle, 0, 0, SrcCopy);
      if rX < 0 then
        if FTransparentColor <> clNone then
        begin
          if FProgMask = true then
            InternalDrawTransBmpRect(Buffer.canvas, Buffer.Width + rX, rY, FTempSprite, FTransparentColor, false)
          else
            DrawSprite(Buffer.canvas.handle, Buffer.Width + rX, rY, FWidth, FHeight,
              FTempSprite.canvas.handle, FMask[FFramePos].canvas.handle, 0, 0);
        end else BitBlt(Buffer.canvas.handle, Buffer.Width + rX, rY, FWidth, FHeight, FTempSprite.canvas.handle, 0, 0, SrcCopy);
    end;
    if FSwapY = true then
    begin
      if rY + FHeight >= Buffer.Height + FHeight then FY := 0;
      if rY <= - FHeight then FY := Buffer.Height - FHeight;
      if rY + FHeight > Buffer.Height then
        if FTransparentColor <> clNone then
        begin
          if FProgMask = true then
            InternalDrawTransBmpRect(Buffer.canvas, rX, - (Buffer.Height - rY), FTempSprite, FTransparentColor, false)
          else
            DrawSprite(Buffer.canvas.handle, rX, - (Buffer.Height - rY), FWidth, FHeight,
              FTempSprite.canvas.handle, FMask[FFramePos].canvas.handle, 0, 0);
        end else BitBlt(Buffer.canvas.handle, rX, - (Buffer.Height - rY), FWidth, FHeight, FTempSprite.canvas.handle, 0, 0, SrcCopy);
      if rY < 0 then
        if FTransparentColor <> clNone then
        begin
          if FProgMask = true then
            InternalDrawTransBmpRect(Buffer.canvas, rX, Buffer.Height + rY, FTempSprite, FTransparentColor, false)
          else
            DrawSprite(Buffer.canvas.handle, rX, Buffer.Height + rY, FWidth, FHeight,
              FTempSprite.canvas.handle, FMask[FFramePos].canvas.handle, 0, 0);
        end else BitBlt(Buffer.canvas.handle, rX, Buffer.Height + rY, FWidth, FHeight, FTempSprite.canvas.handle, 0, 0, SrcCopy);
    end;
    if FAnimated then
      if (FAnimCount mod FAnimSleep) = 0 then
      begin
        if FAnimEnd > FAnimStart then
          SetFramePos(FFramePos + FAnimStep);
        if FAnimEnd < FAnimStart then
          SetFramePos(FFramePos - FAnimStep);
        if FAnimEnd = FAnimStart then
          SetFramePos(FAnimStart);
        FAnimCount := 1;
      end else inc(FAnimCount);
  end;
end;

procedure TSprite.Rotate(RotType: TRotate);
begin
  RotateImage(FRotSprite, FRes[FFramePos], RotType, FRotX, FRotY,
    round(FRotAngle), FTransparentColor);
end;

procedure TSprite.Move;
begin
  if FMoveSpeed <> 0 then
    MovePoint(FX, FY, FMoveAngle, FMoveSpeed);
end;

function TSprite.Collision(Spr: TSprite): TCollisionParams;
var
  StartX1, StartX2, StartY1, StartY2, EndX1, EndY1: real;
  i, j, x2, y2,
  RStartX1, RStartX2, RStartY1, RStartY2,
  REndX1, REndY1: integer;
  p1, p2: PARGB;
  RValue1, GValue1, BValue1,
  RValue2, GValue2, BValue2: TColor;
begin
  Result.Collisioned := false;
  if (Self <> nil) and (FVisible = true) then
  begin
    RValue1 := GetRValue(FTransparentColor);
    GValue1 := GetGValue(FTransparentColor);
    BValue1 := GetBValue(FTransparentColor);
    if (Spr <> nil) and (Spr <> self) and (Spr.FVisible = true) then
    begin
      RValue2 := GetRValue(Spr.FTransparentColor);
      GValue2 := GetGValue(Spr.FTransparentColor);
      BValue2 := GetBValue(Spr.FTransparentColor);
      if (FX + FWidth > Spr.FX) and (FX < Spr.FX + Spr.FWidth) and
      (FY + FHeight > Spr.FY) and (FY < Spr.FY + Spr.FHeight) then
      begin
        Result.Orient := onUnknow;
        if ((FX + FWidth >= Spr.FX) and (FX <= Spr.FX + Spr.FWidth)) and
        (((FY <= Spr.FY) and (FY + FHeight >= Spr.FY)) or
        ((FY <= Spr.FY + Spr.FHeight) and (FY + FHeight >= Spr.FY + Spr.FHeight))) then
        begin
          if Spr.FY + Spr.FHeight div 2 - (FY + (FHeight div 2)) > 0 then Result.Orient := onTop;
          if Spr.FY + Spr.FHeight div 2 - (FY + (FHeight div 2)) < 0 then Result.Orient := onBottom;
          FLastOrient := Result.Orient;
        end;
        if ((FY >= Spr.FY) and (FY + FHeight <= Spr.FY + Spr.FHeight)) and
        (((FX <= Spr.FX) and (FX + FWidth >= Spr.FX)) or
        ((FX <= Spr.FX + Spr.FWidth) and (FX + FWidth >= Spr.FX + Spr.FWidth))) then
        begin
          if Spr.FX + Spr.FWidth div 2 - (FX + (FWidth div 2)) > 0 then Result.Orient := onLeft;
          if Spr.FX + Spr.FWidth div 2 - (FX + (FWidth div 2)) < 0 then Result.Orient := onRight;
          FLastOrient := Result.Orient;
        end;
        Result.Orient := FLastOrient;
        if PixelCollision = false then
        begin
          Result.Collisioned := true;
          exit;
        end;
        if (FX + FWidth > Spr.FX) and (FX < Spr.FX + Spr.FWidth)
        and (FY + FHeight > Spr.FY) and (FY < Spr.FY + Spr.FHeight) then
        begin
          if FX <= Spr.FX then
            StartX1 := Spr.FX - FX
          else StartX1 := 0;
          if FY <= Spr.FY then
            StartY1 := Spr.FY - FY
          else StartY1 := 0;
          if FX + FWidth > Spr.FX + Spr.FWidth then
            EndX1 := Spr.FX + Spr.FWidth - FX
          else EndX1 := FWidth;
          if FY + FHeight > Spr.FY + Spr.FHeight then
            EndY1 := Spr.FY + Spr.FHeight - FY
          else EndY1 := FHeight;
        end;
        if StartX1 > 0 then StartX2 := 0;
        if StartX1 = 0 then StartX2 := FX - Spr.FX;
        if StartY1 > 0 then StartY2 := 0;
        if StartY1 = 0 then StartY2 := FY - Spr.FY;
        RStartX1 := round(StartX1);
        RStartX2 := round(StartX2);
        RStartY1 := round(StartY1);
        RStartY2 := round(StartY2);
        REndX1 := round(EndX1);
        REndY1 := round(EndY1);
        X2 := RStartX2;
        Y2 := RStartY2;
      for i := RStartY1 to REndY1 - 1 do
      begin
        if FRotated then
        begin
          p1 := FRotSprite.ScanLine[i];
          p2 := Spr.FRotSprite.ScanLine[Y2];
        end else
        begin
          p1 := FRes[FFramePos].ScanLine[i];
          p2 := Spr.FRes[Spr.FFramePos].ScanLine[Y2];
        end;
        for j := RStartX1 to REndX1 - 1 do
        begin
          if ((p1[j].r <> RValue1) or
          (p1[j].g <> GValue1) or
          (p1[j].b <> BValue1)) and
          ((p2[X2].r <> RValue2) or
          (p2[X2].g <> GValue2) or
          (p2[X2].b <> BValue2)) then
          begin
            Result.Collisioned := true;
            exit;
          end;
          X2 := X2 + 1;
        end;
        X2 := RStartX2;
        Y2 := Y2 + 1;
      end;
    end;
  end;
end;
end;

procedure TSpriteList.SetListLength(ListLength: word);
begin
  SetLength(SprList, FListLength);
end;

procedure TSprite.SetParams(ResFileName, MaskFileName: string; Width, Height: word; TransColor: TColor; bID: byte);
var
  i: word;
  bRes, bMask: TBitmap;
  jRes, jMask: TJpegImage;
begin
  bRes := TBitmap.Create;
  jRes := TJpegImage.Create;
  if FileExists(ResFileName) then
    if ExtractFileExt(ResFileName) = '.jpg' then
    begin
      jRes.LoadFromFile(ResFileName);
      bRes.Assign(jRes);
    end else bRes.LoadFromFile(ResFileName);
  bMask := TBitmap.Create;
  jMask := TJpegImage.Create;
  if FileExists(MaskFileName) then
    if ExtractFileExt(MaskFileName) = '.jpg' then
    begin
      jMask.LoadFromFile(MaskFileName);
      bMask.Assign(jMask);
    end else bMask.LoadFromFile(MaskFileName);
  FWidth := Width;
  FHeight := Height;
  if bRes.Width < FWidth then FWidth := bRes.Width;
  if bRes.Height < FHeight then FHeight := bRes.Height;
  FNumFrames := bRes.Width div FWidth;
  FAnimEnd := FNumFrames - 1;
  FTransparentColor := TransColor;
  SetLength(FRes, FNumFrames);
  SetLength(FMask, FNumFrames);
  for i := 0 to FNumFrames - 1 do
  begin
    FRes[i] := TBitmap.Create;
    FRes[i].PixelFormat := pf24bit;
    FRes[i].Width := FWidth;
    FRes[i].Height := FHeight;
    BitBlt(FRes[i].canvas.handle, 0, 0, FWidth, FHeight,
      bRes.canvas.handle, FWidth * i, 0, SRCCOPY);
    FMask[i] := TBitmap.Create;
    FMask[i].Width := FWidth;
    FMask[i].Height := FHeight;
    if (MaskFileName <> '') and (TransColor <> clNone) then
      BitBlt(FMask[i].canvas.handle, 0, 0, FWidth, FHeight,
        bMask.canvas.handle, FWidth * i, 0, SRCCOPY)
    else
      CreateMask(i, FRes[i]);
  end;
  FRotSprite.Width := FWidth;
  FRotSprite.Height := FHeight;
  SetFramePos(0);
  BitBlt(FRotSprite.canvas.handle, 0, 0, FWidth, FHeight,
    FRes[FFramePos].canvas.handle, 0, 0, SRCCOPY);
  FTempSprite.Width := FWidth;
  FTempSprite.Height := FHeight;
  BitBlt(FTempSprite.canvas.handle, 0, 0, FWidth, FHeight,
    FRes[FFramePos].canvas.handle, 0, 0, SRCCOPY);
  FAnimCount := 1;
  FID := bID;
  bRes.Free;
  bMask.Free;
  jRes.Free;
  jMask.Free;
end;

procedure TSprite.SetMaskLen(Len: word);
var
  i: integer;
begin
  SetLength(FMask, Len);
  for i := 0 to Len - 1 do
    if FMask[i] = nil then
    begin
      FMask[i] := TBitmap.Create;
      FMask[i].Width := FWidth;
      FMask[i].Height := FHeight;
    end;
end;

procedure TSprite.BmpInMask(Pos: word; Bmp: TBitmap);
begin
  BmpInSpr(Pos, Bmp, false);
end;

procedure TSprite.BmpInMask(Pos: word; MaskFileName: string);
begin
  BmpInSpr(Pos, MaskFileName, false);
end;

function TSprite.Res(Pos: word): TBitmap;
begin
  if Pos > High(FRes) then Pos := High(FRes);
  Result := FRes[Pos];
end;

procedure TSprite.SetResLen(Len: word);
var
  i: integer;
begin
  SetLength(FRes, Len);
  for i := 0 to Len - 1 do
    if FRes[i] = nil then
    begin
      FRes[i] := TBitmap.Create;
      FRes[i].PixelFormat := pf24bit;
      FRes[i].Width := FWidth;
      FRes[i].Height := FHeight;
    end;
end;

procedure TSprite.SetNumFrames(NumFrames: word);
begin
  FNumFrames := NumFrames;
  FAnimEnd := NumFrames - 1;
end;

function TSprite.Mask(Pos: word): TBitmap;
begin
  if Pos > High(FMask) then Pos := High(FMask);
  Result := FMask[Pos];
end;

procedure TSprite.SetHeight(H: word);
var
  i: word;
begin
  FHeight := H;
  for i := 0 to High(FRes) do
  begin
    FRes[i].Height := H;
    FMask[i].Height := H;
  end;
  FRotSprite.Height := H;
  FTempSprite.Height := H;
end;

procedure TSprite.SetWidth(W: word);
var
  i: word;
begin
  FWidth := W;
  for i := 0 to High(FRes) do
  begin
    FRes[i].Width := W;
    FMask[i].Width := W;
  end;
  FRotSprite.Width := W;
  FTempSprite.Width := W;
end;

function TSprite.MaskLen: word;
begin
  Result := Length(FMask);
end;

function TSprite.ResLen: word;
begin
  Result := Length(FRes);
end;

procedure TSprite.SetAnimSleep(Sleep: byte);
begin
  if Sleep = 0 then Sleep := 1;
  FAnimSleep := Sleep;
end;

procedure TSprite.SetAngle(var DestAngle: real; Angle: real);
begin
  if Angle > MaxAngle then Angle := Angle - MaxAngle;
  if Angle < 0 then Angle := MaxAngle + Angle;
  DestAngle := Angle;
end;

procedure TSprite.SetMoveAngle(Angle: real);
begin
  SetAngle(FMoveAngle, Angle);
end;

procedure TSprite.SetRotAngle(Angle: real);
begin
  SetAngle(FRotAngle, Angle);
end;

procedure TSprite.BmpInSpr(Pos: word; Bmp: TBitmap; InRes: boolean);
var
  i, endW: word;
begin
  endW := round(Bmp.Width / FWidth - 1);
  if InRes then
    if Pos + endW >= ResLen then
    begin
      SetResLen(Pos + endW + 1);
      SetNumFrames(Pos + endW + 1);
    end;
  if not InRes then
    if Pos + endW >= MaskLen then
    begin
      SetMaskLen(Pos + endW + 1);
      SetNumFrames(Pos + endW + 1);
    end;
  for i := 0 to endW do
  begin
    if InRes then
      BitBlt(FRes[Pos + i].canvas.handle, 0, 0, FRes[Pos + i].Width, FRes[Pos + i].Height,
        Bmp.canvas.handle, FWidth * i, 0, SRCCOPY)
    else
      BitBlt(FMask[Pos + i].canvas.handle, 0, 0, FMask[Pos + i].Width, FMask[Pos + i].Height,
        Bmp.canvas.handle, FWidth * i, 0, SRCCOPY);
  end;
end;

procedure TSprite.BmpInSpr(Pos: word; FileName: string; InRes: boolean);
var
  jSpr: TJpegImage;
  bSpr: TBitmap;
begin
  bSpr := TBitmap.Create;
  jSpr := TJpegImage.Create;
  if FileExists(FileName) then
    if ExtractFileExt(FileName) = '.jpg' then
    begin
      jSpr.LoadFromFile(FileName);
      bSpr.Assign(jSpr);
    end else bSpr.LoadFromFile(FileName);
  if InRes then
    BmpInSpr(Pos, bSpr, true)
  else
    BmpInSpr(Pos, bSpr, false);
  bSpr.Free;
  jSpr.Free;
end;

procedure TSprite.BmpInRes(Pos: word; Bmp: TBitmap);
begin
  BmpInSpr(Pos, Bmp, true);
end;

procedure TSprite.BmpInRes(Pos: word; ResFileName: string);
begin
  BmpInSpr(Pos, ResFileName, true);
end;

procedure TSprite.CreateMask(Pos: word; Bmp: TBitmap);
begin
  if Pos >= MaskLen then
  begin
    SetMaskLen(Pos + 1);
    SetNumFrames(Pos + 1);
  end;
  InternalDrawTransBmpRect(FMask[Pos].canvas, 0, 0, Bmp, TransparentColor, true);
end;


end.
