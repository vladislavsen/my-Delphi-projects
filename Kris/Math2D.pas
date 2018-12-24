unit Math2D;

interface

uses
  Windows;

var
  MaxAngle: word;
  CosTable: array of real;
  SinTable: array of real;

  FiveDegrees: real;

type
  TRotate = (rX, rY, rXY);

type
  TVertex = record
    X, Y: real;
end;

type
  TVector = record
    a, b: TVertex;
end;

type TCross = record
  Vector: TVector;
  Length, Angle: real;
  Vertex: TVertex;
  IsCross: boolean;
end;

type TTripleCross = record
  Cross1, Cross2, Cross3: TCross;
  NearCross: byte;
end;

procedure SetMaxAngle(Angle: integer);
procedure InitSinCosTables;
procedure CalcSinCosTables;
procedure MovePoint(var X, Y: real; MoveAngle, MoveSpeed: Real);
function RotatePointX(X: real; CenterX: real; Angle: word): real;
function RotatePointY(Y: real; CenterY: real; Angle: word): real;
function VecLen(x1, y1, x2, y2: real): real;
function Vertex(rX, rY: real): TVertex;
function PointIsPolygon(X, Y: integer; Polygon: array of TPoint; NumVertex: longint): boolean;
function VectorsCrossVertex(Vec1, Vec2: TVector): TVertex;
function DotProduct(Vec1, Vec2: TVector): real;
function Dec2(X, Min: integer): integer;
function Inc2(X, Max: integer): integer;

implementation

var
  CircLength: real;

procedure SetMaxAngle(Angle: integer);
begin
  MaxAngle := Angle;
  CircLength := 2 * Pi / MaxAngle;
end;

procedure InitSinCosTables;
begin
  SetLength(CosTable, MaxAngle + 1);
  SetLength(SinTable, MaxAngle + 1);
end;

procedure CalcSinCosTables;
var
  i: integer;
begin
  for i := 0 to MaxAngle do
  begin
    CosTable[i] := cos(i * CircLength);
    SinTable[i] := sin(i * CircLength);
  end;
end;

procedure MovePoint(var X, Y: real; MoveAngle, MoveSpeed: Real);
begin
  X := X + CosTable[round(MoveAngle)] * MoveSpeed;
  Y := Y + SinTable[round(MoveAngle)] * MoveSpeed;
end;

function RotatePoint(Coord: real; CenterCoord: real; Angle: word; RotType: TRotate): real;
var
  CosA, SinA: real;
begin
  CosA := CosTable[Angle];
  SinA := SinTable[Angle];
  case RotType of
    rX: Result := CosA * VecLen(CenterCoord, 0, Coord, 0) + CenterCoord;
    rY: Result := SinA * VecLen(0, CenterCoord, 0, Coord) + CenterCoord;
  end;
end;

function RotatePointX(X: real; CenterX: real; Angle: word): real;
begin
  Result := RotatePoint(X, CenterX, Angle, rX);
end;

function RotatePointY(Y: real; CenterY: real; Angle: word): real;
begin
  Result := RotatePoint(Y, CenterY, Angle, rY);
end;

function VecLen(x1, y1, x2, y2: real): real;
begin
  Result := sqrt(sqr(X2 - X1) + sqr(Y2 - Y1));
end;

function Vertex(rX, rY: real): TVertex;
begin
  with Result do
  begin
    X := rX;
    Y := rY;
  end;
end;

function PointIsPolygon(X, Y: integer; Polygon: array of TPoint; NumVertex: longint): boolean;
var
  TempVertex: array of TPoint;
  i, NumTempVertex, CurrTempVertex,
  Counter,
  TempX,
  X1, Y1, X2, Y2, LastVertex: longint;
  TempY: real;
  EndVertex: boolean;
begin
  Result := false;
  NumTempVertex := 0;
  CurrTempVertex := 0;
  Counter := 0;
  EndVertex := false;
  LastVertex := 0;
  SetLength(TempVertex, NumVertex);
  {Сортировка векторов}
  for i := 0 to NumVertex - 1 do
  begin
    X1 := Polygon[i].X;
    Y1 := Polygon[i].Y;
    X2 := Polygon[i + 1].X;
    Y2 := Polygon[i + 1].Y;
    if (((i - LastVertex <> 2) or
    ((Polygon[LastVertex + 1].Y <> Y) or (Polygon[LastVertex + 2].Y <> Y))) and
    ((Polygon[LastVertex + 1].X <> Polygon[i].X) or
    (Polygon[LastVertex + 1].Y <> Polygon[i].Y))) or
    (not EndVertex) then
      if (Y1 <> Y2) and
      ((X1 <= X) or (X2 <= X)) and
      (((Y1 <= Y) and (Y2 >= Y)) or
      ((Y1 >= Y) and (Y2 <= Y))) then
      begin
        {Сортировка по X}
        if X1 < X2 then
        begin
          TempVertex[NumTempVertex].X := X1;
          TempVertex[NumTempVertex].Y := Y1;
          TempVertex[NumTempVertex + 1].X := X2;
          TempVertex[NumTempVertex + 1].Y := Y2;
        end else
        begin
          TempVertex[NumTempVertex].X := X2;
          TempVertex[NumTempVertex].Y := Y2;
          TempVertex[NumTempVertex + 1].X := X1;
          TempVertex[NumTempVertex + 1].Y := Y1;
        end;
        {Не является ли Y равным Y конца или начала вектора}
        if (Y1 = Y) or (Y2 = Y) then
          EndVertex := true;
        LastVertex := i;
        inc(NumTempVertex, 2);
      end;
  end;
  if NumTempVertex = 0 then exit;
  while CurrTempVertex < NumTempVertex do
  begin
    {Не является ли вектор вертикальным}
    if TempVertex[CurrTempVertex].X <> TempVertex[CurrTempVertex + 1].X then
    begin
      TempY := (TempVertex[CurrTempVertex + 1].X - TempVertex[CurrTempVertex].X) /
        (TempVertex[CurrTempVertex + 1].Y - TempVertex[CurrTempVertex].Y);
      if TempVertex[CurrTempVertex].X < TempVertex[CurrTempVertex + 1].X then
        TempX := TempVertex[CurrTempVertex].X + abs(round(TempY * (TempVertex[CurrTempVertex].Y - Y)))
      else
        TempX := TempVertex[CurrTempVertex].X - abs(round(TempY * (TempVertex[CurrTempVertex].Y - Y)));
      if X >= TempX then inc(Counter);
    end else
      if X >= TempVertex[CurrTempVertex].X then inc(Counter);
    inc(CurrTempVertex, 2);
  end;
  if Odd(Counter) then Result := true;
end;

function VectorsCrossVertex(Vec1, Vec2: TVector): TVertex;
var
  X1, Y1, X2, Y2, X3, Y3, X4, Y4,
  Ua, Ub, n: real;
begin
  Result.X := 0;
  Result.Y := 0;
  X1 := Vec1.a.X;
  Y1 := Vec1.a.Y;
  X2 := Vec1.b.X;
  Y2 := Vec1.b.Y;
  X3 := Vec2.a.X;
  Y3 := Vec2.a.Y;
  X4 := Vec2.b.X;
  Y4 := Vec2.b.Y;
  n := (Y4 - Y3) * (X2 - X1) - (X4 - X3) * (Y2 - Y1);
  if n = 0 then exit;
  Ua := ((X4 - X3) * (Y1 - Y3) - (Y4 - Y3) * (X1 - X3)) / n;
  Ub := ((X2 - X1) * (Y1 - Y3) - (Y2 - Y1) * (X1 - X3)) / n;
  if (Ua >= 0) and (Ua <= 1) and
  (Ub >= 0) and (Ub <= 1) then
  begin
    Result.X := X1 + Ua * (X2 - X1);
    Result.Y := Y1 + Ua * (Y2 - Y1);
  end;
end;

function DotProduct(Vec1, Vec2: TVector): real;
var
  Dot: real;
  X1, Y1, X2, Y2: real;
  n1, n2: real;
begin
  X1 := Vec1.a.X - Vec1.b.X;
  Y1 := Vec1.a.Y - Vec1.b.Y;
  X2 := Vec2.a.X - Vec2.b.X;
  Y2 := Vec2.a.Y - Vec2.b.Y;
  Dot := X1 * X2 + Y1 * Y2;
  n1 := X1 * X1 + Y1 * Y1;
  n2 := X2 * X2 + Y2 * Y2;
  if n1 * n2 = 0 then
    Result := Dot
  else
    Result := Dot / sqrt(n1 * n2);
end;

function Dec2(X, Min: integer): integer;
begin
  if X > Min then
    dec(X);
  Result := X;
end;

function Inc2(X, Max: integer): integer;
begin
  if X < Max then
    inc(X);
  Result := X;
end;


end.
