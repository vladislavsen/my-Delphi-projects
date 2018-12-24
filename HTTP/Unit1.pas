unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP;

type
  TForm1 = class(TForm)
    IdHTTP1: TIdHTTP;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  MS : TMemoryStream;
implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  MS := TMemoryStream.Create;
  try
  idHttp1 := TIdHTTP.Create(nil);
  try
    idHttp1.Get('http://meteoinfo.by/radar/RAKD/RAKD_latest.png', MS);
  finally
    idHttp1.Free;
  end;
  finally
    MS.Free;
  end;

end;
end.


