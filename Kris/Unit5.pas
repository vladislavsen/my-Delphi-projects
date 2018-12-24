unit Unit5;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jpeg, ExtCtrls;

type
  TForm5 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Image1: TImage;
    Image2: TImage;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses Unit4, Unit3, Unit2, Unit6;

{$R *.dfm}

procedure TForm5.Button1Click(Sender: TObject);
begin
ShowMessage('Если че, используй ESC для выхода (:');
form6.show;
form5.Close;

end;

end.
