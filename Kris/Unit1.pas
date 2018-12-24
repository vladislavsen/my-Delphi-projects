unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Unit2, XPMan, ComCtrls, jpeg, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    XPManifest1: TXPManifest;
    RichEdit1: TRichEdit;
    Image1: TImage;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit3, Unit4, Unit5;

{$R *.dfm}

procedure TForm1.Button2Click(Sender: TObject);
begin
 ShowMessage('Сорян, но тут без вариантов :D');
 Form1.Hide;
 Form2.showmodal;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 Form1.Hide;
 Form2.showmodal;
 
end;

end.
